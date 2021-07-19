use inkwell::targets::{TargetMachine, FileType, TargetTriple, Target, RelocMode, CodeModel, InitializationConfig};
use inkwell::context::Context;
use inkwell::builder::Builder;
use json::{JsonValue, Array};
use std::fs::File;
use std::io::Read;
use inkwell::{OptimizationLevel, AddressSpace, IntPredicate};
use inkwell::basic_block::BasicBlock;
use inkwell::types::{AnyType, AnyTypeEnum, FunctionType, BasicTypeEnum, BasicType, IntType};
use std::collections::{HashMap, VecDeque};
use inkwell::values::{FunctionValue, GlobalValue, AnyValue, AnyValueEnum, BasicValueEnum, BasicValue, IntValue, FloatValue};
use inkwell::module::{Module, Linkage};
use std::ops::Index;
use std::hash::Hash;
use std::borrow::Borrow;

pub struct CompilerObject<'a> {
    target_machine: TargetMachine,
    context: &'a mut Context,
    builder: Builder<'a>,
    tree: JsonValue,
    module: Module<'a>,
}
fn get_cpu(from: &str) -> &str {
    let things = from.split("-").collect::<Vec<&str>>();
    match things[0] {
        "x86_64" => "x86-64",
        n => n
    }
}
struct LocalContextTree<'a> {
    outer: Option<&'a LocalContextTree<'a>>,
    context: HashMap<&'a str, BasicValueEnum<'a>>
}
struct CompleteContext<'l, 'g> {
    local: LocalContextTree<'l>,
    global: &'g HashMap<&'g str, GlobalValue<'g>>
}
impl<'a> CompilerObject<'a> {
    pub fn new(infile: &str, outfile: &str, target: &str, ctx: &'a mut Context) -> Self {
        Target::initialize_all(&InitializationConfig::default());
        let module = ctx.create_module("text");
        let triple_str = target;
        let triple = TargetTriple::create(triple_str);
        let target =  Target::from_triple(&triple).expect("Could not get target!");
        let target_machine = target.create_target_machine(
            &triple,
            get_cpu(triple_str),
            //TODO: Parse features from user
            "",
            //TODO: Parse optimization level from user
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default
        ).expect("Could not get target machine!");
        let output_filename = outfile;
        target_machine.write_to_file(&module, FileType::Object, output_filename.as_ref());
        let input_filename = infile;
        let mut data = String::new();
        {
            let mut infile = File::open(input_filename).expect("Could not open file!");
            infile.read_to_string(&mut data).expect("Could not read file to string!");
        };
        let builder = ctx.create_builder();
        let code = json::parse(&*data).expect("Could not parse JSON!");
        let mut globals:HashMap<&str, GlobalValue> = HashMap::new();

        CompilerObject {
            target_machine,
            context: ctx,
            builder,
            tree: code,
            module,
        }
    }
    fn parse_type(&self, t: &JsonValue, addr:AddressSpace, typelen: Option<usize>) -> Result<AnyTypeEnum, &str> {
        match t {
            JsonValue::String(s) => match s.as_str() {
                "none" => Ok(self.context.void_type().as_any_type_enum()),
                "i0" => Ok(self.context.bool_type().as_any_type_enum()),
                "i3" => Ok(self.context.i8_type().as_any_type_enum()),
                "i4" => Ok(self.context.i16_type().as_any_type_enum()),
                "i5" => Ok(self.context.i32_type().as_any_type_enum()),
                "i6" => Ok(self.context.i64_type().as_any_type_enum()),
                "i7" => Ok(self.context.i128_type().as_any_type_enum()),
                "str" => Ok(self.context.i8_type().array_type(typelen.expect("Could not get string size") as u32).as_any_type_enum()),
                "number" => Ok(self.context.i64_type().as_any_type_enum()),
                a => {
                    match a.chars().next().expect("Cannot get next char") {
                        'i' => {
                            let e = a[1..a.len()].parse::<u32>().expect("Could not parse type");
                            let val = 2_isize.pow(e);
                            Ok(self.context.custom_width_int_type(val as u32).as_any_type_enum())
                        }
                        _ => {Err("Could not parse type!")}
                    }
                }
            }
            JsonValue::Array(a) => {
                let inner = &a[0];
                let typelen = match &inner["type"] {
                    JsonValue::Null => {None}
                    JsonValue::Short(_) => {None}
                    JsonValue::String(s) => {Some(s.as_str().len())}
                    JsonValue::Number(_) => {None}
                    JsonValue::Boolean(_) => {None}
                    JsonValue::Object(_) => {None}
                    JsonValue::Array(a) => {Some(a.len())}
                };
                match inner["act"].as_str().expect("Could not get action value!") {
                    "type" => self.parse_type(&inner["type"], addr, typelen),
                    "statement" => match inner["name"].as_str().expect("Could not parse name during type determination!") {
                        "ptrvar" => match self.parse_type_basic(&inner["type"], addr, None) {
                            Ok(o) => {Ok(o.ptr_type(addr).as_any_type_enum())}
                            Err(e) => {Err(e)}
                        }
                        _ => Err("Unknown action!")
                    }
                    _ => Err("Invalid act while parsing type!")
                }
            }
            _ => Err("Unknown type entry!")
        }
    }
    fn parse_type_basic(&self, t: &JsonValue, addr:AddressSpace, typelen: Option<usize>) -> Result<BasicTypeEnum, &str> {
        match t {
            JsonValue::String(s) => match s.as_str() {
                "none" => Err("Bad void type!"),
                "i0" => Ok(self.context.bool_type().as_basic_type_enum()),
                "i3" => Ok(self.context.i8_type().as_basic_type_enum()),
                "i4" => Ok(self.context.i16_type().as_basic_type_enum()),
                "i5" => Ok(self.context.i32_type().as_basic_type_enum()),
                "i6" => Ok(self.context.i64_type().as_basic_type_enum()),
                "i7" => Ok(self.context.i128_type().as_basic_type_enum()),
                "number" => Ok(self.context.i64_type().as_basic_type_enum()),
                "str" => Ok(self.context.i8_type().array_type(typelen.expect("Could not get string size") as u32).as_basic_type_enum()),
                a => {
                    match a.chars().next().expect("Could not get next char!") {
                        'i' => {
                            let e = a[1..a.len()].parse::<u32>().expect("Could not parse type");
                            let val = 2_isize.pow(e);
                            Ok(self.context.custom_width_int_type(val as u32).as_basic_type_enum())
                        }
                        _ => {Err("Could not parse type!")}
                    }
                }
            }
            JsonValue::Array(a) => {
                let inner = &a[0];
                let typelen = match &inner["type"] {
                    JsonValue::Null => {None}
                    JsonValue::Short(_) => {None}
                    JsonValue::String(s) => {Some(s.as_str().len())}
                    JsonValue::Number(_) => {None}
                    JsonValue::Boolean(_) => {None}
                    JsonValue::Object(_) => {None}
                    JsonValue::Array(a) => {Some(a.len())}
                };
                match inner["act"].as_str().expect("Could not get action value!") {
                    "type" => self.parse_type_basic(&inner["type"], addr, typelen),
                    "statement" => match inner["name"].as_str().expect("Could not parse name during type determination!") {
                        "ptrvar" => match self.parse_type_basic(&inner["type"], addr, None) {
                            Ok(o) => {Ok(o.ptr_type(addr).as_basic_type_enum())}
                            Err(e) => {Err(e)}
                        },
                        _ => Err("Unknown action!")
                    }
                    _ => Err("Invalid act while parsing type!")
                }
            },
            _ => Err("Unknown type entry!")
        }
    }
    fn parse_type_wrapper(&self, t: &JsonValue, a:AddressSpace) -> Result<AnyTypeEnum, &str> {
        let typelen = match &t["type"] {
            JsonValue::Null => {None}
            JsonValue::Short(_) => {None}
            JsonValue::String(s) => {Some(s.as_str().len())}
            JsonValue::Number(_) => {None}
            JsonValue::Boolean(_) => {None}
            JsonValue::Object(_) => {None}
            JsonValue::Array(a) => {Some(a.len())}
        };
        self.parse_type(&t["type"], a, typelen)
    }
    fn parse_type_basic_wrapper(&self, t: &JsonValue, addr:AddressSpace) -> Result<BasicTypeEnum, &str> {
        let typelen = match &t["type"] {
            JsonValue::Null => {None}
            JsonValue::Short(_) => {None}
            JsonValue::String(s) => {Some(s.as_str().len())}
            JsonValue::Number(_) => {None}
            JsonValue::Boolean(_) => {None}
            JsonValue::Object(_) => {None}
            JsonValue::Array(a) => {Some(a.len())}
        };
        self.parse_type_basic(&t["type"], addr, typelen)
    }
    fn get_value<'ct>(&'ct self, target: &JsonValue, context: &'ct CompleteContext<'a, 'ct>) -> BasicValueEnum<'ct> {
        match target["act"].as_str().expect("Could not parse value!") {
            "const" => {
                match &self.parse_type_basic_wrapper(&target["type"], AddressSpace::Generic).expect("Could not get value!") {
                    BasicTypeEnum::ArrayType(a) => {
                        //assume string
                        //since const cannot be an array
                        self.context.i8_type().const_array(match &target["val"] {
                        JsonValue::String(a) => {
                            let res:Vec<IntValue> = a.as_str()
                                .as_bytes()
                                .iter()
                                .map(|v| {
                                    self.context.i8_type().const_int(*v as u64, false)
                                }
                                ).collect();
                            res.as_slice()
                        },
                        _ => panic!("Could not parse array constant!")
                    }
                    ).as_basic_value_enum()}

                    BasicTypeEnum::FloatType(f) => {
                        f.const_float(target["val"].as_f64().expect("Invalid value")).as_basic_value_enum()
                    }
                    BasicTypeEnum::IntType(i) => {i.const_int(target["val"].as_u64().expect("Invalid value"), false).as_basic_value_enum()}
                    BasicTypeEnum::PointerType(p) => {panic!("Cannot get pointer as const!")}
                    //TODO: Add structs
                    BasicTypeEnum::StructType(s) => {panic!("Struct functionality not added yet!")}
                    BasicTypeEnum::VectorType(v) => {panic!("Vector type not supported!")}
                }
            }
            "oper" => {
                match target["val"].as_str().expect("Invalid operand!") {
                    "<" => {
                        self.builder.build_int_compare(
                            IntPredicate::SLT,
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_CMP_LT"
                        ).as_basic_value_enum()
                    }
                    ">" => {
                        self.builder.build_int_compare(
                            IntPredicate::SGT,
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_CMP_GT"
                        ).as_basic_value_enum()
                    }
                    ">=" => {
                        self.builder.build_int_compare(
                            IntPredicate::SGE,
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_CMP_GE"
                        ).as_basic_value_enum()
                    }
                    "<=" => {
                        self.builder.build_int_compare(
                            IntPredicate::SLE,
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_CMP_LE"
                        ).as_basic_value_enum()
                    }
                    "+" => {
                        self.builder.build_int_add(
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_ADD"
                        ).as_basic_value_enum()
                    }
                    "-" => {
                        self.builder.build_int_sub(
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_SUB"
                        ).as_basic_value_enum()
                    }
                    "/" => {
                        self.builder.build_int_signed_div(
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_DIV"
                        ).as_basic_value_enum()
                    }
                    "*" => {
                        self.builder.build_int_mul(
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_MUL"
                        ).as_basic_value_enum()
                    }
                    "%" => {
                        //spaghetti modulo
                        let arg0 = self.get_value(&target["args"][0], context).into_int_value();
                        let arg1 = self.get_value(&target["args"][1], context).into_int_value();
                        let modulo_div = self.builder.build_int_signed_div(
                            arg0,
                            arg1,
                            "OPR_MOD_DIV1"
                        );
                        let modulo_righthand = self.builder.build_int_mul(modulo_div, arg1, "OPR_MOD_MUL2");
                        self.builder.build_int_sub(arg0, modulo_righthand, "OPR_MOD_SUB3").as_basic_value_enum()
                    }
                    "&" => {
                        self.builder.build_and(
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_AND"
                        ).as_basic_value_enum()
                    }
                    "|" => {
                        self.builder.build_or(
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_OR"
                        ).as_basic_value_enum()
                    }
                    "^" => {
                        self.builder.build_xor(
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_XOR"
                        ).as_basic_value_enum()
                    }
                    "==" => {
                        self.builder.build_int_compare(
                            IntPredicate::EQ,
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_EQ"
                        ).as_basic_value_enum()
                    }
                    "!=" => {
                        self.builder.build_int_compare(
                            IntPredicate::NE,
                            self.get_value(&target["args"][0], context).into_int_value(),
                            self.get_value(&target["args"][1], context).into_int_value(),
                            "OPR_NE"
                        ).as_basic_value_enum()
                    }
                    "||" => {
                        let arg0 = self.builder.build_bitcast(self.get_value(&target["args"][0], context), self.context.bool_type(), "EVAL_ARG0").into_int_value();
                        let arg1 = self.builder.build_bitcast(self.get_value(&target["args"][1], context), self.context.bool_type(), "EVAL_ARG1").into_int_value();
                        self.builder.build_or(arg0, arg1, "OPR_BOOL_OR").as_basic_value_enum()
                    }
                    "&&" => {
                        let arg0 = self.builder.build_bitcast(self.get_value(&target["args"][0], context), self.context.bool_type(), "EVAL_ARG0").into_int_value();
                        let arg1 = self.builder.build_bitcast(self.get_value(&target["args"][1], context), self.context.bool_type(), "EVAL_ARG1").into_int_value();
                        self.builder.build_and(arg0, arg1, "OPR_BOOL_AND").as_basic_value_enum()
                    }
                    _ => panic!("Could not parse comparison!")
                }
            }
            "call" => {
                let fun = context.global[target["name"].as_str().expect("Could not get function name!")].as_any_value_enum().into_function_value();
                let mut argvector:Vec<BasicValueEnum> = vec!();
                for arg in target["args"].members() {
                    argvector.push(self.get_value(arg, context));
                }
                let val = self.builder.build_call(fun.clone(), argvector.as_slice(), "value_call")
                    .try_as_basic_value()
                    .left()
                    .expect("Could not get function value!");
                return val;
            }
            "getvar" => {
                let var_name = target["name"].as_str().expect("Could not get variable name");
                let var_ptr = self.dereference_value(var_name, context)
                    .into_pointer_value();
                self.builder.build_load(var_ptr, format!("GET_VAR {}", var_name).as_str())
            }
            "varop" => {
                let var_name = target["var"].as_str().expect("Could not get variable name");
                match target["name"].as_str().expect("Could not get operation name!") {
                    "ptr" => self.dereference_value(var_name, context),
                    unknown => panic!("Unknown varop!")
                }
            }
            "statement" => {
                match target["name"].as_str().expect("Could not get statement name!") {
                    "not" => {
                        let val = self.get_value(target["args"].members().next().expect("Could not get arg for not!"), context).into_int_value();
                        self.builder.build_not(val, "OPR_NOT").as_basic_value_enum()
                    }
                    unknown => panic!("Invalid statement value {}", unknown)
                }
            }
            _ => panic!("Invalid act!")
        }
    }
    fn dereference_value<'b>(&self, name:&str, complete_context: &'b CompleteContext<'a, 'b>) -> BasicValueEnum<'b> {
        let mut iterator = &complete_context.local;
        loop {
            let ctx = &iterator.context;
            if ctx.contains_key(name) {
                return ctx[name];
            }
            match iterator.outer {
                Some(i) => iterator = i,
                None => break
            }
        }
        if complete_context.global.contains_key(name) {
            return complete_context.global[name].as_basic_value_enum();
        }
        panic!("Undefined value \"{}\"!", name);
    }
    fn build_body<'c>(&mut self, function_value:&FunctionValue, complete_context:CompleteContext<'a, 'c>, body:&JsonValue, depth: usize) {
        let mut current_context = complete_context.local.context;
        match body {
            JsonValue::Object(o) => {
                //this is a singular command
                match body["act"].as_str().expect("Could not get act!") {
                    "vardef" => {
                        let var_name = body["name"].as_str().expect("Could not get variable name!");
                        let init_value = self.get_value(body["args"].members().next().expect("No initializer value given!"), &complete_context);
                        let init_type = init_value.get_type();
                        let var_addr = self.builder.build_alloca(init_type, format!("ALLOC_VAR {}", var_name).as_str());
                        self.builder.build_store(var_addr, init_value);
                        current_context[var_name] = var_addr.as_basic_value_enum();
                    }
                    "call" => {
                        let fun = complete_context.global[body["name"].as_str().expect("Could not get function name!")].as_any_value_enum().into_function_value();
                        let mut argvector:Vec<BasicValueEnum> = vec!();
                        for arg in body["args"].members() {
                            argvector.push(self.get_value(arg, &complete_context));
                        }
                        self.builder.build_call(fun, argvector.as_slice(), "value_call"); //discard return value
                    }
                    "varset" => {
                        let var_name = body["name"].as_str().expect("Could not get variable name!");
                        let set_value = self.get_value(body["args"].members().next().expect("No initializer value given!"), &complete_context);
                        let var_addr = self.dereference_value(var_name, &complete_context).into_pointer_value();
                        self.builder.build_store(var_addr, set_value);
                    }
                    "statement" => {
                        match body["name"].as_str().expect("Could not get statement type!") {
                            "if" => {
                                let if_start = self.context.append_basic_block(*function_value, "if_start");
                                let if_body = self.context.append_basic_block(*function_value, "if_body");
                                let if_end = self.context.append_basic_block(*function_value, "if_end");
                                self.builder.build_unconditional_branch(if_start);
                                self.builder.position_at_end(if_start);
                                self.builder.build_conditional_branch(
                                    self.get_value(body["args"].members().next().expect("Could not get while condition!"), &complete_context).into_int_value(),
                                    if_body,
                                    if_end
                                );
                                self.builder.position_at_end(if_body);
                                self.build_body(
                                    function_value,
                                    CompleteContext {
                                        local: LocalContextTree {
                                            outer: Some(&complete_context.local),
                                            context: Default::default()
                                        },
                                        global: complete_context.global
                                    },
                                    &body["body"],
                                    depth+1
                                );
                                self.builder.position_at_end(if_end);
                            }
                            "while" => {
                                let while_start = self.context.append_basic_block(*function_value, "while_start");
                                let while_body = self.context.append_basic_block(*function_value, "while_body");
                                let while_end = self.context.append_basic_block(*function_value, "while_end");
                                self.builder.build_unconditional_branch(while_start);
                                self.builder.position_at_end(while_start);
                                self.builder.build_conditional_branch(
                                    self.get_value(body["args"].members().next().expect("Could not get while condition!"), &complete_context).into_int_value(),
                                    while_body,
                                    while_end
                                );
                                self.builder.position_at_end(while_body);
                                self.build_body(
                                    function_value,
                                    CompleteContext {
                                        local: LocalContextTree {
                                            outer: Some(&complete_context.local),
                                            context: Default::default()
                                        },
                                        global: complete_context.global
                                    },
                                    &body["body"],
                                    depth+1
                                );
                                self.builder.build_unconditional_branch(while_start);
                                self.builder.position_at_end(while_end);
                            }
                            "return" => {
                                self.builder.build_return(match body["args"].members().next() {
                                    None => None,
                                    Some(v) => Some(&self.get_value(v, &complete_context) as &dyn BasicValue)
                                });
                            }
                            unknown => panic!("Unknown statement type {} !", unknown)
                        }
                    }
                    _ => panic!("Unknown act!")
                }
            }
            JsonValue::Array(a) => {
                //this is a list of commands
                for command in a {
                    self.build_body(
                        function_value,
                        CompleteContext {
                            local: LocalContextTree {
                                outer: Some(&complete_context.local),
                                context: Default::default()
                            },
                            global: complete_context.global
                        },
                        &body["body"],
                        depth+1
                    );
                }
            }
            _ => panic!("Invalid body!")
        }
    }
    pub fn compile(&mut self) {
        let body = &self.tree["globals"];
        let mut global:HashMap<&str, GlobalValue> = HashMap::new();
        //Pass 1 - parse global values, get definitions
        for (key, entry) in body.entries() {
            match entry["act"].as_str().expect("Invalid global entry!") {
                "fundef" => {
                    let ret_type = self.parse_type_wrapper(&entry["return"],AddressSpace::Generic
                    )
                        .expect("Could not parse return type!");
                    let args = match &entry["args"] {
                        JsonValue::Array(a) => {Ok(a)}
                        _ => {Err("")}
                    }.expect("Invalid args!");
                    let arg_types:Vec<BasicTypeEnum> = args.iter().map(|t| -> BasicTypeEnum {
                        self.parse_type_basic_wrapper(t, AddressSpace::Generic).expect("Could not parse arg type!")
                    }).collect();
                    let arg_array = arg_types.as_slice();
                    let fn_type = match ret_type {
                        AnyTypeEnum::ArrayType(t) => Ok(t.fn_type(arg_array, false)),
                        AnyTypeEnum::FloatType(t) => Ok(t.fn_type(arg_array, false)),
                        AnyTypeEnum::FunctionType(t) => Err("Invalid return type!"),
                        AnyTypeEnum::IntType(t) => Ok(t.fn_type(arg_array, false)),
                        AnyTypeEnum::PointerType(t) => Ok(t.fn_type(arg_array, false)),
                        AnyTypeEnum::StructType(t) => Ok(t.fn_type(arg_array, false)),
                        AnyTypeEnum::VectorType(t) => Ok(t.fn_type(arg_array, false)),
                        AnyTypeEnum::VoidType(t) => Ok(t.fn_type(arg_array, false)),
                    }.expect("Could not create function type!");
                    let function = self.module.add_function(entry["name"].as_str().expect("Could not get function name!"), fn_type, Some(Linkage::External))
                        .as_global_value();
                    global.insert(key.clone(), function);
                }
                "glbdef" => {
                    let var_type = self.parse_type_basic_wrapper(&entry["type"],AddressSpace::Generic).expect("Could not parse type!");
                    let var = self.module.add_global(var_type, Some(AddressSpace::Global), entry["name"].as_str().expect("Could not get variable name!"));
                    var.set_initializer(&self.get_value(&entry["body"], &CompleteContext {
                        local: LocalContextTree {
                            outer: None,
                            context: Default::default()
                        },
                        global: &global
                    }) as &dyn BasicValue);
                    global.insert(key.clone(), var);
                }
                unknown => {
                    println!("Warning, unknown global \"{}\" ignored", unknown);
                }
            }

        }
        //Pass 2 - codegen
        for (key, entry) in body.entries() {
            match entry["act"].as_str().expect("Could not parse entry!") {
                "fundef" => {

                    let function_block = self.context.append_basic_block(global[key].as_any_value_enum().into_function_value(), "entry");
                    self.builder.position_at_end(function_block);
                    self.build_body(
                        &global[key].as_any_value_enum().into_function_value(),
                        CompleteContext {
                            local: LocalContextTree {
                                outer: None,
                                context: Default::default()
                            },
                            global: &global
                        },
                        &entry["body"],
                        0
                    );
                }
                "glbdef" => {}
                unknown => {println!("Warning, unknown global \"{}\" ignored during codegen", unknown)}
            }
        }
    }
}