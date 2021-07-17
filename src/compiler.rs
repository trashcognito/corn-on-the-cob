use inkwell::targets::{TargetMachine, FileType, TargetTriple, Target, RelocMode, CodeModel, InitializationConfig};
use inkwell::context::Context;
use inkwell::builder::Builder;
use json::{JsonValue, Array};
use std::fs::File;
use std::io::Read;
use inkwell::{OptimizationLevel, AddressSpace};
use inkwell::basic_block::BasicBlock;
use inkwell::types::{AnyType, AnyTypeEnum, FunctionType, BasicTypeEnum, BasicType};
use std::collections::HashMap;
use inkwell::values::{FunctionValue, GlobalValue};
use inkwell::module::{Module, Linkage};
use std::ops::Index;

pub struct CompilerObject<'a> {
    target_machine: TargetMachine,
    context: Context,
    builder: Builder<'a>,
    tree: JsonValue,
    module: Module<'a>
}
fn get_cpu(from: &str) -> &str {
    let things = t.split("-").collect::<Vec<&str>>();
    match things[0] {
        "x86_64" => "x86-64",
        n => n
    }
}
impl CompilerObject {
    pub fn new(infile: &str, outfile: &str, target: &str) -> Self {
        let ctx = Context::create();
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
        CompilerObject {
            target_machine,
            context: ctx,
            builder,
            tree: code,
            module
        }
    }
    fn parse_type(&self, t: &str) -> Result<AnyTypeEnum, &str> {
        match self.parse_type_basic(t) {
            Ok(t) => {Ok(t.as_any_type_enum())},
            Err => match t {
                "none" => Ok(self.context.void_type().as_any_type_enum()),
                _ => Err("Could not parse type!")
            }
        }
    }
    fn parse_type_basic(&self, t: &str) -> Result<BasicTypeEnum, &str> {
        match t {
            "none" => Err("Bad void type!"),
            "i0" => Ok(self.context.bool_type().as_basic_type_enum()),
            "i3" => Ok(self.context.i8_type().as_basic_type_enum()),
            "i4" => Ok(self.context.i16_type().as_basic_type_enum()),
            "i5" => Ok(self.context.i32_type().as_basic_type_enum()),
            "i6" => Ok(self.context.i64_type().as_basic_type_enum()),
            "i7" => Ok(self.context.i128_type().as_basic_type_enum()),

            "number" => Ok(self.context.i64_type().as_basic_type_enum()),
            a => {
                match a[0] {
                    'i' => {
                        let e = a[1..a.len()].parse::<u32>().expect("Could not parse type");
                        let val = 2.pow(e);
                        Ok(self.context.custom_width_int_type(val as u32).as_basic_type_enum())
                    }
                    _ => {Err("Could not parse type!")}
                }
            }
        }
    }
    pub fn compile(&self) {
        let body = &self.tree["globals"];
        let mut globals:HashMap<&str, GlobalValue> = HashMap::new();
        //Pass 1 - parse global values, get definitions
        for (key, entry) in body.entries() {
            match entry["act"].as_str().expect("Invalid global entry!") {
                "fundef" => {
                    let ret_type = self.parse_type(entry["return"]
                        .as_str()
                        .expect("Invalid function definition!")
                    )
                        .expect("Could not parse return type!");
                    let args = match &entry["args"] {
                        JsonValue::Array(a) => {Ok(a)}
                        _ => {Err}
                    }.expect("Invalid args!");
                    let arg_types:Vec<BasicTypeEnum> = args.iter().map(|t| -> BasicTypeEnum {
                        self.parse_type_basic(t.as_str().expect("Could not parse arg!")).expect("Could not parse arg type!")
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
                    globals.insert(key, function);
                }
                "glbdef" => {
                    let var_type = self.parse_type(entry["type"]
                        .as_str().expect("Could not get type!")).expect("Could not parse type!");
                    let var = self.module.add_global(var_type, Some(AddressSpace::Global), entry["name"].as_str().expect("Could not get variable name!"));
                    globals.insert(key, var);

                }
            }
        }
    }
}