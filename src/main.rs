extern crate inkwell;
#[macro_use]
extern crate clap;
extern crate json;
use inkwell::{builder, context::Context, OptimizationLevel, AddressSpace};
use clap::Arg;
use inkwell::targets::{InitializationConfig, TargetMachine, Target, TargetTriple, RelocMode, CodeModel, FileType};
use std::fs::File;
use std::io::Read;
use std::collections::HashMap;
use inkwell::values::{GlobalValue, BasicValue, AnyValue};
use json::JsonValue;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum};
use inkwell::module::Linkage;
mod compiler;


fn main() {
    let default_targettriple = TargetMachine::get_default_triple().to_string();
    let tt_string = default_targettriple.as_str();
    let args = clap::app_from_crate!()
        .arg(Arg::with_name("infile")
            .index(1)
            .required(true)
            .help("input .json file name")
        )
        .arg(Arg::with_name("outfile")
            .required(true)
            .short("o")
            .help("output file name")
            )
        .arg(Arg::with_name("target")
            .required(false)
            .default_value(tt_string))
        .help("target triplet")
        .get_matches();
    {

        let mut compiler_object = compiler::CompilerObject::new(
            args.value_of("infile").expect("Could not get infile!"),
            args.value_of("target").expect("Could not get target!"),
        );
        compiler_object.compile(args.value_of("outfile").expect("Could not get outfile!"));
    }
    println!("Hello, world!");
}
