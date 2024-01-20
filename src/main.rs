use std::{collections::HashMap, path::Path};

use backend::{
    codegen::CompiledFunction,
    function::FunctionSignature,
    ir::{IrCompiler, IrFunction},
    type_pool::TypePool,
    types::SculkType,
    validate::{Validator, ValidatorOutput}, DefaultBackend, Backend,
};
use data::ResourceLocation;
use error::CompileError;
use parser::Parser;

use crate::backend::codegen::CodeGen;

mod backend;
mod data;
mod error;
mod lexer;
mod parser;

#[derive(argh::FromArgs)]
/// Configuration for the compiler.
pub struct Config {
    /// a list of paths to sculk files that should be compiled
    #[argh(positional)]
    files: Vec<String>,

    /// the name of the datapack to generate
    #[argh(option, short = 'n', default = "String::from(\"pack\")")]
    pack: String,

    /// the backend to use for compilation
    #[argh(option, short = 'b', default = "String::from(\"default\")")]
    backend: String,

    #[argh(switch, short = 'd')]
    /// dumps sculk's ir to a file for debugging purposes
    dump_ir: bool,
}

fn main() {
    let config: Config = argh::from_env();

    if config.files.is_empty() {
        println!("no files to compile");
        return;
    }

    let mut errors = Vec::new();

    for file in &config.files {
        let (info, result) = compile_file(&config, file);

        if let Err(errs) = result {
            errors.push((file, errs, info));
        }
    }

    for (file, errs, info) in errors {
        let info = info.expect("info should be present if there are errors");
        let file_content = match std::fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                println!("failed to read file: {}", err);
                return;
            }
        };

        for err in errs {
            error::print_report(file, &file_content, &err, &info.types, &info.signatures);
        }
    }
}

fn compile_file(
    config: &Config,
    path: &str,
) -> (
    Option<Info>,
    Result<(), Vec<CompileError>>,
) {
    let file_content = match std::fs::read_to_string(path) {
        Ok(content) => content,
        Err(err) => {
            println!("failed to read file: {}", err);
            return (None, Err(Vec::new()));
        }
    };

    let mut errors = Vec::new();

    let parser = Parser::new(&file_content);
    let parser_output = parser.parse();

    errors.extend(parser_output.errors.into_iter().map(CompileError::Parse));

    let validator = Validator::new(config.pack.clone());
    let validator_output = validator.validate_program(&parser_output.ast);

    errors.extend(
        validator_output
            .errors
            .into_iter()
            .map(CompileError::Validate),
    );

    if !errors.is_empty() {
        return (
            Some(Info {
                types: validator_output.types,
                signatures: validator_output.global_functions,
            }),
            Err(errors),
        );
    }

    let mut ir_compiler = IrCompiler::new(
        config.pack.clone(),
        validator_output.types,
        validator_output.global_functions,
        validator_output.tags
    );

    ir_compiler.visit_program(parser_output.ast.as_program());

    let (signatures, types, tags, funcs) = ir_compiler.dissolve();

    if config.dump_ir {
        dump_ir(&config, &types, &signatures, &funcs);
    }

    match config.backend.as_str() {
        "default" => DefaultBackend::compile(config, &funcs),
        _ => {
            println!("unknown backend: {}", config.backend);
            return (None, Err(Vec::new()));
        }
    }

    (Some(Info { types, signatures }), Ok(()))
}

fn dump_ir(config: &Config, types: &TypePool, signatures: &HashMap<ResourceLocation, FunctionSignature>, funcs: &[IrFunction]) {
    let mut s = String::new();

    for func in funcs {
        s.push_str(
            format!(
                "fn {}",
                func.objective(),
            ).as_str()
        );

        s.push_str("\n");

        for instr in func.body() {
            s.push_str(format!("    {}\n", instr).as_str());
        }
    }

    if let Err(err) = std::fs::write("ir_dump.txt", s) {
        println!("failed to dump ir: {}", err);
    }
}

struct Info {
    types: TypePool,
    signatures: HashMap<ResourceLocation, FunctionSignature>,
}
