use std::path::Path;

use backend::{ir::IrCompiler, validate::Validator, types::SculkType};
use data::ResourceLocation;
use parser::Parser;

use crate::backend::codegen::CodeGen;

mod backend;
mod data;
mod lexer;
mod parser;

fn main() {
    let test = r#"
    struct BlockPos {
        x: int,
        y: int,
        z: int,
    }

    fn is_prime(n: int) -> bool {
        for let i = 2; i * i <= n; i += 1 {
            if n % i == 0 { return false; }
        }
        /say This is just a test;
        return true;
    }
    "#;

    let parser = Parser::new(test);
    let parser_output = parser.parse();

    parser_output
        .errs
        .iter()
        .for_each(|err| println!("{:#?}", err));

    let mut validator = Validator::new(String::from("pack"));
    validator.validate_program(&parser_output.ast);

    let (signatures, types, errs) = validator.dissolve();

    errs.iter().for_each(|err| println!("{:#?}", err));

    if !parser_output.errs.is_empty() || !errs.is_empty() {
        println!("Errors encountered, refusing to lower to IR");
        return;
    }

    let mut ir_compiler = IrCompiler::new(String::from("pack"), types, signatures);
    ir_compiler.visit_program(parser_output.ast.as_program());

    let (signatures, types, funcs) = ir_compiler.dissolve();
    let mut s = String::new();

    s.push_str("TYPE POOL\n\n");

    for ty in types.iter() {
        if !ty.get().is_struct() {
            continue;
        }

        let type_ref = ty.get();
        let def = type_ref.as_struct_def();

        s.push_str(format!("struct {} {{\n", def.name()).as_str());

        for field in def.fields() {
            s.push_str(format!("    {}: {},\n", field.name(), field.field_type()).as_str());
        }

        s.push_str("}\n\n");
    }

    for func in &funcs {
        s.push_str(
            format!(
                "fn {}({}) -> {}",
                func.name(),
                signatures
                    .get(&ResourceLocation::new(String::from("pack"), func.name().to_owned()))
                    .unwrap()
                    .params()
                    .iter()
                    .map(|p| format!("{}: {}", p.name(), p.param_type()))
                    .collect::<Vec<String>>()
                    .join(", "),
                signatures.get(&ResourceLocation::new(String::from("pack"), func.name().to_owned())).unwrap().return_type()
            )
            .as_str(),
        );

        s.push_str("\n");

        for instr in func.body() {
            s.push_str(format!("    {}\n", instr).as_str());
        }
    }

    std::fs::write("ir", &s);
    println!("Wrote IR to file");

    let codegen = CodeGen::new("pack".to_owned(), signatures, types);
    let final_output = codegen.generate(&funcs);

    for func in final_output {
        let (location, file_content) = func.finalize();

        // Write file_content at output/location.namespace/location.path
        std::fs::create_dir_all(Path::new("output").join(location.namespace.clone())).unwrap();
        std::fs::write(
            Path::new("output")
                .join(location.namespace)
                .join(location.path)
                .with_extension("mcfunction"),
            file_content,
        );
    }

}
