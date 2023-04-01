use std::path::Path;

use crate::backend::codegen::CodeGenerator;

mod backend;
mod data;
mod lexer;
mod parser;
mod types;

fn main() {
    let test = "
        fn g() {
            let x = 10;
            x = 0;
            x += x * 2;
        }
    ";

    let codegen = CodeGenerator::compile_src(test, "test");

    match codegen {
        Ok(gen) => {
            gen.output_to_dir(Path::new("output"));
        }
        Err(errs) => println!("{:#?}", errs),
    };
}
