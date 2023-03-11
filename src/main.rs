use std::path::Path;

use crate::codegen::CodeGenerator;

mod lexer;
mod parser;
mod codegen;
mod types;

fn main() {
    let test = "fn determinant(a: int, b: int, c: int) -> int {
        return b * b - 4 * a * c;
    }
    
    fn main() {
        determinant(4, 1, 2);
    }
    ";

    let codegen = CodeGenerator::compile_src(test, "test");

    match codegen {
        Ok(gen) => {
            gen.output_to_dir(Path::new("output"));
        },
        Err(errs) => println!("{:?}", errs)
    };
}
