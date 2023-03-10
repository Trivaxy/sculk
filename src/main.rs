use logos::Logos;

use crate::{lexer::Token, parser::Parser, codegen::CodeGenerator};

mod lexer;
mod parser;
mod codegen;
mod types;

fn main() {
    let test = "fn determinant(a: int, b: int, c: int) -> int { return b * b - 4 * a * c; }";
    let codegen = CodeGenerator::compile_src(test);

    match codegen {
        Ok(gen) => {
            println!("{}", gen.get_output());
        },
        Err(errs) => println!("{:?}", errs)
    };
}
