use std::path::Path;

use crate::backend::codegen::CodeGenerator;

mod backend;
mod data;
mod lexer;
mod parser;
mod types;

fn main() {
    let test = r#"
        fn fizzbuzz(x: int) {
            for let i = 1; i <= x; i += 1; {
                if i == 15 {
                    break;
                }

                /say hiiiii;
            }

            /say wagwan g;
        }
    "#;

    let codegen = CodeGenerator::compile_src(test, "test");

    match codegen {
        Ok(gen) => {
            gen.output_to_dir(Path::new("output"));
        }
        Err(errs) => println!("{:#?}", errs),
    };
}
