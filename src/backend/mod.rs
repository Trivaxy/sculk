use std::path::Path;

use crate::Config;

use self::{codegen::CodeGen, ir::IrFunction, type_pool::TypePool};

pub mod codegen;
pub mod dpc_backend;
pub mod function;
pub mod ir;
pub mod resolve;
pub mod type_pool;
pub mod types;
pub mod validate;

pub trait Backend {
    fn compile(config: &Config, ir: &[IrFunction], types: &TypePool);
}

pub struct DefaultBackend;

impl Backend for DefaultBackend {
    fn compile(config: &Config, ir: &[IrFunction], types: &TypePool) {
        let _ = types;
        let mut codegen = CodeGen::new(config.pack.clone());
        codegen.compile_ir_functions(ir);

        let compiled_funcs = codegen.dissolve();

        for func in compiled_funcs {
            let namespace_path = Path::new(&config.pack);

            if let Err(err) = std::fs::create_dir_all(&namespace_path) {
                println!("failed to create namespace directory: {}", err);
                return;
            }

            if let Err(err) = std::fs::write(
                format!(
                    "{}.mcfunction",
                    namespace_path.join(func.name().path.clone()).display()
                ),
                func.to_string(),
            ) {
                println!("failed to write function file: {}", err);
                return;
            }
        }
    }
}
