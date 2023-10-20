use std::collections::HashMap;

use super::{ir::{IrFunction, Instruction}, function::FunctionSignature, type_pool::TypePool};

pub struct CodeGen {
    func_irs: Vec<IrFunction>,
    func_signatures: HashMap<String, FunctionSignature>,
    types: TypePool,
    compiled: HashMap<String, CompiledFunction>
}

impl CodeGen {
    fn new_from_ir(func_irs: Vec<IrFunction>, func_signatures: HashMap<String, FunctionSignature>, types: TypePool) -> Self {
        Self {
            func_irs,
            func_signatures,
            types,
            compiled: HashMap::new()
        }
    }

    fn generate(&mut self) {
        for func in &self.func_irs {
            for instr in func.body() {
                match instr {
                    _ => todo!()
                }
            }
        }
    }

    fn compile_func_ir(&mut self, func: &IrFunction) {

    }
}

struct CompiledFunction {
    name: String,
    body: Vec<String>
}

impl CompiledFunction {
    fn new(name: String) -> Self {
        CompiledFunction { name: name, body: Vec::new() }
    }

    fn push(&mut self, command: impl Into<String>) {
        self.body.push(command.into());
    }
}