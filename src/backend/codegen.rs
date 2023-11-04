use std::{collections::{HashMap, VecDeque}, default};

use crate::data::ResourceLocation;

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
            self.compile_func_ir(func);
        }
    }

    fn compile_func_ir(&self, func: &IrFunction) -> CompiledFunction {
        for instr in func.body() {
            match instr {
                Instruction::PushInteger(_) => todo!(),
                Instruction::PushBoolean(_) => todo!(),
                Instruction::PushLocal(_) => todo!(),
                Instruction::StoreLocal(_) => todo!(),
                Instruction::Add => todo!(),
                Instruction::Subtract => todo!(),
                Instruction::Multiply => todo!(),
                Instruction::Divide => todo!(),
                Instruction::Modulo => todo!(),
                Instruction::Equal => todo!(),
                Instruction::NotEqual => todo!(),
                Instruction::GreaterThan => todo!(),
                Instruction::LessThan => todo!(),
                Instruction::GreaterThanOrEqual => todo!(),
                Instruction::LessThanOrEqual => todo!(),
                Instruction::And => todo!(),
                Instruction::Or => todo!(),
                Instruction::Not => todo!(),
                Instruction::Return => todo!(),
                Instruction::Break => todo!(),
                Instruction::Call(_) => todo!(),
                Instruction::StartBlock => todo!(),
                Instruction::EndBlock => todo!(),
                Instruction::JumpInIf => todo!(),
                Instruction::JumpOutIf => todo!(),
                Instruction::JumpInEither => todo!(),
                Instruction::RepeatIf => todo!(),
                Instruction::EmitCommandLiteral(_) => todo!(),
                Instruction::AccessField(_) => todo!(),
                Instruction::Duplicate => todo!(),
            }
        }
    }
}

struct IrFunctionCompiler<'a> {
    ir: &'a IrFunction,
    eval_stack: EvaluationStack,
    types: TypePool,
    func_signatures: &'a HashMap<String, FunctionSignature>
}

impl IrFunctionCompiler<'_> {
    fn new(ir: &IrFunction, types: TypePool, func_signatures: &HashMap<String, FunctionSignature>) -> Self {
        Self {
            ir,
            eval_stack: EvaluationStack::new(),
            types,
            func_signatures
        }
    }
}

#[derive(Default)]
struct EvaluationStack {
    slots: VecDeque<i32>, // tmps in scoreboard
    commands: Vec<CommandAction>,
    stack: Vec<i32>,
}

impl EvaluationStack {
    fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    fn push_integer(&mut self, n: i32) {

    }

    fn find_free_slot(&mut self) -> i32 {
        if self.slots.is_empty() {
            self.slots.push_front(0);
        }
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

enum CommandAction {
    SetScoreboardEntry {
        entry: ResourceLocation,
        value: GreenValue
    }
}

enum GreenValue {
    Number(i32),
    ScoreboardEntry(ResourceLocation)
}