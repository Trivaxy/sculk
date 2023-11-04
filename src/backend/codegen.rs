use std::{collections::{HashMap, VecDeque, HashSet}, default};

use crate::{data::ResourceLocation, parser::Operation};

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
            eval_stack: EvaluationStack::new(ir.name().to_string()),
            types,
            func_signatures
        }
    }
}

#[derive(Default)]
struct EvaluationStack {
    objective: String,
    available_slots: VecDeque<i32>,
    used_slots: HashSet<i32>,
    commands: Vec<CommandAction>,
    stack: Vec<i32>,
}

impl EvaluationStack {
    fn new(objective: String) -> Self {
        Self {
            objective,
            ..Default::default()
        }
    }

    fn push_integer(&mut self, n: i32) {
        let slot = self.find_free_slot();

        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: self.slot_location(slot),
            value: GreenValue::Number(n)
        });
    }

    fn push_boolean(&mut self, b: bool) {
        let slot = self.find_free_slot();

        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: self.slot_location(slot),
            value: GreenValue::Number(if b { 1 } else { 0 })
        });
    }

    fn push_local(&mut self, local: usize) {
        let slot = self.find_free_slot();

        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: self.slot_location(slot),
            value: GreenValue::ScoreboardEntry(self.local_location(local))
        });
    }

    fn add(&mut self) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        self.free_slot(b);

        self.commands.push(CommandAction::ScoreboardOperation {
            op: ScoreboardOperationType::Add,
            a: self.slot_location(a),
            b: self.slot_location(b)
        });
    }

    fn handle_arithmetic_op(&mut self, op: ScoreboardOperationType) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        self.free_slot(b);

        self.commands.push(CommandAction::ScoreboardOperation {
            op,
            a: self.slot_location(a),
            b: self.slot_location(b),
        });

        self.stack.push(a);
    }

    fn handle_comparison_op(&mut self, op: Operation) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        self.free_slot(b);

        self.commands.push(CommandAction::ScoreboardOperation {
            op: ScoreboardOperationType::Subtract,
            a: self.slot_location(a),
            b: self.slot_location(b),
        });

        let condition = match op {
            Operation::Equal => format!("score {} matches 0", self.slot_location(a)),
            Operation::NotEqual => format!("score {} matches 0", self.slot_location(a)),
            Operation::GreaterThan => format!("score {} matches 1..", self.slot_location(a)),
            Operation::LessThan => format!("score {} matches ..-1", self.slot_location(a)),
            Operation::GreaterThanOrEqual => format!("score {} matches 0..", self.slot_location(a)),
            Operation::LessThanOrEqual => format!("score {} matches ..0", self.slot_location(a)),
            _ => unreachable!()
        };

        // NotEqual is the only one that needs to be inverted
        if op == Operation::NotEqual {
            self.commands.push(CommandAction::ExecuteUnless {
                condition,
                run: Box::new(CommandAction::SetScoreboardEntry {
                    entry: self.slot_location(a),
                    value: GreenValue::Number(1)
                })
            });
        } else {
            self.commands.push(CommandAction::ExecuteIf {
                condition,
                run: Box::new(CommandAction::SetScoreboardEntry {
                    entry: self.slot_location(a),
                    value: GreenValue::Number(1)
                })
            });
        }

        self.stack.push(a);
    }

    fn handle_bin_op(&mut self, op: Operation) {
        match op {
            Operation::Add => self.handle_arithmetic_op(ScoreboardOperationType::Add),
            Operation::Subtract => self.handle_arithmetic_op(ScoreboardOperationType::Subtract),
            Operation::Multiply => self.handle_arithmetic_op(ScoreboardOperationType::Multiply),
            Operation::Divide => self.handle_arithmetic_op(ScoreboardOperationType::Divide),
            Operation::Modulo => self.handle_arithmetic_op(ScoreboardOperationType::Modulo),
            Operation::CheckEquals 
            | Operation::NotEquals
            | Operation::GreaterThan
            | Operation::LessThan
            | Operation::GreaterThanOrEquals
            | Operation::LessThanOrEquals => self.handle_comparison_op(op),
            Operation::Not => {
                let a = self.stack.pop().unwrap();

                self.commands.push(CommandAction::ExecuteIf {
                    condition: format!("score {} matches 0", self.slot_location(a)),
                    run: Box::new(CommandAction::SetScoreboardEntry {
                        entry: self.slot_location(a),
                        value: GreenValue::Number(1)
                    })
                });

                self.commands.push(CommandAction::ExecuteIf {
                    condition: format!("score {} matches 1", self.slot_location(a)),
                    run: Box::new(CommandAction::SetScoreboardEntry {
                        entry: self.slot_location(a),
                        value: GreenValue::Number(0)
                    })
                });

                self.stack.push(a);
            },
            Operation::Negate => {
                let a = self.stack.pop().unwrap();

                self.commands.push(CommandAction::ScoreboardOperation {
                    op: ScoreboardOperationType::Multiply,
                    a: self.slot_location(a),
                    b: self.const_location(-1),
                });

                self.stack.push(a);
            },
        }
    }

    fn find_free_slot(&mut self) -> i32 {
        match self.available_slots.pop_front() {
            Some(slot) => slot,
            None => {
                let slot = self.used_slots.len() as i32;
                self.used_slots.insert(slot);
                slot
            }
        }
    }

    fn free_slot(&mut self, slot: i32) {
        self.used_slots.remove(&slot);
        self.available_slots.push_back(slot);
    }

    fn slot_location(&self, slot: i32) -> ResourceLocation {
        ResourceLocation::scoreboard(self.objective, format!("s{}", slot))
    }

    fn local_location(&self, local: usize) -> ResourceLocation {
        ResourceLocation::scoreboard(self.objective, format!("l{}", local))
    }

    fn const_location(&self, constant: i32) -> ResourceLocation {
        ResourceLocation::scoreboard("const".to_string(), format!("c{}", constant))
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
    },
    ScoreboardOperation {
        op: ScoreboardOperationType,
        a: ResourceLocation,
        b: ResourceLocation,
    },
    ExecuteIf {
        condition: String,
        run: Box<CommandAction>
    },
    ExecuteUnless {
        condition: String,
        run: Box<CommandAction>
    },
}

enum GreenValue {
    Number(i32),
    ScoreboardEntry(ResourceLocation)
}

enum ScoreboardOperationType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    // add the others when i feel like it
}