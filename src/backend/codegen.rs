use std::{collections::{HashMap, VecDeque, HashSet}, default, sync::atomic::{AtomicI32, Ordering}, fmt::{Display, Formatter}};

use crate::{data::{ResourceLocation, ScoreboardSlot}, parser::Operation};

use super::{ir::{IrFunction, Instruction}, function::FunctionSignature, type_pool::TypePool};

static ANON_FUNC_COUNT: AtomicI32 = AtomicI32::new(0);

pub struct CodeGen {
    pack_name: String,
    func_signatures: HashMap<ResourceLocation, FunctionSignature>,
    types: TypePool,
}

impl CodeGen {
    pub fn new(pack_name: String, func_signatures: HashMap<ResourceLocation, FunctionSignature>, types: TypePool) -> Self {
        Self {
            pack_name,
            func_signatures,
            types,
        }
    }

    pub fn generate(&self, funcs: &[IrFunction]) -> Vec<CompiledFunction> {
        let mut compiled = Vec::new();

        for func in funcs {
            let mut output = self.compile_instr_sequence(ResourceLocation::new(self.pack_name.clone(), func.name().to_owned()), func.body(), EvaluationStack::new(ResourceLocation::new(self.pack_name.clone(), func.name().to_owned()).with_separator('_').to_string()));
            compiled.append(&mut output.0);
        }

        compiled
    }

    // Takes a sequence of IR instructions and compiles them into .mcfunctions. 
    // The sequence is assumed to either be the body of an IrFunction, or the body of a block (excluding the delimiting startblock/endblock instructions)
    // Returns a tuple containing the compiled functions, and two booleans representing whether the function should propagate a return or break
    // The LAST function in the returned collection is the outermost function/the function's body, and the rest are anonymous functions
    fn compile_instr_sequence(&self, name: ResourceLocation, ir: &[Instruction], mut eval_stack: EvaluationStack) -> (Vec<CompiledFunction>, bool, bool) {
        let mut block_stack = Vec::new();
        let mut propagate_return = false;
        let mut propagate_break = false;
        let mut compiled = Vec::new();

        let mut i = 0;
        while i < ir.len() {
            let instr = &ir[i];

            match instr {
                Instruction::PushInteger(n) => eval_stack.push_integer(*n),
                Instruction::PushBoolean(b) => eval_stack.push_boolean(*b),
                Instruction::PushLocal(idx) => eval_stack.push_local(*idx),
                Instruction::StoreLocal(idx) => eval_stack.store_local(*idx),
                Instruction::Add => eval_stack.handle_op(Operation::Add),
                Instruction::Subtract => eval_stack.handle_op(Operation::Subtract),
                Instruction::Multiply => eval_stack.handle_op(Operation::Multiply),
                Instruction::Divide => eval_stack.handle_op(Operation::Divide),
                Instruction::Modulo => eval_stack.handle_op(Operation::Modulo),
                Instruction::Equal => eval_stack.handle_op(Operation::CheckEquals),
                Instruction::NotEqual => eval_stack.handle_op(Operation::NotEquals),
                Instruction::GreaterThan => eval_stack.handle_op(Operation::GreaterThan),
                Instruction::LessThan => eval_stack.handle_op(Operation::LessThan),
                Instruction::GreaterThanOrEqual => eval_stack.handle_op(Operation::GreaterThanOrEquals),
                Instruction::LessThanOrEqual => eval_stack.handle_op(Operation::LessThanOrEquals),
                Instruction::And => todo!(),
                Instruction::Or => todo!(),
                Instruction::Not => eval_stack.handle_op(Operation::Not),
                Instruction::Return => {
                    eval_stack.handle_return();
                    propagate_return = true;
                }
                Instruction::Break => {
                    eval_stack.handle_break();
                    propagate_break = true;
                }
                Instruction::Call(location) => eval_stack.handle_call(location, self.func_signatures.get(location).unwrap()),
                Instruction::StartBlock(is_loop) => {
                    // Find the index of the delimiting EndBlock instruction
                    let mut end_idx = i + 1;
                    let mut block_depth = 1;

                    while end_idx < ir.len() {
                        match &ir[end_idx] {
                            Instruction::StartBlock(_) => block_depth += 1,
                            Instruction::EndBlock => {
                                block_depth -= 1;

                                if block_depth == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }

                        end_idx += 1;
                    }

                    if end_idx == ir.len() {
                        panic!("malformed ir");
                    }

                    let block_ir = &ir[i + 1..end_idx];
                    let compiled_block_info = self.compile_instr_sequence(ResourceLocation::new(name.namespace.clone(), format!("{}{}", name.path, ANON_FUNC_COUNT.fetch_add(1, Ordering::Relaxed))), block_ir, eval_stack.create_child());

                    propagate_return |= compiled_block_info.1;
                    propagate_break |= compiled_block_info.2;

                    block_stack.push((*is_loop, compiled_block_info.0));

                    i = end_idx + 1;
                    continue;
                }
                Instruction::EndBlock => unreachable!("malformed ir"),
                Instruction::JumpInIf => {
                    let (is_loop, mut functions) = block_stack.pop().unwrap();

                    eval_stack.handle_jump_in(&functions.last().unwrap(), is_loop, propagate_return, &mut propagate_break);

                    compiled.append(&mut functions);
                }
                Instruction::JumpInEither => {
                    let (mut false_block, mut true_block) = (block_stack.pop().unwrap(), block_stack.pop().unwrap());

                    eval_stack.handle_jump_in_either(&true_block.1.last().unwrap(), &false_block.1.last().unwrap(), true_block.0 || false_block.0, propagate_return, &mut propagate_break);

                    compiled.append(&mut true_block.1);
                    compiled.append(&mut false_block.1);
                }
                Instruction::RepeatIf => eval_stack.handle_repeat_if(&name),
                Instruction::EmitCommandLiteral(literal) => eval_stack.direct_emit(literal),
                Instruction::AccessField(_) => todo!(),
            }

            i += 1;
        }

        compiled.push(CompiledFunction::new(name, eval_stack.commands));

        (compiled, propagate_return, propagate_break)
    }
}

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
            available_slots: VecDeque::new(),
            used_slots: HashSet::new(),
            commands: Vec::new(),
            stack: Vec::new(),
        }
    }

    fn push_integer(&mut self, n: i32) {
        let slot = self.find_free_slot();

        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: self.slot_location(slot),
            value: n
        });

        self.stack.push(slot);
    }

    fn push_boolean(&mut self, b: bool) {
        let slot = self.find_free_slot();

        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: self.slot_location(slot),
            value: if b { 1 } else { 0 }
        });

        self.stack.push(slot);
    }

    fn push_local(&mut self, local: usize) {
        let slot = self.find_free_slot();

        self.commands.push(CommandAction::ScoreboardOperation {
            a: self.slot_location(slot),
            op: ScoreboardOperationType::Set,
            b: self.local_location(local)
        });

        self.stack.push(slot);
    }

    fn store_local(&mut self, local: usize) {
        let slot = self.stack.pop().unwrap();

        self.commands.push(CommandAction::ScoreboardOperation {
            a: self.local_location(local),
            op: ScoreboardOperationType::Set,
            b: self.slot_location(slot)
        });

        self.free_slot(slot);
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
            Operation::CheckEquals => format!("score {} matches 0", self.slot_location(a)),
            Operation::NotEquals => format!("score {} matches 0", self.slot_location(a)),
            Operation::GreaterThan => format!("score {} matches 1..", self.slot_location(a)),
            Operation::LessThan => format!("score {} matches ..-1", self.slot_location(a)),
            Operation::GreaterThanOrEquals => format!("score {} matches 0..", self.slot_location(a)),
            Operation::LessThanOrEquals => format!("score {} matches ..0", self.slot_location(a)),
            _ => unreachable!()
        };

        // NotEqual is the only one that needs to be inverted
        if op == Operation::NotEquals {
            self.commands.push(CommandAction::ExecuteUnless {
                condition,
                run: Box::new(CommandAction::SetScoreboardEntry {
                    entry: self.slot_location(a),
                    value: 1
                })
            });
        } else {
            self.commands.push(CommandAction::ExecuteIf {
                condition,
                run: Box::new(CommandAction::SetScoreboardEntry {
                    entry: self.slot_location(a),
                    value: 1
                })
            });
        }

        self.stack.push(a);
    }

    fn handle_op(&mut self, op: Operation) {
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
                        value: 1
                    })
                });

                self.commands.push(CommandAction::ExecuteIf {
                    condition: format!("score {} matches 1", self.slot_location(a)),
                    run: Box::new(CommandAction::SetScoreboardEntry {
                        entry: self.slot_location(a),
                        value: 0
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
            }
        }
    }

    // #rf represents the return flag. If this is set to 1, the return will propagate up the anonymous function call stack
    // #rv represents the return value. It will not be present for void functions, but will always be set if the function returns a value
    fn handle_return(&mut self) {
        let return_value = self.stack.pop();

        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: ScoreboardSlot::new(self.objective.clone(), "#rf".to_string()),
            value: 1
        });

        if let Some(slot) = return_value {
            self.commands.push(CommandAction::ScoreboardOperation {
                a: ScoreboardSlot::new(self.objective.clone(), "#rv".to_string()),
                op: ScoreboardOperationType::Set,
                b: self.slot_location(slot)
            });

            self.free_slot(slot);
        }

        self.commands.push(CommandAction::Return);
    }

    fn handle_break(&mut self) {
        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: ScoreboardSlot::new(self.objective.clone(), "#bf".to_string()),
            value: 1
        });

        self.commands.push(CommandAction::Return);
    }

    fn handle_jump_in(&mut self, block: &CompiledFunction, is_loop: bool, propagate_return: bool, propagate_break: &mut bool) {
        let condition = self.stack.pop().unwrap();

        self.free_slot(condition);

        self.commands.push(CommandAction::ExecuteIf {
            condition: format!("score {} matches 1", self.slot_location(condition)),
            run: Box::new(CommandAction::Call(block.name.clone()))
        });

        if propagate_return {
            self.commands.push(CommandAction::ExecuteIf {
                condition: format!("score {} matches 1", ScoreboardSlot::new(self.objective.clone(), "#rf".to_string())),
                run: Box::new(CommandAction::Return)
            })
        }

        if *propagate_break {
            self.commands.push(CommandAction::ExecuteIf {
                condition: format!("score {} matches 1", ScoreboardSlot::new(self.objective.clone(), "#bf".to_string())),
                run: Box::new(CommandAction::Return)
            });

            if is_loop {
                *propagate_break = false;

                self.commands.push(CommandAction::SetScoreboardEntry {
                    entry: ScoreboardSlot::new(self.objective.clone(), "#bf".to_string()),
                    value: 0
                });
            }
        }
    }

    fn handle_jump_in_either(&mut self, true_block: &CompiledFunction, false_block: &CompiledFunction, is_loop: bool, propagate_return: bool, propagate_break: &mut bool) {
        let condition = self.stack.pop().unwrap();

        self.commands.push(CommandAction::ExecuteIf {
            condition: format!("score {} matches 1", self.slot_location(condition)),
            run: Box::new(CommandAction::Call(true_block.name.clone()))
        });

        self.commands.push(CommandAction::ExecuteUnless {
            condition: format!("score {} matches 1", self.slot_location(condition)),
            run: Box::new(CommandAction::Call(false_block.name.clone()))
        });

        self.free_slot(condition);

        if propagate_return {
            self.commands.push(CommandAction::ExecuteIf {
                condition: format!("score {} matches 1", ScoreboardSlot::new(self.objective.clone(), "#rf".to_string())),
                run: Box::new(CommandAction::Return)
            })
        }

        if *propagate_break {
            self.commands.push(CommandAction::ExecuteIf {
                condition: format!("score {} matches 1", ScoreboardSlot::new(self.objective.clone(), "#bf".to_string())),
                run: Box::new(CommandAction::Return)
            });

            if is_loop {
                *propagate_break = false;

                self.commands.push(CommandAction::SetScoreboardEntry {
                    entry: ScoreboardSlot::new(self.objective.clone(), "#bf".to_string()),
                    value: 0
                });
            }
        }
    }

    fn handle_call(&mut self, name: &ResourceLocation, signature: &FunctionSignature) {
        for i in (0..signature.params().len()).rev() {
            let slot = self.stack.pop().unwrap();

            self.commands.push(CommandAction::ScoreboardOperation {
                a: self.parameter_location(name, i),
                op: ScoreboardOperationType::Set,
                b: self.slot_location(slot)
            });

            self.free_slot(slot);
        }
    }

    fn handle_repeat_if(&mut self, block: &ResourceLocation) {
        let condition = self.stack.pop().unwrap();

        self.commands.push(CommandAction::ExecuteIf {
            condition: format!("score {} matches 1", self.slot_location(condition)),
            run: Box::new(CommandAction::Call(block.clone()))
        });

        self.free_slot(condition);
    }

    fn direct_emit(&mut self, literal: &str) {
        self.commands.push(CommandAction::Literal(literal.to_string()));
    }

    fn create_child(&self) -> Self {
        Self {
            objective: self.objective.clone(),
            available_slots: self.available_slots.clone(),
            used_slots: self.used_slots.clone(),
            commands: Vec::new(),
            stack: self.stack.clone()
        }
    }

    fn find_free_slot(&mut self) -> i32 {
        let slot = match self.available_slots.pop_front() {
            Some(slot) => slot,
            None => self.used_slots.len() as i32
        };

        self.used_slots.insert(slot);
        slot
    }

    fn free_slot(&mut self, slot: i32) {
        self.used_slots.remove(&slot);
        self.available_slots.push_back(slot);
    }

    fn slot_location(&self, slot: i32) -> ScoreboardSlot {
        ScoreboardSlot::new(self.objective.clone(), format!("s{}", slot))
    }

    fn local_location(&self, local: usize) -> ScoreboardSlot {
        ScoreboardSlot::new(self.objective.clone(), format!("l{}", local))
    }

    fn parameter_location(&self, function: &ResourceLocation, idx: usize) -> ScoreboardSlot {
        ScoreboardSlot::new(function.with_separator('_').to_string(), format!("l{}", idx))
    }

    fn const_location(&self, constant: i32) -> ScoreboardSlot {
        ScoreboardSlot::new("const".to_string(), format!("c{}", constant))
    }
}

pub struct CompiledFunction {
    name: ResourceLocation,
    body: Vec<CommandAction>
}

impl CompiledFunction {
    fn new(name: ResourceLocation, body: Vec<CommandAction>) -> Self {
        CompiledFunction { name, body }
    }

    pub fn finalize(self) -> (ResourceLocation, String) {
        let mut text = String::new();

        for action in self.body {
            text.push_str(format!("{}\n", action).as_str());
        }

        (self.name, text)
    }

    pub fn name(&self) -> &ResourceLocation {
        &self.name
    }
}

enum CommandAction {
    SetScoreboardEntry {
        entry: ScoreboardSlot,
        value: i32,
    },
    ScoreboardOperation {
        op: ScoreboardOperationType,
        a: ScoreboardSlot,
        b: ScoreboardSlot,
    },
    ExecuteIf {
        condition: String,
        run: Box<CommandAction>
    },
    ExecuteUnless {
        condition: String,
        run: Box<CommandAction>
    },
    Call(ResourceLocation),
    Return,
    Literal(String)
}

impl Display for CommandAction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandAction::SetScoreboardEntry { entry, value } => write!(f, "scoreboard players set {} {}", entry, value),
            CommandAction::ScoreboardOperation { op, a, b } => write!(f, "scoreboard players operation {} {} {}", a, op, b),
            CommandAction::ExecuteIf { condition, run } => write!(f, "execute if {} run {}", condition, run),
            CommandAction::ExecuteUnless { condition, run } => write!(f, "execute unless {} run {}", condition, run),
            CommandAction::Call(location) => write!(f, "function {}", location),
            CommandAction::Return => write!(f, "return 0"),
            CommandAction::Literal(literal) => write!(f, "{}", literal)
        }
    }

}

enum GreenValue {
    Number(i32),
    ScoreboardEntry(ResourceLocation)
}

impl Display for GreenValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GreenValue::Number(n) => write!(f, "{}", n),
            GreenValue::ScoreboardEntry(entry) => write!(f, "{}", entry)
        }
    }
}

enum ScoreboardOperationType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Set
    // add the others when i feel like it
}

impl Display for ScoreboardOperationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScoreboardOperationType::Add => write!(f, "+="),
            ScoreboardOperationType::Subtract => write!(f, "-="),
            ScoreboardOperationType::Multiply => write!(f, "*="),
            ScoreboardOperationType::Divide => write!(f, "/="),
            ScoreboardOperationType::Modulo => write!(f, "%="),
            ScoreboardOperationType::Set => write!(f, "=")
        }
    }
}