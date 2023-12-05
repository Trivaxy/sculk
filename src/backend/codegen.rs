use std::{
    collections::{HashMap, HashSet, VecDeque},
    default,
    fmt::{Display, Formatter},
    sync::atomic::{AtomicI32, Ordering},
};

use crate::{
    data::{ResourceLocation, ScoreboardSlot},
    parser::Operation,
};

use super::{
    function::FunctionSignature,
    ir::{Instruction, IrFunction},
    type_pool::{TypePool, TypeKey, self}, types::SculkType,
};

static ANON_FUNC_COUNT: AtomicI32 = AtomicI32::new(0);

pub struct CodeGen {
    pack_name: String,
    func_signatures: HashMap<ResourceLocation, FunctionSignature>,
    types: TypePool,
}

impl CodeGen {
    pub fn new(
        pack_name: String,
        func_signatures: HashMap<ResourceLocation, FunctionSignature>,
        types: TypePool,
    ) -> Self {
        Self {
            pack_name,
            func_signatures,
            types,
        }
    }

    pub fn generate(&self, funcs: &[IrFunction]) -> Vec<CompiledFunction> {
        let mut compiled = Vec::new();

        for func in funcs {
            let mut output = self.compile_instr_sequence(
                ResourceLocation::new(self.pack_name.clone(), func.name().to_owned()),
                func.body(),
                EvaluationStack::new(
                    ResourceLocation::new(self.pack_name.clone(), func.name().to_owned())
                        .with_separator('_')
                        .to_string(),
                    &self.types,
                    self.pack_name.clone(),
                ),
            );
            compiled.append(&mut output.0);
        }

        compiled
    }

    pub fn dissolve(self) -> (HashMap<ResourceLocation, FunctionSignature>, TypePool) {
        (self.func_signatures, self.types)
    }

    // Takes a sequence of IR instructions and compiles them into .mcfunctions.
    // The sequence is assumed to either be the body of an IrFunction, or the body of a block (excluding the delimiting startblock/endblock instructions)
    // Returns a tuple containing the compiled functions, and two booleans representing whether the function should propagate a return or break
    // The LAST function in the returned collection is the outermost function/the function's body, and the rest are anonymous functions
    fn compile_instr_sequence(
        &self,
        name: ResourceLocation,
        ir: &[Instruction],
        mut eval_stack: EvaluationStack,
    ) -> (Vec<CompiledFunction>, bool, bool) {
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
                Instruction::GreaterThanOrEqual => {
                    eval_stack.handle_op(Operation::GreaterThanOrEquals)
                }
                Instruction::LessThanOrEqual => eval_stack.handle_op(Operation::LessThanOrEquals),
                Instruction::And => eval_stack.handle_logical_op(Operation::And),
                Instruction::Or => eval_stack.handle_logical_op(Operation::Or),
                Instruction::Not => eval_stack.handle_op(Operation::Not),
                Instruction::Return => {
                    eval_stack.handle_return();
                    propagate_return = true;
                }
                Instruction::Break => {
                    eval_stack.handle_break();
                    propagate_break = true;
                }
                Instruction::CallGlobal(location) => {
                    eval_stack.handle_call(location, self.func_signatures.get(location).unwrap())
                }
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
                    let compiled_block_info = self.compile_instr_sequence(
                        ResourceLocation::new(
                            name.namespace.clone(),
                            format!(
                                "{}{}",
                                name.path,
                                ANON_FUNC_COUNT.fetch_add(1, Ordering::Relaxed)
                            ),
                        ),
                        block_ir,
                        eval_stack.create_child(),
                    );

                    propagate_return |= compiled_block_info.1;
                    propagate_break |= compiled_block_info.2;

                    block_stack.push((*is_loop, compiled_block_info.0));

                    i = end_idx + 1;
                    continue;
                }
                Instruction::EndBlock => unreachable!("malformed ir"),
                Instruction::JumpInIf => {
                    let (is_loop, mut functions) = block_stack.pop().unwrap();

                    eval_stack.handle_jump_in(
                        &functions.last().unwrap(),
                        is_loop,
                        propagate_return,
                        &mut propagate_break,
                    );

                    compiled.append(&mut functions);
                }
                Instruction::JumpInEither => {
                    let (mut false_block, mut true_block) =
                        (block_stack.pop().unwrap(), block_stack.pop().unwrap());

                    eval_stack.handle_jump_in_either(
                        &true_block.1.last().unwrap(),
                        &false_block.1.last().unwrap(),
                        true_block.0 || false_block.0,
                        propagate_return,
                        &mut propagate_break,
                    );

                    compiled.append(&mut true_block.1);
                    compiled.append(&mut false_block.1);
                }
                Instruction::RepeatIf => eval_stack.handle_repeat_if(&name),
                Instruction::EmitCommandLiteral(literal) => eval_stack.direct_emit(literal),
                Instruction::CallMethod(ty, idx) => eval_stack.handle_method_call(*ty, *idx),
                Instruction::Construct(ty) => eval_stack.handle_constructor_call(*ty),
                Instruction::PushField(idx) => eval_stack.handle_push_field(*idx),
                Instruction::StoreField(idx) => eval_stack.handle_store_field(*idx),
                
            }

            i += 1;
        }

        compiled.push(CompiledFunction::new(name, eval_stack.commands));

        (compiled, propagate_return, propagate_break)
    }
}

#[derive(Clone)]
struct StackValue {
    slot: i32,
    ty: TypeKey,
}

impl StackValue {
    fn new(slot: i32, ty: TypeKey) -> Self {
        Self { slot, ty }
    }
}

struct EvaluationStack<'a> {
    objective: String,
    available_slots: VecDeque<i32>,
    used_slots: HashSet<i32>,
    commands: Vec<CommandAction>,
    stack: Vec<StackValue>,
    local_types: HashMap<usize, TypeKey>,
    types: &'a TypePool,
    pack_name: String,
}

impl<'a> EvaluationStack<'a> {
    fn new(objective: String, type_pool: &'a TypePool, pack_name: String) -> Self {
        Self {
            objective,
            available_slots: VecDeque::new(),
            used_slots: HashSet::new(),
            commands: Vec::new(),
            stack: Vec::new(),
            local_types: HashMap::new(),
            types: type_pool,
            pack_name,
        }
    }

    fn push_integer(&mut self, n: i32) {
        let slot = self.find_free_slot();

        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: self.slot_location(slot),
            value: n,
        });

        self.stack.push(StackValue::new(slot, self.types.int()));
    }

    fn push_boolean(&mut self, b: bool) {
        let slot = self.find_free_slot();

        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: self.slot_location(slot),
            value: if b { 1 } else { 0 },
        });

        self.stack.push(StackValue::new(slot, self.types.bool()));
    }

    fn push_local(&mut self, local: usize) {
        let slot = self.find_free_slot();
        let local_type = self.local_types.get(&local).unwrap();

        match local_type.from(self.types) {
            SculkType::Integer
            | SculkType::Bool => {
                self.commands.push(CommandAction::ScoreboardOperation {
                    a: self.slot_location(slot),
                    op: ScoreboardOperationType::Set,
                    b: self.local_location(local),
                });
            }
            SculkType::Struct(def) => {
                for (idx, field) in def.fields().iter().enumerate() {
                    self.commands.push(CommandAction::ScoreboardOperation {
                        a: self.slot_location_suffixed(slot, format!("_{}", idx)),
                        op: ScoreboardOperationType::Set,
                        b: self.local_location_suffixed(local, format!("_{}", idx)),
                    });
                }
            }
            _ => unreachable!(),
        }

        self.stack.push(StackValue::new(slot, local_type.clone()));
    }

    fn store_local(&mut self, local: usize) {
        let value = self.stack.pop().unwrap();

        match self.local_types.get(&local).unwrap().from(self.types) {
            SculkType::Integer
            | SculkType::Bool => {
                self.commands.push(CommandAction::ScoreboardOperation {
                    a: self.local_location(local),
                    op: ScoreboardOperationType::Set,
                    b: self.slot_location(value.slot),
                });
            }
            SculkType::Struct(def) => {
                for (idx, field) in def.fields().iter().enumerate() {
                    self.commands.push(CommandAction::ScoreboardOperation {
                        a: self.local_location_suffixed(local, format!("_{}", idx)),
                        op: ScoreboardOperationType::Set,
                        b: self.slot_location_suffixed(value.slot, format!("_{}", idx)),
                    });
                }
            }
            _ => unreachable!(),
        }

        self.free_slot(value.slot);
    }

    fn handle_arithmetic_op(&mut self, op: ScoreboardOperationType) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        self.free_slot(b.slot);

        self.commands.push(CommandAction::ScoreboardOperation {
            op,
            a: self.slot_location(a.slot),
            b: self.slot_location(b.slot),
        });

        self.stack.push(a);
    }

    fn handle_logical_op(&mut self, op: Operation) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        self.free_slot(b.slot);

        match op {
            Operation::And => {
                self.commands.push(CommandAction::ScoreboardOperation {
                    op: ScoreboardOperationType::Multiply,
                    a: self.slot_location(a.slot),
                    b: self.slot_location(b.slot),
                });
            }
            Operation::Or => {
                self.commands.push(CommandAction::ScoreboardOperation {
                    op: ScoreboardOperationType::Add,
                    a: self.slot_location(a.slot),
                    b: self.slot_location(b.slot),
                });

                self.commands.push(CommandAction::ScoreboardOperation {
                    op: ScoreboardOperationType::Divide,
                    a: self.slot_location(a.slot),
                    b: self.slot_location(a.slot),
                });
            }
            _ => unreachable!("invalid logical op")
        }

        self.stack.push(a);
    }

    fn handle_comparison_op(&mut self, op: Operation) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        self.free_slot(b.slot);

        self.commands.push(CommandAction::ScoreboardOperation {
            op: ScoreboardOperationType::Subtract,
            a: self.slot_location(a.slot),
            b: self.slot_location(b.slot),
        });

        let condition = match op {
            Operation::CheckEquals => format!("score {} matches 0", self.slot_location(a.slot)),
            Operation::NotEquals => format!("score {} matches 0", self.slot_location(a.slot)),
            Operation::GreaterThan => format!("score {} matches 1..", self.slot_location(a.slot)),
            Operation::LessThan => format!("score {} matches ..-1", self.slot_location(a.slot)),
            Operation::GreaterThanOrEquals => {
                format!("score {} matches 0..", self.slot_location(a.slot))
            }
            Operation::LessThanOrEquals => format!("score {} matches ..0", self.slot_location(a.slot)),
            _ => unreachable!(),
        };

        // NotEqual is the only one that needs to be inverted
        if op == Operation::NotEquals {
            self.commands.push(CommandAction::ExecuteUnless {
                condition,
                run: Box::new(CommandAction::SetScoreboardEntry {
                    entry: self.slot_location(a.slot),
                    value: 1,
                }),
            });
        } else {
            self.commands.push(CommandAction::ExecuteIf {
                condition,
                run: Box::new(CommandAction::SetScoreboardEntry {
                    entry: self.slot_location(a.slot),
                    value: 1,
                }),
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
            Operation::And
            | Operation::Or => self.handle_logical_op(op),
            Operation::Not => {
                let a = self.stack.pop().unwrap();

                self.commands.push(CommandAction::ExecuteIf {
                    condition: format!("score {} matches 0", self.slot_location(a.slot)),
                    run: Box::new(CommandAction::SetScoreboardEntry {
                        entry: self.slot_location(a.slot),
                        value: 1,
                    }),
                });

                self.commands.push(CommandAction::ExecuteIf {
                    condition: format!("score {} matches 1", self.slot_location(a.slot)),
                    run: Box::new(CommandAction::SetScoreboardEntry {
                        entry: self.slot_location(a.slot),
                        value: 0,
                    }),
                });

                self.stack.push(a);
            }
            Operation::Negate => {
                let a = self.stack.pop().unwrap();

                self.commands.push(CommandAction::ScoreboardOperation {
                    op: ScoreboardOperationType::Multiply,
                    a: self.slot_location(a.slot),
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
            value: 1,
        });

        if let Some(value) = return_value {
            self.commands.push(CommandAction::ScoreboardOperation {
                a: ScoreboardSlot::new(self.objective.clone(), "#rv".to_string()),
                op: ScoreboardOperationType::Set,
                b: self.slot_location(value.slot),
            });

            self.free_slot(value.slot);
        }

        self.commands.push(CommandAction::Return);
    }

    fn handle_break(&mut self) {
        self.commands.push(CommandAction::SetScoreboardEntry {
            entry: ScoreboardSlot::new(self.objective.clone(), "#bf".to_string()),
            value: 1,
        });

        self.commands.push(CommandAction::Return);
    }

    fn handle_jump_in(
        &mut self,
        block: &CompiledFunction,
        is_loop: bool,
        propagate_return: bool,
        propagate_break: &mut bool,
    ) {
        let condition = self.stack.pop().unwrap();

        self.free_slot(condition.slot);

        self.commands.push(CommandAction::ExecuteIf {
            condition: format!("score {} matches 1", self.slot_location(condition.slot)),
            run: Box::new(CommandAction::Call(block.name.clone())),
        });

        if propagate_return {
            self.commands.push(CommandAction::ExecuteIf {
                condition: format!(
                    "score {} matches 1",
                    ScoreboardSlot::new(self.objective.clone(), "#rf".to_string())
                ),
                run: Box::new(CommandAction::Return),
            })
        }

        if *propagate_break {
            self.commands.push(CommandAction::ExecuteIf {
                condition: format!(
                    "score {} matches 1",
                    ScoreboardSlot::new(self.objective.clone(), "#bf".to_string())
                ),
                run: Box::new(CommandAction::Return),
            });

            if is_loop {
                *propagate_break = false;

                self.commands.push(CommandAction::SetScoreboardEntry {
                    entry: ScoreboardSlot::new(self.objective.clone(), "#bf".to_string()),
                    value: 0,
                });
            }
        }
    }

    fn handle_jump_in_either(
        &mut self,
        true_block: &CompiledFunction,
        false_block: &CompiledFunction,
        is_loop: bool,
        propagate_return: bool,
        propagate_break: &mut bool,
    ) {
        let condition = self.stack.pop().unwrap();

        self.commands.push(CommandAction::ExecuteIf {
            condition: format!("score {} matches 1", self.slot_location(condition.slot)),
            run: Box::new(CommandAction::Call(true_block.name.clone())),
        });

        self.commands.push(CommandAction::ExecuteUnless {
            condition: format!("score {} matches 1", self.slot_location(condition.slot)),
            run: Box::new(CommandAction::Call(false_block.name.clone())),
        });

        self.free_slot(condition.slot);

        if propagate_return {
            self.commands.push(CommandAction::ExecuteIf {
                condition: format!(
                    "score {} matches 1",
                    ScoreboardSlot::new(self.objective.clone(), "#rf".to_string())
                ),
                run: Box::new(CommandAction::Return),
            })
        }

        if *propagate_break {
            self.commands.push(CommandAction::ExecuteIf {
                condition: format!(
                    "score {} matches 1",
                    ScoreboardSlot::new(self.objective.clone(), "#bf".to_string())
                ),
                run: Box::new(CommandAction::Return),
            });

            if is_loop {
                *propagate_break = false;

                self.commands.push(CommandAction::SetScoreboardEntry {
                    entry: ScoreboardSlot::new(self.objective.clone(), "#bf".to_string()),
                    value: 0,
                });
            }
        }
    }

    fn handle_call(&mut self, name: &ResourceLocation, signature: &FunctionSignature) {
        for i in (0..signature.params().len()).rev() {
            let value = self.stack.pop().unwrap();

            self.commands.push(CommandAction::ScoreboardOperation {
                a: self.parameter_location(name, i),
                op: ScoreboardOperationType::Set,
                b: self.slot_location(value.slot),
            });

            self.free_slot(value.slot);
        }
    }

    fn handle_method_call(&mut self, ty: TypeKey, idx: usize) {
        let owner = ty.from(self.types).as_struct_def();
        let method = &owner.functions()[idx];
        let location = ResourceLocation::new(self.pack_name.clone(), format!("{}.{}", owner.name(), method.name()));
        self.handle_call(&location, &method);
    }

    fn handle_constructor_call(&mut self, ty: TypeKey) {
        let owner = ty.from(self.types).as_struct_def();
        let constructor = owner.constructor();
        let location = ResourceLocation::new(self.pack_name.clone(), format!("{}{}", owner.name(), constructor.name()));
        self.handle_call(&location, &constructor);
    }

    fn handle_push_field(&mut self, idx: usize) {
        let slot = self.find_free_slot();
        let value = self.stack.pop().unwrap();
        let field = &value.ty.from(self.types).as_struct_def().fields()[idx];

        self.free_slot(value.slot);

        match field.field_type().from(self.types) {
            SculkType::Integer
            | SculkType::Bool => {
                self.commands.push(CommandAction::ScoreboardOperation {
                    a: self.slot_location(slot),
                    op: ScoreboardOperationType::Set,
                    b: self.slot_location_suffixed(value.slot, format!("_{}", idx)),
                });
            }
            SculkType::Struct(def) => {
                for (idx, field) in def.fields().iter().enumerate() {
                    self.commands.push(CommandAction::ScoreboardOperation {
                        a: self.slot_location_suffixed(value.slot, format!("_{}", idx)),
                        op: ScoreboardOperationType::Set,
                        b: self.slot_location_suffixed(value.slot, format!("_{}", idx)),
                    });
                }
            }
            _ => unreachable!(),
        }

        self.stack.push(StackValue::new(slot, field.field_type()));
    }

    fn handle_store_field(&mut self, idx: usize) {
        let value = self.stack.pop().unwrap();
        let target = self.stack.pop().unwrap();
        let field = &target.ty.from(self.types).as_struct_def().fields()[idx];

        match field.field_type().from(self.types) {
            SculkType::Integer
            | SculkType::Bool => {
                self.commands.push(CommandAction::ScoreboardOperation {
                    a: self.slot_location_suffixed(target.slot, format!("_{}", idx)),
                    op: ScoreboardOperationType::Set,
                    b: self.slot_location(value.slot),
                });
            }
            SculkType::Struct(def) => {
                for (idx, field) in def.fields().iter().enumerate() {
                    self.commands.push(CommandAction::ScoreboardOperation {
                        a: self.slot_location_suffixed(target.slot, format!("_{}", idx)),
                        op: ScoreboardOperationType::Set,
                        b: self.slot_location_suffixed(value.slot, format!("_{}", idx)),
                    });
                }
            }
            _ => unreachable!(),
        }

        self.free_slot(value.slot);
        self.free_slot(target.slot);
    }

    fn handle_repeat_if(&mut self, block: &ResourceLocation) {
        let condition = self.stack.pop().unwrap();

        self.commands.push(CommandAction::ExecuteIf {
            condition: format!("score {} matches 1", self.slot_location(condition.slot)),
            run: Box::new(CommandAction::Call(block.clone())),
        });

        self.free_slot(condition.slot);
    }

    fn direct_emit(&mut self, literal: &str) {
        self.commands
            .push(CommandAction::Literal(literal.to_string()));
    }

    fn create_child(&self) -> Self {
        Self {
            objective: self.objective.clone(),
            available_slots: self.available_slots.clone(),
            used_slots: self.used_slots.clone(),
            commands: Vec::new(),
            stack: self.stack.clone(),
            local_types: self.local_types.clone(),
            types: self.types,
            pack_name: self.pack_name.clone(),
        }
    }

    fn find_free_slot(&mut self) -> i32 {
        let slot = match self.available_slots.pop_front() {
            Some(slot) => slot,
            None => self.used_slots.len() as i32,
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

    fn slot_location_suffixed(&self, slot: i32, suffix: String) -> ScoreboardSlot {
        ScoreboardSlot::new(self.objective.clone(), format!("s{}{}", slot, suffix))
    }

    fn local_location(&self, local: usize) -> ScoreboardSlot {
        ScoreboardSlot::new(self.objective.clone(), format!("l{}", local))
    }

    fn local_location_suffixed(&self, local: usize, suffix: String) -> ScoreboardSlot {
        ScoreboardSlot::new(self.objective.clone(), format!("l{}{}", local, suffix))
    }

    fn parameter_location(&self, function: &ResourceLocation, idx: usize) -> ScoreboardSlot {
        ScoreboardSlot::new(
            function.with_separator('_').to_string(),
            format!("l{}", idx),
        )
    }

    fn const_location(&self, constant: i32) -> ScoreboardSlot {
        ScoreboardSlot::new("const".to_string(), format!("c{}", constant))
    }
}

pub struct CompiledFunction {
    name: ResourceLocation,
    body: Vec<CommandAction>,
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
        run: Box<CommandAction>,
    },
    ExecuteUnless {
        condition: String,
        run: Box<CommandAction>,
    },
    Call(ResourceLocation),
    Return,
    Literal(String),
}

impl Display for CommandAction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandAction::SetScoreboardEntry { entry, value } => {
                write!(f, "scoreboard players set {} {}", entry, value)
            }
            CommandAction::ScoreboardOperation { op, a, b } => {
                write!(f, "scoreboard players operation {} {} {}", a, op, b)
            }
            CommandAction::ExecuteIf { condition, run } => {
                write!(f, "execute if {} run {}", condition, run)
            }
            CommandAction::ExecuteUnless { condition, run } => {
                write!(f, "execute unless {} run {}", condition, run)
            }
            CommandAction::Call(location) => write!(f, "function {}", location),
            CommandAction::Return => write!(f, "return 0"),
            CommandAction::Literal(literal) => write!(f, "{}", literal),
        }
    }
}

enum GreenValue {
    Number(i32),
    ScoreboardEntry(ResourceLocation),
}

impl Display for GreenValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GreenValue::Number(n) => write!(f, "{}", n),
            GreenValue::ScoreboardEntry(entry) => write!(f, "{}", entry),
        }
    }
}

enum ScoreboardOperationType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Set, // add the others when i feel like it
}

impl Display for ScoreboardOperationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScoreboardOperationType::Add => write!(f, "+="),
            ScoreboardOperationType::Subtract => write!(f, "-="),
            ScoreboardOperationType::Multiply => write!(f, "*="),
            ScoreboardOperationType::Divide => write!(f, "/="),
            ScoreboardOperationType::Modulo => write!(f, "%="),
            ScoreboardOperationType::Set => write!(f, "="),
        }
    }
}
