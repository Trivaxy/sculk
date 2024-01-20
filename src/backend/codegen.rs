use std::{sync::atomic::{AtomicI32, Ordering}, fmt::{Display, Formatter}, collections::HashMap};

use crate::{data::{ResourceLocation, ScoreboardSlot, ScoreboardOperationType, Objective}, parser::Operation};

use super::ir::{IrFunction, Instruction, BinaryOperation};

static ANON_FUNC_COUNT: AtomicI32 = AtomicI32::new(0);

pub struct CodeGen {
    pack_name: String,
    functions: Vec<CompiledFunction>,
    block_info: HashMap<usize, BlockInfo>
}

impl CodeGen {
    pub fn new(pack_name: String) -> Self {
        Self {
            pack_name,
            functions: Vec::new(),
            block_info: HashMap::new()
        }
    }

    pub fn dissolve(self) -> Vec<CompiledFunction> {
        self.functions
    }

    pub fn compile_ir_functions(&mut self, funcs: &[IrFunction]) {
        for func in funcs {
            self.compile_ir_function(func);
        }
    }

    fn compile_ir_function(&mut self, func: &IrFunction) {
        self.compile_ir_sequence(func.objective(), func.body(), usize::MAX);
    }

    fn compile_ir_sequence(&mut self, objective: &Objective, ir: &[Instruction], block_id: usize) {
        let mut actions = Vec::new();

        let ensure_control_flow = |this: &Self, block_id, actions: &mut Vec<CommandAction>| {
            if let Some(block_info) = this.block_info.get(block_id) {
                if block_info.returns {
                    actions.push(CommandAction::ExecuteIf {
                        condition: Condition::ScoreMatches {
                            a: ScoreboardSlot::new(Objective(format!("{}.return", objective)), "flag".to_string()),
                            b: 1
                        },
                        run: Box::new(CommandAction::Return)
                    });
                }

                if block_info.breaks {
                    actions.push(CommandAction::ExecuteIf {
                        condition: Condition::ScoreMatches {
                            a: ScoreboardSlot::new(Objective(format!("{}.break", objective)), "flag".to_string()),
                            b: 1
                        },
                        run: Box::new(CommandAction::Return)
                    });
                }
            }
        };
    
        for instr in ir {
            actions.push(match instr {
                Instruction::SetValueToValue { source, target } => {
                    CommandAction::ScoreboardOperation {
                        op: ScoreboardOperationType::Set,
                        a: ScoreboardSlot::from(target),
                        b: ScoreboardSlot::from(source),
                    }
                }
                Instruction::SetValueToConstant { target, constant } => {
                    CommandAction::SetScoreboardEntry {
                        entry: ScoreboardSlot::from(target),
                        value: *constant,
                    }
                }
                Instruction::ValueBinaryOperation { source, target, op } => {
                    match *op {
                        BinaryOperation::Add => CommandAction::ScoreboardOperation {
                            op: ScoreboardOperationType::Add,
                            a: ScoreboardSlot::from(target),
                            b: ScoreboardSlot::from(source),
                        },
                        BinaryOperation::Subtract => CommandAction::ScoreboardOperation {
                            op: ScoreboardOperationType::Subtract,
                            a: ScoreboardSlot::from(target),
                            b: ScoreboardSlot::from(source),
                        },
                        BinaryOperation::Multiply => CommandAction::ScoreboardOperation {
                            op: ScoreboardOperationType::Multiply,
                            a: ScoreboardSlot::from(target),
                            b: ScoreboardSlot::from(source),
                        },
                        BinaryOperation::Divide => CommandAction::ScoreboardOperation {
                            op: ScoreboardOperationType::Divide,
                            a: ScoreboardSlot::from(target),
                            b: ScoreboardSlot::from(source),
                        },
                        BinaryOperation::Modulo => CommandAction::ScoreboardOperation {
                            op: ScoreboardOperationType::Modulo,
                            a: ScoreboardSlot::from(target),
                            b: ScoreboardSlot::from(source),
                        },
                        BinaryOperation::GreaterThan => CommandAction::ExecuteIf {
                            condition: Condition::ScoreCompare {
                                a: ScoreboardSlot::from(target),
                                b: ScoreboardSlot::from(source),
                                op: ConditionOperator::GreaterThan,
                            },
                            run: Box::new(CommandAction::SetScoreboardEntry { entry: ScoreboardSlot::from(target), value: 1 })
                        },
                        BinaryOperation::LessThan => CommandAction::ExecuteIf {
                            condition: Condition::ScoreCompare {
                                a: ScoreboardSlot::from(target),
                                b: ScoreboardSlot::from(source),
                                op: ConditionOperator::LessThan,
                            },
                            run: Box::new(CommandAction::SetScoreboardEntry { entry: ScoreboardSlot::from(target), value: 1 })
                        },
                        BinaryOperation::GreaterThanOrEquals => CommandAction::ExecuteIf {
                            condition: Condition::ScoreCompare {
                                a: ScoreboardSlot::from(target),
                                b: ScoreboardSlot::from(source),
                                op: ConditionOperator::GreaterThanOrEquals,
                            },
                            run: Box::new(CommandAction::SetScoreboardEntry { entry: ScoreboardSlot::from(target), value: 1 })
                        },
                        BinaryOperation::LessThanOrEquals => CommandAction::ExecuteIf {
                            condition: Condition::ScoreCompare {
                                a: ScoreboardSlot::from(target),
                                b: ScoreboardSlot::from(source),
                                op: ConditionOperator::LessThanOrEquals,
                            },
                            run: Box::new(CommandAction::SetScoreboardEntry { entry: ScoreboardSlot::from(target), value: 1 })
                        },
                        BinaryOperation::CheckEquals => CommandAction::ExecuteIf {
                            condition: Condition::ScoreCompare {
                                a: ScoreboardSlot::from(target),
                                b: ScoreboardSlot::from(source),
                                op: ConditionOperator::Equals,
                            },
                            run: Box::new(CommandAction::SetScoreboardEntry { entry: ScoreboardSlot::from(target), value: 1 })
                        },
                        BinaryOperation::NotEquals => CommandAction::ExecuteUnless {
                            condition: Condition::ScoreCompare {
                                a: ScoreboardSlot::from(target),
                                b: ScoreboardSlot::from(source),
                                op: ConditionOperator::Equals,
                            },
                            run: Box::new(CommandAction::SetScoreboardEntry { entry: ScoreboardSlot::from(target), value: 1 })
                        },
                        BinaryOperation::And => CommandAction::ScoreboardOperation {
                            op: ScoreboardOperationType::Multiply,
                            a: ScoreboardSlot::from(target),
                            b: ScoreboardSlot::from(source),
                        },
                        BinaryOperation::Or => CommandAction::Several(vec![
                            CommandAction::ScoreboardOperation {
                                op: ScoreboardOperationType::Add,
                                a: ScoreboardSlot::from(target),
                                b: ScoreboardSlot::from(source),
                            },
                            CommandAction::ScoreboardOperation {
                                op: ScoreboardOperationType::Divide,
                                a: ScoreboardSlot::from(target),
                                b: ScoreboardSlot::from(source),
                            },
                        ]),
                    }
                },
                Instruction::ToggleValue { target } => CommandAction::Several(vec![
                    CommandAction::ScoreboardRemove {
                        entry: ScoreboardSlot::from(target),
                        value: 1,
                    },
                    CommandAction::ExecuteIf {
                        condition: Condition::ScoreMatches {
                            a: ScoreboardSlot::from(target),
                            b: -1,
                        },
                        run: Box::new(CommandAction::SetScoreboardEntry {
                            entry: ScoreboardSlot::from(target),
                            value: 1,
                        }),
                    }
                ]),
                Instruction::ModifyValue { target, value } => {
                    match value {
                        0.. => CommandAction::ScoreboardAdd {
                            entry: ScoreboardSlot::from(target),
                            value: *value,
                        },
                        _ => CommandAction::ScoreboardRemove {
                            entry: ScoreboardSlot::from(target),
                            value: -*value,
                        },
                    }
                }
                Instruction::Call { function } => CommandAction::Call(function.clone()),
                Instruction::Return { source, size } => {
                    self.block_info.entry(block_id).or_insert(BlockInfo {
                        returns: true,
                        breaks: false
                    }).returns = true;

                    CommandAction::Several(vec![
                        CommandAction::SetScoreboardEntry {
                            entry: ScoreboardSlot::new(Objective(format!("{}.return", objective)), "flag".to_string()),
                            value: 1,
                        },
                        CommandAction::Return,
                    ])
                }
                Instruction::Break => {
                    self.block_info.entry(block_id).or_insert(BlockInfo {
                        returns: false,
                        breaks: true
                    }).breaks = true;
    
                    CommandAction::Several(vec![
                        CommandAction::SetScoreboardEntry {
                            entry: ScoreboardSlot::new(Objective(format!("{}.break", objective)), "flag".to_string()),
                            value: 1,
                        },
                        CommandAction::Return,
                    ])
                }
                Instruction::CreateBlock { id, is_loop, body } => {
                    self.compile_ir_sequence(objective, body, *id);

                    if let Some(block_info) = self.block_info.get_mut(id) {
                        if *is_loop {
                            block_info.breaks = false;
                        }
                    }

                    CommandAction::Noop
                }
                Instruction::EnterBlock { id } => {
                    let mut actions = vec![
                        CommandAction::Call(ResourceLocation::new(self.pack_name.clone(), format!("b{}", id)))
                    ];

                    ensure_control_flow(self, id, &mut actions);

                    CommandAction::Several(actions)
                }
                Instruction::IfValueMatchesRunBlock { source, value, block } => {
                    let mut actions = vec![
                        CommandAction::ExecuteIf {
                            condition: Condition::ScoreMatches {
                                a: ScoreboardSlot::from(source),
                                b: *value
                            },
                            run: Box::new(CommandAction::Call(ResourceLocation::new(self.pack_name.clone(), format!("b{}", block))))
                        }
                    ];

                    ensure_control_flow(self, block, &mut actions);

                    CommandAction::Several(actions)
                }
                Instruction::PlaceCommandLiteral(literal) => CommandAction::Literal(literal.clone()),
            });
        }
    
        if block_id != usize::MAX {
            self.functions.push(CompiledFunction {
                name: ResourceLocation::new(self.pack_name.clone(), format!("b{}", block_id)),
                actions
            });
        } else {
            self.functions.push(CompiledFunction {
                name: ResourceLocation::new(self.pack_name.clone(), objective.clone().0),
                actions
            });
        }
    }
}

struct BlockInfo {
    returns: bool,
    breaks: bool
}

pub enum CommandAction {
    Noop,
    SetScoreboardEntry {
        entry: ScoreboardSlot,
        value: i32,
    },
    ScoreboardOperation {
        op: ScoreboardOperationType,
        a: ScoreboardSlot,
        b: ScoreboardSlot,
    },
    ScoreboardAdd {
        entry: ScoreboardSlot,
        value: i32,
    },
    ScoreboardRemove {
        entry: ScoreboardSlot,
        value: i32,
    },
    ExecuteIf {
        condition: Condition,
        run: Box<CommandAction>,
    },
    ExecuteUnless {
        condition: Condition,
        run: Box<CommandAction>,
    },
    Several(Vec<CommandAction>),
    Call(ResourceLocation),
    Return,
    Literal(String),
}

impl Display for CommandAction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandAction::Noop => write!(f, ""),
            CommandAction::SetScoreboardEntry { entry, value } => {
                write!(f, "scoreboard players set {} {}", entry, value)
            }
            CommandAction::ScoreboardOperation { op, a, b } => {
                write!(f, "scoreboard players operation {} {} {}", a, op, b)
            }
            CommandAction::ScoreboardAdd { entry, value } => {
                write!(f, "scoreboard players add {} {}", entry, value)
            }
            CommandAction::ScoreboardRemove { entry, value } => {
                write!(f, "scoreboard players remove {} {}", entry, value)
            }
            CommandAction::ExecuteIf { condition, run } => {
                write!(f, "execute if {} run {}", condition, run)
            }
            CommandAction::ExecuteUnless { condition, run } => {
                write!(f, "execute unless {} run {}", condition, run)
            }
            CommandAction::Several(actions) => {
                write!(f, "{}", actions.iter().map(|a| format!("{}", a)).collect::<Vec<String>>().join("\n"))
            }
            CommandAction::Call(location) => write!(f, "function {}", location),
            CommandAction::Return => write!(f, "return 0"),
            CommandAction::Literal(literal) => write!(f, "{}", literal),
        }
    }
}

pub enum Condition {
    ScoreCompare {
        a: ScoreboardSlot,
        b: ScoreboardSlot,
        op: ConditionOperator,
    },
    ScoreMatches {
        a: ScoreboardSlot,
        b: i32
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Condition::ScoreCompare { a, b, op } => {
                write!(f, "score {} {} {}", a, op, b)
            }
            Condition::ScoreMatches { a, b } => {
                write!(f, "score {} matches {}", a, b)
            }
        }
    }
}

pub enum ConditionOperator {
    LessThan,
    LessThanOrEquals,
    Equals,
    GreaterThan,
    GreaterThanOrEquals,
}

impl Display for ConditionOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConditionOperator::LessThan => write!(f, "<"),
            ConditionOperator::LessThanOrEquals => write!(f, "<="),
            ConditionOperator::Equals => write!(f, "="),
            ConditionOperator::GreaterThan => write!(f, ">"),
            ConditionOperator::GreaterThanOrEquals => write!(f, ">="),
        }
    }
}

pub struct CompiledFunction {
    name: ResourceLocation,
    actions: Vec<CommandAction>,
}

impl CompiledFunction {
    pub fn name(&self) -> &ResourceLocation {
        &self.name
    }

    pub fn actions(&self) -> &[CommandAction] {
        &self.actions
    }
}

impl Display for CompiledFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: optimize
        write!(f, "{}", self.actions.iter().map(|a| format!("{}", a)).collect::<Vec<String>>().join("\n"))
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
