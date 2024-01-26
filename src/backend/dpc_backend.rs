use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use dpc::common::condition::Condition;
use dpc::common::function::{
    CallInterface, FunctionAnnotations, FunctionInterface, FunctionSignature, ReturnType,
};
use dpc::common::ty::{DataType, DataTypeContents, ScoreType, ScoreTypeContents};
use dpc::common::val::{MutableValue, Value};
use dpc::common::{DeclareBinding, Identifier, ResourceLocation as DPCResourceLocation};
use dpc::ir::{Block, IRFunction, InstrKind, Instruction as DPCInstruction, IR};
use dpc::output::datapack::Datapack;
use dpc::project::{OptimizationLevel, ProjectSettingsBuilder};
use dpc::{codegen_ir, CodegenIRSettings};

use crate::data::{Objective, ResourceLocation};
use crate::Config;

use itertools::Itertools;

use super::ir::{BinaryOperation, Instruction, IrFunction, ValueLocation};
use super::type_pool::TypePool;
use super::Backend;

pub struct DPCBackend;

impl Backend for DPCBackend {
    fn compile(config: &Config, ir: &[IrFunction], types: &TypePool) {
        let datapack = dpc_codegen(ir, config, types).expect("Failed to do DPC codegen");
        datapack
            .output(&PathBuf::from(format!("./{}", config.pack)))
            .expect("Failed to output pack");
    }
}

fn dpc_codegen(
    functions: &[IrFunction],
    config: &Config,
    types: &TypePool,
) -> Result<Datapack, String> {
    // dbg!(&functions);
    let mut ir = IR::new();
    let mut blocks = HashMap::new();
    for function in functions {
        let mut ret_len = 0;
        let mut defs = DefManager::new();
        let mut block = codegen_block(
            function.body(),
            function.objective(),
            function.signature(),
            &mut blocks,
            &mut defs,
            &mut ret_len,
        );
        // Insert hoisted defs
        let prelude = defs.hoisted_defs.into_iter().sorted().map(|x| {
            DPCInstruction::new(InstrKind::Declare {
                left: x.get_reg(),
                ty: DataType::Score(ScoreType::Score),
                right: DeclareBinding::Null,
            })
        });
        block.contents = prelude.chain(block.contents).collect();

        let function_id = format!("{}:{}", config.pack, function.objective().0);
        let params = function
            .signature()
            .params()
            .iter()
            .map(|_| DataType::Score(ScoreType::Score))
            .collect();
        let ret = function.signature().return_type();
        let ret = ret.from(types);
        let ret = if ret.is_none() {
            ReturnType::Void
        } else {
            ReturnType::Standard(
                (0..ret_len)
                    .into_iter()
                    .map(|_| DataType::Score(ScoreType::Score))
                    .collect(),
            )
        };
        let annotations = FunctionAnnotations {
            preserve: true,
            no_inline: false,
            no_strip: false,
        };
        let interface = FunctionInterface::with_all(
            function_id.clone().into(),
            FunctionSignature::with_all(params, ret),
            annotations,
        );
        ir.functions
            .insert(function_id.into(), IRFunction { interface, block });
    }

    // dbg!(&ir);

    let proj = ProjectSettingsBuilder::new(&config.pack);
    let proj = proj.op_level(OptimizationLevel::More);
    let settings = CodegenIRSettings {
        debug: false,
        debug_functions: false,
        ir_passes: true,
        mir_passes: true,
        lir_passes: true,
    };
    let datapack = codegen_ir(ir, &proj.build(), settings).map_err(|x| format!("{x:?}"))?;
    Ok(datapack)
}

fn codegen_block(
    body: &[Instruction],
    func_obj: &Objective,
    func_sig: &super::function::FunctionSignature,
    blocks: &mut HashMap<usize, Block>,
    defs: &mut DefManager,
    ret_len: &mut usize,
) -> Block {
    let mut block = Block::new();
    let mut calls = HashMap::new();
    let mut finished_calls = Vec::new();
    for (i, instr) in body.into_iter().enumerate() {
        let instr = match instr {
            Instruction::CreateBlock { id, is_loop, body } => {
                if *is_loop {
                    panic!("Looping is not supported yet");
                }
                let subblock = codegen_block(body, func_obj, func_sig, blocks, defs, ret_len);
                blocks.insert(*id, subblock);
                None
            }
            Instruction::Call { function } => {
                let entry = calls.entry(function.path.clone()).or_insert_with(|| {
                    CallBuilder::from_resource_loc(function.clone(), block.contents.len())
                });
                entry.pos = block.contents.len();
                entry.resource_location = Some(function.to_string().into());
                if entry.can_be_finished {
                    finished_calls.extend(calls.remove(&function.path));
                } else {
                    entry.can_be_finished = true;
                }
                Some(InstrKind::Comment {
                    comment: format!("call @{}", function),
                })
            }
            Instruction::SetValueToConstant { target, constant } => {
                let val =
                    Value::Constant(DataTypeContents::Score(ScoreTypeContents::Score(*constant)));
                if let Some(arg) = target.objective.get_arg(func_obj) {
                    let entry = calls
                        .entry(arg.to_string())
                        .or_insert_with(|| CallBuilder::from_obj(arg));
                    if entry.can_be_finished {
                        finished_calls.extend(calls.remove(arg));
                    } else {
                        entry.args.push(val);
                    }
                    None
                } else {
                    defs.ensure_defined(target);
                    Some(InstrKind::Assign {
                        left: MutableValue::Reg(target.get_reg()),
                        right: val,
                    })
                }
            }
            Instruction::SetValueToValue { target, source } => {
                let val = source.get_val(func_sig);
                if let Some(ret) = source.objective.get_ret(func_obj) {
                    defs.ensure_defined(target);
                    let entry = calls
                        .entry(ret.to_string())
                        .or_insert_with(|| CallBuilder::from_obj(ret));
                    entry.rets.push(target.get_val(func_sig));
                    entry.can_be_finished = true;
                    None
                } else {
                    defs.ensure_defined(source);
                    if let Some(arg) = target.objective.get_arg(func_obj) {
                        let entry = calls
                            .entry(arg.to_string())
                            .or_insert_with(|| CallBuilder::from_obj(arg));
                        if entry.can_be_finished {
                            finished_calls.extend(calls.remove(arg));
                            let mut new_call = CallBuilder::from_obj(arg);
                            new_call.args.push(Value::Mutable(val));
                            calls.insert(arg.to_string(), new_call);
                        } else {
                            entry.args.push(Value::Mutable(val));
                        }
                        None
                    } else {
                        defs.ensure_defined(target);
                        Some(InstrKind::Assign {
                            left: MutableValue::Reg(target.get_reg()),
                            right: Value::Mutable(val),
                        })
                    }
                }
            }
            Instruction::ValueBinaryOperation { target, op, source } => {
                defs.ensure_defined(target);
                defs.ensure_defined(source);
                Some(op.to_instr_kind(target, source, &mut block.contents, defs, func_sig))
            }
            Instruction::ToggleValue { target } => {
                defs.ensure_defined(target);
                Some(InstrKind::Not {
                    value: MutableValue::Reg(target.get_reg()),
                })
            }
            Instruction::ModifyValue { target, value } => {
                defs.ensure_defined(target);
                Some(InstrKind::Add {
                    left: MutableValue::Reg(target.get_reg()),
                    right: Value::Constant(DataTypeContents::Score(ScoreTypeContents::Score(
                        *value,
                    ))),
                })
            }
            Instruction::PlaceCommandLiteral(lit) => Some(InstrKind::Command {
                command: lit.clone(),
            }),
            Instruction::EnterBlock { id } => {
                let block = blocks.get(id).expect("Block was not created");
                // Workaround for DPC not having an inline block instruction
                Some(InstrKind::If {
                    condition: Condition::Bool(Value::Constant(DataTypeContents::Score(
                        ScoreTypeContents::Bool(true),
                    ))),
                    body: Box::new(block.clone()),
                })
            }
            Instruction::IfValueMatchesRunBlock {
                source,
                value,
                block,
            } => {
                let block = blocks.get(block).expect("Block was not created");
                // Workaround for DPC not having an inline block instruction
                Some(InstrKind::If {
                    condition: Condition::Equal(
                        Value::Mutable(source.get_val(func_sig)),
                        Value::Constant(DataTypeContents::Score(ScoreTypeContents::Score(*value))),
                    ),
                    body: Box::new(block.clone()),
                })
            }
            Instruction::Return { source, size } => {
                if let Some(source) = source {
                    for i in 0..*size {
                        let val = source.offset(i);
                        block
                            .contents
                            .push(DPCInstruction::new(InstrKind::ReturnValue {
                                index: i,
                                value: Value::Mutable(val.get_val(func_sig)),
                            }));
                    }
                }
                *ret_len = *size;
                if i == body.len() - 1 {
                    None
                } else {
                    Some(InstrKind::Return {
                        value: Value::Constant(DataTypeContents::Score(ScoreTypeContents::Score(
                            0,
                        ))),
                    })
                }
            }
            _ => None,
        };
        if let Some(instr) = instr {
            block.contents.push(DPCInstruction::new(instr));
        }
    }

    // dbg!(&calls, &finished_calls);

    // Insert calls
    for call in calls
        .into_iter()
        .map(|x| x.1)
        .chain(finished_calls.into_iter())
    {
        if let Some(function) = call.resource_location {
            let instr = block
                .contents
                .get_mut(call.pos)
                .expect("Index should exist");
            if instr.kind
                != (InstrKind::Comment {
                    comment: format!("call @{}", function),
                })
            {
                panic!("Call inserted in wrong place");
            }
            instr.kind = InstrKind::Call {
                call: CallInterface {
                    function,
                    args: call.args,
                    ret: call.rets,
                },
            };
        }
    }

    block
}

/// Helper struct used for register definitions
#[derive(Default)]
struct DefManager {
    // All register definitions that have been done
    defs: HashSet<ValueLocation>,
    // Register definitions to be put at the top of the block
    hoisted_defs: HashSet<ValueLocation>,
    counter: u32,
}

impl DefManager {
    fn new() -> Self {
        Self::default()
    }

    fn ensure_defined(&mut self, val: &ValueLocation) {
        if !self.defs.contains(val) {
            self.hoisted_defs.insert(val.clone());
        }
    }

    fn new_reg(&mut self) -> Identifier {
        let old_val = self.counter;
        self.counter += 1;
        format!("_sculk_backend_{old_val}").into()
    }
}

impl ValueLocation {
    fn get_reg(&self) -> Identifier {
        format!("sculk_val_{}", self).replace(' ', "_").into()
    }

    fn get_val(&self, func_sig: &super::function::FunctionSignature) -> MutableValue {
        // Sculk uses the slots that are within the length of the
        // parameters as arguments
        if self.slot < func_sig.params().len() {
            MutableValue::Arg(self.slot)
        } else {
            MutableValue::Reg(self.get_reg())
        }
    }
}

impl BinaryOperation {
    fn to_instr_kind(
        &self,
        left: &ValueLocation,
        right: &ValueLocation,
        prelude: &mut Vec<DPCInstruction>,
        defs: &mut DefManager,
        func_sig: &super::function::FunctionSignature,
    ) -> InstrKind {
        let left = MutableValue::Reg(left.get_reg());
        let right = Value::Mutable(right.get_val(func_sig));

        fn create_cond_op(
            left: MutableValue,
            condition: Condition,
            prelude: &mut Vec<DPCInstruction>,
            defs: &mut DefManager,
        ) -> InstrKind {
            let new_reg = defs.new_reg();
            prelude.push(DPCInstruction::new(InstrKind::Declare {
                left: new_reg.clone(),
                ty: DataType::Score(ScoreType::Bool),
                right: DeclareBinding::Condition(condition),
            }));
            InstrKind::Assign {
                left,
                right: Value::Mutable(MutableValue::Reg(new_reg)),
            }
        }

        match self {
            Self::Add => InstrKind::Add { left, right },
            Self::Subtract => InstrKind::Sub { left, right },
            Self::Multiply => InstrKind::Mul { left, right },
            Self::Divide => InstrKind::Div { left, right },
            Self::Modulo => InstrKind::Mod { left, right },
            Self::And => InstrKind::And { left, right },
            Self::Or => InstrKind::Or { left, right },
            Self::CheckEquals => create_cond_op(
                left.clone(),
                Condition::Equal(Value::Mutable(left), right),
                prelude,
                defs,
            ),
            Self::NotEquals => create_cond_op(
                left.clone(),
                Condition::Not(Box::new(Condition::Equal(Value::Mutable(left), right))),
                prelude,
                defs,
            ),
            Self::GreaterThan => create_cond_op(
                left.clone(),
                Condition::GreaterThan(Value::Mutable(left), right),
                prelude,
                defs,
            ),
            Self::GreaterThanOrEquals => create_cond_op(
                left.clone(),
                Condition::GreaterThanOrEqual(Value::Mutable(left), right),
                prelude,
                defs,
            ),
            Self::LessThan => create_cond_op(
                left.clone(),
                Condition::LessThan(Value::Mutable(left), right),
                prelude,
                defs,
            ),
            Self::LessThanOrEquals => create_cond_op(
                left.clone(),
                Condition::LessThanOrEqual(Value::Mutable(left), right),
                prelude,
                defs,
            ),
        }
    }
}

/// Used for storing information about a call,
/// which sculk IR splits up into multiple instructions,
/// so that it can be combined for DPC
#[derive(Debug)]
struct CallBuilder {
    #[allow(dead_code)]
    func_id: String,
    pos: usize,
    resource_location: Option<DPCResourceLocation>,
    args: Vec<Value>,
    rets: Vec<MutableValue>,
    can_be_finished: bool,
}

impl CallBuilder {
    fn from_obj(obj: &str) -> Self {
        Self {
            func_id: obj.to_string(),
            pos: 0,
            resource_location: None,
            args: Vec::new(),
            rets: Vec::new(),
            can_be_finished: false,
        }
    }

    fn from_resource_loc(loc: ResourceLocation, pos: usize) -> Self {
        Self {
            func_id: loc.path.clone(),
            pos,
            resource_location: Some(loc.to_string().into()),
            args: Vec::new(),
            rets: Vec::new(),
            can_be_finished: true,
        }
    }
}

impl Objective {
    fn split(&self) -> (&str, Option<&str>) {
        if let Some(period) = self.0.rfind('.') {
            let split = self.0.split_at(period);
            (split.0, Some(&split.1[1..]))
        } else {
            (&self.0, None)
        }
    }

    fn get_arg(&self, func_obj: &Objective) -> Option<&str> {
        if self.0 != func_obj.0 {
            Some(&self.0)
        } else {
            None
        }
    }

    fn get_ret(&self, func_obj: &Objective) -> Option<&str> {
        let split = self.split();
        if let Some(right) = split.1 {
            if right == "return" && self.0 != func_obj.0 {
                Some(split.0)
            } else {
                None
            }
        } else {
            None
        }
    }
}
