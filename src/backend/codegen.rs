use std::{
    collections::{HashMap},
    io::Write,
    path::Path,
    sync::atomic::{AtomicI32, Ordering},
};

use crate::{
    data::{ResourceLocation, ScoreboardVariable},
    parser::{FunctionDefinition, Operation, ParseError, Parser, ParserNode},
    types::SculkType,
};

use super::{
    rebranch,
    validate::{ValidationError, Validator},
};

pub struct CodeGenerator {
    unfinished_functions: Vec<Function>,
    ready_functions: HashMap<String, Function>,
    func_defs: HashMap<String, FunctionDefinition>,
    eval_stacks: Vec<EvaluationStack>,
    bin_op_depth: i32,
    anon_func_depth: i32,
    flag_tmp_count: i32,
    namespace: String,
}

impl CodeGenerator {
    pub fn compile_src(src: &str, namespace: &str) -> Result<Self, Vec<CompileError>> {
        let parser = Parser::new(src);
        let mut errors = Vec::new();

        let mut parse_output = parser.parse();

        let validator = Validator::new();
        let validation_errs = validator.validate_program(&parse_output.ast);

        errors.extend(
            parse_output
                .errs
                .into_iter()
                .map(|err| CompileError::Parse(err)),
        );

        errors.extend(
            validation_errs
                .into_iter()
                .map(|err| CompileError::Validate(err)),
        );

        if errors.len() > 0 {
            return Err(errors);
        }

        rebranch::rebranch(&mut parse_output.ast);

        let mut sculk_main = Function::new_empty(
            "_sculkmain".to_string(),
            ResourceLocation::new(namespace.to_string(), "_sculkmain".to_string()),
            vec![],
            SculkType::None,
        );

        let mut gen = Self {
            unfinished_functions: vec![],
            ready_functions: HashMap::new(),
            func_defs: parse_output.func_defs,
            eval_stacks: vec![],
            bin_op_depth: 0,
            anon_func_depth: 0,
            flag_tmp_count: 0,
            namespace: namespace.to_string(),
        };

        gen.compile(&parse_output.ast);

        for func in gen.ready_functions.values().filter(|func| !func.anonymous) {
            sculk_main.actions.push(Action::CreateStorage {
                name: ResourceLocation::new(namespace.to_string(), func.name.clone())
                    .as_scoreboard(),
            });
        }

        sculk_main.actions.push(Action::CallFunction {
            target: ResourceLocation::new(namespace.to_string(), "main".to_string()),
        });

        gen.ready_functions
            .insert("_sculkmain".to_string(), sculk_main);

        Ok(gen)
    }

    // TODO: no more unwraps here
    pub fn output_to_dir(&self, dir: &Path) {
        std::fs::create_dir_all(dir).unwrap();
        let mut output = String::new();

        for (_, func) in &self.ready_functions {
            for action in &func.actions {
                Self::write_action(&mut output, &action);
                output.push_str("\r\n");
            }

            let mut path = dir.to_path_buf();
            path.push(&func.name);
            path.set_extension("mcfunction");

            let mut file = std::fs::File::create(path).unwrap();
            file.write(output.as_bytes()).unwrap();

            output.clear();
        }
    }

    fn compile(&mut self, ast: &ParserNode) {
        self.visit_node(ast);
    }

    fn visit_node(&mut self, node: &ParserNode) {
        match node {
            ParserNode::NumberLiteral(num) => self.visit_number(*num),
            ParserNode::BoolLiteral(bool) => self.visit_bool(*bool),
            ParserNode::Identifier(name) => self.visit_identifier(name),
            ParserNode::Operation(lhs, rhs, op) => self.visit_binary_operation(lhs, rhs, *op),
            ParserNode::VariableDeclaration { name, expr, ty: _ } => {
                self.visit_variable_assignment(name, expr)
            }
            ParserNode::VariableAssignment { name, expr } => {
                self.visit_variable_assignment(name, expr)
            }
            ParserNode::Program(nodes) => self.visit_program(nodes),
            ParserNode::FunctionDeclaration {
                name,
                args,
                return_ty,
                body,
            } => self.visit_function_declaration(name, args, return_ty, body),
            ParserNode::FunctionCall { name, args } => self.visit_function_call(name, args),
            ParserNode::Return(expr) => self.visit_return(expr),
            ParserNode::Block(nodes) => self.visit_block(nodes),
            ParserNode::SelectorLiteral(_selector) => todo!(),
            ParserNode::TypedIdentifier { .. } => {}
            ParserNode::Unary(expr, op) => self.visit_unary(expr, *op),
            ParserNode::If {
                cond,
                body,
                else_ifs,
                else_body,
            } => self.visit_if(cond, body, else_ifs, else_body),
            ParserNode::ReturnSafe(expr) => {
                self.visit_return_safe(expr);
            }
        }
    }

    fn visit_program(&mut self, nodes: &[ParserNode]) {
        for node in nodes {
            self.visit_node(node);
        }
    }

    fn visit_block(&mut self, nodes: &[ParserNode]) {
        for node in nodes {
            self.visit_node(node);
        }
    }

    fn visit_number(&mut self, num: i32) {
        self.push_eval_instr(EvaluationInstruction::PushNumber(num));
    }

    fn visit_bool(&mut self, bool: bool) {
        self.push_eval_instr(EvaluationInstruction::PushBool(bool));
    }

    // TODO: implement shadowing
    fn visit_identifier(&mut self, name: &str) {
        self.push_eval_instr(EvaluationInstruction::PushVariable(
            self.local_variable(name),
        ));
    }

    fn visit_binary_operation(&mut self, lhs: &ParserNode, rhs: &ParserNode, op: Operation) {
        self.bin_op_depth += 1;

        self.visit_node(lhs);
        self.visit_node(rhs);
        self.push_eval_instr(EvaluationInstruction::Operation(op));

        self.bin_op_depth -= 1;
    }

    fn visit_unary(&mut self, expr: &ParserNode, op: Operation) {
        match op {
            Operation::Negate => {
                self.visit_node(expr);
                self.push_eval_instr(EvaluationInstruction::PushNumber(-1));
                self.push_eval_instr(EvaluationInstruction::Operation(Operation::Multiply));
            }
            Operation::Not => {
                self.push_eval_instr(EvaluationInstruction::PushNumber(1));
                self.visit_node(expr);
                self.push_eval_instr(EvaluationInstruction::Operation(Operation::Subtract));
            }
            _ => unreachable!(),
        }
    }

    fn visit_variable_assignment(&mut self, name: &str, val: &ParserNode) {
        self.begin_evaluation_for_scoreboard(self.active_scoreboard(), 0);
        self.visit_node(val);
        let result_tmp = self.end_current_evaluation();
        let var = self.local_variable(name);
        self.emit_action(Action::SetVariableToVariable {
            first: var,
            second: self.get_tmp(result_tmp),
        });
    }

    // TODO: stop people from defining the same function twice or within other functions
    fn visit_function_declaration(
        &mut self,
        name: &str,
        args: &[ParserNode],
        return_ty: &SculkType,
        body: &ParserNode,
    ) {
        self.unfinished_functions.push(Function::new_empty(
            name.to_string(),
            self.resource_location(name),
            args.iter()
                .map(|node| node.as_identifier().to_string())
                .collect(),
            return_ty.clone(),
        ));

        if !return_ty.is_none() {
            self.emit_action(Action::SetVariableToNumber {
                var: self.local_variable("_RETFLAG"),
                val: 0,
            });
        }

        self.visit_node(body);

        self.ready_functions
            .insert(name.to_string(), self.unfinished_functions.pop().unwrap());
        self.flag_tmp_count = 0;
    }

    fn visit_function_call(&mut self, name: &str, args: &[ParserNode]) {
        let use_new_stack = self.eval_stacks.is_empty();

        if use_new_stack {
            self.begin_evaluation_for_scoreboard(self.active_scoreboard(), 0);
        }

        for arg in args.iter() {
            self.visit_node(arg);
        }

        self.push_eval_instr(EvaluationInstruction::CallFunction(
            self.resource_location(name),
            self.func_defs[name].args.clone(),
        ));

        if use_new_stack {
            self.end_current_evaluation();
        }
    }

    fn visit_return(&mut self, expr: &ParserNode) {
        self.begin_evaluation_for_scoreboard(self.active_scoreboard(), 0);
        self.visit_node(expr);
        let result_tmp = self.end_current_evaluation();

        self.emit_action(Action::ExecuteIf {
            condition: format!("score {} matches 0", self.local_variable("_RETFLAG")),
            then: Box::new(Action::SetVariableToVariable {
                first: self.local_variable("_RET"),
                second: self.get_tmp(result_tmp),
            }),
        });

        self.emit_action(Action::ExecuteIf {
            condition: format!("score {} matches 0", self.local_variable("_RETFLAG")),
            then: Box::new(Action::SetVariableToNumber {
                var: self.local_variable("_RETFLAG"),
                val: 1,
            }),
        });
    }

    fn visit_return_safe(&mut self, expr: &ParserNode) {
        let not_ret_func = self.current_function().make_anonymous_child();
        let not_ret_func_name = not_ret_func.name.clone();

        self.unfinished_functions.push(not_ret_func);
        self.visit_node(expr);
        self.ready_functions
            .insert(not_ret_func_name.clone(), self.unfinished_functions.pop().unwrap());

        self.emit_action(Action::ExecuteUnless {
            condition: format!("score {} matches 1", self.local_variable("_RETFLAG")),
            then: Box::new(Action::CallFunction {
                target: self.resource_location(&not_ret_func_name),
            }),
        });
    }

    fn visit_if(
        &mut self,
        cond: &ParserNode,
        body: &ParserNode,
        _else_ifs: &[(ParserNode, ParserNode)], // at this stage, there are no else-ifs, they have been converted to nested ifs
        else_body: &Option<Box<ParserNode>>,
    ) {
        self.begin_evaluation_for_scoreboard(self.active_scoreboard(), 0);
        self.visit_node(cond);
        let result_tmp = self.end_current_evaluation();
        let flag_tmp = self.flag_tmp_count;
        self.flag_tmp_count += 1;

        let true_func = self.current_function().make_anonymous_child();
        let true_func_name = true_func.name.clone();

        self.unfinished_functions.push(true_func);
        self.visit_node(body);
        self.ready_functions.insert(
            true_func_name.clone(),
            self.unfinished_functions.pop().unwrap(),
        );

        self.emit_action(Action::SetVariableToVariable {
            first: self.get_flag(flag_tmp),
            second: self.get_tmp(result_tmp),
        });
        self.emit_action(Action::ExecuteIf {
            condition: format!("score {} matches 1", self.get_flag(flag_tmp)),
            then: Box::new(Action::CallFunction {
                target: self.resource_location(&true_func_name),
            }),
        });

        if let Some(else_body) = else_body {
            let false_func = self.current_function().make_anonymous_child();
            let false_func_name = false_func.name.clone();

            self.unfinished_functions.push(false_func);
            self.visit_node(else_body);
            self.ready_functions.insert(
                false_func_name.clone(),
                self.unfinished_functions.pop().unwrap(),
            );

            self.emit_action(Action::ExecuteUnless {
                condition: format!("score {} matches 1", self.get_flag(flag_tmp)),
                then: Box::new(Action::CallFunction {
                    target: self.resource_location(&false_func_name),
                }),
            });
        }
    }

    fn emit_action(&mut self, action: Action) {
        self.current_function_mut().actions.push(action);
    }

    fn current_function(&self) -> &Function {
        self.unfinished_functions.last().unwrap()
    }

    fn current_function_mut(&mut self) -> &mut Function {
        self.unfinished_functions.last_mut().unwrap()
    }

    fn push_eval_instr(&mut self, instr: EvaluationInstruction) {
        self.eval_stacks.last_mut().unwrap().push_instruction(instr);
    }

    fn get_tmp(&self, num: i32) -> ScoreboardVariable {
        self.local_variable(&format!("_TMP{}", num))
    }

    fn get_flag(&self, num: i32) -> ScoreboardVariable {
        self.local_variable(&format!("_FLAG{}", num))
    }

    fn active_scoreboard(&self) -> ResourceLocation {
        self.current_function().scoreboard.clone()
    }

    fn begin_evaluation_for_scoreboard(&mut self, scoreboard: ResourceLocation, min_tmp: i32) {
        self.eval_stacks
            .push(EvaluationStack::new(scoreboard, min_tmp));
    }

    fn end_current_evaluation(&mut self) -> i32 {
        let mut last_stack = self.eval_stacks.pop().unwrap();
        let result_tmp = last_stack.flush();

        let actions = last_stack.actions;

        for action in actions {
            self.emit_action(action);
        }

        result_tmp
    }

    fn write_action(str: &mut String, action: &Action) {
        match action {
            Action::CreateStorage { name } => {
                str.push_str(&format!("scoreboard objectives add {} dummy", name))
            }
            Action::SetVariableToNumber { var: name, val } => {
                str.push_str(&format!("scoreboard players set {} {}", name, val))
            }
            Action::AddVariables { first, second } => str.push_str(&format!(
                "scoreboard players operation {} += {}",
                first, second
            )),
            Action::SubtractVariables { first, second } => str.push_str(&format!(
                "scoreboard players operation {} -= {}",
                first, second
            )),
            Action::MultiplyVariables { first, second } => str.push_str(&format!(
                "scoreboard players operation {} *= {}",
                first, second
            )),
            Action::DivideVariables { first, second } => str.push_str(&format!(
                "scoreboard players operation {} /= {}",
                first, second
            )),
            Action::ModuloVariables { first, second } => str.push_str(&format!(
                "scoreboard players operation {} %= {}",
                first, second
            )),
            Action::SetVariableToVariable { first, second } => str.push_str(&format!(
                "scoreboard players operation {} = {}",
                first, second
            )),
            Action::CallFunction { target } => str.push_str(&format!("function {}", target)),
            Action::ExecuteIf { condition, then } => {
                str.push_str(&format!("execute if {} run ", condition));
                Self::write_action(str, then);
            }
            Action::ExecuteUnless { condition, then } => {
                str.push_str(&format!("execute unless {} run ", condition));
                Self::write_action(str, then);
            }
        }
    }

    fn resource_location(&self, path: &str) -> ResourceLocation {
        ResourceLocation::new(self.namespace.clone(), path.to_string())
    }

    fn local_variable(&self, name: &str) -> ScoreboardVariable {
        ScoreboardVariable::new(self.current_function().scoreboard.clone(), name.to_string())
    }
}

#[derive(Debug)]
enum Action {
    CreateStorage {
        name: String,
    },
    SetVariableToNumber {
        var: ScoreboardVariable,
        val: i32,
    },
    AddVariables {
        first: ScoreboardVariable,
        second: ScoreboardVariable,
    },
    SubtractVariables {
        first: ScoreboardVariable,
        second: ScoreboardVariable,
    },
    MultiplyVariables {
        first: ScoreboardVariable,
        second: ScoreboardVariable,
    },
    DivideVariables {
        first: ScoreboardVariable,
        second: ScoreboardVariable,
    },
    ModuloVariables {
        first: ScoreboardVariable,
        second: ScoreboardVariable,
    },
    SetVariableToVariable {
        first: ScoreboardVariable,
        second: ScoreboardVariable,
    },
    CallFunction {
        target: ResourceLocation,
    },
    ExecuteIf {
        condition: String,
        then: Box<Action>,
    },
    ExecuteUnless {
        condition: String,
        then: Box<Action>,
    },
}

struct Function {
    name: String,
    scoreboard: ResourceLocation,
    args: HashMap<String, usize>,
    idx_to_arg: HashMap<usize, String>,
    return_ty: SculkType,
    actions: Vec<Action>,
    anonymous: bool,
}

static ANONYMOUS_FUNC_COUNT: AtomicI32 = AtomicI32::new(0);

impl Function {
    fn new_empty(
        name: String,
        scoreboard: ResourceLocation,
        args: Vec<String>,
        return_ty: SculkType,
    ) -> Self {
        Function {
            name,
            scoreboard,
            args: args
                .iter()
                .enumerate()
                .map(|(i, arg)| (arg.clone(), i))
                .collect(),
            idx_to_arg: args
                .iter()
                .enumerate()
                .map(|(i, arg)| (i, arg.clone()))
                .collect(),
            return_ty,
            actions: Vec::new(),
            anonymous: false,
        }
    }

    fn new_empty_mapped_args(
        name: String,
        scoreboard: ResourceLocation,
        args: HashMap<String, usize>,
        idx_to_arg: HashMap<usize, String>,
        return_ty: SculkType,
    ) -> Self {
        Function {
            name,
            scoreboard,
            args,
            idx_to_arg,
            return_ty,
            actions: Vec::new(),
            anonymous: false,
        }
    }

    fn make_anonymous_child(&self) -> Self {
        let mut func = Function::new_empty_mapped_args(
            format!("anon_{}", ANONYMOUS_FUNC_COUNT.load(Ordering::Relaxed)),
            self.scoreboard.clone(),
            self.args.clone(),
            self.idx_to_arg.clone(),
            self.return_ty.clone(),
        );
        ANONYMOUS_FUNC_COUNT.fetch_add(1, Ordering::SeqCst);
        func.anonymous = true;
        func
    }
}

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    Validate(ValidationError),
    InvalidTypes,
}

impl CompileError {
    fn parse_error(error: ParseError) -> Self {
        CompileError::Parse(error)
    }
}

#[derive(Debug, Clone)]
enum EvaluationInstruction {
    PushNumber(i32),
    PushBool(bool),
    PushVariable(ScoreboardVariable),
    Operation(Operation),
    CallFunction(ResourceLocation, Vec<String>),
}

impl EvaluationInstruction {
    fn as_operation(self) -> Operation {
        match self {
            EvaluationInstruction::Operation(op) => op,
            _ => panic!("Cannot convert non-operation to operation"),
        }
    }
}

struct EvaluationStack {
    instructions: Vec<EvaluationInstruction>,
    actions: Vec<Action>,
    available_tmps: Vec<i32>,
    max_tmps: i32,
    scoreboard: ResourceLocation,
}

impl EvaluationStack {
    fn new(scoreboard: ResourceLocation, min_tmp: i32) -> Self {
        EvaluationStack {
            instructions: Vec::new(),
            actions: Vec::new(),
            available_tmps: Vec::new(),
            max_tmps: min_tmp,
            scoreboard,
        }
    }

    fn push_instruction(&mut self, instr: EvaluationInstruction) {
        self.instructions.push(instr);
    }

    fn flush(&mut self) -> i32 {
        // keep track of tmps that were used for intermediate operations
        // we need to free them after the full operation is done
        let mut intermediate_tmps = Vec::new();

        for i in 0..self.instructions.len() {
            let instr = self.instructions[i].clone(); // i am so mad

            match instr {
                EvaluationInstruction::PushNumber(num) => {
                    let tmp_idx = self.reserve_available_tmp();
                    let tmp_var = self.get_tmp(tmp_idx);
                    self.emit_action(Action::SetVariableToNumber {
                        var: tmp_var,
                        val: num,
                    });
                    intermediate_tmps.push(tmp_idx);
                }
                EvaluationInstruction::PushBool(bool) => {
                    let tmp_idx = self.reserve_available_tmp();
                    let tmp_var = self.get_tmp(tmp_idx);
                    let bool_val = if bool { 1 } else { 0 };
                    self.emit_action(Action::SetVariableToNumber {
                        var: tmp_var,
                        val: bool_val,
                    });
                    intermediate_tmps.push(tmp_idx);
                }
                EvaluationInstruction::PushVariable(name) => {
                    let tmp_idx = self.reserve_available_tmp();
                    let tmp_var = self.get_tmp(tmp_idx);
                    self.emit_action(Action::SetVariableToVariable {
                        first: tmp_var,
                        second: name,
                    });
                    intermediate_tmps.push(tmp_idx);
                }
                EvaluationInstruction::Operation(op) => {
                    let tmp_b_idx = intermediate_tmps.pop().unwrap();
                    let tmp_a_idx = *intermediate_tmps.last().unwrap();

                    let tmp_a_var = self.get_tmp(tmp_a_idx);
                    let tmp_b_var = self.get_tmp(tmp_b_idx);

                    match op {
                        Operation::Add => self.emit_action(Action::AddVariables {
                            first: tmp_a_var,
                            second: tmp_b_var,
                        }),
                        Operation::Subtract => self.emit_action(Action::SubtractVariables {
                            first: tmp_a_var,
                            second: tmp_b_var,
                        }),
                        Operation::Multiply => self.emit_action(Action::MultiplyVariables {
                            first: tmp_a_var,
                            second: tmp_b_var,
                        }),
                        Operation::Divide => self.emit_action(Action::DivideVariables {
                            first: tmp_a_var,
                            second: tmp_b_var,
                        }),
                        Operation::Modulo => self.emit_action(Action::ModuloVariables {
                            first: tmp_a_var,
                            second: tmp_b_var,
                        }),
                        Operation::GreaterThan => {
                            self.emit_action(Action::SubtractVariables {
                                first: tmp_a_var.clone(),
                                second: tmp_b_var.clone(),
                            });
                            self.emit_action(Action::ExecuteIf {
                                condition: format!("score {} matches 1..", &tmp_a_var),
                                then: Box::new(Action::SetVariableToNumber {
                                    var: tmp_a_var,
                                    val: 1,
                                }),
                            });
                        }
                        Operation::LessThan => {
                            self.emit_action(Action::SubtractVariables {
                                first: tmp_a_var.clone(),
                                second: tmp_b_var.clone(),
                            });
                            self.emit_action(Action::ExecuteIf {
                                condition: format!("score {} matches ..-1", &tmp_a_var),
                                then: Box::new(Action::SetVariableToNumber {
                                    var: tmp_a_var,
                                    val: 1,
                                }),
                            });
                        }
                        Operation::GreaterThanOrEquals => {
                            self.emit_action(Action::SubtractVariables {
                                first: tmp_a_var.clone(),
                                second: tmp_b_var.clone(),
                            });
                            self.emit_action(Action::ExecuteIf {
                                condition: format!("score {} matches 0..", &tmp_a_var),
                                then: Box::new(Action::SetVariableToNumber {
                                    var: tmp_a_var,
                                    val: 1,
                                }),
                            });
                        }
                        Operation::LessThanOrEquals => {
                            self.emit_action(Action::SubtractVariables {
                                first: tmp_a_var.clone(),
                                second: tmp_b_var.clone(),
                            });
                            self.emit_action(Action::ExecuteIf {
                                condition: format!("score {} matches ..0", &tmp_a_var),
                                then: Box::new(Action::SetVariableToNumber {
                                    var: tmp_a_var,
                                    val: 1,
                                }),
                            });
                        }
                        Operation::CheckEquals => {
                            self.emit_action(Action::SubtractVariables {
                                first: tmp_a_var.clone(),
                                second: tmp_b_var.clone(),
                            });
                            self.emit_action(Action::ExecuteIf {
                                condition: format!("score {} matches 0", &tmp_a_var),
                                then: Box::new(Action::SetVariableToNumber {
                                    var: tmp_a_var,
                                    val: 1,
                                }),
                            });
                        }
                        Operation::NotEquals => {
                            self.emit_action(Action::SubtractVariables {
                                first: tmp_a_var.clone(),
                                second: tmp_b_var.clone(),
                            });
                            self.emit_action(Action::ExecuteUnless {
                                condition: format!("score {} matches 0", &tmp_a_var),
                                then: Box::new(Action::SetVariableToNumber {
                                    var: tmp_a_var,
                                    val: 1,
                                }),
                            });
                        }
                        _ => panic!("unsupported operation: {:?}", op),
                    }

                    // tmp_b is no longer needed, free it
                    self.free_tmp(tmp_b_idx);
                }
                EvaluationInstruction::CallFunction(func, args) => {
                    let arg_tmps =
                        intermediate_tmps.split_off(intermediate_tmps.len() - args.len());

                    for i in 0..args.len() {
                        let arg_tmp = self.get_tmp(arg_tmps[i]);
                        self.emit_action(Action::SetVariableToVariable {
                            first: ScoreboardVariable::new(func.clone(), args[i].clone()),
                            second: arg_tmp,
                        });
                    }

                    self.emit_action(Action::CallFunction {
                        target: func.clone(),
                    });

                    for tmp in arg_tmps {
                        self.free_tmp(tmp);
                    }

                    let ret_tmp = self.reserve_available_tmp();
                    self.emit_action(Action::SetVariableToVariable {
                        first: self.get_tmp(ret_tmp),
                        second: ScoreboardVariable::new(func.clone(), "_RET".to_string()),
                    });
                    intermediate_tmps.push(ret_tmp);
                }
            }
        }

        let target_tmp = *intermediate_tmps.first().unwrap();

        if intermediate_tmps.len() == 2 {
            // the last intermediate tmp is the result of the full operation.
            // we have to move it to the first tmp, which is where the result is expected to be
            let result_tmp = *intermediate_tmps.last().unwrap();

            // optimization: except sometimes we don't need to move if the target tmp is the same as the result tmp
            if result_tmp != target_tmp {
                self.emit_action(Action::SetVariableToVariable {
                    first: self.get_tmp(target_tmp),
                    second: self.get_tmp(result_tmp),
                });
            }
        }

        for tmp in intermediate_tmps {
            self.free_tmp(tmp);
        }

        self.instructions.clear();
        target_tmp
    }

    fn emit_action(&mut self, action: Action) {
        self.actions.push(action);
    }

    fn reserve_available_tmp(&mut self) -> i32 {
        match self.available_tmps.pop() {
            Some(num) => num,
            None => {
                self.max_tmps += 1;
                self.max_tmps
            }
        }
    }

    fn free_tmp(&mut self, num: i32) {
        self.available_tmps.push(num);
    }

    fn get_tmp(&self, num: i32) -> ScoreboardVariable {
        ScoreboardVariable::new(self.scoreboard.clone(), format!("_TMP{}", num))
    }

    fn local_variable(&self, str: &str) -> ScoreboardVariable {
        ScoreboardVariable::new(self.scoreboard.clone(), str.to_string())
    }
}
