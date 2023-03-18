use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Formatter, format},
    io::Write,
    path::Path, sync::atomic::{AtomicI32, Ordering},
};

use crate::{
    data::{ResourceLocation, ScoreboardVariable},
    parser::{FunctionDefinition, Operation, ParseError, Parser, ParserNode},
    types::SculkType,
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
        let mut parser = Parser::new(src);

        match parser.parse() {
            Ok(parse_output) => {
                let mut sculk_main = Function::new_empty("_sculkmain".to_string(), ResourceLocation::new(namespace.to_string(), "_sculkmain".to_string()), vec![], None);

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
            Err(_) => Err(parser
                .errors()
                .into_iter()
                .map(|err| CompileError::parse_error(err.clone()))
                .collect()),
        }
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
                self.visit_variable_declaration(name, expr)
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
            ParserNode::SelectorLiteral(selector) => todo!(),
            ParserNode::TypedIdentifier { .. } => {}
            ParserNode::Unary(expr, op) => self.visit_unary(expr, *op),
            ParserNode::If(expr, body) => self.visit_if(expr, body),
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

    fn visit_variable_declaration(&mut self, name: &str, val: &ParserNode) {
        self.begin_evaluation_for_scoreboard(self.current_function().scoreboard.clone(), 0);
        self.visit_node(val);
        let result_tmp = self.end_current_evaluation();
        let var = self.local_variable(name);
        self.emit_action(Action::SetVariableToVariable {
            first: var,
            second: self.get_tmp(result_tmp),
        });
    }

    // TODO: stop people from defining functions twice
    fn visit_function_declaration(
        &mut self,
        name: &str,
        args: &[ParserNode],
        return_ty: &Option<Box<ParserNode>>,
        body: &ParserNode,
    ) {
        self.unfinished_functions.push(Function::new_empty(
            name.to_string(),
            self.resource_location(name),
            args.iter()
                .map(|node| node.as_identifier().to_string())
                .collect(),
            return_ty
                .as_ref()
                .map(|node| node.as_identifier().to_string()),
        ));
        self.visit_node(body);

        for _ in 0..=self.anon_func_depth {
            let func = self.unfinished_functions.pop().unwrap();
            self.ready_functions.insert(func.name.clone(), func);
        }

        self.anon_func_depth = 0;
        self.flag_tmp_count = 0;
    }

    fn visit_function_call(&mut self, name: &str, args: &[ParserNode]) {
        for arg in args.iter() {
            self.visit_node(arg);
        }

        self.push_eval_instr(EvaluationInstruction::CallFunction(
            self.resource_location(name),
            self.func_defs[name].args.clone(),
        ));
    }

    // TODO: we have a big problem. returns work fine if there is only one at the end of the method
    // but if there are multiple returns on different paths, we need to find a way to "return" from the function
    // minecraft has no concept of returning from a function, so we need to find a way to emulate it
    // it's likely we will need to compile paths that end in a return to separate functions, and then call them as needed
    // the easier but more inefficient option is to wrap every generated command in a "execute unless [we have returned] ..."
    fn visit_return(&mut self, expr: &ParserNode) {
        self.begin_evaluation_for_scoreboard(self.current_function().scoreboard.clone(), 0);
        self.visit_node(expr);
        let result_tmp = self.end_current_evaluation();
        self.emit_action(Action::SetVariableToVariable {
            first: self.local_variable("_RET"),
            second: self.get_tmp(result_tmp),
        });
    }

    fn visit_if(&mut self, cond: &ParserNode, body: &ParserNode) {
        self.begin_evaluation_for_scoreboard(self.current_function().scoreboard.clone(), 0);
        self.visit_node(cond);
        let cond_tmp = self.end_current_evaluation();

        // preserve the condition in a flag, as the tmp may be overwritten
        self.emit_action(Action::SetVariableToVariable {
            first: self.get_flag(self.flag_tmp_count),
            second: self.get_tmp(cond_tmp)
        });

        let if_func = self.current_function().make_anonymous_child();
        self.unfinished_functions.push(if_func);
        self.visit_node(body);

        let if_func = self.unfinished_functions.pop().unwrap();
        let if_func_name = if_func.name.clone();
        self.ready_functions.insert(if_func.name.clone(), if_func);

        self.emit_action(Action::ExecuteIf { condition: format!("score {} matches 1", self.get_flag(self.flag_tmp_count)), then: Box::new(Action::CallFunction { target: self.resource_location(&if_func_name) }) });

        let hidden_else_func = self.current_function().make_anonymous_child();
        let hidden_else_func_name = hidden_else_func.name.clone();

        self.emit_action(Action::ExecuteUnless { condition: format!("score {} matches 1", self.get_flag(self.flag_tmp_count)), then: Box::new(Action::CallFunction { target: self.resource_location(&hidden_else_func_name) }) });

        self.unfinished_functions.push(hidden_else_func); // the rest of the function will now reside in this anonymous function
        self.anon_func_depth += 1;
        self.flag_tmp_count += 1;
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
    return_ty: Option<String>, // TODO: convert to Option<SculkType>
    actions: Vec<Action>,
    anonymous: bool
}

static ANONYMOUS_FUNC_COUNT: AtomicI32 = AtomicI32::new(0);

impl Function {
    fn new_empty(name: String, scoreboard: ResourceLocation, args: Vec<String>, return_ty: Option<String>) -> Self {
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

    fn new_empty_mapped_args(name: String, scoreboard: ResourceLocation, args: HashMap<String, usize>, idx_to_arg: HashMap<usize, String>, return_ty: Option<String>) -> Self {
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
        let mut func = Function::new_empty_mapped_args(format!("{}_anon_{}", &self.name, ANONYMOUS_FUNC_COUNT.load(Ordering::Relaxed)), self.scoreboard.clone(), self.args.clone(), self.idx_to_arg.clone(), self.return_ty.clone());
        ANONYMOUS_FUNC_COUNT.fetch_add(1, Ordering::SeqCst);
        func.anonymous = true;
        func
    }
}

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
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
