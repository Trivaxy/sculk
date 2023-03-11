use std::collections::{HashSet, HashMap};

use crate::{parser::{ParserNode, Parser, ParseError, Operation}, types::SculkType};

pub struct CodeGenerator {
    unfinished_functions: Vec<Function>,
    ready_functions: Vec<Function>,
    available_tmps: Vec<i32>,
    max_tmps: i32,
    output: String,
    eval_instrs: Vec<EvaluationInstruction>,
    bin_op_depth: i32,
}

impl CodeGenerator {
    // TODO: right now, any statements that are outside functions do not get printed because
    // they are put in an implicit "main" function that never gets pushed to ready_functions
    // this is a temporary problem until I implement a proper main function
    pub fn compile_src(src: &str) -> Result<Self, Vec<CompileError>> {
        let mut parser = Parser::new(src);
        
        match parser.parse() {
            Ok(ast) => {
                dbg!(&ast);
                let mut gen = Self {
                    unfinished_functions: vec![Function::new_empty("main".to_string(), vec![], None)],
                    ready_functions: vec![],
                    available_tmps: vec![2, 1, 0],
                    max_tmps: 2,
                    output: String::with_capacity(8192),
                    eval_instrs: vec![],
                    bin_op_depth: 0
                };

                gen.compile(&ast);
                Ok(gen)
            }
            Err(_) => Err(parser.errors().into_iter().map(|err| CompileError::parse_error(err.clone())).collect())
        }
    }

    fn compile(&mut self, ast: &ParserNode) {
        self.visit_node(ast);
        self.compile_to_mcfunctions();
    }

    fn visit_node(&mut self, node: &ParserNode) {
        match node {
            ParserNode::NumberLiteral(num) => self.visit_number(*num),
            ParserNode::BoolLiteral(bool) => self.visit_bool(*bool),
            ParserNode::Identifier(name) => self.visit_identifier(name),
            ParserNode::Operation(lhs, rhs, op) => self.visit_binary_operation(lhs, rhs, *op),
            ParserNode::VariableDeclaration { name, expr, ty: _ } => self.visit_variable_declaration(name, expr),
            ParserNode::Program(nodes) => self.visit_program(nodes),
            ParserNode::FunctionDeclaration { name, args, return_ty, body } => self.visit_function_declaration(name, args, return_ty, body),
            ParserNode::FunctionCall { name, args } => self.visit_function_call(name, args),
            ParserNode::Return(expr) => self.visit_return(expr),
            ParserNode::Block(nodes) => self.visit_block(nodes),
            _ => todo!()
        }

        if self.bin_op_depth == 0 {
            self.flush_eval_instrs();
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
        self.eval_instrs.push(EvaluationInstruction::PushNumber(num));
    }

    fn visit_bool(&mut self, bool: bool) {
        self.eval_instrs.push(EvaluationInstruction::PushBool(bool));
    }

    // TODO: implement shadowing
    // TODO: function arguments everywhere should resolve to something shorter than _ARG[NAME][INDEX]
    fn visit_identifier(&mut self, name: &str) {
        let current_func = self.unfinished_functions.last().unwrap();
        if let Some(index) = current_func.args.get(name) {
            self.eval_instrs.push(EvaluationInstruction::PushVariable(format!("_ARG{}{}", current_func.name, index)))
        } else {
            self.eval_instrs.push(EvaluationInstruction::PushVariable(format!("_VAR{}", name.to_string())));
        }
    }

    fn visit_binary_operation(&mut self, lhs: &ParserNode, rhs: &ParserNode, op: Operation) {
        self.bin_op_depth += 1;

        self.visit_node(lhs);
        self.visit_node(rhs);
        self.eval_instrs.push(EvaluationInstruction::Operation(op));

        self.bin_op_depth -= 1;
    }

    fn visit_variable_declaration(&mut self, name: &str, val: &ParserNode) {
        self.visit_node(val);
        let name = Self::var_name(name);
        self.emit_action(Action::SetVariableToVariable { first: name, second: "_TMP0".to_string() });
    }

    // TODO: stop people from defining functions twice
    fn visit_function_declaration(&mut self, name: &str, args: &[ParserNode], return_ty: &Option<Box<ParserNode>>, body: &ParserNode) {
        self.unfinished_functions.push(Function::new_empty(name.to_string(), args.iter().map(|node| node.as_identifier().to_string()).collect(), return_ty.as_ref().map(|node| node.as_identifier().to_string())));
        self.visit_node(body);
        self.ready_functions.push(self.unfinished_functions.pop().unwrap());
    }

    fn visit_function_call(&mut self, name: &str, args: &[ParserNode]) {
        for (i, arg) in args.iter().enumerate() {
            self.visit_node(arg);
            self.emit_action(Action::SetVariableToVariable { first: format!("_ARG{}{}", name, i), second: Self::get_tmp(0) });
        }

        self.emit_action(Action::CallFunction { target: name.to_string() });
    }

    // TODO: we have a big problem. returns work fine if there is only one at the end of the method
    // but if there are multiple returns on different paths, we need to find a way to "return" from the function
    // minecraft has no concept of returning from a function, so we need to find a way to emulate it
    // it's likely we will need to compile paths that end in a return to separate functions, and then call them as needed
    // the easier but more inefficient option is to wrap every generated command in a "execute unless [we have returned] ..." 
    fn visit_return(&mut self, expr: &ParserNode) {
        self.visit_node(expr);
        self.emit_action(Action::SetVariableToVariable { first: "_RET".to_string(), second: Self::get_tmp(0) });
    }

    fn emit_action(&mut self, action: Action) {
        self.current_function().actions.push(action);
    }

    fn current_function(&mut self) -> &mut Function {
        self.unfinished_functions.last_mut().unwrap()
    }

    // TODO: check if the stack is malformed, will help in catching bugs
    fn flush_eval_instrs(&mut self) {
        if self.eval_instrs.is_empty() {
            return;
        }

        // keep track of tmps that were used for intermediate operations
        // we need to free them after the full operation is done
        let mut intermediate_tmps = Vec::new();

        for i in 0..self.eval_instrs.len() {
            let instr = self.eval_instrs[i].clone(); // i am so mad

            match instr {
                EvaluationInstruction::PushNumber(num) => {
                    let tmp_idx = self.reserve_available_tmp();
                    let tmp_var = Self::get_tmp(tmp_idx);
                    self.emit_action(Action::SetVariableToNumber { name: tmp_var, val: num });
                    intermediate_tmps.push(tmp_idx);
                }
                EvaluationInstruction::PushBool(bool) => {
                    let tmp_idx = self.reserve_available_tmp();
                    let tmp_var = Self::get_tmp(tmp_idx);
                    let bool_val = if bool { 1 } else { 0 };
                    self.emit_action(Action::SetVariableToNumber { name: tmp_var, val: bool_val });
                    intermediate_tmps.push(tmp_idx);
                }
                EvaluationInstruction::PushVariable(name) => {
                    let tmp_idx = self.reserve_available_tmp();
                    let tmp_var = Self::get_tmp(tmp_idx);
                    self.emit_action(Action::SetVariableToVariable { first: tmp_var, second: name });
                    intermediate_tmps.push(tmp_idx);
                }
                EvaluationInstruction::Operation(op) => {
                    let tmp_b_idx = intermediate_tmps.pop().unwrap();
                    let tmp_a_idx = *intermediate_tmps.last().unwrap();

                    let tmp_a_var = Self::get_tmp(tmp_a_idx);
                    let tmp_b_var = Self::get_tmp(tmp_b_idx);

                    match op {
                        Operation::Add => self.emit_action(Action::AddVariables { first: tmp_a_var, second: tmp_b_var }),
                        Operation::Subtract => self.emit_action(Action::SubtractVariables { first: tmp_a_var, second: tmp_b_var }),
                        Operation::Multiply => self.emit_action(Action::MultiplyVariables { first: tmp_a_var, second: tmp_b_var }),
                        Operation::Divide => self.emit_action(Action::DivideVariables { first: tmp_a_var, second: tmp_b_var }),
                        Operation::Modulo => self.emit_action(Action::ModuloVariables { first: tmp_a_var, second: tmp_b_var }),
                    }

                    // tmp_b is no longer needed, free it
                    self.free_tmp(tmp_b_idx);
                }
            }
        }

        // the last intermediate tmp is the result of the full operation.
        // if it's already in tmp0, we don't need to do anything (optimization)
        // otherwise, we need to move it to tmp0
        let result_tmp = *intermediate_tmps.last().unwrap();

        if result_tmp != 0 {
            self.emit_action(Action::SetVariableToVariable { first: Self::get_tmp(0), second: Self::get_tmp(result_tmp) });
        }

        for tmp in intermediate_tmps {
            self.free_tmp(tmp);
        }

        self.eval_instrs.clear();
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

    fn compile_to_mcfunctions(&mut self) {
        self.output.push_str("scoreboard objectives add _SCULK dummy\n");

        for function in &self.ready_functions {
            self.output.push_str(&format!("\r\n# function {}\n", function.name));

            for action in &function.actions {
                Self::write_action(&mut self.output, action);
                self.output.push_str("\r\n");
            }
        }
    }

    fn get_tmp(num: i32) -> String {
        format!("_TMP{}", num)
    }

    fn write_action(str: &mut String, action: &Action) {
        match action {
            Action::SetVariableToNumber { name, val } => str.push_str(&format!("scoreboard players set {} _SCULK {}", name, val)),
            Action::AddVariables { first, second } => str.push_str(&format!("scoreboard players operation {} _SCULK += {} _SCULK", first, second)),
            Action::SubtractVariables { first, second } => str.push_str(&format!("scoreboard players operation {} _SCULK -= {} _SCULK", first, second)),
            Action::MultiplyVariables { first, second } => str.push_str(&format!("scoreboard players operation {} _SCULK *= {} _SCULK", first, second)),
            Action::DivideVariables { first, second } => str.push_str(&format!("scoreboard players operation {} _SCULK /= {} _SCULK", first, second)),
            Action::ModuloVariables { first, second } => str.push_str(&format!("scoreboard players operation {} _SCULK %= {} _SCULK", first, second)),
            Action::SetVariableToVariable { first, second } => str.push_str(&format!("scoreboard players operation {} _SCULK = {} _SCULK", first, second)),
            Action::CallFunction { target } => str.push_str(&format!("function {}", target))
        }
    }

    pub fn get_output(&self) -> &str {
        &self.output
    }

    fn var_name(name: &str) -> String {
        format!("_VAR_{}", name)
    }

}

#[derive(Debug)]
enum Action {
    SetVariableToNumber { name: String, val: i32 },
    AddVariables { first: String, second: String },
    SubtractVariables { first: String, second: String },
    MultiplyVariables { first: String, second: String },
    DivideVariables { first: String, second: String },
    ModuloVariables { first: String, second: String },
    SetVariableToVariable { first: String, second: String },
    CallFunction { target: String }
}

#[derive(Debug, Clone)]
enum EvaluationInstruction {
    PushNumber(i32),
    PushBool(bool),
    PushVariable(String),
    Operation(Operation)
}

impl EvaluationInstruction {
    fn as_operation(self) -> Operation {
        match self {
            EvaluationInstruction::Operation(op) => op,
            _ => panic!("Cannot convert non-operation to operation")
        }
    }

    fn into_action(&self, target_tmp: i32) -> Action {
        match self {
            EvaluationInstruction::PushNumber(num) => Action::SetVariableToNumber { name: format!("_TMP{}", target_tmp), val: *num },
            EvaluationInstruction::PushVariable(name) => Action::SetVariableToVariable { first: format!("_TMP{}", target_tmp), second: name.clone() },
            _ => panic!("Cannot convert operation to action")
        }
    }
}

struct Function {
    name: String,
    args: HashMap<String, usize>,
    return_ty: Option<String>, // TODO: convert to Option<SculkType>
    actions: Vec<Action>
}

impl Function {
    fn new_empty(name: String, args: Vec<String>, return_ty: Option<String>) -> Self {
        Function {
            name,
            args: args.into_iter().enumerate().map(|(i, arg)| (arg, i)).collect(),
            return_ty,
            actions: Vec::new()
        }
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