use std::{collections::HashMap, fmt::Display};

use crate::{parser::{Operation, ParserNode}, data::ResourceLocation};

use super::{function::FunctionSignature, type_pool::TypePool, types::FieldDef};

/// The instructions that make up Sculk's Intermediate Representation (IR).
/// These are generated during the IR generation phase of compilation, where the AST is compiled to IR.
/// If compiled in release, the IR is analyzed and optimized before being sent to the output phase.
/// In debug compilations the IR is sent straight to the output phase without any optimization.
/// 
/// Sculk's IR is stack-based, and is kept simple intentionally.
/// At this stage of compilation, information about parameter and local names is lost.
#[derive(Debug, PartialEq)]
pub enum Instruction {
    PushInteger(i32),
    PushBoolean(bool),
    PushLocal(usize),
    StoreLocal(usize),
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    And,
    Or,
    Not,
    Return,
    Break,
    Call(ResourceLocation),
    // Explanation: In Sculk IR any form of control flow happens via jumping in and out of blocks
    // There is no branching/jumping directly into a label/offset since that is hellish to accomplish in Minecraft
    // For example, consider: if x < 5 { broadcast(y); }
    // This would compile into:
    //  startblock         // start a new block
    //  pushlocal 1 # y
    //  call broadcast
    //  endblock           // <-- a reference to this block is now on the stack
    //  pushlocal 0 # x
    //  pushinteger 5
    //  lessthan
    //  jumpinif           // pop a condition off the stack, pop the block off the stack, jump into the block if the condition is true
    //
    // The boolean value represents whether or not the block is actually a loop. This is needed to make break statements function properly
    StartBlock(bool),
    EndBlock,
    JumpInIf,
    JumpInEither,
    RepeatIf, // only valid when inside a block. will repeat the block if the condition is true
    EmitCommandLiteral(String),
    AccessField(String),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;

        match self {
            PushInteger(n) => write!(f, "pushinteger {}", n),
            PushBoolean(b) => write!(f, "pushboolean {}", b),
            PushLocal(idx) => write!(f, "pushlocal {}", idx),
            StoreLocal(idx) => write!(f, "storelocal {}", idx),
            Add => write!(f, "add"),
            Subtract => write!(f, "subtract"),
            Multiply => write!(f, "multiply"),
            Divide => write!(f, "divide"),
            Modulo => write!(f, "modulo"),
            Equal => write!(f, "equal"),
            NotEqual => write!(f, "notequal"),
            GreaterThan => write!(f, "greaterthan"),
            LessThan => write!(f, "lessthan"),
            GreaterThanOrEqual => write!(f, "greaterthanorequal"),
            LessThanOrEqual => write!(f, "lessthanorequal"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Not => write!(f, "not"),
            Return => write!(f, "return"),
            Break => write!(f, "break"),
            Call(name) => write!(f, "call {}", name),
            StartBlock(is_loop) => write!(f, "startblock [loop={}]", is_loop),
            EndBlock => write!(f, "endblock"),
            JumpInIf => write!(f, "jumpinif"),
            JumpInEither => write!(f, "jumpineither"),
            RepeatIf => write!(f, "repeatif"),
            EmitCommandLiteral(cmd) => write!(f, "emitcommandliteral {}", cmd),
            AccessField(name) => write!(f, "accessfield {}", name),
        }
    }
}

/// Takes in the validated AST, the type pool, function signatures, and compiles every function into Sculk IR.
pub struct IrCompiler {
    pack_name: String,
    types: TypePool,
    func_signatures: HashMap<ResourceLocation, FunctionSignature>,
    builder: IrFunctionBuilder,
    compiled_funcs: Vec<IrFunction>,
}

impl IrCompiler {
    pub fn new(pack_name: String, types: TypePool, func_signatures: HashMap<ResourceLocation, FunctionSignature>) -> Self {
        Self {
            pack_name,
            types,
            func_signatures,
            builder: IrFunctionBuilder::new(),
            compiled_funcs: Vec::new(),
        }
    }

    pub fn dissolve(
        self,
    ) -> (
        HashMap<ResourceLocation, FunctionSignature>,
        TypePool,
        Vec<IrFunction>,
    ) {
        (self.func_signatures, self.types, self.compiled_funcs)
    }

    // Takes in the top-level node in the AST which is typically just a vector of functions and their bodies
    // In the future this will account for top-level statements as well
    pub fn visit_program(&mut self, program: &[ParserNode]) {
        for node in program {
            match node {
                ParserNode::FunctionDeclaration {
                    name,
                    args,
                    return_ty,
                    body,
                } => {
                    let func = self.compile_function(name.to_owned(), body.as_block());
                    self.compiled_funcs.push(func);
                },
                ParserNode::StructDefinition { .. } => {} // nothing to be done
                _ => unreachable!(),
            }
        }
    }

    // Takes in a function's name and its respective body in the AST, and compiles it into Sculk IR
    fn compile_function(&mut self, name: String, body: &[ParserNode]) -> IrFunction {
        self.builder.begin(self.func_signatures.get(&ResourceLocation::new(self.pack_name.clone(), name.clone())).unwrap());

        for node in body {
            self.visit_node(node);
        }

        self.builder.finish()
    }

    fn visit_node(&mut self, node: &ParserNode) {
        match node {
            ParserNode::NumberLiteral(n) => self.visit_number_literal(*n),
            ParserNode::BoolLiteral(b) => self.visit_bool_literal(*b),
            ParserNode::Identifier(name) => self.visit_identifier(name),
            ParserNode::VariableDeclaration { name, expr, .. } => {
                self.visit_variable_store(name, expr)
            }
            ParserNode::VariableAssignment { name, expr } => self.visit_variable_store(name, expr),
            ParserNode::Operation(lhs, rhs, op) => self.visit_binary_operation(lhs, rhs, *op),
            ParserNode::Unary(expr, op) => self.visit_unary_operation(expr, *op),
            ParserNode::OpEquals { name, expr, op } => self.visit_operation_equals(name, expr, *op),
            ParserNode::FunctionCall { name, args } => self.visit_function_call(name, args),
            ParserNode::Block(body) => self.visit_block(body),
            ParserNode::Return(expr) => self.visit_return(expr),
            ParserNode::Break => self.builder.emit(Instruction::Break),
            ParserNode::If {
                cond,
                body,
                else_body,
                ..
            } => self.visit_if(cond, body, else_body),
            ParserNode::For {
                init,
                cond,
                step,
                body,
            } => self.visit_for(init, cond, step, body),
            ParserNode::CommandLiteral(cmd) => self
                .builder
                .emit(Instruction::EmitCommandLiteral(cmd.to_owned())),
            _ => {} // the rest of the nodes don't need any work done, they're handled at an earlier stage
        }
    }

    fn visit_number_literal(&mut self, n: i32) {
        self.builder.emit(Instruction::PushInteger(n));
    }

    fn visit_bool_literal(&mut self, b: bool) {
        self.builder.emit(Instruction::PushBoolean(b));
    }

    fn visit_identifier(&mut self, name: &str) {
        let idx = self.builder.get_local_index(name);
        self.builder.emit(Instruction::PushLocal(idx));
    }

    fn visit_variable_store(&mut self, name: &str, expr: &ParserNode) {
        self.visit_node(expr);

        let idx = self.builder.get_local_index(name);
        self.builder.emit(Instruction::StoreLocal(idx));
    }

    fn visit_binary_operation(&mut self, lhs: &ParserNode, rhs: &ParserNode, op: Operation) {
        self.visit_node(lhs);
        self.visit_node(rhs);

        let instr = Self::op_to_instr(op);

        self.builder.emit(instr);
    }

    fn visit_unary_operation(&mut self, expr: &ParserNode, op: Operation) {
        self.visit_node(expr);

        match op {
            Operation::Negate => {
                self.builder.emit(Instruction::PushInteger(-1));
                self.builder.emit(Instruction::Multiply);
            }
            Operation::Not => self.builder.emit(Instruction::Not),
            _ => unreachable!(),
        }
    }

    fn visit_operation_equals(&mut self, name: &str, expr: &ParserNode, op: Operation) {
        let idx = self.builder.get_local_index(name);
        self.builder.emit(Instruction::PushLocal(idx));

        self.visit_node(expr);

        let instr = Self::op_to_instr(op);
        self.builder.emit(instr);

        self.builder.emit(Instruction::StoreLocal(idx));
    }

    fn visit_function_call(&mut self, name: &str, args: &[ParserNode]) {
        for arg in args {
            self.visit_node(arg);
        }

        self.builder.emit(Instruction::Call(ResourceLocation::new(self.pack_name.clone(), name.to_owned())));
    }

    fn visit_block(&mut self, body: &[ParserNode]) {
        for node in body {
            self.visit_node(node);
        }
    }

    fn visit_return(&mut self, expr: &Option<Box<ParserNode>>) {
        if let Some(expr) = expr {
            self.visit_node(expr);
        }

        self.builder.emit(Instruction::Return);
    }

    fn visit_if(
        &mut self,
        cond: &ParserNode,
        body: &ParserNode,
        else_body: &Option<Box<ParserNode>>,
    ) {
        self.builder.emit(Instruction::StartBlock(false));
        self.visit_node(body);
        self.builder.emit(Instruction::EndBlock);

        if let Some(else_body) = else_body {
            self.builder.emit(Instruction::StartBlock(false));
            self.visit_node(else_body);
            self.builder.emit(Instruction::EndBlock);
        }

        self.visit_node(cond);

        if else_body.is_some() {
            self.builder.emit(Instruction::JumpInEither);
        } else {
            self.builder.emit(Instruction::JumpInIf);
        }
    }

    fn visit_for(
        &mut self,
        init: &ParserNode,
        cond: &ParserNode,
        step: &ParserNode,
        body: &ParserNode,
    ) {
        self.visit_node(init);

        self.builder.emit(Instruction::StartBlock(true));

        self.visit_node(body);
        self.visit_node(step);
        self.visit_node(cond);
        self.builder.emit(Instruction::RepeatIf);

        self.builder.emit(Instruction::EndBlock);

        self.visit_node(cond);
        self.builder.emit(Instruction::JumpInIf);
    }

    fn op_to_instr(op: Operation) -> Instruction {
        match op {
            Operation::Add => Instruction::Add,
            Operation::Subtract => Instruction::Subtract,
            Operation::Multiply => Instruction::Multiply,
            Operation::Divide => Instruction::Divide,
            Operation::Modulo => Instruction::Modulo,
            Operation::CheckEquals => Instruction::Equal,
            Operation::NotEquals => Instruction::NotEqual,
            Operation::GreaterThan => Instruction::GreaterThan,
            Operation::LessThan => Instruction::LessThan,
            Operation::GreaterThanOrEquals => Instruction::GreaterThanOrEqual,
            Operation::LessThanOrEquals => Instruction::LessThanOrEqual,
            _ => unreachable!(), // other operations are unary
        }
    }
}

/// A function that has been compiled into Sculk IR.
pub struct IrFunction {
    name: String,
    body: Vec<Instruction>,
}

impl IrFunction {
    fn new(name: String) -> Self {
        Self {
            name,
            body: Vec::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn body(&self) -> &[Instruction] {
        &self.body
    }
}

/// A helper struct that assists in building Sculk IR functions.
struct IrFunctionBuilder {
    function: Option<IrFunction>,
    locals: HashMap<String, usize>,
}

impl IrFunctionBuilder {
    fn new() -> Self {
        Self {
            function: None,
            locals: HashMap::new(),
        }
    }

    fn begin(&mut self, signature: &FunctionSignature) {
        self.function = Some(IrFunction::new(signature.name().to_owned()));
        self.locals.clear();

        // Give the first local indices to the function parameters
        for (idx, param) in signature.params().iter().enumerate() {
            self.locals.insert(param.name().to_owned(), idx);
        }
    }

    fn emit(&mut self, instr: Instruction) {
        self.function
            .as_mut()
            .expect("not building any function")
            .body
            .push(instr);
    }

    fn finish(&mut self) -> IrFunction {
        self.function.take().expect("not building any function")
    }

    fn get_local_index(&mut self, name: &str) -> usize {
        match self.locals.get(name) {
            Some(idx) => *idx,
            None => {
                let idx = self.locals.len();
                self.locals.insert(name.to_owned(), idx);
                idx
            }
        }
    }
}
