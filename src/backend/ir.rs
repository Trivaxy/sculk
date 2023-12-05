use std::{collections::HashMap, fmt::Display};

use crate::{
    data::ResourceLocation,
    parser::{Operation, ParserNode, ParserNodeKind},
};

use super::{function::FunctionSignature, type_pool::{TypePool, TypeKey}, types::FieldDef, resolve::{Resolver, Resolution}, validate::ScopeStack};

/// The instructions that make up Sculk's Intermediate Representation (IR).
/// These are generated during the IR generation phase of compilation, where the AST is compiled to IR.
/// If compiled in release, the IR is analyzed and optimized before being sent to the output phase.
/// In debug compilations the IR is sent straight to the output phase without any optimization.
///
/// Sculk's IR is stack-based, and is kept simple intentionally.
/// At this stage of compilation, information about parameter and local names is lost.
#[derive(Debug, PartialEq)]
pub enum Instruction {
    PushInteger(i32),                           // pushes an integer onto the stack
    PushBoolean(bool),                          // pushes a boolean onto the stack
    PushLocal(usize),                           // pushes a local value with the given index onto the stack
    StoreLocal(usize),                          // pops a value off the stack and stores it in the local with the given index
    Add,                                        // pops two integers off the stack, adds them, and pushes the result onto the stack
    Subtract,                                   // pops two integers off the stack, subtracts them, and pushes the result onto the stack
    Multiply,                                   // pops two integers off the stack, multiplies them, and pushes the result onto the stack
    Divide,                                     // pops two integers off the stack, divides them, and pushes the result onto the stack
    Modulo,                                     // pops two integers off the stack, mods them, and pushes the result onto the stack
    Equal,                                      // pops two values off the stack, compares them for equality, and pushes the resulting boolean onto the stack
    NotEqual,                                   // pops two integers off the stack, compares them for inequality, and pushes the resulting boolean onto the stack
    GreaterThan,                                // pops two integers off the stack, compares them for greater-than, and pushes the resulting boolean onto the stack
    LessThan,                                   // pops two integers off the stack, compares them for less-than, and pushes the resulting boolean onto the stack
    GreaterThanOrEqual,                         // pops two integers off the stack, compares them for greater-than-or-equal, and pushes the resulting boolean onto the stack
    LessThanOrEqual,                            // pops two integers off the stack, compares them for less-than-or-equal, and pushes the resulting boolean onto the stack
    And,                                        // pops two booleans off the stack, ands them, and pushes the resulting boolean onto the stack
    Or,                                         // pops two booleans off the stack, ors them, and pushes the resulting boolean onto the stack
    Not,                                        // pops a boolean off the stack, negates it, and pushes the resulting boolean onto the stack
    Return,                                     // returns from the function
    Break,                                      // breaks out of the current block
    CallGlobal(ResourceLocation),               // pops arguments from the stack and calls the global function at the given location
    CallMethod(TypeKey, usize),                 // pops arguments from the stack and calls the method at the given index for the given type                      
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
    JumpInIf,                                   // pops a boolean off the stack, pops a block, and jumps into the block if the condition is true
    JumpInEither,                               // pops a boolean off the stack, pops the else block, pops the if block, and jumps into one of the blocks depending on the condition
    RepeatIf,                                   // only valid when inside a block. pops a boolean and jumps to the block's start if true
    EmitCommandLiteral(String),                 // hacks
    Construct(TypeKey),                         // pops arguments from the stack and calls the constructor for the given type, pushes the instance onto the stack
    PushField(usize),                           // pops a struct off the stack and pushes the field at the given index onto the stack
    StoreField(usize),                          // pops a struct and value off the stack and stores it in the field at the given index
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
            CallGlobal(name) => write!(f, "callglobal {}", name),
            CallMethod(ty, idx) => write!(f, "callmethod {}.{}", ty, idx),
            StartBlock(is_loop) => write!(f, "startblock [loop={}]", is_loop),
            EndBlock => write!(f, "endblock"),
            JumpInIf => write!(f, "jumpinif"),
            JumpInEither => write!(f, "jumpineither"),
            RepeatIf => write!(f, "repeatif"),
            EmitCommandLiteral(cmd) => write!(f, "emitcommandliteral {}", cmd),
            Construct(type_key) => write!(f, "construct {}", type_key),
            PushField(idx) => write!(f, "pushfield {}", idx),
            StoreField(idx) => write!(f, "storefield {}", idx),
        }
    }
}

/// This is the compilation step that happens right after validation.
/// At this point, the program is assumed to be sound, so the IrCompiler doesn't do any verification.
/// This type will take in the validated AST, as well as the global function signatures and types constructed during validation.
/// All functions will be compiled to their intermediate representation, before the final step (CodeGen) is run.
pub struct IrCompiler {
    pack_name: String,
    types: TypePool,
    global_functions: HashMap<ResourceLocation, FunctionSignature>,
    builder: IrFunctionBuilder,
    compiled_funcs: Vec<IrFunction>,
}

impl IrCompiler {
    pub fn new(
        pack_name: String,
        types: TypePool,
        global_functions: HashMap<ResourceLocation, FunctionSignature>,
    ) -> Self {
        Self {
            pack_name,
            types,
            global_functions,
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
        (self.global_functions, self.types, self.compiled_funcs)
    }

    // Takes in the top-level node in the AST which is typically just a vector of functions and their bodies
    // In the future this will account for top-level statements as well
    pub fn visit_program(&mut self, program: &[ParserNode]) {
        for node in program {
            match node.kind() {
                ParserNodeKind::FunctionDeclaration {
                    name,
                    body,
                    ..
                } => {
                    let func = self.compile_function(name.to_owned(), body.as_block());
                    self.compiled_funcs.push(func);
                }
                ParserNodeKind::StructDefinition { .. } => {} // nothing to be done
                _ => unreachable!(),
            }
        }
    }

    // Takes in a function's name and its respective body in the AST, and compiles it into Sculk IR
    fn compile_function(&mut self, name: String, body: &[ParserNode]) -> IrFunction {
        self.builder.begin(
            self.global_functions
                .get(&ResourceLocation::new(self.pack_name.clone(), name.clone()))
                .unwrap(),
        );

        for node in body {
            self.visit_node(node);
        }

        self.builder.finish()
    }

    fn visit_node(&mut self, node: &ParserNode) {
        match node.kind() {
            ParserNodeKind::NumberLiteral(n) => self.visit_number_literal(*n),
            ParserNodeKind::BoolLiteral(b) => self.visit_bool_literal(*b),
            ParserNodeKind::Identifier(name) => self.visit_identifier(name),
            ParserNodeKind::VariableDeclaration { name, expr, .. } => {
                self.visit_variable_store(name, expr)
            }
            ParserNodeKind::VariableAssignment { name, expr } => {
                self.visit_variable_store(name, expr)
            }
            ParserNodeKind::Expression(expr) => self.visit_node(expr),
            ParserNodeKind::Operation(lhs, rhs, op) => self.visit_binary_operation(lhs, rhs, *op),
            ParserNodeKind::Unary(expr, op) => self.visit_unary_operation(expr, *op),
            ParserNodeKind::OpEquals { name, expr, op } => {
                self.visit_operation_equals(name, expr, *op)
            }
            ParserNodeKind::FunctionCall { .. } => self.visit_function_call(node),
            ParserNodeKind::Block(body) => self.visit_block(body),
            ParserNodeKind::Return(expr) => self.visit_return(expr),
            ParserNodeKind::Break => self.builder.emit(Instruction::Break),
            ParserNodeKind::If {
                cond,
                body,
                else_body,
                ..
            } => self.visit_if(cond, body, else_body),
            ParserNodeKind::For {
                init,
                cond,
                step,
                body,
            } => self.visit_for(init, cond, step, body),
            ParserNodeKind::CommandLiteral(cmd) => self
                .builder
                .emit(Instruction::EmitCommandLiteral(cmd.to_owned())),
            ParserNodeKind::MemberAccess { expr, member } => self.visit_member_access(expr, member.as_identifier()),
            // the below nodes don't need any work, they've been handled by previous phases of compilation
            ParserNodeKind::Program(_) => {}
            ParserNodeKind::TypedIdentifier { .. } => {}
            ParserNodeKind::FunctionDeclaration { .. } => {}
            ParserNodeKind::StructDefinition { .. } => {}
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

    fn visit_function_call(&mut self, node: &ParserNode) {
        let (expr, args) = node.as_function_call();

        self.visit_node(expr);

        for arg in args {
            self.visit_node(arg);
        }

        match Resolver::new(&self.pack_name, &self.global_functions, &self.types, &ScopeStack::empty()).resolve(node).unwrap() {
            Resolution::GlobalFunction(name) => self.builder.emit(Instruction::CallGlobal(ResourceLocation::new(self.pack_name.to_string(), name))),
            Resolution::Method(ty, name) => self.builder.emit(Instruction::CallMethod(ty, ty.from(&self.types).as_struct_def().function_idx(&name).unwrap())),
            Resolution::Constructor(ty) => self.builder.emit(Instruction::Construct(ty)),
            _ => unreachable!(),
        }
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

    fn visit_member_access(&mut self, expr: &ParserNode, member: &str) {
        self.visit_node(expr);

        match Resolver::new(&self.pack_name, &self.global_functions, &self.types, &ScopeStack::empty()).resolve(expr).unwrap() {
            Resolution::Variable(ty) => {
                let struct_def = ty.from(&self.types).as_struct_def();

                if let Some(field_idx) = struct_def.field_idx(member) {
                    self.builder.emit(Instruction::PushField(field_idx));
                } else {
                    unreachable!()
                }
            },
            Resolution::Field(ty, name) => {
                let struct_def = ty.from(&self.types).as_struct_def();
                let field_type_def = struct_def.field(&name).unwrap().field_type().from(&self.types).as_struct_def();

                if let Some(field_idx) = field_type_def.field_idx(member) {
                    self.builder.emit(Instruction::PushField(field_idx));
                } else {
                    unreachable!()
                }
            },
            _ => unreachable!(),
        }
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
            Operation::And => Instruction::And,
            Operation::Or => Instruction::Or,
            Operation::Not
            | Operation::Negate => unreachable!() // handled by visit_unary_operation
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
