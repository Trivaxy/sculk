use std::{
    collections::HashMap,
    fmt::Display,
    sync::atomic::{AtomicUsize, Ordering},
};

use dpc::output::datapack::Tag;

use crate::{
    data::{Objective, ResourceLocation, ScoreboardSlot},
    parser::{Operation, ParserNode, ParserNodeKind},
};

use super::{
    function::FunctionSignature,
    resolve::{Resolution, ResolvedPart},
    type_pool::TypePool,
    validate::TagPool,
};

pub struct Ir {
    pub functions: Vec<IrFunction>,
    pub function_tags: HashMap<ResourceLocation, Tag>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ValueLocation {
    pub slot: usize,
    pub offset: usize,
    pub objective: Objective,
}

impl ValueLocation {
    pub fn new(slot: usize, offset: usize, objective: Objective) -> Self {
        Self {
            slot,
            offset,
            objective,
        }
    }

    pub fn offset(&self, amt: usize) -> Self {
        Self {
            slot: self.slot,
            offset: self.offset + amt,
            objective: self.objective.clone(),
        }
    }

    fn dummy() -> Self {
        Self {
            slot: 0,
            offset: 0,
            objective: Objective(String::new()),
        }
    }
}

impl Display for ValueLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{} {}", self.slot, self.offset, self.objective)
    }
}

impl From<ValueLocation> for ScoreboardSlot {
    fn from(loc: ValueLocation) -> Self {
        let offset = if loc.offset == 0 {
            String::new()
        } else {
            format!(".{}", loc.offset)
        };
        ScoreboardSlot::new(loc.objective.clone(), format!("v{}{}", loc.slot, offset))
    }
}

impl From<&ValueLocation> for ScoreboardSlot {
    fn from(loc: &ValueLocation) -> Self {
        Self::from(loc.clone())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    CheckEquals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEquals,
    LessThan,
    LessThanOrEquals,
    And,
    Or,
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOperation::*;

        match self {
            Add => write!(f, "+"),
            Subtract => write!(f, "-"),
            Multiply => write!(f, "*"),
            Divide => write!(f, "/"),
            Modulo => write!(f, "%"),
            CheckEquals => write!(f, "=="),
            NotEquals => write!(f, "!="),
            GreaterThan => write!(f, ">"),
            GreaterThanOrEquals => write!(f, ">="),
            LessThan => write!(f, "<"),
            LessThanOrEquals => write!(f, "<="),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    // Sets target to the value of source
    SetValueToValue {
        source: ValueLocation,
        target: ValueLocation,
    },
    // Sets target to a raw integer value
    SetValueToConstant {
        target: ValueLocation,
        constant: i32,
    },
    // Performs target = target <op> source
    ValueBinaryOperation {
        source: ValueLocation,
        target: ValueLocation,
        op: BinaryOperation,
    },
    // Performs target = !target. This assumes a boolean representation
    ToggleValue {
        target: ValueLocation,
    },
    // Performs target += <value>. value can be negative
    ModifyValue {
        target: ValueLocation,
        value: i32,
    },
    // Returns from the function
    Return {
        source: Option<ValueLocation>,
        size: usize, // the size of the return value
    },
    // Keeps jumping out of blocks until the encapsulating loop block is found
    Break,
    // Calls a function at the given location
    Call {
        function: ResourceLocation,
    },
    // Creates a new block with the given ID and body of instructions. is_loop indicates if it stops a break's propagation
    CreateBlock {
        id: usize,
        is_loop: bool,
        body: Vec<Instruction>,
    },
    // Jumps to the start of the block with the given ID
    EnterBlock {
        id: usize,
    },
    // Conditionally executes a block if source == value
    IfValueMatchesRunBlock {
        source: ValueLocation,
        value: i32,
        block: usize,
    },
    PlaceCommandLiteral(String),
}

impl Instruction {
    fn fmt_with_indent(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        use Instruction::*;

        match self {
            SetValueToValue { source, target } => {
                write!(
                    f,
                    "{:indent$}set T({}) = S({})",
                    "",
                    target,
                    source,
                    indent = indent
                )
            }
            SetValueToConstant { target, constant } => {
                write!(
                    f,
                    "{:indent$}set T({}) = {}",
                    "",
                    target,
                    constant,
                    indent = indent
                )
            }
            ValueBinaryOperation { source, target, op } => {
                write!(
                    f,
                    "{:indent$}op T({}) = T({}) {} S({})",
                    "",
                    target,
                    target,
                    op,
                    source,
                    indent = indent
                )
            }
            ToggleValue { target } => write!(
                f,
                "{:indent$}op T({}) = !T({})",
                "",
                target,
                target,
                indent = indent
            ),
            ModifyValue { target, value } => write!(
                f,
                "{:indent$}op T({}) += {}",
                "",
                target,
                value,
                indent = indent
            ),
            Return { source, size } => write!(
                f,
                "{:indent$}return{}",
                "",
                match source {
                    Some(source) => format!(" S({}) SIZE={}", source, size),
                    None => String::new(),
                },
                indent = indent
            ),
            Break => write!(f, "{:indent$}break", "", indent = indent),
            Call { function } => write!(f, "{:indent$}call {}", "", function, indent = indent),
            CreateBlock { id, is_loop, body } => {
                writeln!(f)?;
                writeln!(
                    f,
                    "{:indent$}block {} (loop: {})",
                    "",
                    id,
                    is_loop,
                    indent = indent
                )?;

                for instr in body {
                    instr.fmt_with_indent(f, indent + 2)?;
                    writeln!(f)?;
                }

                Ok(())
            }
            EnterBlock { id } => write!(f, "{:indent$}enter B({})", "", id, indent = indent),
            IfValueMatchesRunBlock {
                source,
                value,
                block,
            } => {
                write!(
                    f,
                    "{:indent$}if S({}) == {} then block {}",
                    "",
                    source,
                    value,
                    block,
                    indent = indent
                )
            }
            PlaceCommandLiteral(cmd) => write!(f, "{:indent$}/{}", "", cmd, indent = indent),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

/// This is the compilation step that happens right after validation.
/// At this point, the program is assumed to be sound, so the IrCompiler doesn't do any verification.
/// This type will take in the validated AST, as well as the global function signatures and types constructed during validation.
/// All functions will be compiled to their intermediate representation, before the final step (CodeGen) is run.
pub struct IrCompiler<'a> {
    pack_name: String,
    types: TypePool,
    global_functions: HashMap<ResourceLocation, FunctionSignature>,
    tags: TagPool<'a>,
    compiled_funcs: Vec<IrFunction>,
    on_tick: Vec<ResourceLocation>,
    on_load: Vec<ResourceLocation>,
}

impl<'a> IrCompiler<'a> {
    pub fn new(
        pack_name: String,
        types: TypePool,
        global_functions: HashMap<ResourceLocation, FunctionSignature>,
        tags: TagPool<'a>,
    ) -> Self {
        Self {
            pack_name,
            types,
            global_functions,
            tags,
            compiled_funcs: Vec::new(),
            on_tick: Vec::new(),
            on_load: Vec::new(),
        }
    }

    pub fn dissolve(
        self,
    ) -> (
        HashMap<ResourceLocation, FunctionSignature>,
        TypePool,
        TagPool<'a>,
        Ir,
    ) {
        let mut tags = HashMap::new();
        if !self.on_tick.is_empty() {
            let mut tag = Tag::new();
            tag.inner.values = self.on_tick.into_iter().map(|x| x.to_string()).collect();
            tags.insert(
                ResourceLocation::new("minecraft".into(), "tick".into()),
                tag,
            );
        }

        if !self.on_load.is_empty() {
            let mut tag = Tag::new();
            tag.inner.values = self.on_load.into_iter().map(|x| x.to_string()).collect();
            tags.insert(
                ResourceLocation::new("minecraft".into(), "load".into()),
                tag,
            );
        }

        (
            self.global_functions,
            self.types,
            self.tags,
            Ir {
                functions: self.compiled_funcs,
                function_tags: tags,
            },
        )
    }

    // Takes in the top-level node in the AST which is typically just a vector of functions and their bodies
    // In the future this will account for top-level statements as well
    pub fn visit_program(&mut self, program: &'a [ParserNode]) {
        for node in program {
            match node.kind() {
                ParserNodeKind::FunctionDeclaration {
                    name, body, events, ..
                } => {
                    // Build the function
                    let resource_loc = ResourceLocation::new(self.pack_name.clone(), name.clone());
                    let mut builder = IrFunctionBuilder::new(
                        self.global_functions.get(&resource_loc).unwrap(),
                        Objective(name.clone()),
                        self.pack_name.clone(),
                        &self.global_functions,
                        &self.types,
                        &self.tags,
                    );

                    builder.visit_node(body);

                    let mut func = builder.finish();

                    // Attach any events
                    for event in events {
                        func.preserve = true;
                        let event = event.as_event_listener();
                        match event {
                            "on_tick" => {
                                self.on_tick.push(resource_loc.clone());
                            }
                            "on_load" => {
                                self.on_load.push(resource_loc.clone());
                            }
                            other => panic!("Invalid event {other}"),
                        }
                    }

                    self.compiled_funcs.push(func);
                }
                ParserNodeKind::StructDefinition { name, members } => {
                    for method in members.iter().filter(|m| m.is_func_declaration()) {
                        let mut builder = IrFunctionBuilder::new(
                            self.types
                                .get_type_key(name)
                                .unwrap()
                                .from(&self.types)
                                .as_struct_def()
                                .function(method.as_func_name())
                                .unwrap(),
                            Objective(format!("{}.{}", name, method.as_func_name())),
                            self.pack_name.clone(),
                            &self.global_functions,
                            &self.types,
                            &self.tags,
                        );

                        builder.visit_node(method.as_func_body());

                        self.compiled_funcs.push(builder.finish());
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

/// A function that has been compiled into Sculk IR.
#[derive(Debug)]
pub struct IrFunction {
    objective: Objective,
    body: Vec<Instruction>,
    signature: FunctionSignature,
    preserve: bool,
}

impl IrFunction {
    fn new(objective: Objective, body: Vec<Instruction>, signature: FunctionSignature) -> Self {
        Self {
            objective,
            body,
            signature,
            preserve: false,
        }
    }

    pub fn objective(&self) -> &Objective {
        &self.objective
    }

    pub fn body(&self) -> &[Instruction] {
        &self.body
    }

    pub fn signature(&self) -> &FunctionSignature {
        &self.signature
    }
}

static NEXT_BLOCK_ID: AtomicUsize = AtomicUsize::new(0);

/// A helper struct that assists in building Sculk IR functions.
struct IrFunctionBuilder<'a> {
    body: Vec<Instruction>,
    locals: HashMap<String, usize>,
    next_slot: usize,
    blocks: Vec<Vec<Instruction>>,
    objective: Objective,
    pack_name: String,
    global_functions: &'a HashMap<ResourceLocation, FunctionSignature>,
    types: &'a TypePool,
    tags: &'a TagPool<'a>,
    signature: &'a FunctionSignature,
}

impl<'a> IrFunctionBuilder<'a> {
    fn new(
        signature: &'a FunctionSignature,
        objective: Objective,
        pack_name: String,
        global_functions: &'a HashMap<ResourceLocation, FunctionSignature>,
        types: &'a TypePool,
        tags: &'a TagPool,
    ) -> Self {
        let mut s = Self {
            body: Vec::new(),
            locals: HashMap::new(),
            next_slot: 0,
            blocks: Vec::new(),
            objective,
            pack_name,
            global_functions,
            types,
            tags,
            signature,
        };

        // Give the first local indices to the function parameters
        for (idx, param) in signature.params().iter().enumerate() {
            s.locals.insert(param.name().to_owned(), idx);
            s.next_slot += 1;
        }

        s
    }

    fn finish(self) -> IrFunction {
        // TODO: avoid clone
        IrFunction::new(self.objective, self.body, self.signature.clone())
    }

    fn emit(&mut self, instr: Instruction) {
        match self.blocks.last_mut() {
            Some(block) => block.push(instr),
            None => self.body.push(instr),
        }
    }

    fn emit_value_copy(&mut self, target: ValueLocation, source: ValueLocation, size: usize) {
        for i in 0..size {
            let source =
                ValueLocation::new(source.slot, source.offset + i, source.objective.clone());
            let target =
                ValueLocation::new(target.slot, target.offset + i, target.objective.clone());

            self.emit(Instruction::SetValueToValue { source, target });
        }
    }

    fn get_local(&mut self, name: &str) -> ValueLocation {
        match self.locals.get(name) {
            Some(idx) => ValueLocation::new(*idx, 0, self.objective.clone()),
            None => {
                let idx = self.next_slot;
                self.next_slot += 1;
                self.locals.insert(name.to_owned(), idx);
                ValueLocation::new(idx, 0, self.objective.clone())
            }
        }
    }

    fn get_free_location(&mut self) -> ValueLocation {
        let slot = self.next_slot;
        self.next_slot += 1;
        ValueLocation::new(slot, 0, self.objective.clone())
    }

    fn create_block(
        is_loop: bool,
        builder: &mut Self,
        emitted: impl FnOnce(usize, &mut Self),
    ) -> usize {
        let id = NEXT_BLOCK_ID.fetch_add(1, Ordering::Relaxed);

        builder.blocks.push(Vec::new());

        emitted(id, builder);

        let body = builder.blocks.pop().unwrap();

        builder.emit(Instruction::CreateBlock { id, is_loop, body });

        id
    }

    // Takes in a function's name, signature and its respective body in the AST, and compiles it into Sculk IR
    fn compile_function(&mut self, body: &'a [ParserNode]) {
        for node in body {
            self.visit_node(node);
        }
    }

    fn visit_node(&mut self, node: &ParserNode) -> ValueLocation {
        match node.kind() {
            ParserNodeKind::NumberLiteral(n) => self.visit_number_literal(*n),
            ParserNodeKind::BoolLiteral(b) => self.visit_bool_literal(*b),
            ParserNodeKind::Identifier(_) => self.visit_identifier(node),
            ParserNodeKind::VariableDeclaration { name, expr, .. } => {
                self.visit_variable_declaration(name.as_identifier(), expr);
                ValueLocation::dummy()
            }
            ParserNodeKind::VariableAssignment { path, expr } => {
                self.visit_variable_assignment(path, expr);
                ValueLocation::dummy()
            }
            ParserNodeKind::Expression(expr) => self.visit_node(expr),
            ParserNodeKind::Operation(lhs, rhs, op) => self.visit_binary_operation(lhs, rhs, *op),
            ParserNodeKind::Unary(expr, op) => self.visit_unary_operation(expr, *op),
            ParserNodeKind::OpEquals { path, expr, op } => {
                self.visit_operation_equals(path, expr, *op);
                ValueLocation::dummy()
            }
            ParserNodeKind::FunctionCall { .. } => match self.visit_function_call(node) {
                Some(target) => target,
                None => ValueLocation::dummy(),
            },
            ParserNodeKind::Block(body) => {
                self.visit_block(body);
                ValueLocation::dummy()
            }
            ParserNodeKind::Return(expr) => {
                self.visit_return(expr);
                ValueLocation::dummy()
            }
            ParserNodeKind::Break => {
                self.emit(Instruction::Break);
                ValueLocation::dummy()
            }
            ParserNodeKind::If {
                cond,
                body,
                else_body,
                ..
            } => {
                self.visit_if(cond, body, else_body);
                ValueLocation::dummy()
            }
            ParserNodeKind::For {
                init,
                cond,
                step,
                body,
            } => {
                self.visit_for(init, cond, step, body);
                ValueLocation::dummy()
            }
            ParserNodeKind::CommandLiteral(cmd) => {
                self.emit(Instruction::PlaceCommandLiteral(cmd.to_owned()));
                ValueLocation::dummy()
            }
            ParserNodeKind::MemberAccess { expr, member } => {
                self.visit_member_access(expr, member.as_identifier())
            }
            // the below nodes don't need any work, they've been handled by previous phases of compilation
            ParserNodeKind::Program(_) => ValueLocation::dummy(),
            ParserNodeKind::TypedIdentifier { .. } => ValueLocation::dummy(),
            ParserNodeKind::FunctionDeclaration { .. } => ValueLocation::dummy(),
            ParserNodeKind::StructDefinition { .. } => ValueLocation::dummy(),
            ParserNodeKind::EventListener { .. } => ValueLocation::dummy(),
        }
    }

    fn visit_number_literal(&mut self, n: i32) -> ValueLocation {
        let target = self.get_free_location();

        self.emit(Instruction::SetValueToConstant {
            target: target.clone(),
            constant: n,
        });

        target
    }

    fn visit_bool_literal(&mut self, b: bool) -> ValueLocation {
        let target = self.get_free_location();

        self.emit(Instruction::SetValueToConstant {
            target: target.clone(),
            constant: if b { 1 } else { 0 },
        });

        target
    }

    fn visit_identifier(&mut self, identifier: &ParserNode) -> ValueLocation {
        let source = self.get_local(identifier.as_identifier());
        let target = self.get_free_location();
        let size = self
            .tags
            .get_type(identifier)
            .from(&self.types)
            .total_size(&self.types);

        self.emit_value_copy(target.clone(), source, size);

        target
    }

    fn visit_variable_declaration(&mut self, name: &str, expr: &ParserNode) {
        let source = self.visit_node(expr);
        let target = self.get_local(name);

        self.emit_value_copy(
            target,
            source,
            self.tags
                .get_type(expr)
                .from(&self.types)
                .total_size(&self.types),
        );
    }

    fn visit_variable_assignment(&mut self, path: &ParserNode, expr: &ParserNode) {
        let source = self.visit_node(expr);
        let resolution = self.tags.get_resolution(path);
        let target = self.resolve_location(resolution);

        self.emit_value_copy(
            target,
            source,
            self.tags
                .get_type(expr)
                .from(&self.types)
                .total_size(&self.types),
        );
    }

    fn visit_binary_operation(
        &mut self,
        lhs: &ParserNode,
        rhs: &ParserNode,
        op: Operation,
    ) -> ValueLocation {
        let op = match op {
            Operation::Add => BinaryOperation::Add,
            Operation::Subtract => BinaryOperation::Subtract,
            Operation::Multiply => BinaryOperation::Multiply,
            Operation::Divide => BinaryOperation::Divide,
            Operation::Modulo => BinaryOperation::Modulo,
            Operation::CheckEquals => BinaryOperation::CheckEquals,
            Operation::NotEquals => BinaryOperation::NotEquals,
            Operation::GreaterThan => BinaryOperation::GreaterThan,
            Operation::GreaterThanOrEquals => BinaryOperation::GreaterThanOrEquals,
            Operation::LessThan => BinaryOperation::LessThan,
            Operation::LessThanOrEquals => BinaryOperation::LessThanOrEquals,
            Operation::And => BinaryOperation::And,
            Operation::Or => BinaryOperation::Or,
            _ => unreachable!(),
        };

        let target = self.visit_node(lhs);
        let source = self.visit_node(rhs);

        self.emit(Instruction::ValueBinaryOperation {
            source,
            target: target.clone(),
            op,
        });

        target
    }

    fn visit_unary_operation(&mut self, expr: &ParserNode, op: Operation) -> ValueLocation {
        let target = self.visit_node(expr);

        match op {
            Operation::Negate => {
                let n = self.get_free_location();

                self.emit(Instruction::SetValueToConstant {
                    target: n.clone(),
                    constant: -1,
                });

                self.emit(Instruction::ValueBinaryOperation {
                    source: n,
                    target: target.clone(),
                    op: BinaryOperation::Multiply,
                });
            }
            Operation::Not => {
                self.emit(Instruction::ToggleValue {
                    target: target.clone(),
                });
            }
            _ => unreachable!(),
        }

        target
    }

    fn visit_operation_equals(&mut self, path: &ParserNode, expr: &ParserNode, op: Operation) {
        let resolution = self.tags.get_resolution(path);
        let target = self.resolve_location(resolution);
        let source = self.visit_node(expr);

        let op = match op {
            Operation::Add => BinaryOperation::Add,
            Operation::Subtract => BinaryOperation::Subtract,
            Operation::Multiply => BinaryOperation::Multiply,
            Operation::Divide => BinaryOperation::Divide,
            Operation::Modulo => BinaryOperation::Modulo,
            _ => unreachable!(),
        };

        self.emit(Instruction::ValueBinaryOperation { source, target, op });
    }

    fn visit_function_call(&mut self, node: &ParserNode) -> Option<ValueLocation> {
        let (expr, params) = node.as_function_call();
        let resolution = self.tags.get_resolution(node);

        let mut args = vec![];

        if let ResolvedPart::Method(ty, name) = resolution.last() {
            if !ty
                .from(&self.types)
                .as_struct_def()
                .function(&name)
                .unwrap()
                .is_static()
            {
                args.push(self.visit_node(expr)); // self parameter
            }
        }

        args.extend(params.iter().map(|arg| self.visit_node(arg)));

        let func_objective;
        let func_signature;
        let handle_return;

        match resolution.last() {
            ResolvedPart::GlobalFunction(name) => {
                let func_location = ResourceLocation::new(self.pack_name.clone(), name.clone());

                func_objective = Objective(name.clone());
                func_signature = self.global_functions.get(&func_location).unwrap();
                handle_return = func_signature.return_type() != self.types.none();
            }
            ResolvedPart::Method(ty, name) => {
                let struct_def = ty.from(&self.types).as_struct_def();

                func_objective = Objective(format!("{}.{}", struct_def.name(), name));
                func_signature = struct_def.function(name).unwrap();
                handle_return = func_signature.return_type() != self.types.none();
            }
            // Constructors are a special case. To the user they seem like regular functions,
            // but on the IR level, they construct a new value in-place
            ResolvedPart::Constructor(ty) => {
                let struct_def = ty.from(&self.types).as_struct_def();
                let target = self.get_free_location();

                for (param, arg) in struct_def.constructor().params().iter().zip(args) {
                    self.emit_value_copy(
                        ValueLocation::new(
                            target.slot,
                            target.offset + struct_def.field_offset(param.name()),
                            target.objective.clone(),
                        ),
                        arg,
                        param.param_type().from(&self.types).total_size(&self.types),
                    );
                }

                return Some(target);
            }
            _ => unreachable!(),
        }

        for (i, (param, arg)) in func_signature.params().iter().zip(args).enumerate() {
            let target = ValueLocation::new(i, 0, func_objective.clone());

            self.emit_value_copy(
                target,
                arg,
                param.param_type().from(&self.types).total_size(&self.types),
            );
        }

        self.emit(Instruction::Call {
            function: ResourceLocation::new(self.pack_name.clone(), func_objective.0.clone()),
        });

        if handle_return {
            let target = self.get_free_location();
            let source =
                ValueLocation::new(0, 0, Objective(format!("{}.return", func_objective.0)));

            self.emit_value_copy(
                target.clone(),
                source,
                self.tags
                    .get_type(node)
                    .from(&self.types)
                    .total_size(&self.types),
            );

            Some(target)
        } else {
            None
        }
    }

    fn visit_block(&mut self, body: &[ParserNode]) {
        for node in body {
            self.visit_node(node);
        }
    }

    fn visit_return(&mut self, expr: &Option<Box<ParserNode>>) {
        match expr {
            Some(expr) => {
                let source = self.visit_node(expr);
                let size = self
                    .tags
                    .get_type(expr)
                    .from(&self.types)
                    .total_size(&self.types);

                self.emit(Instruction::Return {
                    source: Some(source),
                    size,
                });
            }
            None => {
                self.emit(Instruction::Return {
                    source: None,
                    size: 0,
                });
            }
        }
    }

    fn visit_if(
        &mut self,
        cond: &ParserNode,
        body: &ParserNode,
        else_body: &Option<Box<ParserNode>>,
    ) {
        let true_body = Self::create_block(false, self, |_, builder| {
            builder.visit_node(body);
        });

        let else_body = else_body.as_ref().map(|else_body| {
            Self::create_block(false, self, |_, builder| {
                builder.visit_node(else_body);
            })
        });

        let cond = self.visit_node(cond);

        self.emit(Instruction::IfValueMatchesRunBlock {
            source: cond.clone(),
            value: 1,
            block: true_body,
        });

        match else_body {
            Some(else_body) => {
                self.emit(Instruction::IfValueMatchesRunBlock {
                    source: cond,
                    value: 0,
                    block: else_body,
                });
            }
            None => {}
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

        let looping_body = Self::create_block(true, self, |id, builder| {
            builder.visit_node(body);
            builder.visit_node(step);

            let cond = builder.visit_node(cond);

            builder.emit(Instruction::IfValueMatchesRunBlock {
                source: cond,
                value: 1,
                block: id,
            });
        });

        let cond = self.visit_node(cond);
        self.emit(Instruction::IfValueMatchesRunBlock {
            source: cond,
            value: 1,
            block: looping_body,
        });
    }

    fn visit_member_access(&mut self, expr: &ParserNode, member: &str) -> ValueLocation {
        let source = self.visit_node(expr);
        let target = self.get_free_location();
        let expr_type = self.tags.get_type(expr).from(&self.types).as_struct_def();

        self.emit_value_copy(
            target.clone(),
            ValueLocation::new(
                source.slot,
                source.offset + expr_type.field_offset(member),
                source.objective.clone(),
            ),
            expr_type
                .field(member)
                .unwrap()
                .field_type()
                .from(&self.types)
                .total_size(&self.types),
        );

        target
    }

    fn resolve_location(&mut self, resolution: &Resolution) -> ValueLocation {
        let mut offset = 0;
        let mut slot = 0;

        for part in resolution.iter() {
            match part {
                ResolvedPart::Variable(_, name) => {
                    slot = self.get_local(name).slot;
                }
                ResolvedPart::Field(ty, name) => {
                    let struct_def = ty.from(&self.types).as_struct_def();
                    offset += struct_def.field_offset(name);
                }
                _ => unreachable!(),
            }
        }

        ValueLocation::new(slot, offset, self.objective.clone())
    }
}
