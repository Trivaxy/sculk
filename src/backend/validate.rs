use std::{collections::HashMap, ops::Range};

use by_address::ByAddress;

use crate::{
    backend::type_pool::{TypeKey, TypePool},
    backend::types::{FieldDef, SculkType, StructDef},
    data::ResourceLocation,
    parser::{Operation, ParserNode, ParserNodeKind},
};

use super::{
    function::{FunctionSignature, ParamDef},
    resolve::{Resolution, ResolutionError, ResolvedPart, Resolver},
};

// The validation stage happens right after the parser produces an AST
// In this phase, we perform type checking, make sure statements don't appear illegally (e.g break when not in a loop),
// and collect struct definitions and function declarations
pub struct Validator<'a> {
    pack_name: String,
    global_functions: HashMap<ResourceLocation, FunctionSignature>,
    current_return_type: Option<TypeKey>,
    types: TypePool,
    tags: TagPool<'a>,
    scope_stack: ScopeStack,
    errors: ValidationErrorList,
    current_struct: Option<TypeKey>,
}

impl<'a> Validator<'a> {
    pub fn new(pack_name: String) -> Self {
        Self {
            pack_name,
            global_functions: HashMap::new(),
            current_return_type: None,
            types: TypePool::new_with_primitives(),
            tags: TagPool::new(),
            scope_stack: ScopeStack::new(),
            errors: ValidationErrorList::new(),
            current_struct: None,
        }
    }

    pub fn validate_program(mut self, ast: &'a ParserNode) -> ValidatorOutput {
        self.scan_struct_defs(ast.as_program());
        self.scan_func_defs(ast.as_program());
        self.visit_node(ast);

        self.dissolve()
    }

    pub fn dissolve(self) -> ValidatorOutput<'a> {
        ValidatorOutput {
            global_functions: self.global_functions,
            types: self.types,
            errors: self.errors.dissolve(),
            tags: self.tags,
        }
    }

    fn visit_node(&mut self, node: &'a ParserNode) -> TypeKey {
        let ty = match node.kind() {
            ParserNodeKind::Program(nodes) => {
                for node in nodes {
                    self.visit_node(node);
                }

                self.types.none()
            }
            ParserNodeKind::FunctionDeclaration {
                name,
                body,
                is_static,
                events,
                ..
            } => {
                if *is_static && self.current_struct.is_none() {
                    self.errors
                        .add(ValidationErrorKind::StaticNotAllowed, node.span());
                }

                let func_signature = match self.current_struct {
                    Some(ty) => ty.from(&self.types).as_struct_def().function(name).unwrap(),
                    None => self
                        .global_functions
                        .get(&ResourceLocation::new(self.pack_name.clone(), name.clone()))
                        .unwrap(),
                };

                self.current_return_type = Some(func_signature.return_type());

                if func_signature.return_type() != self.types.none()
                    && !self.check_node_returns(body)
                {
                    self.errors
                        .add(ValidationErrorKind::NotAllPathsReturn, node.span());
                }

                if !events.is_empty() {
                    if !*is_static && self.current_struct.is_some() {
                        self.errors
                            .add(ValidationErrorKind::EventListenerNotAllowed, node.span());
                    }

                    if !func_signature.params().is_empty() {
                        self.errors
                            .add(ValidationErrorKind::InvalidEventListenerArgs, node.span());
                    }
                }

                self.scope_stack.push();

                if !is_static && self.current_struct.is_some() {
                    self.scope_stack
                        .register_variable("self".to_string(), self.current_struct.unwrap());
                }

                func_signature.params().iter().for_each(|param| {
                    self.scope_stack
                        .register_variable(param.name().to_string(), param.param_type());
                });

                self.visit_node(body);

                for event in events {
                    self.visit_node(event);
                }

                self.current_return_type = None;
                self.scope_stack.pop();

                self.types.none()
            }
            ParserNodeKind::Block(nodes) => {
                self.scope_stack.push();

                for node in nodes {
                    self.visit_node(node);
                }

                self.scope_stack.pop();

                self.types.none()
            }
            ParserNodeKind::If {
                cond,
                body,
                else_ifs,
                else_body,
                ..
            } => {
                let cond_type = self.visit_node(cond);

                if cond_type != self.types.bool() {
                    self.errors.add(
                        ValidationErrorKind::ExpectedBoolInIf(cond_type),
                        cond.span(),
                    );
                }

                self.scope_stack.push();
                self.visit_node(body);
                self.scope_stack.pop();

                for (cond, body) in else_ifs {
                    let cond_type = self.visit_node(cond);

                    if cond_type != self.types.bool() {
                        self.errors.add(
                            ValidationErrorKind::ExpectedBoolInIf(cond_type),
                            cond.span(),
                        );
                    }

                    self.scope_stack.push();
                    self.visit_node(body);
                    self.scope_stack.pop();
                }

                if let Some(body) = else_body {
                    self.scope_stack.push();
                    self.visit_node(body);
                    self.scope_stack.pop();
                }

                self.types.none()
            }
            ParserNodeKind::For {
                init,
                cond,
                step,
                body,
            } => {
                self.scope_stack.push_loop();

                self.visit_node(init);

                let cond_type = self.visit_node(cond);

                if cond_type != self.types.bool() {
                    self.errors.add(
                        ValidationErrorKind::ExpectedBoolInForCondition(cond_type),
                        cond.span(),
                    );
                }

                self.visit_node(step);

                self.visit_node(body);

                self.scope_stack.pop();

                self.types.none()
            }
            ParserNodeKind::Break => {
                if !self.scope_stack.is_in_loop() {
                    self.errors
                        .add(ValidationErrorKind::CannotBreakOutsideLoop, node.span());
                }

                self.types.none()
            }
            ParserNodeKind::NumberLiteral(_) => self.types.int(),
            ParserNodeKind::BoolLiteral(_) => self.types.bool(),
            ParserNodeKind::Identifier(ident) => match self.scope_stack.find_variable_type(ident) {
                Some(ty) => ty,
                None => {
                    self.errors.add(
                        ValidationErrorKind::UnknownVariable(ident.clone()),
                        node.span(),
                    );
                    self.types.unknown()
                }
            },
            ParserNodeKind::TypedIdentifier { .. } => self.types.none(),
            ParserNodeKind::VariableDeclaration { name, expr, ty } => {
                let name = name.as_identifier();

                if self.scope_stack.variable_exists(name) {
                    self.errors.add(
                        ValidationErrorKind::VariableAlreadyDefined(name.to_string()),
                        node.span(),
                    );
                }

                let specified_type = match ty {
                    Some(ty) => self.types.get_type_key(ty).or_else(|| {
                        self.errors
                            .add(ValidationErrorKind::UnknownType(ty.clone()), node.span());
                        Some(self.types.unknown())
                    }),
                    None => None,
                };

                let expr_type = self.visit_node(expr);

                if let Some(specified_type) = specified_type {
                    if specified_type != expr_type {
                        self.errors.add(
                            ValidationErrorKind::VariableAssignmentTypeMismatch {
                                expected: specified_type,
                                actual: expr_type.clone(),
                                expr_span: expr.span(),
                            },
                            node.span(),
                        );
                    }
                }

                self.scope_stack
                    .register_variable(name.to_string(), expr_type);

                self.types.none()
            }
            ParserNodeKind::VariableAssignment { path, expr } => {
                let resolution = match self.resolver().resolve(path) {
                    Ok(resolution) => resolution,
                    Err(err) => {
                        self.errors
                            .add(ValidationErrorKind::CouldNotResolve(err), path.span());

                        return self.types.unknown();
                    }
                };

                let expr_type = self.visit_node(expr);

                match resolution.find_assignable_type(&self.types) {
                    Some(ty) => {
                        if ty != expr_type {
                            self.errors.add(
                                ValidationErrorKind::VariableAssignmentTypeMismatch {
                                    expected: ty,
                                    actual: expr_type,
                                    expr_span: expr.span(),
                                },
                                node.span(),
                            );
                        }
                    }
                    None => {
                        self.errors
                            .add(ValidationErrorKind::NotAssignable, node.span());
                    }
                }

                self.tags.tag_resolution(path, resolution);
                self.types.none()
            }
            ParserNodeKind::Return(expr) => {
                match self.current_return_type.clone() {
                    Some(expected_type) => {
                        if let Some(return_expr) = expr {
                            let expr_type = self.visit_node(return_expr);

                            if expr_type != expected_type {
                                self.errors.add(
                                    ValidationErrorKind::ReturnTypeMismatch {
                                        expected: expected_type.clone(),
                                        actual: expr_type,
                                        expr_span: return_expr.span(),
                                    },
                                    node.span(),
                                );
                            }
                        } else {
                            self.errors.add(
                                ValidationErrorKind::ReturnValueExpected(expected_type.clone()),
                                node.span(),
                            );
                        }
                    }
                    None => {
                        self.errors.add(
                            ValidationErrorKind::ReturnValueExpected(self.types.none()),
                            node.span(),
                        );
                    }
                }

                self.types.none()
            }
            ParserNodeKind::FunctionCall {
                expr,
                args: arg_nodes,
            } => {
                let callee = match self.resolver().resolve(node) {
                    Ok(resolution) => resolution,
                    Err(err) => {
                        self.errors
                            .add(ValidationErrorKind::CouldNotResolve(err), node.span());

                        return self.types.unknown();
                    }
                };

                let (expected_types, ret_type, param_names) = {
                    let func_signature = match &callee.last() {
                        ResolvedPart::GlobalFunction(name) => self
                            .global_functions
                            .get(&ResourceLocation::new(self.pack_name.clone(), name.clone()))
                            .unwrap(),
                        ResolvedPart::Method(ty, name) => ty
                            .from(&self.types)
                            .as_struct_def()
                            .function(&name)
                            .unwrap(),
                        ResolvedPart::Constructor(ty) => {
                            ty.from(&self.types).as_struct_def().constructor()
                        }
                        _ => unreachable!(),
                    };

                    let expected_types = func_signature
                        .params()
                        .iter()
                        .map(|param| param.param_type())
                        .collect::<Vec<TypeKey>>();

                    let ret_type = func_signature.return_type();

                    let param_names = func_signature
                        .params()
                        .iter()
                        .map(|param| param.name().to_string())
                        .collect::<Vec<String>>();

                    (expected_types, ret_type, param_names)
                };

                let param_count = expected_types.len();

                if arg_nodes.len() < param_count {
                    let missing_count = param_count - arg_nodes.len();
                    let missing = &param_names[param_count - missing_count..];

                    self.errors.add(
                        ValidationErrorKind::NotEnoughArguments {
                            callee_span: expr.span(),
                            missing: missing.to_vec(),
                        },
                        node.span(),
                    );
                }

                for (i, (arg, expected_type)) in arg_nodes.iter().zip(expected_types).enumerate() {
                    let arg_type = self.visit_node(arg);

                    if arg_type != expected_type {
                        self.errors.add(
                            ValidationErrorKind::FunctionCallArgTypeMismatch {
                                name: param_names[i].to_string(),
                                expected: expected_type,
                                actual: arg_type,
                            },
                            arg.span(),
                        );
                    }
                }

                self.tags.tag_resolution(node, callee);

                ret_type
            }
            ParserNodeKind::Expression(expr) => self.visit_node(expr),
            ParserNodeKind::Operation(lhs, rhs, op) => {
                let lhs_type = self.visit_node(lhs);
                let rhs_type = self.visit_node(rhs);

                match op {
                    Operation::CheckEquals
                    | Operation::NotEquals
                    | Operation::GreaterThan
                    | Operation::LessThan
                    | Operation::GreaterThanOrEquals
                    | Operation::LessThanOrEquals => {
                        if lhs_type != self.types.int() || rhs_type != self.types.int() {
                            self.errors.add(
                                ValidationErrorKind::ComparisonOperatorTypeMismatch {
                                    lhs: lhs_type,
                                    rhs: rhs_type,
                                    op: *op,
                                },
                                node.span(),
                            );
                        }

                        self.types.bool()
                    }
                    Operation::And | Operation::Or => {
                        if lhs_type != self.types.bool() || rhs_type != self.types.bool() {
                            self.errors.add(
                                ValidationErrorKind::LogicalOperatorTypeMismatch {
                                    lhs: lhs_type,
                                    rhs: rhs_type,
                                    op: *op,
                                },
                                node.span(),
                            );
                        }

                        self.types.bool()
                    }
                    _ => {
                        if lhs_type != rhs_type {
                            self.errors.add(
                                ValidationErrorKind::OperationTypeMismatch {
                                    lhs: lhs_type.clone(),
                                    rhs: rhs_type.clone(),
                                    op: *op,
                                },
                                node.span(),
                            );
                        }

                        if lhs_type != self.types.int() {
                            self.errors.add(
                                ValidationErrorKind::ArithmeticUnsupported {
                                    ty: lhs_type.clone(),
                                },
                                lhs.span(),
                            )
                        }

                        if rhs_type != self.types.int() {
                            self.errors.add(
                                ValidationErrorKind::ArithmeticUnsupported { ty: rhs_type },
                                rhs.span(),
                            );
                        }

                        lhs_type
                    }
                }
            }
            ParserNodeKind::OpEquals { path, expr, .. } => {
                let expr_type = self.visit_node(expr);

                if expr_type != self.types.int() {
                    self.errors.add(
                        ValidationErrorKind::ArithmeticUnsupported { ty: expr_type },
                        expr.span(),
                    );
                }

                let resolution = match self.resolver().resolve(path) {
                    Ok(resolution) => resolution,
                    Err(err) => {
                        self.errors
                            .add(ValidationErrorKind::CouldNotResolve(err), node.span());

                        return self.types.unknown();
                    }
                };

                match resolution.find_assignable_type(&self.types) {
                    Some(ty) => {
                        let ty = ty;

                        if ty != self.types.int() {
                            self.errors.add(
                                ValidationErrorKind::ArithmeticUnsupported { ty },
                                node.span(),
                            );
                        }
                    }
                    None => {
                        self.errors
                            .add(ValidationErrorKind::NotAssignable, node.span());
                    }
                }

                self.tags.tag_resolution(path, resolution);
                self.types.none()
            }
            ParserNodeKind::MemberAccess { expr, member } => {
                self.visit_node(expr);

                match self.resolver().resolve(node) {
                    Ok(resolution) => match resolution.last() {
                        ResolvedPart::Field(ty, name) => ty
                            .from(&self.types)
                            .as_struct_def()
                            .field(&name)
                            .unwrap()
                            .field_type(),
                        ResolvedPart::Method(_, _) => {
                            self.errors.add(
                                ValidationErrorKind::CannotReferenceMethodAsValue,
                                member.span(),
                            );

                            return self.types.unknown();
                        }
                        _ => unreachable!(),
                    },
                    Err(err) => {
                        self.errors
                            .add(ValidationErrorKind::CouldNotResolve(err), node.span());

                        return self.types.unknown();
                    }
                }
            }
            ParserNodeKind::Unary(expr, _) => self.visit_node(expr),
            ParserNodeKind::CommandLiteral(_) => self.types.none(),
            ParserNodeKind::StructDefinition { name, members } => {
                self.current_struct = self.types.get_type_key(name);

                for member in members {
                    self.visit_node(member);
                }

                self.current_struct = None;
                self.types.none()
            }
            ParserNodeKind::EventListener { name } => {
                match name.as_str() {
                    "on_tick" | "on_load" => {}
                    _ => self
                        .errors
                        .add(ValidationErrorKind::InvalidEventListener, node.span()),
                }
                self.types.none()
            }
        };

        if ty != self.types.none() && ty != self.types.unknown() {
            self.tags.tag_type(node, ty);
        }

        ty
    }

    fn resolver(&self) -> Resolver {
        Resolver::new(
            &self.pack_name,
            &self.global_functions,
            &self.types,
            &self.scope_stack,
        )
    }

    fn check_node_returns(&self, node: &ParserNode) -> bool {
        match node.kind() {
            ParserNodeKind::Return(_) => true,
            ParserNodeKind::If {
                body,
                else_ifs,
                else_body,
                ..
            } => {
                self.check_node_returns(&body)
                    && else_ifs
                        .iter()
                        .all(|(_, body)| self.check_node_returns(body))
                    && else_body
                        .as_ref()
                        .map_or(false, |body| self.check_node_returns(&body))
            }
            ParserNodeKind::For { .. } => false,
            ParserNodeKind::Block(nodes) => nodes.iter().any(|node| self.check_node_returns(node)),
            _ => false,
        }
    }

    // should only be passed the contents of the root Program node
    fn scan_struct_defs(&mut self, nodes: &[ParserNode]) {
        let struct_defs = nodes
            .iter()
            .filter_map(|node| match node.kind() {
                ParserNodeKind::StructDefinition { name, members } => {
                    Some((name, members.as_slice()))
                }
                _ => None,
            })
            .collect::<Vec<(&String, &[ParserNode])>>();

        // first pass to register empty struct definitions
        for ((name, ..), node) in struct_defs.iter().zip(nodes) {
            if self.types.has_type(name) {
                self.errors.add(
                    ValidationErrorKind::StructAlreadyDefined(name.to_string()),
                    node.span(),
                );
                continue;
            }

            self.types.insert(
                name.to_string(),
                SculkType::Struct(StructDef::new_empty(name.to_string())),
            );
        }

        // second pass to fill in concrete struct types from the nodes
        for (name, members) in &struct_defs {
            for member in *members {
                let struct_type = self.types.get_type_key(name).unwrap();

                match member.kind() {
                    ParserNodeKind::TypedIdentifier { name, ty } => {
                        let field_type = self.types.get_type_key(ty);

                        match field_type {
                            Some(ty) => {
                                match struct_type
                                    .from_mut(&mut self.types)
                                    .as_struct_def_mut()
                                    .add_field(FieldDef::new(name.to_string(), ty))
                                {
                                    Ok(_) => {}
                                    Err(_) => self.errors.add(
                                        ValidationErrorKind::StructFieldAlreadyDefined {
                                            struct_name: name.to_string(),
                                            field_name: name.to_string(),
                                        },
                                        member.span(),
                                    ),
                                }
                            }
                            None => self.errors.add(
                                ValidationErrorKind::UnknownType(ty.to_string()),
                                member.span(),
                            ),
                        }
                    }
                    ParserNodeKind::FunctionDeclaration {
                        name, is_static, ..
                    } => {
                        let owner = match is_static {
                            true => None,
                            false => Some(struct_type),
                        };

                        let func_signature = self.create_func_def(owner, member);

                        match struct_type
                            .from_mut(&mut self.types)
                            .as_struct_def_mut()
                            .add_function(func_signature)
                        {
                            Ok(_) => {}
                            Err(_) => self.errors.add(
                                ValidationErrorKind::FunctionAlreadyDefined(name.clone()),
                                member.span(),
                            ),
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }

        // third pass to check for self-referencing structs
        for ((name, ..), nodes) in struct_defs.into_iter().zip(nodes) {
            let struct_type = self.types.get_type_key(name).unwrap();
            let struct_def = struct_type.from(&self.types).as_struct_def();

            if struct_def.self_referencing(&self.types) {
                self.errors.add(
                    ValidationErrorKind::StructSelfReferences(struct_def.name().to_string()),
                    nodes.span(),
                );
            }
        }

        // Finalize the type pool (calculates field offsets and assigns constructors)
        self.types.finalize();
    }

    // should only be passed the contents of the root Program node
    fn scan_func_defs(&mut self, nodes: &[ParserNode]) {
        let func_defs = nodes
            .iter()
            .filter(|node| match node.kind() {
                ParserNodeKind::FunctionDeclaration { .. } => true,
                _ => false,
            })
            .collect::<Vec<&ParserNode>>();

        for node in func_defs {
            match node.kind() {
                ParserNodeKind::FunctionDeclaration { name, .. } => {
                    let func_signature = self.create_func_def(None, node);

                    if self
                        .global_functions
                        .contains_key(&ResourceLocation::new(self.pack_name.clone(), name.clone()))
                    {
                        self.errors.add(
                            ValidationErrorKind::FunctionAlreadyDefined(name.clone()),
                            node.span(),
                        );
                    }

                    if self.types.has_type(&name) {
                        self.errors.add(
                            ValidationErrorKind::FunctionStructNameClash(name.clone()),
                            node.span(),
                        );
                    }

                    self.global_functions.insert(
                        ResourceLocation::new(self.pack_name.clone(), name.clone()),
                        func_signature,
                    );
                }
                _ => unreachable!(),
            }
        }
    }

    fn create_func_def(&mut self, owner: Option<TypeKey>, func: &ParserNode) -> FunctionSignature {
        let (name, args, return_ty_str, is_static) = match func.kind() {
            ParserNodeKind::FunctionDeclaration {
                name,
                args,
                return_ty,
                is_static,
                ..
            } => (name, args, return_ty, is_static),
            _ => unreachable!(),
        };

        let mut arg_types = Vec::new();

        for arg in args {
            let (_, arg_type_str) = arg.as_typed_identifier();
            let arg_type = self.types.get_type_key(arg_type_str);

            match arg_type {
                Some(ty) => arg_types.push(ty),
                None => {
                    arg_types.push(self.types.unknown());
                    self.errors.add(
                        ValidationErrorKind::UnknownType(arg_type_str.to_string()),
                        arg.span(),
                    );
                }
            }
        }

        let return_type = match return_ty_str {
            Some(return_ty_str) => match self.types.get_type_key(return_ty_str) {
                Some(ty) => ty,
                None => {
                    self.errors.add(
                        ValidationErrorKind::UnknownType(return_ty_str.to_string()),
                        func.span(),
                    );
                    self.types.none()
                }
            },
            None => self.types.none(),
        };

        // If owner is not None, then the first parameter must be named "self" and of type owner
        let mut params = args
            .iter()
            .map(|arg| arg.as_identifier())
            .zip(arg_types)
            .map(|(name, ty)| ParamDef::new(name.to_string(), ty))
            .collect::<Vec<ParamDef>>();

        if let Some(owner) = owner {
            params.insert(0, ParamDef::new("self".to_string(), owner));
        }

        FunctionSignature::new(name.clone(), params, return_type, *is_static)
    }
}

pub struct ValidatorOutput<'a> {
    pub global_functions: HashMap<ResourceLocation, FunctionSignature>,
    pub types: TypePool,
    pub errors: Vec<ValidationError>,
    pub tags: TagPool<'a>,
}

#[derive(Clone, Debug)]
pub struct ValidationError {
    pub kind: ValidationErrorKind,
    pub span: Range<usize>,
}

impl ValidationError {
    pub fn new(kind: ValidationErrorKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Debug)]
pub enum ValidationErrorKind {
    CannotBreakOutsideLoop,
    ExpectedBoolInIf(TypeKey),
    ExpectedBoolInForCondition(TypeKey),
    UnknownVariable(String),
    UnknownFunction(String),
    VariableAlreadyDefined(String),
    VariableAssignmentTypeMismatch {
        expected: TypeKey,
        actual: TypeKey,
        expr_span: Range<usize>,
    },
    ReturnTypeMismatch {
        expected: TypeKey,
        actual: TypeKey,
        expr_span: Range<usize>,
    },
    ReturnValueExpected(TypeKey),
    NotAllPathsReturn,
    FunctionCallArgTypeMismatch {
        name: String,
        expected: TypeKey,
        actual: TypeKey,
    },
    NotEnoughArguments {
        callee_span: Range<usize>,
        missing: Vec<String>,
    },
    OperationTypeMismatch {
        lhs: TypeKey,
        rhs: TypeKey,
        op: Operation,
    },
    ComparisonOperatorTypeMismatch {
        lhs: TypeKey,
        rhs: TypeKey,
        op: Operation,
    },
    LogicalOperatorTypeMismatch {
        lhs: TypeKey,
        rhs: TypeKey,
        op: Operation,
    },
    ArithmeticUnsupported {
        ty: TypeKey,
    },
    FunctionAlreadyDefined(String),
    FunctionStructNameClash(String),
    StructAlreadyDefined(String),
    StructFieldAlreadyDefined {
        struct_name: String,
        field_name: String,
    },
    AmbiguousCall(String),
    StructSelfReferences(String),
    UnknownType(String),
    CouldNotResolve(ResolutionError),
    CannotReferenceMethodAsValue,
    NotAssignable,
    StaticNotAllowed,
    EventListenerNotAllowed,
    InvalidEventListener,
    InvalidEventListenerArgs,
}

pub struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    fn push(&mut self) {
        self.scopes.push(Scope::new(false));
    }

    fn push_loop(&mut self) {
        self.scopes.push(Scope::new(true));
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }

    fn last_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn is_in_loop(&self) -> bool {
        self.scopes.iter().rev().any(|scope| scope.is_loop)
    }

    fn register_variable(&mut self, name: String, ty: TypeKey) {
        self.last_mut().add_variable(name, ty);
    }

    pub fn find_variable_type(&self, name: &str) -> Option<TypeKey> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get_variable(name) {
                return Some(ty);
            }
        }

        None
    }

    fn variable_exists(&self, name: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.get_variable(name).is_some() {
                return true;
            }
        }

        return false;
    }
}

pub struct Scope {
    variables: HashMap<String, TypeKey>,
    is_loop: bool,
}

impl Scope {
    fn new(is_loop: bool) -> Self {
        Self {
            variables: HashMap::new(),
            is_loop,
        }
    }

    fn add_variable(&mut self, name: String, ty: TypeKey) {
        self.variables.insert(name, ty);
    }

    fn get_variable(&self, name: &str) -> Option<TypeKey> {
        match self.variables.get(name) {
            Some(ty) => Some(ty.clone()),
            None => None,
        }
    }
}

struct ValidationErrorList(Vec<ValidationError>);

impl ValidationErrorList {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn add(&mut self, kind: ValidationErrorKind, span: Range<usize>) {
        self.0.push(ValidationError::new(kind, span));
    }

    fn dissolve(self) -> Vec<ValidationError> {
        self.0
    }
}

pub struct TagPool<'a> {
    types: HashMap<ByAddress<&'a ParserNode>, TypeKey>,
    resolutions: HashMap<ByAddress<&'a ParserNode>, Resolution>,
}

impl<'a> TagPool<'a> {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            resolutions: HashMap::new(),
        }
    }

    pub fn tag_type(&mut self, node: &'a ParserNode, ty: TypeKey) {
        self.types.insert(ByAddress(node), ty);
    }

    pub fn tag_resolution(&mut self, node: &'a ParserNode, resolution: Resolution) {
        self.resolutions.insert(ByAddress(node), resolution);
    }

    pub fn get_type(&self, node: &'a ParserNode) -> TypeKey {
        *self.types.get(&ByAddress(node)).unwrap()
    }

    pub fn get_resolution(&self, node: &'a ParserNode) -> &Resolution {
        self.resolutions.get(&ByAddress(node)).unwrap()
    }
}
