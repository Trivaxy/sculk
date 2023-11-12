use std::{cell::Cell, collections::HashMap, env::ArgsOs, ops::Range, rc::Rc};

use crate::{
    backend::type_pool::{TypeKey, TypePool},
    backend::types::{FieldDef, SculkType, StructDef},
    data::ResourceLocation,
    parser::{Operation, ParserNode, ParserNodeKind},
};

use super::function::{FunctionSignature, ParamDef};

// The validation stage happens right after the parser produces an AST
// In this phase, we perform type checking, make sure statements don't appear illegally (e.g break when not in a loop),
// and collect struct definitions and function declarations
pub struct Validator {
    pack_name: String,
    func_signatures: HashMap<ResourceLocation, FunctionSignature>,
    current_return_type: Option<TypeKey>,
    type_pool: TypePool,
    scope_stack: ScopeStack,
    errors: ValidationErrorList,
}

impl Validator {
    pub fn new(pack_name: String) -> Self {
        Self {
            pack_name,
            func_signatures: HashMap::new(),
            current_return_type: None,
            type_pool: TypePool::new_with_primitives(),
            scope_stack: ScopeStack::new(),
            errors: ValidationErrorList::new(),
        }
    }

    pub fn validate_program(mut self, ast: &ParserNode) -> ValidatorOutput {
        self.scan_struct_defs(ast.as_program());
        self.scan_func_defs(ast.as_program());
        self.visit_node(ast);

        self.dissolve()
    }

    pub fn dissolve(self) -> ValidatorOutput {
        ValidatorOutput {
            signatures: self.func_signatures,
            types: self.type_pool,
            errors: self.errors.dissolve(),
        }
    }

    fn visit_node(&mut self, node: &ParserNode) -> TypeKey {
        match node.kind() {
            ParserNodeKind::Program(nodes) => {
                for node in nodes {
                    self.visit_node(node);
                }

                self.type_pool.none()
            }
            ParserNodeKind::FunctionDeclaration {
                name,
                args,
                return_ty,
                body,
            } => {
                self.scope_stack.push();

                let func_signature = self
                    .func_signatures
                    .get(&ResourceLocation::new(self.pack_name.clone(), name.clone()))
                    .unwrap();

                self.current_return_type = Some(func_signature.return_type());

                func_signature.params().iter().for_each(|param| {
                    self.scope_stack
                        .register_variable(param.name().to_string(), param.param_type());
                });

                self.visit_node(body);

                self.current_return_type = None;
                self.scope_stack.pop();

                self.type_pool.none()
            }
            ParserNodeKind::Block(nodes) => {
                self.scope_stack.push();

                for node in nodes {
                    self.visit_node(node);
                }

                self.scope_stack.pop();

                self.type_pool.none()
            }
            ParserNodeKind::If {
                cond,
                body,
                else_ifs,
                else_body,
                ..
            } => {
                let cond_type = self.visit_node(cond);

                if cond_type != self.type_pool.bool() {
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

                    if cond_type != self.type_pool.bool() {
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

                self.type_pool.none()
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

                if cond_type != self.type_pool.bool() {
                    self.errors.add(
                        ValidationErrorKind::ExpectedBoolInForCondition(cond_type),
                        cond.span(),
                    );
                }

                self.visit_node(step);

                self.visit_node(body);

                self.scope_stack.pop();

                self.type_pool.none()
            }
            ParserNodeKind::Break => {
                if !self.scope_stack.is_in_loop() {
                    self.errors
                        .add(ValidationErrorKind::CannotBreakOutsideLoop, node.span());
                }

                self.type_pool.none()
            }
            ParserNodeKind::NumberLiteral(_) => self.type_pool.int(),
            ParserNodeKind::BoolLiteral(_) => self.type_pool.bool(),
            ParserNodeKind::Identifier(ident) => match self.scope_stack.find_variable_type(ident) {
                Some(ty) => ty,
                None => {
                    self.errors.add(
                        ValidationErrorKind::UnknownVariable(ident.clone()),
                        node.span(),
                    );
                    self.type_pool.unknown()
                }
            },
            ParserNodeKind::TypedIdentifier { .. } => unreachable!(),
            ParserNodeKind::VariableDeclaration { name, expr, ty } => {
                if self.scope_stack.variable_exists(name) {
                    self.errors.add(
                        ValidationErrorKind::VariableAlreadyDefined(name.clone()),
                        node.span(),
                    );
                }

                let specified_type = match ty {
                    Some(ty) => self.type_pool.get_type_key(ty).or_else(|| {
                        self.errors
                            .add(ValidationErrorKind::UnknownType(ty.clone()), node.span());
                        Some(self.type_pool.unknown())
                    }),
                    None => None,
                };

                let expr_type = self.visit_node(expr);

                if let Some(specified_type) = specified_type {
                    if specified_type != expr_type {
                        self.errors.add(
                            ValidationErrorKind::VariableAssignmentTypeMismatch {
                                name: name.clone(),
                                expected: specified_type,
                                actual: expr_type.clone(),
                            },
                            node.span(),
                        );
                    }
                }

                self.scope_stack.register_variable(name.clone(), expr_type);

                self.type_pool.none()
            }
            ParserNodeKind::VariableAssignment { name, expr } => {
                let expr_type = self.visit_node(expr);

                match self.scope_stack.find_variable_type(name) {
                    Some(ty) => {
                        if ty != expr_type {
                            self.errors.add(
                                ValidationErrorKind::VariableAssignmentTypeMismatch {
                                    name: name.clone(),
                                    expected: ty,
                                    actual: expr_type,
                                },
                                node.span(),
                            );
                        }
                    }
                    None => {
                        self.errors.add(
                            ValidationErrorKind::UnknownVariable(name.clone()),
                            node.span(),
                        );
                    }
                }

                self.type_pool.none()
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
                            ValidationErrorKind::ReturnValueExpected(self.type_pool.none()),
                            node.span(),
                        );
                    }
                }

                self.type_pool.none()
            }
            ParserNodeKind::FunctionCall {
                name,
                args: arg_nodes,
            } => {
                let ret_type = match (
                    self.func_signatures
                        .get(&ResourceLocation::new(self.pack_name.clone(), name.clone())),
                    self.type_pool.get_type_key(name),
                ) {
                    (Some(_), Some(_)) => {
                        self.errors.add(
                            ValidationErrorKind::AmbiguousCall(name.clone()),
                            node.span(),
                        );
                        return self.type_pool.unknown();
                    }
                    (Some(func), None) => func.return_type(),
                    (None, Some(struct_type)) => struct_type,
                    (None, None) => {
                        self.errors.add(
                            ValidationErrorKind::UnknownFunction(name.clone()),
                            node.span(),
                        );
                        return self.type_pool.none();
                    }
                };

                let (expected_types, arg_names): (Vec<TypeKey>, Vec<String>) = match self
                    .func_signatures
                    .get(&ResourceLocation::new(self.pack_name.clone(), name.clone()))
                {
                    Some(func) => (
                        func.params()
                            .iter()
                            .map(|param| param.param_type())
                            .collect(),
                        func.params()
                            .iter()
                            .map(|param| param.name().to_owned())
                            .collect(),
                    ),
                    None => {
                        let struct_def = self.type_pool.get_type_key(name).unwrap();
                        let struct_def = struct_def.get();

                        if let SculkType::Struct(struct_def) = &*struct_def {
                            (
                                struct_def
                                    .fields()
                                    .iter()
                                    .map(|field| field.field_type().clone())
                                    .collect(),
                                struct_def
                                    .fields()
                                    .iter()
                                    .map(|field| field.name().to_owned())
                                    .collect(),
                            )
                        } else {
                            unreachable!()
                        }
                    }
                };

                if arg_nodes.len() < expected_types.len() {
                    let missing_count = expected_types.len() - arg_nodes.len();
                    let missing = &arg_names[arg_names.len() - missing_count..];

                    self.errors.add(
                        ValidationErrorKind::NotEnoughArguments {
                            name: name.clone(),
                            missing: missing.into_iter().map(|s| s.to_string()).collect(),
                        },
                        node.span(),
                    );
                }

                for (arg, expected_type) in arg_nodes.iter().zip(expected_types) {
                    let arg_type = self.visit_node(arg);

                    if arg_type != expected_type {
                        self.errors.add(
                            ValidationErrorKind::FunctionCallArgTypeMismatch {
                                name: name.clone(),
                                expected: expected_type,
                                actual: arg_type,
                            },
                            arg.span(),
                        );
                    }
                }

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
                        if lhs_type != self.type_pool.int() || rhs_type != self.type_pool.int() {
                            self.errors.add(
                                ValidationErrorKind::ConditionalOperatorTypeMismatch {
                                    lhs: lhs_type,
                                    rhs: rhs_type,
                                },
                                node.span(),
                            );
                        }

                        return self.type_pool.bool();
                    }
                    _ => {}
                }

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

                if lhs_type != self.type_pool.int() {
                    self.errors.add(
                        ValidationErrorKind::ArithmeticUnsupported {
                            ty: lhs_type.clone(),
                        },
                        lhs.span(),
                    )
                }

                if rhs_type != self.type_pool.int() {
                    self.errors.add(
                        ValidationErrorKind::ArithmeticUnsupported { ty: rhs_type },
                        rhs.span(),
                    );
                }

                lhs_type
            }
            ParserNodeKind::OpEquals { name, expr, .. } => {
                let expr_type = self.visit_node(expr);

                if expr_type != self.type_pool.int() {
                    self.errors.add(
                        ValidationErrorKind::ArithmeticUnsupported { ty: expr_type },
                        expr.span(),
                    );
                }

                match self.scope_stack.find_variable_type(name) {
                    Some(ty) => {
                        let ty = ty;

                        if ty != self.type_pool.int() {
                            self.errors.add(
                                ValidationErrorKind::ArithmeticUnsupported { ty },
                                node.span(),
                            );
                        }
                    }
                    None => {
                        self.errors.add(
                            ValidationErrorKind::UnknownVariable(name.clone()),
                            node.span(),
                        );
                    }
                }

                self.type_pool.none()
            }
            ParserNodeKind::Unary(expr, _) => self.visit_node(expr),
            ParserNodeKind::CommandLiteral(_) => self.type_pool.none(),
            ParserNodeKind::StructDefinition { .. } => self.type_pool.none(), // structs are already scanned in advance
            ParserNodeKind::MemberAccess { .. } => todo!(),
        }
    }

    // should only be passed the contents of the root Program node
    fn scan_struct_defs(&mut self, nodes: &[ParserNode]) {
        let struct_defs = nodes
            .iter()
            .filter_map(|node| match node.kind() {
                ParserNodeKind::StructDefinition { name, fields } => {
                    Some((name, fields.as_slice()))
                }
                _ => None,
            })
            .collect::<Vec<(&String, &[ParserNode])>>();

        // first pass to register empty struct definitions
        for ((name, fields), node) in struct_defs.iter().zip(nodes) {
            if self.type_pool.has_type(name) {
                self.errors.add(
                    ValidationErrorKind::StructAlreadyDefined(name.to_string()),
                    node.span(),
                );
                continue;
            }

            self.type_pool.insert(
                name.to_string(),
                SculkType::Struct(StructDef::new_empty(name.to_string())),
            );
        }

        // second pass to fill in concrete struct types from the nodes
        for (name, fields) in &struct_defs {
            for field in *fields {
                let struct_type_key = self.type_pool.get_type_key(name).unwrap();

                let mut struct_type = struct_type_key.get_mut();

                let (field_name, type_str) = field.as_typed_identifier();
                let field_type = self.type_pool.get_type_key(type_str);

                match field_type {
                    Some(ty) => {
                        match struct_type
                            .as_struct_def_mut()
                            .add_field(FieldDef::new(field_name.to_string(), ty))
                        {
                            Ok(_) => {}
                            Err(_) => self.errors.add(
                                ValidationErrorKind::StructFieldAlreadyDefined {
                                    struct_name: name.to_string(),
                                    field_name: field_name.to_string(),
                                },
                                field.span(),
                            ),
                        }
                    }
                    None => self.errors.add(
                        ValidationErrorKind::UnknownType(type_str.to_string()),
                        field.span(),
                    ),
                }
            }
        }

        // third pass to check for self-referencing structs
        for ((name, fields), nodes) in struct_defs.into_iter().zip(nodes) {
            let struct_type_key = self.type_pool.get_type_key(name).unwrap();

            let struct_type = struct_type_key.get();
            let struct_def = struct_type.as_struct_def();

            if struct_def.self_referencing() {
                self.errors.add(
                    ValidationErrorKind::StructSelfReferences(struct_def.name().to_string()),
                    nodes.span(),
                );
            }
        }
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
                ParserNodeKind::FunctionDeclaration {
                    name,
                    args,
                    return_ty: return_ty_str,
                    ..
                } => {
                    if self
                        .func_signatures
                        .contains_key(&ResourceLocation::new(self.pack_name.clone(), name.clone()))
                    {
                        self.errors.add(
                            ValidationErrorKind::FunctionAlreadyDefined(name.clone()),
                            node.span(),
                        );
                        continue;
                    }

                    if self.type_pool.has_type(name) {
                        self.errors.add(
                            ValidationErrorKind::FunctionStructNameClash(name.clone()),
                            node.span(),
                        );
                        continue;
                    }

                    let mut arg_types = Vec::new();

                    for arg in args {
                        let (arg_name, arg_type_str) = arg.as_typed_identifier();
                        let arg_type = self.type_pool.get_type_key(arg_type_str);

                        match arg_type {
                            Some(ty) => arg_types.push(ty),
                            None => {
                                arg_types.push(self.type_pool.unknown());
                                self.errors.add(
                                    ValidationErrorKind::UnknownType(arg_type_str.to_string()),
                                    arg.span(),
                                );
                            }
                        }
                    }

                    let return_type = match return_ty_str {
                        Some(return_ty_str) => match self.type_pool.get_type_key(return_ty_str) {
                            Some(ty) => ty,
                            None => {
                                self.errors.add(
                                    ValidationErrorKind::UnknownType(return_ty_str.to_string()),
                                    node.span(),
                                );
                                self.type_pool.none()
                            }
                        },
                        None => self.type_pool.none(),
                    };

                    let params = args
                        .iter()
                        .map(|arg| arg.as_identifier())
                        .zip(arg_types)
                        .map(|(name, ty)| ParamDef::new(name.to_string(), ty))
                        .collect::<Vec<ParamDef>>();

                    let func_signature = FunctionSignature::new(name.clone(), params, return_type);

                    self.func_signatures.insert(
                        ResourceLocation::new(self.pack_name.clone(), name.clone()),
                        func_signature,
                    );
                }
                _ => unreachable!(),
            }
        }
    }
}

pub struct ValidatorOutput {
    pub signatures: HashMap<ResourceLocation, FunctionSignature>,
    pub types: TypePool,
    pub errors: Vec<ValidationError>,
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
        name: String,
        expected: TypeKey,
        actual: TypeKey,
    },
    ReturnTypeMismatch {
        expected: TypeKey,
        actual: TypeKey,
    },
    ReturnValueExpected(TypeKey),
    FunctionCallArgTypeMismatch {
        name: String,
        expected: TypeKey,
        actual: TypeKey,
    },
    NotEnoughArguments {
        name: String,
        missing: Vec<String>,
    },
    OperationTypeMismatch {
        lhs: TypeKey,
        rhs: TypeKey,
        op: Operation,
    },
    ConditionalOperatorTypeMismatch {
        lhs: TypeKey,
        rhs: TypeKey,
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
}

struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self {
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

    fn find_variable_type(&self, name: &str) -> Option<TypeKey> {
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

struct Scope {
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
