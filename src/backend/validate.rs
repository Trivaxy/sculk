use std::{cell::Cell, collections::HashMap};

use crate::{
    parser::{Operation, ParserNode},
    types::SculkType,
};

// Performs type checking on the AST, and collects a list of errors if any are found
pub struct Validator {
    func_signatures: Cell<HashMap<String, FunctionSignature>>,
    current_return_type: SculkType,
    scopes: Vec<Scope>,
    errors: Vec<ValidationError>,
}

impl Validator {
    pub fn new() -> Self {
        Self {
            func_signatures: Cell::new(HashMap::new()),
            current_return_type: SculkType::None,
            scopes: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn validate_program(mut self, ast: &ParserNode) -> Vec<ValidationError> {
        self.visit_node(ast);
        self.errors
    }

    fn visit_node(&mut self, node: &ParserNode) -> SculkType {
        match node {
            ParserNode::Program(nodes) => {
                for node in nodes {
                    self.visit_node(node);
                }

                SculkType::None
            }
            ParserNode::FunctionDeclaration {
                name,
                args,
                return_ty,
                body,
            } => {
                self.func_signatures.get_mut().insert(
                    name.clone(),
                    FunctionSignature::new(
                        args.iter().map(|arg| arg.as_type()).collect(),
                        return_ty.clone(),
                    ),
                );

                self.current_return_type = return_ty.clone();
                self.push_scope();

                for arg in args {
                    self.register_variable(arg.as_identifier().to_string(), arg.as_type());
                }

                self.visit_node(body);

                self.current_return_type = SculkType::None;
                self.pop_scope();

                SculkType::None
            }
            ParserNode::Block(nodes) => {
                self.push_scope();

                for node in nodes {
                    self.visit_node(node);
                }

                self.pop_scope();

                SculkType::None
            }
            ParserNode::If {
                cond,
                body,
                else_ifs,
                else_body,
                ..
            } => {
                let cond_type = self.visit_node(cond);

                if cond_type != SculkType::Bool {
                    self.error(ValidationError::ExpectedBoolInIf(cond_type));
                }

                self.push_scope();
                self.visit_node(body);
                self.pop_scope();

                for (cond, body) in else_ifs {
                    let cond_type = self.visit_node(cond);

                    if cond_type != SculkType::Bool {
                        self.error(ValidationError::ExpectedBoolInIf(cond_type));
                    }

                    self.push_scope();
                    self.visit_node(body);
                    self.pop_scope();
                }

                if let Some(body) = else_body {
                    self.push_scope();
                    self.visit_node(body);
                    self.pop_scope();
                }

                SculkType::None
            }
            ParserNode::NumberLiteral(_) => SculkType::Integer,
            ParserNode::BoolLiteral(_) => SculkType::Bool,
            ParserNode::SelectorLiteral(_) => SculkType::Selector,
            ParserNode::Identifier(ident) => match self.find_variable_type(ident) {
                Some(ty) => ty.clone(),
                None => {
                    self.error(ValidationError::UnknownVariable(ident.clone()));
                    SculkType::Unknown
                }
            },
            ParserNode::TypedIdentifier { .. } => unreachable!(),
            ParserNode::VariableDeclaration { name, expr, ty } => {
                let specified_type = ty;
                let expr_type = self.visit_node(expr);

                if let Some(specified_type) = specified_type {
                    if *specified_type != expr_type {
                        self.error(ValidationError::VariableAssignmentTypeMismatch {
                            name: name.clone(),
                            expected: specified_type.clone(),
                            actual: expr_type.clone(),
                        });
                    }
                }

                self.register_variable(name.clone(), expr_type.clone());

                SculkType::None
            }
            ParserNode::VariableAssignment { name, expr } => {
                let expr_type = self.visit_node(expr);

                match self.find_variable_type(name) {
                    Some(ty) => {
                        if *ty != expr_type {
                            self.error(ValidationError::VariableAssignmentTypeMismatch {
                                name: name.clone(),
                                expected: ty.clone(),
                                actual: expr_type.clone(),
                            });
                        }
                    }
                    None => {
                        self.error(ValidationError::UnknownVariable(name.clone()));
                    }
                }

                SculkType::None
            }
            ParserNode::Return(expr) => {
                let ret_type = self.visit_node(expr);

                if ret_type != self.current_return_type {
                    self.error(ValidationError::ReturnTypeMismatch {
                        expected: self.current_return_type.clone(),
                        actual: ret_type.clone(),
                    });
                }

                SculkType::None
            }
            ParserNode::FunctionCall { name, args } => {
                let func_signatures = self.func_signatures.take();

                let ret_type = {
                    let func = match func_signatures.get(name) {
                        Some(sig) => sig,
                        None => {
                            self.error(ValidationError::UnknownFunction(name.clone()));
                            return SculkType::Unknown;
                        }
                    };

                    for (arg, expected_type) in args.iter().zip(func.args.iter()) {
                        let arg_type = self.visit_node(arg);

                        if arg_type != *expected_type {
                            self.error(ValidationError::FunctionCallArgTypeMismatch {
                                name: name.clone(),
                                expected: expected_type.clone(),
                                actual: arg_type.clone(),
                            });
                        }
                    }

                    func.return_type.clone()
                };

                self.func_signatures.set(func_signatures);
                ret_type
            }
            ParserNode::Operation(lhs, rhs, op) => {
                let lhs_type = self.visit_node(lhs);
                let rhs_type = self.visit_node(rhs);

                match op {
                    Operation::GreaterThan
                    | Operation::LessThan
                    | Operation::GreaterThanOrEquals
                    | Operation::LessThanOrEquals => {
                        if lhs_type != SculkType::Integer || rhs_type != SculkType::Integer {
                            self.error(ValidationError::ConditionalOperatorTypeMismatch {
                                lhs: lhs_type.clone(),
                                rhs: rhs_type.clone(),
                            });
                        }

                        return SculkType::Bool;
                    }
                    _ => {}
                }

                if lhs_type != rhs_type {
                    self.error(ValidationError::OperationTypeMismatch {
                        lhs: lhs_type.clone(),
                        rhs: rhs_type.clone(),
                        op: op.clone(),
                    });
                }

                lhs_type.clone()
            }
            ParserNode::Unary(expr, _) => self.visit_node(expr),
            ParserNode::ReturnSafe(_) => unreachable!() // does not exist at this stage
        }
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn register_variable(&mut self, name: String, ty: SculkType) {
        self.scope_mut().add_variable(name, ty);
    }

    fn find_variable_type(&self, name: &str) -> Option<&SculkType> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get_variable(name) {
                return Some(ty);
            }
        }

        None
    }

    fn error(&mut self, err: ValidationError) {
        self.errors.push(err);
    }
}

#[derive(Debug)]
pub enum ValidationError {
    ExpectedBoolInIf(SculkType),
    UnknownVariable(String),
    UnknownFunction(String),
    VariableAssignmentTypeMismatch {
        name: String,
        expected: SculkType,
        actual: SculkType,
    },
    ReturnTypeMismatch {
        expected: SculkType,
        actual: SculkType,
    },
    FunctionCallArgTypeMismatch {
        name: String,
        expected: SculkType,
        actual: SculkType,
    },
    OperationTypeMismatch {
        lhs: SculkType,
        rhs: SculkType,
        op: Operation,
    },
    ConditionalOperatorTypeMismatch {
        lhs: SculkType,
        rhs: SculkType,
    },
}

struct Scope {
    variables: HashMap<String, SculkType>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn add_variable(&mut self, name: String, ty: SculkType) {
        self.variables.insert(name, ty);
    }

    fn get_variable(&self, name: &str) -> Option<&SculkType> {
        match self.variables.get(name) {
            Some(ty) => Some(ty),
            None => None,
        }
    }
}

struct FunctionSignature {
    args: Vec<SculkType>,
    return_type: SculkType,
}

impl FunctionSignature {
    fn new(args: Vec<SculkType>, return_type: SculkType) -> Self {
        Self { args, return_type }
    }
}
