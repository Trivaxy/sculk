use std::collections::HashMap;

use crate::{parser::{ParserNode, ParserNodeKind}, data::ResourceLocation};

use super::{type_pool::{TypeKey, TypePool}, validate::ScopeStack, function::FunctionSignature};

/// A helper type for resolving expressions.
/// This is used to figure out what a parser node is referring to, such as a variable, function, field, etc.
pub struct Resolver<'a> {
    pack_name: &'a str,
    global_functions: &'a HashMap<ResourceLocation, FunctionSignature>,
    types: &'a TypePool,
    scope_stack: &'a ScopeStack,
}

impl<'a> Resolver<'a> {
    pub fn new(pack_name: &'a str, global_functions: &'a HashMap<ResourceLocation, FunctionSignature>, types: &'a TypePool, scope_stack: &'a ScopeStack) -> Self {
        Self {
            pack_name,
            global_functions,
            types,
            scope_stack
        }
    }

    // Resolves a series of calls/accesses to figure out what exactly they are referring to.
    // For example, an expression like Point(x, y, z) will resolve to a constructor call.
    //
    // For more complicated expressions, the resolver will step through the nodes, resolving as it goes.
    // Notably, the resolver may have wrong answers midway - it will only be correct once it reaches the end of the expression.
    // Example: an expression like a.b().c.d() will resolve in the following steps:
    //    a -> Variable
    //      .b -> Field
    //         () -> Method
    //            .c -> Field
    //               .d -> Field
    //                  () -> Method
    //
    // The resolver will return an error if it cannot resolve a node.
    pub fn resolve(&mut self, callee: &ParserNode) -> Result<Resolution, ResolutionError> {
        match callee.kind() {
            ParserNodeKind::Identifier(name) => {
                let (variable, global_func, ty) = (self.scope_stack.find_variable_type(name), self.global_functions.get(&ResourceLocation::new(self.pack_name.to_string(), name.to_string())), self.types.get_type_key(name));

                match (variable, global_func, ty) {
                    (Some(type_key), None, None) => Ok(Resolution::Variable(type_key)),
                    (None, Some(_), None) => Ok(Resolution::GlobalFunction(name.clone())),
                    (None, None, Some(type_key)) => Ok(Resolution::Type(type_key)),
                    (None, None, None) => Err(ResolutionError::UnresolvedIdentifier(name.clone())),
                    _ => Err(ResolutionError::AmbiguousIdentifier {
                        name: name.clone(),
                        candidates: vec![
                            variable.map(Resolution::Variable),
                            global_func.map(|_| Resolution::GlobalFunction(name.clone())),
                            ty.map(Resolution::Type)
                        ].into_iter().filter_map(|x| x).collect()
                    }),
                }
            }
            ParserNodeKind::FunctionCall { expr, .. } => {
                match self.resolve(expr)? {
                    Resolution::GlobalFunction(name) => Ok(Resolution::GlobalFunction(name)),
                    Resolution::Field(ty, name) => {
                        let struct_def = ty.from(&self.types).as_struct_def();

                        if struct_def.function(&name).is_some() {
                            Ok(Resolution::Method(ty, name))
                        } else {
                            Err(ResolutionError::CannotCallMember(name))
                        }
                    },
                    Resolution::Type(ty) => Ok(Resolution::Constructor(ty)),
                    _ => Err(ResolutionError::CannotCallExpression),
                }
            },
            ParserNodeKind::MemberAccess { expr, member } => {
                let member = member.as_identifier();

                match self.resolve(expr)? {
                    Resolution::Variable(ty) => {
                        let struct_def = ty.from(&self.types).as_struct_def();

                        if struct_def.field(&member).is_some() {
                            Ok(Resolution::Field(ty, member.to_string()))
                        } else if struct_def.function(&member).is_some() {
                            Ok(Resolution::Method(ty, member.to_string()))
                        } else {
                            Err(ResolutionError::MemberDoesNotExist(ty, member.to_string()))
                        }
                    },
                    Resolution::Field(ty, name) => {
                        let field_type_key = ty.from(&self.types).as_struct_def().field(&name).unwrap().field_type();
                        let field_type_def = field_type_key.from(&self.types).as_struct_def();

                        if field_type_def.field(&member).is_some() || field_type_def.function(&member).is_some() {
                            Ok(Resolution::Field(field_type_key, member.to_string()))
                        } else {
                            Err(ResolutionError::MemberDoesNotExist(field_type_key, member.to_string()))
                        }
                    },
                    Resolution::Method(ty, name) => {
                        let method_type_key = ty.from(&self.types).as_struct_def().function(&name).unwrap().return_type();
                        let method_type_def = method_type_key.from(&self.types).as_struct_def();

                        if method_type_def.field(&member).is_some() || method_type_def.function(&member).is_some() {
                            Ok(Resolution::Field(method_type_key, member.to_string()))
                        } else {
                            Err(ResolutionError::MemberDoesNotExist(ty, member.to_string()))
                        }
                    },
                    Resolution::GlobalFunction(name) => {
                        // edge case: if the expression node isn't a call, it means we're trying to access a global function's member
                        if !callee.is_call() {
                            return Err(ResolutionError::CannotAccessMember(name));
                        }

                        let function = self.global_functions.get(&ResourceLocation::new(self.pack_name.to_string(), name.to_string())).unwrap();
                        let return_type_key = function.return_type();
                        let return_type_def = return_type_key.from(&self.types).as_struct_def();

                        if return_type_def.field(&member).is_some() || return_type_def.function(&member).is_some() {
                            Ok(Resolution::Field(return_type_key, member.to_string()))
                        } else {
                            Err(ResolutionError::MemberDoesNotExist(return_type_key, member.to_string()))
                        }
                    }
                    _ => Err(ResolutionError::CannotAccessMember(member.to_string())),
                }
            }
            _ => unreachable!()
        }
    } 
}

#[derive(Clone, Debug)]
pub enum Resolution {
    Variable(TypeKey),
    GlobalFunction(String),
    Field(TypeKey, String),
    Method(TypeKey, String),
    Type(TypeKey),
    Constructor(TypeKey),
}

#[derive(Clone, Debug)]
pub enum ResolutionError {
    UnresolvedIdentifier(String),
    AmbiguousIdentifier {
        name: String,
        candidates: Vec<Resolution>
    },
    MemberDoesNotExist(TypeKey, String),
    CannotCallExpression,
    CannotCallMember(String),
    CannotAccessMember(String),
}