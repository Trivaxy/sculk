use std::{collections::HashMap, ops::Index};

use crate::{
    data::ResourceLocation,
    parser::{ParserNode, ParserNodeKind},
};

use super::{
    function::FunctionSignature,
    type_pool::{TypeKey, TypePool},
    validate::ScopeStack,
};

/// A helper type for resolving expressions.
/// This is used to figure out what a parser node is referring to, such as a variable, function, field, etc.
pub struct Resolver<'a> {
    pack_name: &'a str,
    global_functions: &'a HashMap<ResourceLocation, FunctionSignature>,
    types: &'a TypePool,
    scope_stack: &'a ScopeStack,
}

impl<'a> Resolver<'a> {
    pub fn new(
        pack_name: &'a str,
        global_functions: &'a HashMap<ResourceLocation, FunctionSignature>,
        types: &'a TypePool,
        scope_stack: &'a ScopeStack,
    ) -> Self {
        Self {
            pack_name,
            global_functions,
            types,
            scope_stack,
        }
    }

    pub fn resolve(&mut self, callee: &ParserNode) -> Result<Resolution, ResolutionError> {
        let mut resolution = Resolution(Vec::new());
        self.resolve_inner(&mut resolution, callee)?;
        resolution.0.reverse();
        Ok(resolution)
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
    pub fn resolve_inner(
        &mut self,
        resolution: &mut Resolution,
        callee: &ParserNode,
    ) -> Result<(), ResolutionError> {
        match callee.kind() {
            ParserNodeKind::Identifier(name) => {
                let (variable, global_func, ty) = (
                    self.scope_stack.find_variable_type(name),
                    self.global_functions.get(&ResourceLocation::new(
                        self.pack_name.to_string(),
                        name.to_string(),
                    )),
                    self.types.get_type_key(name),
                );

                resolution.0.push(match (variable, global_func, ty) {
                    (Some(type_key), None, None) => ResolvedPart::Variable(type_key, name.clone()),
                    (None, Some(_), None) => ResolvedPart::GlobalFunction(name.clone()),
                    (None, None, Some(type_key)) => ResolvedPart::Type(type_key),
                    (None, None, None) => {
                        return Err(ResolutionError::UnresolvedIdentifier(name.clone()))
                    }
                    _ => {
                        return Err(ResolutionError::AmbiguousIdentifier {
                            name: name.clone(),
                            candidates: vec![
                                variable.map(|ty| ResolvedPart::Variable(ty, name.clone())),
                                global_func.map(|_| ResolvedPart::GlobalFunction(name.clone())),
                                ty.map(ResolvedPart::Type),
                            ]
                            .into_iter()
                            .filter_map(|x| x)
                            .collect(),
                        })
                    }
                });

                Ok(())
            }
            ParserNodeKind::FunctionCall { expr, .. } => {
                self.resolve_inner(resolution, expr)?;

                let last = resolution.0.pop().unwrap();
                let mut fix = false;

                resolution.0.push(match last {
                    ResolvedPart::GlobalFunction(name) => {
                        ResolvedPart::GlobalFunction(name.to_string())
                    }
                    ResolvedPart::Field(ty, name) => {
                        let struct_def = ty.from(&self.types).as_struct_def();

                        if struct_def.function(&name).is_some() {
                            fix = true;
                            ResolvedPart::Method(ty, name.to_string())
                        } else {
                            return Err(ResolutionError::CannotCallMember(name.to_string()));
                        }
                    }
                    ResolvedPart::Type(ty) => ResolvedPart::Constructor(ty),
                    _ => return Err(ResolutionError::CannotCallExpression),
                });

                // This is to stop the resolver from giving you something like "Variable -> Field -> Method"
                // When you do something like "a.b()", which logically should just be Variable -> Method
                if fix {
                    resolution.0.remove(resolution.0.len() - 2);
                }

                Ok(())
            }
            ParserNodeKind::MemberAccess { expr, member } => {
                let member = member.as_identifier();
                self.resolve_inner(resolution, expr)?;

                match resolution.0.last().unwrap() {
                    ResolvedPart::Variable(ty, _) => {
                        let struct_def = ty.from(&self.types).as_struct_def();

                        resolution.0.push(if struct_def.field(&member).is_some() {
                            ResolvedPart::Field(*ty, member.to_string())
                        } else if struct_def.function(&member).is_some() {
                            ResolvedPart::Method(*ty, member.to_string())
                        } else {
                            return Err(ResolutionError::MemberDoesNotExist(
                                *ty,
                                member.to_string(),
                            ));
                        });

                        Ok(())
                    }
                    ResolvedPart::Field(ty, name) => {
                        let field_type_key = ty
                            .from(&self.types)
                            .as_struct_def()
                            .field(&name)
                            .unwrap()
                            .field_type();
                        let field_type_def = field_type_key.from(&self.types).as_struct_def();

                        if field_type_def.field(&member).is_some()
                            || field_type_def.function(&member).is_some()
                        {
                            resolution
                                .0
                                .push(ResolvedPart::Field(field_type_key, member.to_string()));
                            Ok(())
                        } else {
                            return Err(ResolutionError::MemberDoesNotExist(
                                field_type_key,
                                member.to_string(),
                            ));
                        }
                    }
                    ResolvedPart::Method(ty, name) => {
                        let method_type_key = ty
                            .from(&self.types)
                            .as_struct_def()
                            .function(&name)
                            .unwrap()
                            .return_type();

                        let method_type_def = method_type_key.from(&self.types).as_struct_def();

                        if method_type_def.field(&member).is_some()
                            || method_type_def.function(&member).is_some()
                        {
                            resolution
                                .0
                                .push(ResolvedPart::Field(method_type_key, member.to_string()));
                            Ok(())
                        } else {
                            return Err(ResolutionError::MemberDoesNotExist(
                                *ty,
                                member.to_string(),
                            ));
                        }
                    }
                    ResolvedPart::GlobalFunction(name) => {
                        // edge case: if the expression node isn't a call, it means we're trying to access a global function's member
                        if !callee.is_call() {
                            return Err(ResolutionError::CannotAccessMember(name.to_string()));
                        }

                        let function = self
                            .global_functions
                            .get(&ResourceLocation::new(
                                self.pack_name.to_string(),
                                name.to_string(),
                            ))
                            .unwrap();
                        let return_type_key = function.return_type();
                        let return_type_def = return_type_key.from(&self.types).as_struct_def();

                        if return_type_def.field(&member).is_some()
                            || return_type_def.function(&member).is_some()
                        {
                            resolution
                                .0
                                .push(ResolvedPart::Field(return_type_key, member.to_string()));
                            Ok(())
                        } else {
                            Err(ResolutionError::MemberDoesNotExist(
                                return_type_key,
                                member.to_string(),
                            ))
                        }
                    }
                    _ => Err(ResolutionError::CannotAccessMember(member.to_string())),
                }
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ResolvedPart {
    Variable(TypeKey, String),
    GlobalFunction(String),
    Field(TypeKey, String),
    Method(TypeKey, String),
    Type(TypeKey),
    Constructor(TypeKey),
}

pub struct Resolution(Vec<ResolvedPart>);

impl Resolution {
    pub fn find_assignable_type(&self, pool: &TypePool) -> Option<TypeKey> {
        match self.0.last().unwrap() {
            ResolvedPart::Variable(ty, _) => Some(*ty),
            ResolvedPart::Field(ty, name) => Some(
                ty.from(pool)
                    .as_struct_def()
                    .field(name)
                    .unwrap()
                    .field_type(),
            ),
            _ => None,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &ResolvedPart> {
        self.0.iter()
    }

    pub fn last(&self) -> &ResolvedPart {
        self.0.last().unwrap()
    }
}

#[derive(Clone, Debug)]
pub enum ResolutionError {
    UnresolvedIdentifier(String),
    AmbiguousIdentifier {
        name: String,
        candidates: Vec<ResolvedPart>,
    },
    MemberDoesNotExist(TypeKey, String),
    CannotCallExpression,
    CannotCallMember(String),
    CannotAccessMember(String),
}
