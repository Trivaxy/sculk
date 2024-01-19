use std::{collections::HashMap, fmt::Display};

use crate::backend::types::SculkType;

use super::function::{FunctionSignature, ParamDef};

/// The collection of all types in a Sculk program, including built-in types.
/// This is constructed during the validation phase and used throughout compilation.
///
/// Accessing a TypePool is done through a TypeKey.
#[derive(Debug)]
pub struct TypePool {
    type_map: HashMap<String, usize>,
    types: Vec<SculkType>,
}

impl TypePool {
    fn new() -> Self {
        Self {
            type_map: HashMap::new(),
            types: Vec::new(),
        }
    }

    pub fn new_with_primitives() -> Self {
        let mut type_pool = Self::new();

        type_pool.insert("none".to_string(), SculkType::None);
        type_pool.insert("unknown".to_string(), SculkType::Unknown);
        type_pool.insert("int".to_string(), SculkType::Integer);
        type_pool.insert("bool".to_string(), SculkType::Bool);

        type_pool
    }

    pub fn insert(&mut self, name: String, mut ty: SculkType) {
        let idx_in_pool = self.types.len();

        if let SculkType::Struct(ref mut def) = ty {
            def.set_constructor(FunctionSignature::new(
                ".ctor".to_string(),
                def.fields()
                    .map(|f| ParamDef::new(f.name().to_string(), f.field_type()))
                    .collect(),
                TypeKey(idx_in_pool),
                true,
            ));

            def.finalize(self);
        }

        self.types.push(ty);
        self.type_map.insert(name, idx_in_pool);
    }

    pub fn int(&self) -> TypeKey {
        self.get_type_key("int").unwrap()
    }

    pub fn bool(&self) -> TypeKey {
        self.get_type_key("bool").unwrap()
    }

    pub fn none(&self) -> TypeKey {
        self.get_type_key("none").unwrap()
    }

    pub fn unknown(&self) -> TypeKey {
        self.get_type_key("unknown").unwrap()
    }

    pub fn get_type_key(&self, name: &str) -> Option<TypeKey> {
        self.type_map.get(name).map(|id| TypeKey(*id))
    }

    pub fn has_type(&self, name: &str) -> bool {
        self.type_map.contains_key(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = &SculkType> {
        self.types.iter()
    }
}

/// Represents a handle to a Sculk type that lives in a TypePool.
#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub struct TypeKey(usize);

impl TypeKey {
    pub fn from<'a>(&self, pool: &'a TypePool) -> &'a SculkType {
        &pool.types[self.0]
    }

    pub fn from_mut<'a>(&self, pool: &'a mut TypePool) -> &'a mut SculkType {
        &mut pool.types[self.0]
    }
}
