use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt::Display,
    rc::Rc,
};

use crate::backend::types::SculkType;

type InternalTypeStorage = Rc<RefCell<Vec<SculkType>>>;

/// The collection of all types in a Sculk program, including built-in types.
/// This is constructed during the validation phase and used throughout compilation.
///
/// Accessing a TypePool is done through a TypeKey.
#[derive(Debug)]
pub struct TypePool {
    type_map: HashMap<String, usize>,
    pool: InternalTypeStorage,
}

impl TypePool {
    fn new() -> Self {
        Self {
            type_map: HashMap::new(),
            pool: InternalTypeStorage::new(RefCell::new(Vec::new())),
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

    pub fn insert(&mut self, name: String, ty: SculkType) {
        let idx_in_pool = self.pool.borrow_mut().len();

        self.pool.borrow_mut().push(ty);
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
        self.type_map
            .get(name)
            .map(|id| TypeKey::new(self.pool.clone(), *id))
    }

    pub fn has_type(&self, name: &str) -> bool {
        self.type_map.contains_key(name)
    }

    pub fn iter(&self) -> TypePoolIterator {
        TypePoolIterator {
            pool: self.pool.clone(),
            idx: 0,
        }
    }
}

pub struct TypePoolIterator {
    pool: InternalTypeStorage,
    idx: usize,
}

impl Iterator for TypePoolIterator {
    type Item = TypeKey;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.pool.borrow().len() {
            return None;
        }

        let ty = TypeKey::new(self.pool.clone(), self.idx);

        self.idx += 1;

        Some(ty)
    }
}

/// Represents a handle to a Sculk type that lives in a TypePool.
#[derive(Debug, Clone)]
pub struct TypeKey {
    pool: InternalTypeStorage,
    id: usize,
}

impl TypeKey {
    pub(self) fn new(pool: InternalTypeStorage, id: usize) -> Self {
        Self { pool, id }
    }

    pub fn get(&self) -> Ref<SculkType> {
        let pool = self.pool.borrow();
        Ref::map(pool, |pool| &pool[self.id])
    }

    pub fn get_mut(&self) -> RefMut<SculkType> {
        let pool = self.pool.borrow_mut();
        RefMut::map(pool, |pool| &mut pool[self.id])
    }
}

impl PartialEq for TypeKey {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && Rc::ptr_eq(&self.pool, &other.pool)
    }
}

impl Display for TypeKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}
