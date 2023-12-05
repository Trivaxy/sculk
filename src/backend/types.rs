use std::{cell::Ref, fmt::Display};

use super::{type_pool::{TypeKey, TypePool}, function::FunctionSignature};

/// The definition of a type in Sculk. These will end up inside a TypePool when the program is validated.
#[derive(Debug, Clone)]
pub enum SculkType {
    None,
    Unknown,
    Integer,
    Bool,
    Struct(StructDef),
}

impl SculkType {
    pub fn is_none(&self) -> bool {
        match self {
            SculkType::None => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            SculkType::Struct(_) => true,
            _ => false,
        }
    }

    pub fn as_struct_def(&self) -> &StructDef {
        match self {
            SculkType::Struct(def) => def,
            _ => panic!("type is not a struct"),
        }
    }

    pub fn as_struct_def_mut(&mut self) -> &mut StructDef {
        match self {
            SculkType::Struct(def) => def,
            _ => panic!("type is not a struct"),
        }
    }
}

impl PartialEq for SculkType {
    fn eq(&self, other: &Self) -> bool {
        use SculkType::*;

        match (self, other) {
            (Integer, Integer) => true,
            (Bool, Bool) => true,
            (Struct(def1), Struct(def2)) => def1.name == def2.name, // TODO: Namespacing
            _ => false,
        }
    }
}

impl Display for SculkType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SculkType::*;

        match self {
            None => write!(f, "none"),
            Unknown => write!(f, "unknown"),
            Integer => write!(f, "int"),
            Bool => write!(f, "bool"),
            Struct(def) => write!(f, "{}", def.name),
        }
    }
}

/// The definition of a struct in Sculk.
#[derive(Debug, Clone)]
pub struct StructDef {
    name: String,
    fields: Vec<FieldDef>,
    functions: Vec<FunctionSignature>,
    // Typically never None, but Option is needed since the constructor is added only after the struct is registered in a type pool
    constructor: Option<FunctionSignature>,
}

impl StructDef {
    pub fn new_empty(name: String) -> Self {
        Self {
            name,
            fields: Vec::new(),
            functions: Vec::new(),
            constructor: None, // this will be set upon registration
        }
    }

    pub fn add_field(&mut self, field: FieldDef) -> Result<(), ()> {
        if self.fields.iter().any(|f| f.name == field.name) {
            return Err(());
        }

        self.fields.push(field);

        Ok(())
    }

    pub fn add_function(&mut self, function: FunctionSignature) -> Result<(), ()> {
        if self.functions.iter().any(|f| f.name() == function.name()) {
            return Err(());
        }

        self.functions.push(function);

        Ok(())
    }

    pub fn self_referencing(&self, types: &TypePool) -> bool {
        let mut field_types = self
            .fields
            .iter()
            .map(|f| f.ty.clone())
            .collect::<Vec<TypeKey>>();

        while !field_types.is_empty() {
            let field_type = field_types.pop().unwrap().from(types);

            if let SculkType::Struct(def) = field_type {
                if def.name == self.name {
                    return true;
                }

                field_types.extend(def.fields.iter().map(|f| f.ty.clone()));
            }
        }

        false
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn fields(&self) -> &[FieldDef] {
        &self.fields
    }

    pub fn functions(&self) -> &[FunctionSignature] {
        &self.functions
    }

    pub fn field(&self, name: &str) -> Option<&FieldDef> {
        self.fields.iter().find(|f| f.name == name)
    }

    pub fn field_idx(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.name == name)
    }

    pub fn function(&self, name: &str) -> Option<&FunctionSignature> {
        self.functions.iter().find(|f| f.name() == name)
    }

    pub fn function_idx(&self, name: &str) -> Option<usize> {
        self.functions.iter().position(|f| f.name() == name)
    }

    pub fn constructor(&self) -> &FunctionSignature {
        self.constructor.as_ref().unwrap()
    }

    pub fn set_constructor(&mut self, constructor: FunctionSignature) {
        assert!(self.constructor.is_none());
        self.constructor = Some(constructor);
    }
}

/// The definition of a struct field in Sculk.
#[derive(Debug, Clone)]
pub struct FieldDef {
    name: String,
    ty: TypeKey,
}

impl FieldDef {
    pub fn new(name: String, ty: TypeKey) -> Self {
        Self { name, ty }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn field_type(&self) -> TypeKey {
        self.ty.clone()
    }
}
