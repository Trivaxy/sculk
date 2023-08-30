use std::cell::Ref;

use crate::type_pool::TypeKey;

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

#[derive(Debug, Clone)]
pub struct StructDef {
    name: String,
    fields: Vec<FieldDef>,
}

impl StructDef {
    pub fn new_empty(name: String) -> Self {
        Self {
            name,
            fields: Vec::new(),
        }
    }

    pub fn add_field(&mut self, field: FieldDef) -> Result<(), ()> {
        if self.fields.iter().any(|f| f.name == field.name) {
            return Err(());
        }

        self.fields.push(field);

        Ok(())
    }

    pub fn self_referencing(&self) -> bool {
        let mut field_types = self
            .fields
            .iter()
            .map(|f| f.ty.clone())
            .collect::<Vec<TypeKey>>();

        while !field_types.is_empty() {
            let field_type_key = field_types.pop().unwrap();
            let field_type = &*field_type_key.get();
            
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
}

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
