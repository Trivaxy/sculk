#[derive(Debug, Clone)]
pub enum SculkType {
    None, // not the "none" in optional types, this refers to void/no type
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
}

impl PartialEq for SculkType {
    fn eq(&self, other: &Self) -> bool {
        use SculkType::*;

        match (self, other) {
            (Integer, Integer) => true,
            (Bool, Bool) => true,
            (None, None) => true,
            (Struct(def1), Struct(def2)) => def1.name == def2.name, // TODO: Namespacing
            _ => false
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
        Self { name, fields: Vec::new() }
    }

    pub fn add_field(&mut self, field: FieldDef) -> Result<(), FieldAddError>{
        if self.fields.iter().any(|f| f.name == field.name) {
            return Err(FieldAddError::DuplicateFields);
        }

        if let SculkType::Struct(struct_def) = &field.ty {
            let mut field_types = struct_def.fields.iter().map(|f| &f.ty).collect::<Vec<&SculkType>>();

            while !field_types.is_empty() {
                let field_type = field_types.pop().unwrap();
                
                if let SculkType::Struct(def) = field_type {
                    if def.name == self.name {
                        return Err(FieldAddError::SelfReference);
                    }

                    field_types.extend(def.fields.iter().map(|f| &f.ty));
                }
            }
        }

        Ok(())
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn fields(&self) -> &[FieldDef] {
        &self.fields
    }
}

#[derive(Debug, Clone)]
struct FieldDef {
    name: String,
    ty: SculkType,
}

#[derive(Debug, Clone, Copy)]
enum FieldAddError {
    DuplicateFields,
    SelfReference
}