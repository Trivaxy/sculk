use super::type_pool::TypeKey;

/// Represents a Sculk function's signature.
/// A signature consists of the function's name, its argument parameters, and its return type.
/// Signatures are used to call functions properly and validate them.
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    name: String,
    args: Vec<ParamDef>,
    return_type: TypeKey,
    is_static: bool,
}

impl FunctionSignature {
    pub fn new(name: String, args: Vec<ParamDef>, return_type: TypeKey, is_static: bool) -> Self {
        Self {
            name,
            args,
            return_type,
            is_static,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn return_type(&self) -> TypeKey {
        self.return_type.clone()
    }

    pub fn params(&self) -> &[ParamDef] {
        &self.args
    }

    pub fn is_static(&self) -> bool {
        self.is_static
    }
}

/// The definition of a function parameter.
#[derive(Debug, Clone)]
pub struct ParamDef {
    name: String,
    ty: TypeKey,
}

impl ParamDef {
    pub fn new(name: String, ty: TypeKey) -> Self {
        Self { name, ty }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn param_type(&self) -> TypeKey {
        self.ty.clone()
    }
}
