use std::sync::atomic::{AtomicI32, Ordering};

use crate::{data::ResourceLocation, type_pool::TypeKey, types::SculkType};

use super::codegen::Action;

pub struct FunctionSignature {
    name: String,
    args: Vec<ParamDef>,
    return_type: TypeKey,
}

impl FunctionSignature {
    pub fn new(name: String, args: Vec<ParamDef>, return_type: TypeKey) -> Self {
        Self {
            name,
            args,
            return_type,
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
}

pub struct Function {
    name: String,
    scoreboard: ResourceLocation,
    args: Vec<ParamDef>,
    return_ty: TypeKey,
    pub(super) actions: Vec<Action>,
    anonymous: bool,
}

static ANONYMOUS_FUNC_COUNT: AtomicI32 = AtomicI32::new(0);

impl Function {
    pub fn new_empty(
        name: String,
        scoreboard: ResourceLocation,
        args: Vec<ParamDef>,
        return_ty: TypeKey,
    ) -> Self {
        Function {
            name,
            scoreboard,
            args,
            return_ty,
            actions: Vec::new(),
            anonymous: false,
        }
    }

    pub fn new_empty_mapped_args(
        name: String,
        scoreboard: ResourceLocation,
        args: Vec<ParamDef>,
        return_ty: TypeKey,
    ) -> Self {
        Function {
            name,
            scoreboard,
            args,
            return_ty,
            actions: Vec::new(),
            anonymous: false,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn is_anonymous(&self) -> bool {
        self.anonymous
    }

    pub fn scoreboard(&self) -> &ResourceLocation {
        &self.scoreboard
    }

    pub fn args(&self) -> &[ParamDef] {
        &self.args
    }

    pub fn get_arg_by_idx(&self, idx: usize) -> &ParamDef {
        &self.args[idx]
    }

    pub fn make_anonymous_child(&self) -> Self {
        let mut func = Function::new_empty_mapped_args(
            format!("anon_{}", ANONYMOUS_FUNC_COUNT.load(Ordering::Relaxed)),
            self.scoreboard.clone(),
            self.args.clone(),
            self.return_ty.clone(),
        );

        ANONYMOUS_FUNC_COUNT.fetch_add(1, Ordering::SeqCst);
        func.anonymous = true;
        func
    }
}

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
