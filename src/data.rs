use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResourceLocation {
    pub namespace: String,
    pub path: String,
}

impl ResourceLocation {
    pub fn new(namespace: String, path: String) -> Self {
        Self {
            namespace,
            path,
        }
    }
}

impl Display for ResourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.namespace, self.path)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Objective(pub String);

impl Objective {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl Display for Objective {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct ScoreboardSlot {
    pub objective: Objective,
    pub entry: String,
}

impl ScoreboardSlot {
    pub fn new(objective: Objective, entry: String) -> Self {
        Self { objective, entry }
    }
}

impl Display for ScoreboardSlot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.entry, self.objective)
    }
}

pub enum ScoreboardOperationType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Set, // add the others when i feel like it
}

impl Display for ScoreboardOperationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScoreboardOperationType::Add => write!(f, "+="),
            ScoreboardOperationType::Subtract => write!(f, "-="),
            ScoreboardOperationType::Multiply => write!(f, "*="),
            ScoreboardOperationType::Divide => write!(f, "/="),
            ScoreboardOperationType::Modulo => write!(f, "%="),
            ScoreboardOperationType::Set => write!(f, "="),
        }
    }
}