use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct ScoreboardEntry {
    pub scoreboard: ResourceLocation,
    pub name: String,
}

impl ScoreboardEntry {
    pub fn new(scoreboard: ResourceLocation, name: String) -> Self {
        Self { scoreboard, name }
    }
}

impl Display for ScoreboardEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{} {}", self.name, self.scoreboard)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResourceLocation {
    pub namespace: String,
    pub path: String,
    pub separator: char,
}

impl ResourceLocation {
    pub fn new(namespace: String, path: String) -> Self {
        Self {
            namespace,
            path,
            separator: ':',
        }
    }

    pub fn with_separator(&self, separator: char) -> Self {
        Self {
            namespace: self.namespace.clone(),
            path: self.path.clone(),
            separator,
        }
    }
}

impl Display for ResourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.namespace, self.separator, self.path)
    }
}

pub struct ScoreboardSlot {
    pub objective: String,
    pub entry: String,
}

impl ScoreboardSlot {
    pub fn new(objective: String, entry: String) -> Self {
        Self { objective, entry }
    }
}

impl Display for ScoreboardSlot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.entry, self.objective)
    }
}