use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct ScoreboardVariable {
    pub scoreboard: ResourceLocation,
    pub name: String,
}

impl ScoreboardVariable {
    pub fn new(scoreboard: ResourceLocation, name: String) -> Self {
        Self { scoreboard, name }
    }
}

impl Display for ScoreboardVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.name, self.scoreboard)
    }
}

#[derive(Debug, Clone)]
pub struct ResourceLocation {
    pub namespace: String,
    pub path: String,
    pub separator: char,
}

impl ResourceLocation {
    pub fn new(namespace: String, path: String) -> Self {
        Self { namespace, path, separator: ':' }
    }

    pub fn scoreboard(namespace: String, path: String) -> Self {
        Self { namespace, path, separator: '.' }
    }
}

impl Display for ResourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.namespace, self.separator, self.path)
    }
}
