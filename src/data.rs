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
        write!(f, "{} {}", self.name, self.scoreboard.as_scoreboard())
    }
}

#[derive(Debug, Clone)]
pub struct ResourceLocation {
    pub namespace: String,
    pub path: String,
}

impl ResourceLocation {
    pub fn new(namespace: String, path: String) -> Self {
        Self { namespace, path }
    }

    pub fn as_scoreboard(&self) -> String {
        format!("{}_{}", self.namespace, self.path)
    }
}

impl Display for ResourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.namespace, self.path)
    }
}
