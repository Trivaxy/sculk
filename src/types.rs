#[derive(Debug, Clone, PartialEq)]
pub enum SculkType {
    None,
    Unknown,
    Integer,
    Bool,
    Selector,
    Struct(String),
}

impl SculkType {
    pub fn is_none(&self) -> bool {
        match self {
            SculkType::None => true,
            _ => false,
        }
    }
}
