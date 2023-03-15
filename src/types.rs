#[derive(Debug, Clone)]
pub enum SculkType {
    Integer,
    Bool,
    Selector,
    Struct(String),
}
