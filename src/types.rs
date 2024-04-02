use std::vec::Vec;

#[derive(Clone, PartialEq)]
pub enum Types {
    Unknown,
    None,
    Int,
    Float,
    String,
    Bool,
    Func(Box<Types>, Vec<Types>),
    Array(u32, Box<Types>),
}
