use std::vec::Vec;

#[derive(Clone, PartialEq)]
pub enum Types {
    Unknown,
    None,
    Int,
    Float,
    String,
    Bool,
    Proc(Box<Types>, Vec<Types>),
    Array(u32, Box<Types>),
}
