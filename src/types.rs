use std::fmt;
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

impl fmt::Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Types::Unknown => write!(f, "<Types::Unknown>"),
            Types::None => write!(f, "<Types::None>"),
            Types::Int => write!(f, "<Types::Int>"),
            Types::Float => write!(f, "<Types::Float>"),
            Types::String => write!(f, "<Types::String>"),
            Types::Bool => write!(f, "<Types::Bool>"),
            Types::Proc(return_type, param_types) => {
                let _ = write!(f, "<Types::Proc(");
                let _ = write!(f, "{}, {{", *return_type);
                for param_type in param_types.iter() {
                    let _ = write!(f, "{}, ", param_type);
                }
                write!(f, "}})>")
            }
            Types::Array(size, base_type) => write!(f, "<Types::Array({}, {})>", size, *base_type),
       }
    }
}
