use std::fmt;
use std::vec::Vec;

#[derive(Clone, PartialEq, Debug)]
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
            Types::Unknown => write!(f, "Unknown"),
            Types::None => write!(f, "None"),
            Types::Int => write!(f, "Int"),
            Types::Float => write!(f, "Float"),
            Types::String => write!(f, "String"),
            Types::Bool => write!(f, "Bool"),
            Types::Proc(return_type, param_types) => {
                write!(f, "Proc(")?;
                write!(f, "{}, {{", *return_type)?;
                for param_type in param_types.iter() {
                    write!(f, "{}, ", param_type)?;
                }
                write!(f, "}})")
            }
            Types::Array(size, base_type) => write!(f, "Array({}, {})", size, *base_type),
       }
    }
}
