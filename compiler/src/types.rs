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
            Types::Unknown => write!(f, "unknown"),
            Types::None => write!(f, "none"),
            Types::Int => write!(f, "integer"),
            Types::Float => write!(f, "float"),
            Types::String => write!(f, "string"),
            Types::Bool => write!(f, "bool"),
            Types::Proc(return_type, param_types) => {
                let mut params = String::new();
                for param_type in param_types.iter() {
                    params.push_str(format!("{}, ", param_type).as_str());
                }
                params.pop();
                params.pop();
                write!(f, "{}({})", *return_type, params)
            }
            Types::Array(size, base_type) => write!(f, "{}[{}]", *base_type, size),
       }
    }
}
