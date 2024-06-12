#[derive(Debug)]
pub struct TerminalError;

#[derive(Clone, PartialEq, Debug)]
pub enum CompilerError {
    Error {
        line: u32,
        msg: String,
    },
    Warning {
        line: u32,
        msg: String,
    },
}
