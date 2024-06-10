use crate::types::Types;

use std::fmt;
use std::fmt::Write;

#[derive(Clone, PartialEq, Debug)]
pub enum RelationOp {
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    NotEq,
}

impl fmt::Display for RelationOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RelationOp::LT => write!(f, "<"),
            RelationOp::LTE => write!(f, "<="),
            RelationOp::GT => write!(f, ">"),
            RelationOp::GTE => write!(f, ">="),
            RelationOp::Eq => write!(f, "=="),
            RelationOp::NotEq => write!(f, "!="),
       }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Ast {
    Program {
        name: String,
        decls: Vec<Box<Ast>>,
        body: Vec<Box<Ast>>,
    },
    VarDecl {
        is_global: bool,
        name: String,
        ty: Types,
    },
    ProcDecl {
        is_global: bool,
        name: String,
        ty: Types,
        params: Vec<Box<Ast>>,
        decls: Vec<Box<Ast>>,
        body: Vec<Box<Ast>>,
    },
    AssignStmt {
        dest: Box<Ast>,
        expr: Box<Ast>,
    },
    IfStmt {
        cond: Box<Ast>,
        then_body: Vec<Box<Ast>>,
        else_body: Vec<Box<Ast>>,
    },
    LoopStmt {
        init: Box<Ast>,
        cond: Box<Ast>,
        body: Vec<Box<Ast>>,
    },
    ReturnStmt {
        expr: Box<Ast>,
    },
    AndOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    OrOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    NotOp {
        operand: Box<Ast>,
    },
    AddOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    SubOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    MulOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    DivOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    Relation {
        op: RelationOp,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    NegateOp {
        operand: Box<Ast>,
    },
    SubscriptOp {
        array: Box<Ast>,
        index: Box<Ast>,
    },
    ProcCall {
        proc: Box<Ast>,
        args: Vec<Box<Ast>>,
    },
    IntLiteral {
        value: u32,
    },
    FloatLiteral {
        value: f32,
    },
    BoolLiteral {
        value: bool,
    },
    StringLiteral {
        value: String,
    },
    Var {
        id: String,
    },
    FloatToInt {
        operand: Box<Ast>
    },
    IntToFloat {
        operand: Box<Ast>
    },
    BoolToInt {
        operand: Box<Ast>
    },
    IntToBool {
        operand: Box<Ast>
    },
    FloatArrayToIntArray {
        operand: Box<Ast>
    },
    IntArrayToFloatArray {
        operand: Box<Ast>
    },
    BoolArrayToIntArray {
        operand: Box<Ast>
    },
    IntArrayToBoolArray {
        operand: Box<Ast>
    },
}

pub trait AstVisitor<T> {
    fn visit_ast(&mut self, ast: &mut Ast) -> T;
}

impl Ast {
    pub fn accept<T>(&mut self, v: &mut impl AstVisitor<T>) -> T {
        v.visit_ast(self)
    }

    fn fmt_string(&self, buf: &mut String, c: &str, width: usize) -> fmt::Result {
        match self {
            Ast::Program { name, decls, body } => {
                write!(buf, "{}program:\n", c.repeat(width))?;
                write!(buf, "{}name: {name}\n", c.repeat(width+1))?;

                write!(buf, "{}decls:\n", c.repeat(width+1))?;
                for decl in decls.iter() {
                    decl.fmt_string(buf, c, width+2)?;
                }

                write!(buf, "{}stmts:\n", c.repeat(width+1))?;
                for stmt in body.iter() {
                    stmt.fmt_string(buf, c, width+2)?;
                }
                Ok(())
            },
            Ast::VarDecl { is_global, name, ty } => {
                write!(buf, "{}{name}: {{ty: {ty}, is_global: {is_global}}}\n", c.repeat(width))?;
                Ok(())
            },
            Ast::ProcDecl { is_global, name, ty, params, decls, body } => {
                write!(buf, "{}{name}:\n", c.repeat(width))?;
                write!(buf, "{}ty: {ty}\n", c.repeat(width+1))?;
                write!(buf, "{}is_global: {is_global}\n", c.repeat(width+1))?;

                write!(buf, "{}params:\n", c.repeat(width+1))?;
                for param in params.iter() {
                    param.fmt_string(buf, c, width+2)?;
                }

                write!(buf, "{}decls:\n", c.repeat(width+1))?;
                for decl in decls.iter() {
                    decl.fmt_string(buf, c, width+2)?;
                }

                write!(buf, "{}stmts:\n", c.repeat(width+1))?;
                for stmt in body.iter() {
                    stmt.fmt_string(buf, c, width+2)?;
                }
                Ok(())
            },
            Ast::AssignStmt { dest, expr } => {
                write!(buf, "{}assign:\n", c.repeat(width))?;
                write!(buf, "{}dest:\n", c.repeat(width+1))?;
                dest.fmt_string(buf, c, width+2)?;
                write!(buf, "{}expr:\n", c.repeat(width+1))?;
                expr.fmt_string(buf, c, width+2)?;
                Ok(())
            },
            Ast::IfStmt { cond, then_body, else_body } => {
                write!(buf, "{}if:\n", c.repeat(width))?;

                write!(buf, "{}cond:\n", c.repeat(width+1))?;
                cond.fmt_string(buf, c, width+2)?;

                write!(buf, "{}then:\n", c.repeat(width+1))?;
                for stmt in then_body.iter() {
                    stmt.fmt_string(buf, c, width+2)?;
                }

                write!(buf, "{}else:\n", c.repeat(width+1))?;
                for stmt in else_body.iter() {
                    stmt.fmt_string(buf, c, width+2)?;
                }
                Ok(())
            },
            Ast::LoopStmt { init, cond, body } => {
                write!(buf, "{}loop:\n", c.repeat(width))?;

                write!(buf, "{}init:\n", c.repeat(width+1))?;
                init.fmt_string(buf, c, width+2)?;

                write!(buf, "{}cond:\n", c.repeat(width+1))?;
                cond.fmt_string(buf, c, width+2)?;

                write!(buf, "{}body:\n", c.repeat(width+1))?;
                for stmt in body.iter() {
                    stmt.fmt_string(buf, c, width+2)?;
                }
                Ok(())
            },
            Ast::ReturnStmt { expr } => {
                write!(buf, "{}return:\n", c.repeat(width))?;

                write!(buf, "{}expr:\n", c.repeat(width+1))?;
                expr.fmt_string(buf, c, width+2)?;
                Ok(())
            },
            Ast::AndOp { lhs, rhs } => {
                write!(buf, "{}and:\n", c.repeat(width))?;
                lhs.fmt_string(buf, c, width+1)?;
                rhs.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::OrOp { lhs, rhs } => {
                write!(buf, "{}or:\n", c.repeat(width))?;
                lhs.fmt_string(buf, c, width+1)?;
                rhs.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::NotOp { operand } => {
                write!(buf, "{}not:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::AddOp { lhs, rhs } => {
                write!(buf, "{}add:\n", c.repeat(width))?;
                lhs.fmt_string(buf, c, width+1)?;
                rhs.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::SubOp { lhs, rhs } => {
                write!(buf, "{}sub:\n", c.repeat(width))?;
                lhs.fmt_string(buf, c, width+1)?;
                rhs.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::MulOp { lhs, rhs } => {
                write!(buf, "{}mul:\n", c.repeat(width))?;
                lhs.fmt_string(buf, c, width+1)?;
                rhs.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::DivOp { lhs, rhs } => {
                write!(buf, "{}div:\n", c.repeat(width))?;
                lhs.fmt_string(buf, c, width+1)?;
                rhs.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::Relation { op, lhs, rhs } => {
                write!(buf, "{}{op}:\n", c.repeat(width))?;
                lhs.fmt_string(buf, c, width+1)?;
                rhs.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::NegateOp { operand } => {
                write!(buf, "{}neg:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::SubscriptOp { array, index } => {
                write!(buf, "{}subscript:\n", c.repeat(width))?;
                write!(buf, "{}base:\n", c.repeat(width+1))?;
                array.fmt_string(buf, c, width+2)?;
                write!(buf, "{}index:\n", c.repeat(width+1))?;
                index.fmt_string(buf, c, width+2)?;
                Ok(())
            },
            Ast::ProcCall { proc, args } => {
                write!(buf, "{}proc_call:\n", c.repeat(width))?;
                write!(buf, "{}proc:\n", c.repeat(width+1))?;
                proc.fmt_string(buf, c, width+2)?;
                write!(buf, "{}args:\n", c.repeat(width+1))?;
                for arg in args.iter() {
                    arg.fmt_string(buf, c, width+2)?;
                }
                Ok(())
            },
            Ast::IntLiteral { value } => {
                write!(buf, "{}int: {value}\n", c.repeat(width))?;
                Ok(())
            },
            Ast::FloatLiteral { value } => {
                write!(buf, "{}float: {value}\n", c.repeat(width))?;
                Ok(())
            },
            Ast::BoolLiteral { value } => {
                write!(buf, "{}bool: {value}\n", c.repeat(width))?;
                Ok(())
            },
            Ast::StringLiteral { value } => {
                write!(buf, "{}string: {value}\n", c.repeat(width))?;
                Ok(())
            },
            Ast::Var { id } => {
                write!(buf, "{}var: {id}\n", c.repeat(width))?;
                Ok(())
            },
            Ast::FloatToInt { operand } => {
                write!(buf, "{}float_to_int:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::IntToFloat { operand } => {
                write!(buf, "{}int_to_float:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::BoolToInt { operand } => {
                write!(buf, "{}bool_to_int:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::IntToBool { operand } => {
                write!(buf, "{}int_to_bool:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::FloatArrayToIntArray { operand } => {
                write!(buf, "{}float_array_to_int_array:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::IntArrayToFloatArray { operand } => {
                write!(buf, "{}int_array_to_float_array:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::BoolArrayToIntArray { operand } => {
                write!(buf, "{}bool_array_to_int_array:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
            Ast::IntArrayToBoolArray { operand } => {
                write!(buf, "{}int_array_to_bool_array:\n", c.repeat(width))?;
                operand.fmt_string(buf, c, width+1)?;
                Ok(())
            },
        }
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::new();
        self.fmt_string(&mut buf, "  ", 0)?;
        write!(f, "{buf}")
    }
}
