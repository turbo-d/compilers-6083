use crate::types::Types;

#[derive(PartialEq)]
pub enum RelationOp {
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    NotEq,
}

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
        //params: Vec<VarDecl>,
        params: Vec<Ast>,
        decls: Vec<Box<Ast>>,
        body: Vec<Box<Ast>>,
    },
    AssignStmt {
        //dest: Box<DestNode>,
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
        //array: Box<Var>,
        array: Box<Ast>,
        index: Box<Ast>,
    },
    ProcCall {
        //proc: Box<Var>,
        proc: Box<Ast>,
        args: Vec<Box<Ast>>,
    },
    IntLiteral {
        value: i32,
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
}

pub trait AstVisitor<T> {
    fn visit_ast(&mut self, ast: &Ast) -> T;
}

impl Ast {
    pub fn accept<T>(&self, v: &mut impl AstVisitor<T>) -> T {
        v.visit_ast(self)
    }
}
