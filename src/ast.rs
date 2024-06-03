use crate::types::Types;

pub enum RelationOp {
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    NotEq,
}

pub enum ASTNode {
    Program {
        name: String,
        decls: Vec<Box<ASTNode>>,
        body: Vec<Box<ASTNode>>,
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
        params: Vec<ASTNode>,
        decls: Vec<Box<ASTNode>>,
        body: Vec<Box<ASTNode>>,
    },
    AssignStmt {
        //dest: Box<DestNode>,
        dest: Box<ASTNode>,
        expr: Box<ASTNode>,
    },
    IfStmt {
        cond: Box<ASTNode>,
        then_body: Vec<Box<ASTNode>>,
        else_body: Vec<Box<ASTNode>>,
    },
    LoopStmt {
        init: Box<ASTNode>,
        cond: Box<ASTNode>,
        body: Vec<Box<ASTNode>>,
    },
    ReturnStmt {
        expr: Box<ASTNode>,
    },
    AndOp {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    OrOp {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    NotOp {
        operand: Box<ASTNode>,
    },
    AddOp {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    SubOp {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    MulOp {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    DivOp {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    Relation {
        op: RelationOp,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    NegateOp {
        operand: Box<ASTNode>,
    },
    SubscriptOp {
        //array: Box<Var>,
        array: Box<ASTNode>,
        index: Box<ASTNode>,
    },
    ProcCall {
        //proc: Box<Var>,
        proc: Box<ASTNode>,
        args: Vec<Box<ASTNode>>,
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
