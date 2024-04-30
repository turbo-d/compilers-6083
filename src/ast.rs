use crate::types::Types;

use std::vec::Vec;

pub trait ASTVisitor<T> {
    fn visit_program(&self, prgm: &Program) -> T;
    fn visit_var_decl(&self, decl: &VarDecl) -> T;
    fn visit_proc_decl(&self, decl: &ProcDecl) -> T;
    fn visit_assign_stmt(&self, stmt: &AssignStmt) -> T;
    fn visit_if_stmt(&self, stmt: &IfStmt) -> T;
    fn visit_loop_stmt(&self, stmt: &LoopStmt) -> T;
    fn visit_return_stmt(&self, stmt: &ReturnStmt) -> T;
    fn visit_and_op(&self, op: &AndOp) -> T;
    fn visit_or_op(&self, op: &OrOp) -> T;
    fn visit_not_op(&self, op: &NotOp) -> T;
    fn visit_add_op(&self, op: &AddOp) -> T;
    fn visit_sub_op(&self, op: &SubOp) -> T;
    fn visit_mul_op(&self, op: &MulOp) -> T;
    fn visit_div_op(&self, op: &DivOp) -> T;
    fn visit_relation(&self, rel: &Relation) -> T;
    fn visit_negate_op(&self, op: &NegateOp) -> T;
    fn visit_subscript_op(&self, op: &SubscriptOp) -> T;
    fn visit_proc_call(&self, pc: &ProcCall) -> T;
    fn visit_int_literal(&self, lit: &IntLiteral) -> T;
    fn visit_float_literal(&self, lit: &FloatLiteral) -> T;
    fn visit_string_literal(&self, lit: &StringLiteral) -> T;
    fn visit_bool_literal(&self, lit: &BoolLiteral) -> T;
    fn visit_var(&self, var: &Var) -> T;
}

pub trait ASTNode {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T;
}

pub struct Program {
    name: String,
    vars: Vec<VarDecl>,
    procs: Vec<ProcDecl>,
    body: Vec<Box<dyn Stmt>>,
}

impl ASTNode for Program {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_program(&self)
    }
}

pub struct VarDecl {
    is_global: bool,
    name: String,
    ty: Types,
}

impl ASTNode for VarDecl {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_var_decl(&self)
    }
}

pub struct ProcDecl {
    is_global: bool,
    name: String,
    ty: Types,
    params: Vec<VarDecl>,
    vars: Vec<VarDecl>,
    procs: Vec<ProcDecl>,
    body: Vec<Box<dyn Stmt>>,
}

impl ASTNode for ProcDecl {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_proc_decl(&self)
    }
}

pub trait Stmt {}

pub struct AssignStmt {
}

impl Stmt for AssignStmt {}

impl ASTNode for AssignStmt {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_assign_stmt(&self)
    }
}

pub struct IfStmt {
    cond: Box<dyn Expr>,
    then_body: Vec<Box<dyn Stmt>>,
    else_body: Vec<Box<dyn Stmt>>,
}

impl Stmt for IfStmt {}

impl ASTNode for IfStmt {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_if_stmt(&self)
    }
}

pub struct LoopStmt {
    init: AssignStmt,
    cond: Box<dyn Expr>,
    body: Vec<Box<dyn Stmt>>,
}

impl Stmt for LoopStmt {}

impl ASTNode for LoopStmt {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_loop_stmt(&self)
    }
}

pub struct ReturnStmt {
    expr: Box<dyn Expr>,
}

impl Stmt for ReturnStmt {}

impl ASTNode for ReturnStmt {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_return_stmt(&self)
    }
}

pub trait Expr {}

pub struct AndOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for AndOp {}

impl ASTNode for AndOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_and_op(&self)
    }
}

pub struct OrOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for OrOp {}

impl ASTNode for OrOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_or_op(&self)
    }
}

pub struct NotOp {
    operand: Box<dyn Expr>,
}

impl Expr for NotOp {}

impl ASTNode for NotOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_not_op(&self)
    }
}

pub struct AddOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for AddOp {}

impl ASTNode for AddOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_add_op(&self)
    }
}

pub struct SubOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for SubOp {}

impl ASTNode for SubOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_sub_op(&self)
    }
}

pub struct MulOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for MulOp {}

impl ASTNode for MulOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_mul_op(&self)
    }
}

pub struct DivOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for DivOp {}

impl ASTNode for DivOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_div_op(&self)
    }
}

pub enum RelationOp {
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    NotEq,
}

pub struct Relation {
    op: RelationOp,
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for Relation {}

impl ASTNode for Relation {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_relation(&self)
    }
}

pub struct NegateOp {
    operand: Box<dyn Expr>,
}

impl Expr for NegateOp {}

impl ASTNode for NegateOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_negate_op(&self)
    }
}

pub struct SubscriptOp {
    array: String,
    index: Box<dyn Expr>,
}

impl Expr for SubscriptOp {}

impl ASTNode for SubscriptOp {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_subscript_op(&self)
    }
}

pub struct ProcCall {
    proc: String,
    args: Vec<Box<dyn Expr>>,
}

impl Expr for ProcCall {}

impl ASTNode for ProcCall {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_proc_call(&self)
    }
}

pub struct IntLiteral {
    value: i32,
}

impl Expr for IntLiteral {}

impl ASTNode for IntLiteral {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_int_literal(&self)
    }
}

pub struct FloatLiteral {
    value: f32,
}

impl Expr for FloatLiteral {}

impl ASTNode for FloatLiteral {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_float_literal(&self)
    }
}

pub struct BoolLiteral {
    value: bool,
}

impl Expr for BoolLiteral {}

impl ASTNode for BoolLiteral {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_bool_literal(&self)
    }
}

pub struct StringLiteral {
    value: String,
}

impl Expr for StringLiteral {}

impl ASTNode for StringLiteral {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_string_literal(&self)
    }
}

pub struct Var {
    pub id: String,
    pub ty: Types,
}

impl Expr for Var {}

impl ASTNode for Var {
    fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
        v.visit_var(&self)
    }
}
