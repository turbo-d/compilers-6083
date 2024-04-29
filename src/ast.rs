use crate::types::Types;

use std::vec::Vec;

pub trait ASTVisitor {
    fn visit_program(&self, p: &Program);
    fn visit_var_decl(&self, d: &VarDecl);
    fn visit_proc_decl(&self, d: &ProcDecl);
    fn visit_assign_stmt(&self, s: &AssignStmt);
    fn visit_if_stmt(&self, s: &IfStmt);
    fn visit_loop_stmt(&self, s: &LoopStmt);
    fn visit_return_stmt(&self, s: &ReturnStmt);
    fn visit_and_op(&self, s: &AndOp);
    fn visit_or_op(&self, s: &OrOp);
    fn visit_not_op(&self, s: &NotOp);
    fn visit_add_op(&self, s: &AddOp);
    fn visit_sub_op(&self, s: &SubOp);
    fn visit_mul_op(&self, s: &MulOp);
    fn visit_div_op(&self, s: &DivOp);
    fn visit_relation(&self, s: &Relation);
    fn visit_negate_op(&self, s: &NegateOp);
    fn visit_subscript_op(&self, s: &SubscriptOp);
    fn visit_proc_call(&self, s: &ProcCall);
    fn visit_int_literal(&self, s: &IntLiteral);
    fn visit_float_literal(&self, s: &FloatLiteral);
    fn visit_string_literal(&self, s: &StringLiteral);
    fn visit_bool_literal(&self, s: &BoolLiteral);
}

pub trait ASTNode {
    fn visit(&self, v: &impl ASTVisitor);
}

pub struct Program {
    name: String,
    vars: Vec<VarDecl>,
    procs: Vec<ProcDecl>,
    body: Vec<Box<dyn Stmt>>,
}

impl ASTNode for Program {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_program(&self);
    }
}

pub struct VarDecl {
    is_global: bool,
    name: String,
    ty: Types,
}

impl ASTNode for VarDecl {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_var_decl(&self);
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
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_proc_decl(&self);
    }
}

pub trait Stmt {}

pub struct AssignStmt {
}

impl Stmt for AssignStmt {}

impl ASTNode for AssignStmt {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_assign_stmt(&self);
    }
}

pub struct IfStmt {
    cond: Box<dyn Expr>,
    then_body: Vec<Box<dyn Stmt>>,
    else_body: Vec<Box<dyn Stmt>>,
}

impl Stmt for IfStmt {}

impl ASTNode for IfStmt {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_if_stmt(&self);
    }
}

pub struct LoopStmt {
    init: AssignStmt,
    cond: Box<dyn Expr>,
    body: Vec<Box<dyn Stmt>>,
}

impl Stmt for LoopStmt {}

impl ASTNode for LoopStmt {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_loop_stmt(&self);
    }
}

pub struct ReturnStmt {
    expr: Box<dyn Expr>,
}

impl Stmt for ReturnStmt {}

impl ASTNode for ReturnStmt {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_return_stmt(&self);
    }
}

pub trait Expr {}

pub struct AndOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for AndOp {}

impl ASTNode for AndOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_and_op(&self);
    }
}

pub struct OrOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for OrOp {}

impl ASTNode for OrOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_or_op(&self);
    }
}

pub struct NotOp {
    operand: Box<dyn Expr>,
}

impl Expr for NotOp {}

impl ASTNode for NotOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_not_op(&self);
    }
}

pub struct AddOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for AddOp {}

impl ASTNode for AddOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_add_op(&self);
    }
}

pub struct SubOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for SubOp {}

impl ASTNode for SubOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_sub_op(&self);
    }
}

pub struct MulOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for MulOp {}

impl ASTNode for MulOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_mul_op(&self);
    }
}

pub struct DivOp {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
}

impl Expr for DivOp {}

impl ASTNode for DivOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_div_op(&self);
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
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_relation(&self);
    }
}

pub struct NegateOp {
    operand: Box<dyn Expr>,
}

impl Expr for NegateOp {}

impl ASTNode for NegateOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_negate_op(&self);
    }
}

pub struct SubscriptOp {
    array: Box<dyn Expr>,
    index: Box<dyn Expr>,
}

impl Expr for SubscriptOp {}

impl ASTNode for SubscriptOp {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_subscript_op(&self);
    }
}

pub struct ProcCall {
    proc: String,
    args: Vec<Box<dyn Expr>>,
}

impl Expr for ProcCall {}

impl ASTNode for ProcCall {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_proc_call(&self);
    }
}

pub struct IntLiteral {
    value: i32,
}

impl Expr for IntLiteral {}

impl ASTNode for IntLiteral {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_int_literal(&self);
    }
}

pub struct FloatLiteral {
    value: f32,
}

impl Expr for FloatLiteral {}

impl ASTNode for FloatLiteral {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_float_literal(&self);
    }
}

pub struct BoolLiteral {
    value: bool,
}

impl Expr for BoolLiteral {}

impl ASTNode for BoolLiteral {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_bool_literal(&self);
    }
}

pub struct StringLiteral {
    value: String,
}

impl Expr for StringLiteral {}

impl ASTNode for StringLiteral {
    fn visit(&self, v: &impl ASTVisitor) {
        v.visit_string_literal(&self);
    }
}
