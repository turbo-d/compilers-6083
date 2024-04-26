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
