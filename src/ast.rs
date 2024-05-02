use crate::symtable::SymTable;
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
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T;
    fn type_check(&self, st: &mut SymTable) -> Types;
}

pub struct Program {
    name: String,
    vars: Vec<VarDecl>,
    procs: Vec<ProcDecl>,
    body: Vec<Box<dyn Stmt>>,
}

impl ASTNode for Program {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_program(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub struct VarDecl {
    is_global: bool,
    name: String,
    ty: Types,
}

impl ASTNode for VarDecl {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_var_decl(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
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
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_proc_decl(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub trait Stmt {}

pub struct AssignStmt {
}

impl Stmt for AssignStmt {}

impl ASTNode for AssignStmt {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_assign_stmt(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub struct IfStmt {
    cond: Box<dyn ASTNode>,
    then_body: Vec<Box<dyn Stmt>>,
    else_body: Vec<Box<dyn Stmt>>,
}

impl Stmt for IfStmt {}

impl ASTNode for IfStmt {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_if_stmt(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub struct LoopStmt {
    init: AssignStmt,
    cond: Box<dyn ASTNode>,
    body: Vec<Box<dyn Stmt>>,
}

impl Stmt for LoopStmt {}

impl ASTNode for LoopStmt {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_loop_stmt(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub struct ReturnStmt {
    expr: Box<dyn ASTNode>,
}

impl Stmt for ReturnStmt {}

impl ASTNode for ReturnStmt {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_return_stmt(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub trait Expr {}

pub struct AndOp {
    lhs: Box<dyn ASTNode>,
    rhs: Box<dyn ASTNode>,
}

impl Expr for AndOp {}

impl ASTNode for AndOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_and_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub struct OrOp {
    lhs: Box<dyn ASTNode>,
    rhs: Box<dyn ASTNode>,
}

impl Expr for OrOp {}

impl ASTNode for OrOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_or_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub struct NotOp {
    operand: Box<dyn ASTNode>,
}

impl Expr for NotOp {}

impl ASTNode for NotOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_not_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Unknown
    }
}

pub struct AddOp {
    pub lhs: Box<dyn ASTNode>,
    pub rhs: Box<dyn ASTNode>,
}

impl Expr for AddOp {}

impl ASTNode for AddOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_add_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let lhs_type = self.lhs.type_check(st);
        let rhs_type = self.rhs.type_check(st);

        if lhs_type != Types::Int && lhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        if rhs_type != Types::Int && rhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        let mut op_type = Types::Float;
        if lhs_type == Types::Int && rhs_type == Types::Int {
            op_type = Types::Int;
        }
        op_type
    }
}

pub struct SubOp {
    pub lhs: Box<dyn ASTNode>,
    pub rhs: Box<dyn ASTNode>,
}

impl Expr for SubOp {}

impl ASTNode for SubOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_sub_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let lhs_type = self.lhs.type_check(st);
        let rhs_type = self.rhs.type_check(st);

        if lhs_type != Types::Int && lhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        if rhs_type != Types::Int && rhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        let mut op_type = Types::Float;
        if lhs_type == Types::Int && rhs_type == Types::Int {
            op_type = Types::Int;
        }
        op_type
    }
}

pub struct MulOp {
    pub lhs: Box<dyn ASTNode>,
    pub rhs: Box<dyn ASTNode>,
}

impl Expr for MulOp {}

impl ASTNode for MulOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_mul_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let lhs_type = self.lhs.type_check(st);
        let rhs_type = self.rhs.type_check(st);

        if lhs_type != Types::Int && lhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        if rhs_type != Types::Int && rhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        if lhs_type == Types::Int && rhs_type == Types::Int {
            return Types::Int
        }
        Types::Float
    }
}

pub struct DivOp {
    pub lhs: Box<dyn ASTNode>,
    pub rhs: Box<dyn ASTNode>,
}

impl Expr for DivOp {}

impl ASTNode for DivOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_div_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let lhs_type = self.lhs.type_check(st);
        let rhs_type = self.rhs.type_check(st);

        if lhs_type != Types::Int && lhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        if rhs_type != Types::Int && rhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        Types::Float
    }
}

#[derive(PartialEq)]
pub enum RelationOp {
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    NotEq,
}

pub struct Relation {
    pub op: RelationOp,
    pub lhs: Box<dyn ASTNode>,
    pub rhs: Box<dyn ASTNode>,
}

impl Expr for Relation {}

impl ASTNode for Relation {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_relation(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let lhs_type = self.lhs.type_check(st);
        let rhs_type = self.rhs.type_check(st);

        match lhs_type {
            Types::Bool => {
                if rhs_type != Types::Bool && rhs_type != Types::Int {
                    panic!("Type mismatch. Right operand must be of bool or integer type");
                }
            }
            Types::Int => {
                if rhs_type != Types::Int && rhs_type != Types::Bool {
                    panic!("Type mismatch. Right operand must be of integer or bool type");
                }
            }
            Types::Float => {
                if rhs_type != Types::Float {
                    panic!("Type mismatch. Right operand must be of float type");
                }
            }
            Types::String => {
                if self.op != RelationOp::Eq && self.op != RelationOp::NotEq {
                    panic!("Operator not supported for operands of string type. Only == and != are supported for operands of string type");
                }
                if rhs_type != Types::String {
                    panic!("Type mismatch. Right operand must be of string type");
                }
            }
            _ => panic!("Relational operators not supported for this operand type"),
        }

        Types::Bool
    }
}

pub struct NegateOp {
    pub operand: Box<dyn ASTNode>,
}

impl Expr for NegateOp {}

impl ASTNode for NegateOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_negate_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        // TODO: Type checking for negation operand
        self.operand.type_check(st)
    }
}

pub struct SubscriptOp {
    pub array: Box<Var>,
    pub index: Box<dyn ASTNode>,
}

impl Expr for SubscriptOp {}

impl ASTNode for SubscriptOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_subscript_op(&self)
    //}
    
    fn type_check(&self, st: &mut SymTable) -> Types {
        let array_type = self.array.type_check(st);
        let expr_type = self.index.type_check(st);

        match array_type {
            Types::Array(_, _) => (),
            _ => panic!("Indexing can only be performed on array types"),
        }

        match expr_type {
            Types::Int => (),
            _ => panic!("Array index must be of integer type"),
        }

        array_type
    }
}

pub struct ProcCall {
    pub proc: Box<Var>,
    pub args: Vec<Box<dyn ASTNode>>,
}

impl Expr for ProcCall {}

impl ASTNode for ProcCall {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_proc_call(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let proc_type = self.proc.type_check(st);
        let mut arg_types = Vec::new();
        for arg in self.args.iter() {
            arg_types.push(arg.type_check(st));
        }

        let return_type: Types;
        match proc_type {
            Types::Proc(out_type, param_types) => {
                let n_args = arg_types.len();
                let n_params = param_types.len();
                if n_args != n_params {
                    panic!("Incorrect number of arguments");
                }

                for (i, (arg_type, param_type)) in arg_types.iter().zip(param_types.iter()).enumerate() {
                    if arg_type != param_type {
                        panic!("Type mismatch in argument {i}. (0-indexed)");
                    }
                }

                return_type = *out_type;
            }
            _ => panic!("Expected procedure type"),
        }
        return_type
    }
}

pub struct IntLiteral {
    pub value: i32,
}

impl Expr for IntLiteral {}

impl ASTNode for IntLiteral {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_int_literal(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Int
    }
}

pub struct FloatLiteral {
    pub value: f32,
}

impl Expr for FloatLiteral {}

impl ASTNode for FloatLiteral {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_float_literal(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Float
    }
}

pub struct BoolLiteral {
    pub value: bool,
}

impl Expr for BoolLiteral {}

impl ASTNode for BoolLiteral {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_bool_literal(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::Bool
    }
}

pub struct StringLiteral {
    pub value: String,
}

impl Expr for StringLiteral {}

impl ASTNode for StringLiteral {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_string_literal(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        Types::String
    }
}

pub struct Var {
    pub id: String,
}

impl Expr for Var {}

impl ASTNode for Var {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_var(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let parsed_type: Types;
        match st.get(&self.id) {
            Some(types) => {
                parsed_type = types.clone();
            }
            None => panic!("Missing declaration for {}", self.id),
        }
        parsed_type
    }
}
