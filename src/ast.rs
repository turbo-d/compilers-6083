use crate::codegen::CodeGen;
use crate::symtable::SymTable;
use crate::types::Types;

use inkwell::AddressSpace;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, FloatValue, FunctionValue, IntValue};
use inkwell::FloatPredicate;

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
    pub name: String,
    pub decls: Vec<Box<dyn ASTNode>>,
    pub body: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for Program {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_program(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        for decl in &self.decls {
            decl.type_check(st);
        }

        for stmt in &self.body {
            stmt.type_check(st);
        }

        Types::Unknown
    }
}

pub struct VarDecl {
    pub is_global: bool,
    pub name: String,
    pub ty: Types,
}

impl ASTNode for VarDecl {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_var_decl(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let result: Result<(), String>;
        if self.is_global {
            result = st.insert_global(self.name.clone(), self.ty.clone());
        } else {
            result = st.insert(self.name.clone(), self.ty.clone());
        }

        match result {
            Ok(_) => (),
            Err(_) => panic!("Duplicate declaration. {} is already declared in this scope", self.name),
        }

        self.ty.clone()
    }
}

pub struct ProcDecl {
    pub is_global: bool,
    pub name: String,
    pub ty: Types,
    pub params: Vec<VarDecl>,
    pub decls: Vec<Box<dyn ASTNode>>,
    pub body: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for ProcDecl {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_proc_decl(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let result: Result<(), String>;
        if self.is_global {
            result = st.insert_global(self.name.clone(), self.ty.clone());
        } else {
            result = st.insert(self.name.clone(), self.ty.clone());
        }

        match result {
            Ok(_) => (),
            Err(_) => panic!("Duplicate declaration. {} is already declared in this scope", self.name),
        }

        st.enter_scope(self.ty.clone());

        for param in &self.params {
            param.type_check(st);
        }

        for decl in &self.decls {
            decl.type_check(st);
        }

        for stmt in &self.body {
            stmt.type_check(st);
        }

        st.exit_scope();

        self.ty.clone()
    }
}

impl ProcDecl {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FunctionValue<'ctx> {
        let (ret_type, arg_types) = match self.ty.clone() {
            Types::Proc(ret, args) => (ret, args),
            _ => panic!("Expected Proc type"),
        };

        let args_types = std::iter::repeat(cg.context.f64_type())
            .take(self.params.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let args_types = self.params.iter()
            .map(|p| {
                match p.ty.clone() {
                    Types::Int => BasicMetadataTypeEnum::from(cg.context.i64_type()),
                    Types::Float => BasicMetadataTypeEnum::from(cg.context.f64_type()),
                    //Types::String => cg.context.ptr_type(AddressSpace::default()).fn_type(args_types, false), //TODO
                    Types::String => BasicMetadataTypeEnum::from(cg.context.f64_type()), //TODO
                    Types::Bool => BasicMetadataTypeEnum::from(cg.context.bool_type()),
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => BasicMetadataTypeEnum::from(cg.context.i64_type().array_type(size)),
                            Types::Float => BasicMetadataTypeEnum::from(cg.context.f64_type().array_type(size)),
                            //Types::String => cg.context.ptr_type(AddressSpace::default()).array_type(size).fn_type(args_types, false), //TODO
                            Types::String => BasicMetadataTypeEnum::from(cg.context.f64_type().array_type(size)), //TODO
                            Types::Bool => BasicMetadataTypeEnum::from(cg.context.bool_type().array_type(size)),
                            _ => panic!("Unexpected base type for arrary type"),
                        }
                    }
                    _ => panic!("Unexpected procedure return type"),
                }
            })
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = match *ret_type {
            Types::Int => cg.context.i64_type().fn_type(args_types, false),
            Types::Float => cg.context.f64_type().fn_type(args_types, false),
            //Types::String => cg.context.ptr_type(AddressSpace::default()).fn_type(args_types, false), //TODO
            Types::String => cg.context.f64_type().fn_type(args_types, false), //TODO
            Types::Bool => cg.context.bool_type().fn_type(args_types, false),
            Types::Array(size, base_type) => {
                match *base_type {
                    Types::Int => cg.context.i64_type().array_type(size).fn_type(args_types, false),
                    Types::Float => cg.context.f64_type().array_type(size).fn_type(args_types, false),
                    //Types::String => cg.context.ptr_type(AddressSpace::default()).array_type(size).fn_type(args_types, false), //TODO
                    Types::String => cg.context.f64_type().array_type(size).fn_type(args_types, false), //TODO
                    Types::Bool => cg.context.bool_type().array_type(size).fn_type(args_types, false),
                    _ => panic!("Unexpected base type for arrary type"),
                }
            }
            _ => panic!("Unexpected procedure return type"),
        };

        let fn_val = cg.module.add_function(self.name.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(self.params[i].name.as_str());
        }

        fn_val
    }
}

pub trait Stmt {}

pub struct AssignStmt {
    pub dest: Box<dyn ASTNode>,
    pub expr: Box<dyn ASTNode>,
}

impl Stmt for AssignStmt {}

impl ASTNode for AssignStmt {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_assign_stmt(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let dest_type = self.dest.type_check(st);
        let expr_type = self.expr.type_check(st);
        match dest_type {
            Types::Bool => {
                if expr_type != Types::Bool && expr_type != Types::Int {
                    panic!("Type mismatch. Expression must be of bool or integer type");
                }
            }
            Types::Int => {
                if expr_type != Types::Int && expr_type != Types::Bool {
                    panic!("Type mismatch. Expression must be of integer, float, or bool type");
                }
            }
            Types::Float => {
                if expr_type != Types::Float && expr_type != Types::Int {
                    panic!("Type mismatch. Expression must be of float or integer type");
                }
            }
            Types::String => {
                if expr_type != Types::String {
                    panic!("Type mismatch. Expression must be of string type");
                }
            }
            _ => panic!("Assignment not supported for this operand type"),
        }
        Types::Unknown
    }
}

pub struct IfStmt {
    pub cond: Box<dyn ASTNode>,
    pub then_body: Vec<Box<dyn ASTNode>>,
    pub else_body: Vec<Box<dyn ASTNode>>,
}

impl Stmt for IfStmt {}

impl ASTNode for IfStmt {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_if_stmt(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let cond_expr_type = self.cond.type_check(st);
        if cond_expr_type != Types::Bool && cond_expr_type != Types::Int {
            panic!("The conditional expression must be of bool or integer type");
        }
        for stmt in &self.then_body {
            stmt.type_check(st);
        }
        for stmt in &self.else_body {
            stmt.type_check(st);
        }
        Types::Unknown
    }
}

pub struct LoopStmt {
    pub init: Box<dyn ASTNode>,
    pub cond: Box<dyn ASTNode>,
    pub body: Vec<Box<dyn ASTNode>>,
}

impl Stmt for LoopStmt {}

impl ASTNode for LoopStmt {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_loop_stmt(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        self.init.type_check(st);
        let cond_expr_type = self.cond.type_check(st);
        if cond_expr_type != Types::Bool && cond_expr_type != Types::Int {
            panic!("The conditional expression must be of bool or integer type");
        }
        for stmt in &self.body {
            stmt.type_check(st);
        }
        Types::Unknown
    }
}

pub struct ReturnStmt {
    pub expr: Box<dyn ASTNode>,
}

impl Stmt for ReturnStmt {}

impl ASTNode for ReturnStmt {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_return_stmt(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let expr_type = self.expr.type_check(st);
        let owning_proc_type = st.get_owning_proc_type();

        // Same compatibility rules as assignment
        if let Types::Proc(return_type, _) = owning_proc_type {
            match *return_type {
                Types::Bool => {
                    if expr_type != Types::Bool && expr_type != Types::Int {
                        panic!("Expression type does not match the return type of the owning procedure");
                    }
                }
                Types::Int => {
                    if expr_type != Types::Int && expr_type != Types::Bool {
                        panic!("Expression type does not match the return type of the owning procedure");
                    }
                }
                Types::Float => {
                    if expr_type != Types::Float && expr_type != Types::Int {
                        panic!("Expression type does not match the return type of the owning procedure");
                    }
                }
                Types::String => {
                    if expr_type != Types::String {
                        panic!("Expression type does not match the return type of the owning procedure");
                    }
                }
                _ => panic!("Returns not supported for this operand type"),
            }
        }

        Types::Unknown
    }
}

pub trait Expr {}

pub struct AndOp {
    pub lhs: Box<dyn ASTNode>,
    pub rhs: Box<dyn ASTNode>,
}

impl Expr for AndOp {}

impl ASTNode for AndOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_and_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let lhs_type = self.lhs.type_check(st);
        let rhs_type = self.rhs.type_check(st);

        if lhs_type != Types::Int {
            panic!("Bitwise operations can only be performed on operands of integer type");
        }

        if rhs_type != Types::Int {
            panic!("Bitwise operations can only be performed on operands of integer type");
        }

        Types::Int
    }
}

impl AndOp {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> IntValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let l: u64 = 5;
        let lhs = cg.context.i64_type().const_int(l, false);
        let r: u64 = 10;
        let rhs = cg.context.i64_type().const_int(r, false);

        cg.builder.build_and(lhs, rhs, "tmpand").unwrap()
    }
}

pub struct OrOp {
    pub lhs: Box<dyn ASTNode>,
    pub rhs: Box<dyn ASTNode>,
}

impl Expr for OrOp {}

impl ASTNode for OrOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_or_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let lhs_type = self.lhs.type_check(st);
        let rhs_type = self.rhs.type_check(st);

        if lhs_type != Types::Int {
            panic!("Bitwise operations can only be performed on operands of integer type");
        }

        if rhs_type != Types::Int {
            panic!("Bitwise operations can only be performed on operands of integer type");
        }

        Types::Int
    }
}

impl OrOp {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> IntValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let l: u64 = 5;
        let lhs = cg.context.i64_type().const_int(l, false);
        let r: u64 = 10;
        let rhs = cg.context.i64_type().const_int(r, false);

        cg.builder.build_or(lhs, rhs, "tmpor").unwrap()
    }
}

pub struct NotOp {
    pub operand: Box<dyn ASTNode>,
}

impl Expr for NotOp {}

impl ASTNode for NotOp {
    //fn visit<T>(&self, v: &impl ASTVisitor<T>) -> T {
    //    v.visit_not_op(&self)
    //}

    fn type_check(&self, st: &mut SymTable) -> Types {
        let operand_type = self.operand.type_check(st);

        if operand_type != Types::Int {
            panic!("Bitwise operations can only be performed on operands of integer type");
        }

        Types::Int
    }
}

impl NotOp {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> IntValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let v: u64 = 5;
        let value = cg.context.i64_type().const_int(v, false);

        cg.builder.build_not(value, "tmpnot").unwrap()
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

impl AddOp {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FloatValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let l: f64 = 5.0;
        let lhs = cg.context.f64_type().const_float(l);
        let r: f64 = 10.0;
        let rhs = cg.context.f64_type().const_float(r);

        cg.builder.build_float_add(lhs, rhs, "tmpadd").unwrap()
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

impl SubOp {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FloatValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let l: f64 = 5.0;
        let lhs = cg.context.f64_type().const_float(l);
        let r: f64 = 10.0;
        let rhs = cg.context.f64_type().const_float(r);

        cg.builder.build_float_sub(lhs, rhs, "tmpsub").unwrap()
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

impl MulOp {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FloatValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let l: f64 = 5.0;
        let lhs = cg.context.f64_type().const_float(l);
        let r: f64 = 10.0;
        let rhs = cg.context.f64_type().const_float(r);

        cg.builder.build_float_mul(lhs, rhs, "tmpmul").unwrap()
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

impl DivOp {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FloatValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let l: f64 = 5.0;
        let lhs = cg.context.f64_type().const_float(l);
        let r: f64 = 10.0;
        let rhs = cg.context.f64_type().const_float(r);

        cg.builder.build_float_div(lhs, rhs, "tmpdiv").unwrap()
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

impl Relation {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FloatValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let l: f64 = 5.0;
        let lhs = cg.context.f64_type().const_float(l);
        let r: f64 = 10.0;
        let rhs = cg.context.f64_type().const_float(r);

        let cmp = match self.op {
            RelationOp::LT => cg.builder.build_float_compare(FloatPredicate::ULT, lhs, rhs, "tmpcmp").unwrap(),
            RelationOp::LTE => cg.builder.build_float_compare(FloatPredicate::ULE, lhs, rhs, "tmpcmp").unwrap(),
            RelationOp::GT => cg.builder.build_float_compare(FloatPredicate::UGT, lhs, rhs, "tmpcmp").unwrap(),
            RelationOp::GTE => cg.builder.build_float_compare(FloatPredicate::UGE, lhs, rhs, "tmpcmp").unwrap(),
            RelationOp::Eq => cg.builder.build_float_compare(FloatPredicate::UEQ, lhs, rhs, "tmpcmp").unwrap(),
            RelationOp::NotEq => cg.builder.build_float_compare(FloatPredicate::UNE, lhs, rhs, "tmpcmp").unwrap(),
            _ => panic!("RelationOp not recognized"),
        };

        cg.builder.build_unsigned_int_to_float(cmp, cg.context.f64_type(), "tmpbool").unwrap()
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

impl NegateOp {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FloatValue<'ctx> {
        //let lhs = self.lhs.code_gen(cg);
        //let rhs = self.rhs.code_gen(cg);
        let v: f64 = 5.0;
        let value = cg.context.f64_type().const_float(v);

        cg.builder.build_float_neg(value, "tmpneg").unwrap()
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

impl ProcCall {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FloatValue<'ctx> {
        let proc = match cg.module.get_function(self.proc.id.as_str()) {
            Some(p) => p,
            None => panic!("Unknown function"),
        };

        //let mut compiled_args = Vec::with_capacity(self.args.len());
        let compiled_args: Vec<FloatValue> = Vec::with_capacity(self.args.len());

        //for arg in &self.args {
        //    compiled_args.push(arg.code_gen(cg));
        //}

        let argsv: Vec<BasicMetadataValueEnum> =
            compiled_args.iter().by_ref().map(|&val| val.into()).collect();

        match cg.builder.build_call(proc, argsv.as_slice(), "tmp").unwrap().try_as_basic_value().left() {
            Some(value) => value.into_float_value(),
            None => panic!("Invalid call produced."),
        }
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

impl IntLiteral {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> IntValue<'ctx> {
        cg.context.i64_type().const_int(self.value as u64, false)
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

impl FloatLiteral {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> FloatValue<'ctx> {
        cg.context.f64_type().const_float(self.value as f64)
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

impl BoolLiteral {
    fn code_gen<'a, 'ctx>(&self, cg: &CodeGen<'a, 'ctx>) -> IntValue<'ctx> {
        cg.context.bool_type().const_int(self.value as u64, false)
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

impl StringLiteral {
    fn codegen(&self) {
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
