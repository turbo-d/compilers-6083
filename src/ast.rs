use crate::symtable::SymTable;
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

impl ASTNode {
    pub fn type_check(&self, st: &mut SymTable<Types, Types>) -> Types {
        match self {
            ASTNode::Program { decls, body, .. } => {
                for decl in decls.iter() {
                    decl.type_check(st);
                }

                for stmt in body.iter() {
                    stmt.type_check(st);
                }

                Types::Unknown
            }
            ASTNode::VarDecl { is_global, name, ty } => {
                let result: Result<(), String>;
                if *is_global {
                    result = st.insert_global(name.clone(), ty.clone());
                } else {
                    result = st.insert(name.clone(), ty.clone());
                }

                match result {
                    Ok(_) => (),
                    Err(_) => panic!("Duplicate declaration. {name} is already declared in this scope"),
                }

                ty.clone()
            },
            ASTNode::ProcDecl { is_global, name, ty, params, decls, body } => {
                let result: Result<(), String>;
                if *is_global {
                    result = st.insert_global(name.clone(), ty.clone());
                } else {
                    result = st.insert(name.clone(), ty.clone());
                }

                match result {
                    Ok(_) => (),
                    Err(_) => panic!("Duplicate declaration. {name} is already declared in this scope"),
                }

                st.enter_scope(ty.clone());

                for param in params.iter() {
                    param.type_check(st);
                }

                for decl in decls.iter() {
                    decl.type_check(st);
                }

                for stmt in body.iter() {
                    stmt.type_check(st);
                }

                st.exit_scope();

                ty.clone()
            },
            ASTNode::AssignStmt { dest, expr } => {
                let dest_type = dest.type_check(st);
                let expr_type = expr.type_check(st);
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
            },
            ASTNode::IfStmt { cond, then_body, else_body } => {
                let cond_expr_type = cond.type_check(st);
                if cond_expr_type != Types::Bool && cond_expr_type != Types::Int {
                    panic!("The conditional expression must be of bool or integer type");
                }
                for stmt in then_body.iter() {
                    stmt.type_check(st);
                }
                for stmt in else_body.iter() {
                    stmt.type_check(st);
                }
                Types::Unknown
            },
            ASTNode::LoopStmt { init, cond, body } => {
                init.type_check(st);
                let cond_expr_type = cond.type_check(st);
                if cond_expr_type != Types::Bool && cond_expr_type != Types::Int {
                    panic!("The conditional expression must be of bool or integer type");
                }
                for stmt in body.iter() {
                    stmt.type_check(st);
                }
                Types::Unknown
            },
            ASTNode::ReturnStmt { expr } => {
                let expr_type = expr.type_check(st);
                let owning_proc_type = st.get_local_proc_data();

                // Same compatibility rules as assignment
                if let Types::Proc(return_type, _) = owning_proc_type {
                    match **return_type {
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
            },
            ASTNode::AndOp { lhs, rhs } => {
                let lhs_type = lhs.type_check(st);
                let rhs_type = rhs.type_check(st);

                if lhs_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                if rhs_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                Types::Int
            },
            ASTNode::OrOp { lhs, rhs } => {
                let lhs_type = lhs.type_check(st);
                let rhs_type = rhs.type_check(st);

                if lhs_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                if rhs_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                Types::Int
            },
            ASTNode::NotOp { operand } => {
                let operand_type = operand.type_check(st);

                if operand_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                Types::Int
            },
            ASTNode::AddOp { lhs, rhs } => {
                let lhs_type = lhs.type_check(st);
                let rhs_type = rhs.type_check(st);

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
            },
            ASTNode::SubOp { lhs, rhs } => {
                let lhs_type = lhs.type_check(st);
                let rhs_type = rhs.type_check(st);

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
            },
            ASTNode::MulOp { lhs, rhs } => {
                let lhs_type = lhs.type_check(st);
                let rhs_type = rhs.type_check(st);

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
            },
            ASTNode::DivOp { lhs, rhs } => {
                let lhs_type = lhs.type_check(st);
                let rhs_type = rhs.type_check(st);

                if lhs_type != Types::Int && lhs_type != Types::Float {
                    panic!("Arithmetic operations can only be performed on operands of integer and float type");
                }

                if rhs_type != Types::Int && rhs_type != Types::Float {
                    panic!("Arithmetic operations can only be performed on operands of integer and float type");
                }

                Types::Float
            },
            ASTNode::Relation { op, lhs, rhs } => {
                let lhs_type = lhs.type_check(st);
                let rhs_type = rhs.type_check(st);

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
                        if *op != RelationOp::Eq && *op != RelationOp::NotEq {
                            panic!("Operator not supported for operands of string type. Only == and != are supported for operands of string type");
                        }
                        if rhs_type != Types::String {
                            panic!("Type mismatch. Right operand must be of string type");
                        }
                    }
                    _ => panic!("Relational operators not supported for this operand type"),
                }

                Types::Bool
            },
            ASTNode::NegateOp { operand } => {
                // TODO: Type checking for negation operand. Same as arithmetic.
                operand.type_check(st)
            },
            ASTNode::SubscriptOp { array, index } => {
                let array_type = array.type_check(st);
                let expr_type = index.type_check(st);

                match array_type {
                    Types::Array(_, _) => (),
                    _ => panic!("Indexing can only be performed on array types"),
                }

                match expr_type {
                    Types::Int => (),
                    _ => panic!("Array index must be of integer type"),
                }

                array_type
            },
            ASTNode::ProcCall { proc, args } => {
                let proc_type = proc.type_check(st);
                let mut arg_types = Vec::new();
                for arg in args.iter() {
                    arg_types.push(arg.type_check(st));
                }

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

                        *out_type
                    }
                    _ => panic!("Expected procedure type"),
                }
            },
            ASTNode::IntLiteral { .. } => Types::Int,
            ASTNode::FloatLiteral { .. } => Types::Float,
            ASTNode::BoolLiteral { .. } => Types::Bool,
            ASTNode::StringLiteral { .. } => Types::String,
            ASTNode::Var { id } => {
                match st.get(&id) {
                    Some(types) => types.clone(),
                    None => panic!("Missing declaration for {id}"),
                }
            },
        }
   }
}
