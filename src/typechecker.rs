use crate::ast::{Ast, AstVisitor, RelationOp};
use crate::error::CompilerError;
use crate::symtable::SymTable;
use crate::types::Types;

pub struct TypeChecker {
    st: SymTable<Types, Types>,
    errs: Vec<CompilerError>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        let mut st = SymTable::new(Types::Proc(Box::new(Types::Int), Vec::new()));
        // TODO: Delete this once runtime is finished.
        // This is just for testing
        let _ = st.insert_global(String::from("getbool"), Types::Proc(Box::new(Types::Bool), Vec::new()));
        let _ = st.insert_global(String::from("getinteger"), Types::Proc(Box::new(Types::Int), Vec::new()));
        let _ = st.insert_global(String::from("getfloat"), Types::Proc(Box::new(Types::Float), Vec::new()));
        let _ = st.insert_global(String::from("getstring"), Types::Proc(Box::new(Types::String), Vec::new()));
        let _ = st.insert_global(String::from("putbool"), Types::Proc(Box::new(Types::Bool), vec![Types::Bool]));
        let _ = st.insert_global(String::from("putinteger"), Types::Proc(Box::new(Types::Bool), vec![Types::Int]));
        let _ = st.insert_global(String::from("putfloat"), Types::Proc(Box::new(Types::Bool), vec![Types::Float]));
        let _ = st.insert_global(String::from("putstring"), Types::Proc(Box::new(Types::Bool), vec![Types::String]));
        let _ = st.insert_global(String::from("sqrt"), Types::Proc(Box::new(Types::Float), vec![Types::Int]));

        TypeChecker {
            st,
            errs: Vec::<CompilerError>::new(),
        }
    }
}

impl TypeChecker {
    pub fn get_errors(&self) -> &Vec<CompilerError> {
        &self.errs
    }
}

impl AstVisitor<Types> for TypeChecker {
    fn visit_ast(&mut self, ast: &Ast) -> Types {
        match ast {
            Ast::Program { decls, body, .. } => {
                for decl in decls.iter() {
                    self.visit_ast(decl);
                }

                for stmt in body.iter() {
                    self.visit_ast(stmt);
                }

                Types::Unknown
            }
            Ast::VarDecl { is_global, name, ty } => {
                let result: Result<(), String>;
                if *is_global {
                    result = self.st.insert_global(name.clone(), ty.clone());
                } else {
                    result = self.st.insert(name.clone(), ty.clone());
                }

                match result {
                    Ok(_) => (),
                    Err(_) => panic!("Duplicate declaration. {name} is already declared in this scope"),
                }

                ty.clone()
            },
            Ast::ProcDecl { is_global, name, ty, params, decls, body } => {
                let result: Result<(), String>;
                if *is_global {
                    result = self.st.insert_global(name.clone(), ty.clone());
                } else {
                    result = self.st.insert(name.clone(), ty.clone());
                }

                match result {
                    Ok(_) => (),
                    Err(_) => panic!("Duplicate declaration. {name} is already declared in this scope"),
                }

                self.st.enter_scope(ty.clone());

                for param in params.iter() {
                    self.visit_ast(param);
                }

                for decl in decls.iter() {
                    self.visit_ast(decl);
                }

                for stmt in body.iter() {
                    self.visit_ast(stmt);
                }

                self.st.exit_scope();

                ty.clone()
            },
            Ast::AssignStmt { dest, expr } => {
                let dest_type = self.visit_ast(dest);
                let expr_type = self.visit_ast(expr);
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
            Ast::IfStmt { cond, then_body, else_body } => {
                let cond_expr_type = self.visit_ast(cond);
                if cond_expr_type != Types::Bool && cond_expr_type != Types::Int {
                    panic!("The conditional expression must be of bool or integer type");
                }
                for stmt in then_body.iter() {
                    self.visit_ast(stmt);
                }
                for stmt in else_body.iter() {
                    self.visit_ast(stmt);
                }
                Types::Unknown
            },
            Ast::LoopStmt { init, cond, body } => {
                self.visit_ast(init);
                let cond_expr_type = self.visit_ast(cond);
                if cond_expr_type != Types::Bool && cond_expr_type != Types::Int {
                    panic!("The conditional expression must be of bool or integer type");
                }
                for stmt in body.iter() {
                    self.visit_ast(stmt);
                }
                Types::Unknown
            },
            Ast::ReturnStmt { expr } => {
                let expr_type = self.visit_ast(expr);
                let owning_proc_type = self.st.get_local_proc_data();

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
            Ast::AndOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs);
                let rhs_type = self.visit_ast(rhs);

                if lhs_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                if rhs_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                Types::Int
            },
            Ast::OrOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs);
                let rhs_type = self.visit_ast(rhs);

                if lhs_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                if rhs_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                Types::Int
            },
            Ast::NotOp { operand } => {
                let operand_type = self.visit_ast(operand);

                if operand_type != Types::Int {
                    panic!("Bitwise operations can only be performed on operands of integer type");
                }

                Types::Int
            },
            Ast::AddOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs);
                let rhs_type = self.visit_ast(rhs);

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
            Ast::SubOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs);
                let rhs_type = self.visit_ast(rhs);

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
            Ast::MulOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs);
                let rhs_type = self.visit_ast(rhs);

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
            Ast::DivOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs);
                let rhs_type = self.visit_ast(rhs);

                if lhs_type != Types::Int && lhs_type != Types::Float {
                    panic!("Arithmetic operations can only be performed on operands of integer and float type");
                }

                if rhs_type != Types::Int && rhs_type != Types::Float {
                    panic!("Arithmetic operations can only be performed on operands of integer and float type");
                }

                Types::Float
            },
            Ast::Relation { op, lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs);
                let rhs_type = self.visit_ast(rhs);

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
            Ast::NegateOp { operand } => {
                // TODO: Type checking for negation operand. Same as arithmetic.
                self.visit_ast(operand)
            },
            Ast::SubscriptOp { array, index } => {
                let array_type = self.visit_ast(array);
                let expr_type = self.visit_ast(index);

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
            Ast::ProcCall { proc, args } => {
                let proc_type = self.visit_ast(proc);
                let mut arg_types = Vec::new();
                for arg in args.iter() {
                    arg_types.push(self.visit_ast(arg));
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
            Ast::IntLiteral { .. } => Types::Int,
            Ast::FloatLiteral { .. } => Types::Float,
            Ast::BoolLiteral { .. } => Types::Bool,
            Ast::StringLiteral { .. } => Types::String,
            Ast::Var { id } => {
                match self.st.get(id) {
                    Some(types) => types.clone(),
                    None => panic!("Missing declaration for {id}"),
                }
            },
        }
    }
}
