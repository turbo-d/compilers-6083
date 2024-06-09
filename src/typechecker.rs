use crate::ast::{Ast, AstVisitor, RelationOp};
use crate::error::{CompilerError, TerminalError};
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

impl AstVisitor<Result<Types, TerminalError>> for TypeChecker {
    fn visit_ast(&mut self, ast: &mut Ast) -> Result<Types, TerminalError> {
        match ast {
            Ast::Program { decls, body, .. } => {
                for decl in decls.iter_mut() {
                    self.visit_ast(&mut *decl)?;
                }

                for stmt in body.iter_mut() {
                    self.visit_ast(&mut *stmt)?;
                }

                Ok(Types::Unknown)
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
                    Err(_) => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Duplicate declaration. {name} is already declared in this scope") });
                        return Err(TerminalError);
                    },
                }

                Ok(ty.clone())
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
                    Err(_) => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Duplicate declaration. {name} is already declared in this scope") });
                        return Err(TerminalError);
                    },
                }

                self.st.enter_scope(ty.clone());

                for param in params.iter_mut() {
                    self.visit_ast(&mut *param)?;
                }

                for decl in decls.iter_mut() {
                    self.visit_ast(&mut *decl)?;
                }

                for stmt in body.iter_mut() {
                    self.visit_ast(&mut *stmt)?;
                }

                self.st.exit_scope();

                Ok(ty.clone())
            },
            Ast::AssignStmt { dest, expr } => {
                let dest_type = self.visit_ast(dest)?;
                let expr_type = self.visit_ast(expr)?;
                match dest_type {
                    Types::Bool => {
                        if expr_type != Types::Bool && expr_type != Types::Int {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch. Expression must be of bool or integer type") });
                            return Err(TerminalError);
                        }
                    }
                    Types::Int => {
                        if expr_type != Types::Int && expr_type != Types::Bool {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch. Expression must be of integer, float, or bool type") });
                            return Err(TerminalError);
                        }
                    }
                    Types::Float => {
                        if expr_type != Types::Float && expr_type != Types::Int {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch. Expression must be of float or integer type") });
                            return Err(TerminalError);
                        }
                    }
                    Types::String => {
                        if expr_type != Types::String {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch. Expression must be of string type") });
                            return Err(TerminalError);
                        }
                    }
                    _ => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Assignment not supported for this operand type") });
                        return Err(TerminalError);
                    },
                }
                Ok(Types::Unknown)
            },
            Ast::IfStmt { cond, then_body, else_body } => {
                let cond_expr_type = self.visit_ast(cond)?;
                if cond_expr_type != Types::Bool && cond_expr_type != Types::Int {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("The conditional expression must be of bool or integer type") });
                    return Err(TerminalError);
                }
                for stmt in then_body.iter_mut() {
                    self.visit_ast(&mut *stmt)?;
                }
                for stmt in else_body.iter_mut() {
                    self.visit_ast(&mut *stmt)?;
                }
                Ok(Types::Unknown)
            },
            Ast::LoopStmt { init, cond, body } => {
                self.visit_ast(init)?;
                let cond_expr_type = self.visit_ast(cond)?;
                if cond_expr_type != Types::Bool && cond_expr_type != Types::Int {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("The conditional expression must be of bool or integer type") });
                    return Err(TerminalError);
                }
                for stmt in body.iter_mut() {
                    self.visit_ast(&mut *stmt)?;
                }
                Ok(Types::Unknown)
            },
            Ast::ReturnStmt { expr } => {
                let expr_type = self.visit_ast(expr)?;
                let owning_proc_type = self.st.get_local_proc_data();

                // Same compatibility rules as assignment
                if let Types::Proc(return_type, _) = owning_proc_type {
                    match **return_type {
                        Types::Bool => {
                            if expr_type != Types::Bool && expr_type != Types::Int {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Expression type does not match the return type of the owning procedure") });
                                return Err(TerminalError);
                            }
                        }
                        Types::Int => {
                            if expr_type != Types::Int && expr_type != Types::Bool {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Expression type does not match the return type of the owning procedure") });
                                return Err(TerminalError);
                            }
                        }
                        Types::Float => {
                            if expr_type != Types::Float && expr_type != Types::Int {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Expression type does not match the return type of the owning procedure") });
                                return Err(TerminalError);
                            }
                        }
                        Types::String => {
                            if expr_type != Types::String {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Expression type does not match the return type of the owning procedure") });
                                return Err(TerminalError);
                            }
                        }
                        _ => {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Returns not supported for this operand type") });
                            return Err(TerminalError);
                        },
                    }
                }

                Ok(Types::Unknown)
            },
            Ast::AndOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs)?;
                let rhs_type = self.visit_ast(rhs)?;

                if lhs_type != Types::Int {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Bitwise operations can only be performed on operands of integer type") });
                    return Err(TerminalError);
                }

                if rhs_type != Types::Int {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Bitwise operations can only be performed on operands of integer type") });
                    return Err(TerminalError);
                }

                Ok(Types::Int)
            },
            Ast::OrOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs)?;
                let rhs_type = self.visit_ast(rhs)?;

                if lhs_type != Types::Int {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Bitwise operations can only be performed on operands of integer type") });
                    return Err(TerminalError);
                }

                if rhs_type != Types::Int {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Bitwise operations can only be performed on operands of integer type") });
                    return Err(TerminalError);
                }

                Ok(Types::Int)
            },
            Ast::NotOp { operand } => {
                let operand_type = self.visit_ast(operand)?;

                if operand_type != Types::Int {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Bitwise operations can only be performed on operands of integer type") });
                    return Err(TerminalError);
                }

                Ok(Types::Int)
            },
            Ast::AddOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs)?;
                let rhs_type = self.visit_ast(rhs)?;

                if lhs_type != Types::Int && lhs_type != Types::Float && !matches!(lhs_type, Types::Array(_, _)) {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {lhs_type} type") });
                    return Err(TerminalError);
                }

                if let Types::Array(_, ref base_type) = lhs_type {
                    if **base_type != Types::Int && **base_type != Types::Float {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {lhs_type} type") });
                        return Err(TerminalError);
                    }
                }

                if rhs_type != Types::Int && rhs_type != Types::Float && !matches!(rhs_type, Types::Array(_, _)) {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {rhs_type} type") });
                    return Err(TerminalError);
                }

                if let Types::Array(_, ref base_type) = rhs_type {
                    if **base_type != Types::Int && **base_type != Types::Float {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {rhs_type} type") });
                        return Err(TerminalError);
                    }
                }

                if let Types::Array(lhs_size, ref lhs_base_type) = lhs_type {
                    if let Types::Array(rhs_size, ref rhs_base_type) = rhs_type {
                        if lhs_size != rhs_size {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer array or float array types with matching sizes. {} != {}", lhs_type, rhs_type) });
                            return Err(TerminalError);
                        }

                        if let Types::Int = **lhs_base_type {
                            if let Types::Int = **rhs_base_type {
                                Ok(Types::Array(lhs_size, Box::new(Types::Int)))
                            } else { // Float
                                *lhs = Box::new(Ast::IntArrayToFloatArray {
                                    operand: Box::new(*lhs.clone()),
                                });
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            }
                        } else { // Float
                            if let Types::Int = **rhs_base_type {
                                *rhs = Box::new(Ast::IntArrayToFloatArray {
                                    operand: Box::new(*rhs.clone()),
                                });
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            } else { // Float
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            }
                        }
                    } else if let Types::Int = rhs_type {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    } else { // Float
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                } else if let Types::Int = lhs_type {
                    if let Types::Int = rhs_type {
                        Ok(Types::Int)
                    } else if let Types::Float = rhs_type {
                        *lhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*lhs.clone()),
                        });
                        Ok(Types::Float)
                    } else { // Array
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                } else { // Float
                    if let Types::Int = rhs_type {
                        *rhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*rhs.clone()),
                        });
                        Ok(Types::Float)
                    } else if let Types::Float = rhs_type {
                        Ok(Types::Float)
                    } else { // Array
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                }
            },
            Ast::SubOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs)?;
                let rhs_type = self.visit_ast(rhs)?;

                if lhs_type != Types::Int && lhs_type != Types::Float && !matches!(lhs_type, Types::Array(_, _)) {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {lhs_type} type") });
                    return Err(TerminalError);
                }

                if let Types::Array(_, ref base_type) = lhs_type {
                    if **base_type != Types::Int && **base_type != Types::Float {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {lhs_type} type") });
                        return Err(TerminalError);
                    }
                }

                if rhs_type != Types::Int && rhs_type != Types::Float && !matches!(rhs_type, Types::Array(_, _)) {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {rhs_type} type") });
                    return Err(TerminalError);
                }

                if let Types::Array(_, ref base_type) = rhs_type {
                    if **base_type != Types::Int && **base_type != Types::Float {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {rhs_type} type") });
                        return Err(TerminalError);
                    }
                }

                if let Types::Array(lhs_size, ref lhs_base_type) = lhs_type {
                    if let Types::Array(rhs_size, ref rhs_base_type) = rhs_type {
                        if lhs_size != rhs_size {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer array or float array types with matching sizes. {} != {}", lhs_type, rhs_type) });
                            return Err(TerminalError);
                        }

                        if let Types::Int = **lhs_base_type {
                            if let Types::Int = **rhs_base_type {
                                Ok(Types::Array(lhs_size, Box::new(Types::Int)))
                            } else { // Float
                                *lhs = Box::new(Ast::IntArrayToFloatArray {
                                    operand: Box::new(*lhs.clone()),
                                });
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            }
                        } else { // Float
                            if let Types::Int = **rhs_base_type {
                                *rhs = Box::new(Ast::IntArrayToFloatArray {
                                    operand: Box::new(*rhs.clone()),
                                });
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            } else { // Float
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            }
                        }
                    } else if let Types::Int = rhs_type {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    } else { // Float
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                } else if let Types::Int = lhs_type {
                    if let Types::Int = rhs_type {
                        Ok(Types::Int)
                    } else if let Types::Float = rhs_type {
                        *lhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*lhs.clone()),
                        });
                        Ok(Types::Float)
                    } else { // Array
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                } else { // Float
                    if let Types::Int = rhs_type {
                        *rhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*rhs.clone()),
                        });
                        Ok(Types::Float)
                    } else if let Types::Float = rhs_type {
                        Ok(Types::Float)
                    } else { // Array
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                }
            },
            Ast::MulOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs)?;
                let rhs_type = self.visit_ast(rhs)?;

                if lhs_type != Types::Int && lhs_type != Types::Float && !matches!(lhs_type, Types::Array(_, _)) {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {lhs_type} type") });
                    return Err(TerminalError);
                }

                if let Types::Array(_, ref base_type) = lhs_type {
                    if **base_type != Types::Int && **base_type != Types::Float {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {lhs_type} type") });
                        return Err(TerminalError);
                    }
                }

                if rhs_type != Types::Int && rhs_type != Types::Float && !matches!(rhs_type, Types::Array(_, _)) {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {rhs_type} type") });
                    return Err(TerminalError);
                }

                if let Types::Array(_, ref base_type) = rhs_type {
                    if **base_type != Types::Int && **base_type != Types::Float {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {rhs_type} type") });
                        return Err(TerminalError);
                    }
                }

                if let Types::Array(lhs_size, ref lhs_base_type) = lhs_type {
                    if let Types::Array(rhs_size, ref rhs_base_type) = rhs_type {
                        if lhs_size != rhs_size {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer array or float array types with matching sizes. {} != {}", lhs_type, rhs_type) });
                            return Err(TerminalError);
                        }

                        if let Types::Int = **lhs_base_type {
                            if let Types::Int = **rhs_base_type {
                                Ok(Types::Array(lhs_size, Box::new(Types::Int)))
                            } else { // Float
                                *lhs = Box::new(Ast::IntArrayToFloatArray {
                                    operand: Box::new(*lhs.clone()),
                                });
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            }
                        } else { // Float
                            if let Types::Int = **rhs_base_type {
                                *rhs = Box::new(Ast::IntArrayToFloatArray {
                                    operand: Box::new(*rhs.clone()),
                                });
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            } else { // Float
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            }
                        }
                    } else if let Types::Int = rhs_type {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    } else { // Float
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                } else if let Types::Int = lhs_type {
                    if let Types::Int = rhs_type {
                        Ok(Types::Int)
                    } else if let Types::Float = rhs_type {
                        *lhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*lhs.clone()),
                        });
                        Ok(Types::Float)
                    } else { // Array
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                } else { // Float
                    if let Types::Int = rhs_type {
                        *rhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*rhs.clone()),
                        });
                        Ok(Types::Float)
                    } else if let Types::Float = rhs_type {
                        Ok(Types::Float)
                    } else { // Array
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                }
            },
            Ast::DivOp { lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs)?;
                let rhs_type = self.visit_ast(rhs)?;

                if lhs_type != Types::Int && lhs_type != Types::Float && !matches!(lhs_type, Types::Array(_, _)) {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {lhs_type} type") });
                    return Err(TerminalError);
                }

                if let Types::Array(_, ref base_type) = lhs_type {
                    if **base_type != Types::Int && **base_type != Types::Float {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {lhs_type} type") });
                        return Err(TerminalError);
                    }
                }

                if rhs_type != Types::Int && rhs_type != Types::Float && !matches!(rhs_type, Types::Array(_, _)) {
                    self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {rhs_type} type") });
                    return Err(TerminalError);
                }

                if let Types::Array(_, ref base_type) = rhs_type {
                    if **base_type != Types::Int && **base_type != Types::Float {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {rhs_type} type") });
                        return Err(TerminalError);
                    }
                }

                if let Types::Array(lhs_size, ref lhs_base_type) = lhs_type {
                    if let Types::Array(rhs_size, ref rhs_base_type) = rhs_type {
                        if lhs_size != rhs_size {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer array or float array types with matching sizes. {} != {}", lhs_type, rhs_type) });
                            return Err(TerminalError);
                        }

                        if let Types::Int = **lhs_base_type {
                            if let Types::Int = **rhs_base_type {
                                Ok(Types::Array(lhs_size, Box::new(Types::Int)))
                            } else { // Float
                                *lhs = Box::new(Ast::IntArrayToFloatArray {
                                    operand: Box::new(*lhs.clone()),
                                });
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            }
                        } else { // Float
                            if let Types::Int = **rhs_base_type {
                                *rhs = Box::new(Ast::IntArrayToFloatArray {
                                    operand: Box::new(*rhs.clone()),
                                });
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            } else { // Float
                                Ok(Types::Array(lhs_size, Box::new(Types::Float)))
                            }
                        }
                    } else if let Types::Int = rhs_type {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    } else { // Float
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                } else if let Types::Int = lhs_type {
                    if let Types::Int = rhs_type {
                        *lhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*lhs.clone()),
                        });
                        *rhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*rhs.clone()),
                        });
                        Ok(Types::Float)
                    } else if let Types::Float = rhs_type {
                        *lhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*lhs.clone()),
                        });
                        Ok(Types::Float)
                    } else { // Array
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                } else { // Float
                    if let Types::Int = rhs_type {
                        *rhs = Box::new(Ast::IntToFloat {
                            operand: Box::new(*rhs.clone()),
                        });
                        Ok(Types::Float)
                    } else if let Types::Float = rhs_type {
                        Ok(Types::Float)
                    } else { // Array
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", lhs_type, rhs_type) });
                        Err(TerminalError)
                    }
                }
            },
            Ast::Relation { op, lhs, rhs } => {
                let lhs_type = self.visit_ast(lhs)?;
                let rhs_type = self.visit_ast(rhs)?;

                match lhs_type {
                    Types::Bool => {
                        if rhs_type != Types::Bool && rhs_type != Types::Int {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch. Right operand must be of bool or integer type") });
                            return Err(TerminalError);
                        }
                    }
                    Types::Int => {
                        if rhs_type != Types::Int && rhs_type != Types::Bool {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch. Right operand must be of integer or bool type") });
                            return Err(TerminalError);
                        }
                    }
                    Types::Float => {
                        if rhs_type != Types::Float {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch. Right operand must be of float type") });
                            return Err(TerminalError);
                        }
                    }
                    Types::String => {
                        if *op != RelationOp::Eq && *op != RelationOp::NotEq {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Operator not supported for operands of string type. Only == and != are supported for operands of string type") });
                            return Err(TerminalError);
                        }
                        if rhs_type != Types::String {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch. Right operand must be of string type") });
                            return Err(TerminalError);
                        }
                    }
                    _ => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Relational operators not supported for this operand type") });
                        return Err(TerminalError);
                    },
                }

                Ok(Types::Bool)
            },
            Ast::NegateOp { operand } => {
                match self.visit_ast(operand)? {
                    Types::Int => Ok(Types::Int),
                    Types::Float => Ok(Types::Float),
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => Ok(Types::Array(size, Box::new(Types::Int))),
                            Types::Float => Ok(Types::Array(size, Box::new(Types::Float))),
                            ty => {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Negation of an array can only be performed on integer or float arrays, found {ty} array") });
                                Err(TerminalError)
                            },
                        }
                    },
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Negation can only be performed on integer, float, integer array, or float array types, found {ty} type") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::SubscriptOp { array, index } => {
                match self.visit_ast(index)? {
                    Types::Int => (),
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Array index must be of integer type, found {ty} type") });
                        return Err(TerminalError);
                    },
                }

                match self.visit_ast(array)? {
                    Types::Array(_, base_type) => Ok(*base_type),
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Indexing can only be performed on array types, found {ty} type") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::ProcCall { proc, args } => {
                let proc_name = 
                    if let Ast::Var { ref id } = **proc {
                        id.clone()
                    } else {
                        String::from("")
                    };
                let proc_type = self.visit_ast(proc)?;
                let mut arg_types = Vec::new();
                for arg in args.iter_mut() {
                    arg_types.push(self.visit_ast(&mut *arg)?);
                }

                match proc_type {
                    Types::Proc(out_type, param_types) => {
                        let n_args = arg_types.len();
                        let n_params = param_types.len();
                        if n_args != n_params {
                            self.errs.push(CompilerError::Error { line: 1, msg: format!("Incorrect number of arguments for procedure {proc_name}. Expected {n_params}, found {n_args}.") });
                            return Err(TerminalError);
                        }

                        let mut is_type_mismatch = false;
                        for (i, (arg_type, param_type)) in arg_types.iter().zip(param_types.iter()).enumerate() {
                            if arg_type != param_type {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Type mismatch for argument {i} (0-indexed) in procedure call {proc_name}") });
                                is_type_mismatch = true;
                            }
                        }

                        if is_type_mismatch {
                            return Err(TerminalError);
                        }

                        Ok(*out_type)
                    }
                    _ => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("{proc_name} is not a procedure") });
                        return Err(TerminalError);
                    },
                }
            },
            Ast::IntLiteral { .. } => Ok(Types::Int),
            Ast::FloatLiteral { .. } => Ok(Types::Float),
            Ast::BoolLiteral { .. } => Ok(Types::Bool),
            Ast::StringLiteral { .. } => Ok(Types::String),
            Ast::Var { id } => {
                match self.st.get(id) {
                    Some(types) => Ok(types.clone()),
                    None => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Missing declaration for {id}") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::FloatToInt { operand } => {
                match self.visit_ast(operand)? {
                    Types::Int => Ok(Types::Int),
                    Types::Float => Ok(Types::Int),
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to integer") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::IntToFloat { operand } => {
                match self.visit_ast(operand)? {
                    Types::Int => Ok(Types::Float),
                    Types::Float => Ok(Types::Float),
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to float") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::BoolToInt { operand } => {
                match self.visit_ast(operand)? {
                    Types::Int => Ok(Types::Int),
                    Types::Bool => Ok(Types::Int),
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to integer") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::IntToBool { operand } => {
                match self.visit_ast(operand)? {
                    Types::Int => Ok(Types::Bool),
                    Types::Bool => Ok(Types::Bool),
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to bool") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::FloatArrayToIntArray { operand } => {
                match self.visit_ast(operand)? {
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => Ok(Types::Array(size, Box::new(Types::Int))),
                            Types::Float => Ok(Types::Array(size, Box::new(Types::Int))),
                            ty => {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to integer array") });
                                Err(TerminalError)
                            },
                        }
                    },
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to integer array") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::IntArrayToFloatArray { operand } => {
                match self.visit_ast(operand)? {
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => Ok(Types::Array(size, Box::new(Types::Float))),
                            Types::Float => Ok(Types::Array(size, Box::new(Types::Float))),
                            ty => {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to float array") });
                                Err(TerminalError)
                            },
                        }
                    },
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to float array") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::BoolArrayToIntArray { operand } => {
                match self.visit_ast(operand)? {
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => Ok(Types::Array(size, Box::new(Types::Int))),
                            Types::Bool => Ok(Types::Array(size, Box::new(Types::Int))),
                            ty => {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to integer array") });
                                Err(TerminalError)
                            },
                        }
                    },
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to integer array") });
                        Err(TerminalError)
                    },
                }
            },
            Ast::IntArrayToBoolArray { operand } => {
                match self.visit_ast(operand)? {
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => Ok(Types::Array(size, Box::new(Types::Bool))),
                            Types::Bool => Ok(Types::Array(size, Box::new(Types::Bool))),
                            ty => {
                                self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to bool array") });
                                Err(TerminalError)
                            },
                        }
                    },
                    ty => {
                        self.errs.push(CompilerError::Error { line: 1, msg: format!("Cannot convert {ty} to bool array") });
                        Err(TerminalError)
                    },
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typechecker_add_op_intint() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::IntLiteral { 
                value: 4,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_add_op_floatfloat() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_add_op_intfloat() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_add_op_floatint() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::IntLiteral { 
                value: 4,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 4,
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_add_op_intarrayintarray() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Int));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_add_op_floatarrayfloatarray() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_add_op_intarrayfloatarray() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::IntArrayToFloatArray {
                operand: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_add_op_floatarrayintarray() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::IntArrayToFloatArray {
                operand: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_add_op_err_invalidscalartype() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::StringLiteral { 
                value: String::from("this is a string"),
            }),
        });
        let mut tc = TypeChecker::new();

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {} type", Types::String)}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_add_op_err_invalidarraytype() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::String))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {} type", Types::Array(5, Box::new(Types::String)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_add_op_err_mismatchedarraylengths() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(3, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer array or float array types with matching sizes. {} != {}", Types::Array(5, Box::new(Types::Int)), Types::Array(3, Box::new(Types::Int)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_add_op_err_mixedscalarandarrayoperands() {
        let mut ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", Types::Int, Types::Array(5, Box::new(Types::Int)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_sub_op_intint() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::IntLiteral { 
                value: 4,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_sub_op_floatfloat() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_sub_op_intfloat() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_sub_op_floatint() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::IntLiteral { 
                value: 4,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 4,
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_sub_op_intarrayintarray() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Int));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_sub_op_floatarrayfloatarray() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_sub_op_intarrayfloatarray() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::IntArrayToFloatArray {
                operand: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_sub_op_floatarrayintarray() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::IntArrayToFloatArray {
                operand: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_sub_op_err_invalidscalartype() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::StringLiteral { 
                value: String::from("this is a string"),
            }),
        });
        let mut tc = TypeChecker::new();

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {} type", Types::String)}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_sub_op_err_invalidarraytype() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::String))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {} type", Types::Array(5, Box::new(Types::String)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_sub_op_err_mismatchedarraylengths() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(3, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer array or float array types with matching sizes. {} != {}", Types::Array(5, Box::new(Types::Int)), Types::Array(3, Box::new(Types::Int)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_sub_op_err_mixedscalarandarrayoperands() {
        let mut ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", Types::Int, Types::Array(5, Box::new(Types::Int)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_mul_op_intint() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::IntLiteral { 
                value: 4,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_mul_op_floatfloat() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_mul_op_intfloat() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_mul_op_floatint() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::IntLiteral { 
                value: 4,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 4,
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_mul_op_intarrayintarray() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Int));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_mul_op_floatarrayfloatarray() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_mul_op_intarrayfloatarray() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::IntArrayToFloatArray {
                operand: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_mul_op_floatarrayintarray() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::IntArrayToFloatArray {
                operand: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_mul_op_err_invalidscalartype() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::StringLiteral { 
                value: String::from("this is a string"),
            }),
        });
        let mut tc = TypeChecker::new();

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {} type", Types::String)}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_mul_op_err_invalidarraytype() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::String))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {} type", Types::Array(5, Box::new(Types::String)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_mul_op_err_mismatchedarraylengths() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(3, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer array or float array types with matching sizes. {} != {}", Types::Array(5, Box::new(Types::Int)), Types::Array(3, Box::new(Types::Int)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_mul_op_err_mixedscalarandarrayoperands() {
        let mut ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", Types::Int, Types::Array(5, Box::new(Types::Int)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_div_op_intint() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::IntLiteral { 
                value: 4,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            rhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 4,
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_div_op_floatfloat() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_div_op_intfloat() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            rhs: Box::new(Ast::FloatLiteral { 
                value: 4.3,
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_div_op_floatint() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::IntLiteral { 
                value: 4,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::FloatLiteral { 
                value: 5.2,
            }),
            rhs: Box::new(Ast::IntToFloat {
                operand: Box::new(Ast::IntLiteral { 
                    value: 4,
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_div_op_intarrayintarray() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Int));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_div_op_floatarrayfloatarray() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_div_op_intarrayfloatarray() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::IntArrayToFloatArray {
                operand: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_div_op_floatarrayintarray() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();
        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::IntArrayToFloatArray {
                operand: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
        });

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
        assert_eq!(ast, exp_ast);
    }

    #[test]
    fn typechecker_div_op_err_invalidscalartype() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::StringLiteral { 
                value: String::from("this is a string"),
            }),
        });
        let mut tc = TypeChecker::new();

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {} type", Types::String)}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_div_op_err_invalidarraytype() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(5, Box::new(Types::String))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer, float, integer array, or float array types, found {} type", Types::Array(5, Box::new(Types::String)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_div_op_err_mismatchedarraylengths() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.insert(String::from("b"), Types::Array(3, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Arithmetic operations can only be performed on integer array or float array types with matching sizes. {} != {}", Types::Array(5, Box::new(Types::Int)), Types::Array(3, Box::new(Types::Int)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_div_op_err_mixedscalarandarrayoperands() {
        let mut ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Cannot mix scalar and array operands in arithmetic operations, found {} and {}", Types::Int, Types::Array(5, Box::new(Types::Int)))}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_negate_op_int() {
        let mut ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_negate_op_float() {
        let mut ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::FloatLiteral { 
                value: 5.0,
            }),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_negate_op_intarray() {
        let mut ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Int));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_negate_op_floatarray() {
        let mut ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Float))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Array(5, Box::new(Types::Float));
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_negate_op_err_invalidscalartype() {
        let mut ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::StringLiteral { 
                value: String::from("this is a string"),
            }),
        });
        let mut tc = TypeChecker::new();

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Negation can only be performed on integer, float, integer array, or float array types, found {} type", Types::String)}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_negate_op_err_invalidarraytype() {
        let mut ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::String))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Negation of an array can only be performed on integer or float arrays, found {} array", Types::String)}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_subscript_op() {
        let mut ast = Box::new(Ast::SubscriptOp { 
            array: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            index: Box::new(Ast::IntLiteral {
                value: 0,
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_subscript_op_err_invalidbasetype() {
        let mut ast = Box::new(Ast::SubscriptOp { 
            array: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            index: Box::new(Ast::IntLiteral {
                value: 0,
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Int).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Indexing can only be performed on array types, found {} type", Types::Int)}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_subscript_op_err_invalidindextype() {
        let mut ast = Box::new(Ast::SubscriptOp { 
            array: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            index: Box::new(Ast::FloatLiteral {
                value: 1.0,
            }),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("a"), Types::Array(5, Box::new(Types::Int))).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Array index must be of integer type, found {} type", Types::Float)}
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_proc_call_noarg() {
        let mut ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: Vec::new(),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("foo"), Types::Proc(Box::new(Types::Int), Vec::new())).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_proc_call_singlearg() {
        let mut ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: vec![
                Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            ],
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("foo"), Types::Proc(Box::new(Types::Int), vec![Types::Int])).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_proc_call_multiarg() {
        let mut ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: vec![
                Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
                Box::new(Ast::BoolLiteral { 
                    value: true,
                }),
            ],
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("foo"), Types::Proc(Box::new(Types::Int), vec![Types::Int, Types::Bool])).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_proc_call_recursion() {
        let mut ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: Vec::new(),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert_global(String::from("foo"), Types::Proc(Box::new(Types::Int), Vec::new())).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.enter_scope(Types::Int);

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_proc_call_shadowedrecursion() {
        let mut ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: Vec::new(),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert_global(String::from("foo"), Types::Proc(Box::new(Types::Int), Vec::new())).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.enter_scope(Types::Int);
        tc.st.insert(String::from("foo"), Types::Proc(Box::new(Types::Bool), Vec::new())).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Bool;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_proc_call_err_invalidtype() {
        let mut ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: Vec::new(),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("foo"), Types::Int).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("{} is not a procedure", String::from("foo")) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_proc_call_err_invalidargcount() {
        let mut ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: Vec::new(),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("foo"), Types::Proc(Box::new(Types::Int), vec![Types::Int, Types::Bool])).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Incorrect number of arguments for procedure {}. Expected {}, found {}.", String::from("foo"), 2, 0) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_proc_call_err_mismatchedargtypes() {
        let mut ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: vec![
                Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
                Box::new(Ast::BoolLiteral { 
                    value: true,
                }),
            ],
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("foo"), Types::Proc(Box::new(Types::Int), vec![Types::Bool, Types::Int])).expect("SymTable insertion failed. Unable to setup test.");

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Type mismatch for argument {} (0-indexed) in procedure call {}", 0, String::from("foo")) },
            CompilerError::Error { line: 1, msg: format!("Type mismatch for argument {} (0-indexed) in procedure call {}", 1, String::from("foo")) },
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_int_literal() {
        let mut ast = Box::new(Ast::IntLiteral {
            value: 5,
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_float_literal() {
        let mut ast = Box::new(Ast::FloatLiteral {
            value: 9.3,
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Float;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_bool_literal() {
        let mut ast = Box::new(Ast::BoolLiteral {
            value: true,
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Bool;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_string_literal() {
        let mut ast = Box::new(Ast::StringLiteral {
            value: String::from("this is a string"),
        });
        let mut tc = TypeChecker::new();

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::String;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_var_global() {
        let mut ast = Box::new(Ast::Var {
            id: String::from("a"),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert_global(String::from("a"), Types::Int).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.enter_scope(Types::Int);
        tc.st.enter_scope(Types::Float);

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_var_local() {
        let mut ast = Box::new(Ast::Var {
            id: String::from("a"),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert_global(String::from("a"), Types::String).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.enter_scope(Types::Int);
        tc.st.insert(String::from("a"), Types::Float).expect("SymTable insertion failed. Unable to setup test.");
        tc.st.enter_scope(Types::Float);
        tc.st.insert(String::from("a"), Types::Int).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_var_caseinsensitive() {
        let mut ast = Box::new(Ast::Var {
            id: String::from("TmP"),
        });
        let mut tc = TypeChecker::new();
        tc.st.insert(String::from("tmp"), Types::Int).expect("SymTable insertion failed. Unable to setup test.");

        let act_type = ast.accept(&mut tc).expect("Type checking failed");
        let act_errs = tc.get_errors();

        let exp_type = Types::Int;
        let exp_errs = &Vec::new();

        assert_eq!(act_type, exp_type);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn typechecker_var_err_missingdecl() {
        let mut ast = Box::new(Ast::Var {
            id: String::from("a"),
        });
        let mut tc = TypeChecker::new();

        ast.accept(&mut tc).expect_err(format!("Type check successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = tc.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Missing declaration for {}", String::from("a")) }
        ];

        assert_eq!(act_errs, exp_errs);
    }
}
