use crate::ast::*; //TODO
use crate::symtable::SymTable;
use crate::types::Types;

pub struct TypeChecker {
    st: SymTable,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        let mut st = SymTable::new();
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
            st: st,
        }
    }
}

impl ASTVisitor<Types> for TypeChecker {
    fn visit_program(&self, p: &Program) -> Types {
        Types::Unknown
    }

    fn visit_var_decl(&self, d: &VarDecl) -> Types {
        Types::Unknown
    }

    fn visit_proc_decl(&self, d: &ProcDecl) -> Types {
        Types::Unknown
    }

    fn visit_assign_stmt(&self, s: &AssignStmt) -> Types {
        Types::Unknown
    }

    fn visit_if_stmt(&self, s: &IfStmt) -> Types {
        Types::Unknown
    }

    fn visit_loop_stmt(&self, s: &LoopStmt) -> Types {
        Types::Unknown
    }

    fn visit_return_stmt(&self, s: &ReturnStmt) -> Types {
        Types::Unknown
    }

    fn visit_and_op(&self, s: &AndOp) -> Types {
        Types::Unknown
    }

    fn visit_or_op(&self, s: &OrOp) -> Types {
        Types::Unknown
    }

    fn visit_not_op(&self, s: &NotOp) -> Types {
        Types::Unknown
    }

    fn visit_add_op(&self, s: &AddOp) -> Types {
        Types::Unknown
    }

    fn visit_sub_op(&self, s: &SubOp) -> Types {
        Types::Unknown
    }

    fn visit_mul_op(&self, s: &MulOp) -> Types {
        Types::Unknown
    }

    fn visit_div_op(&self, s: &DivOp) -> Types {
        Types::Unknown
    }

    fn visit_relation(&self, s: &Relation) -> Types {
        Types::Unknown
    }

    fn visit_negate_op(&self, s: &NegateOp) -> Types {
        Types::Unknown
    }

    fn visit_subscript_op(&self, op: &SubscriptOp) -> Types {
        let array_type = op.array.visit(self);
        let expr_type = op.index.visit(self);

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

    fn visit_proc_call(&self, pc: &ProcCall) -> Types {
        let proc_type = pc.proc.visit(self);
        let mut arg_types = Vec::new();
        for arg in pc.args {
            arg_types.push(arg.visit(self));
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

    fn visit_int_literal(&self, s: &IntLiteral) -> Types {
        Types::Unknown
    }

    fn visit_float_literal(&self, s: &FloatLiteral) -> Types {
        Types::Unknown
    }

    fn visit_string_literal(&self, s: &StringLiteral) -> Types {
        Types::Unknown
    }

    fn visit_bool_literal(&self, s: &BoolLiteral) -> Types {
        Types::Unknown
    }

    fn visit_var(&self, var: &Var) -> Types {
        let parsed_type: Types;
        match self.st.get(var.id) {
            Some(types) => {
                parsed_type = types.clone();
            }
            None => panic!("Missing declaration for {}", var.id),
        }
        parsed_type
    }
}
