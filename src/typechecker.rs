use crate::symtable::SymTable;
use crate::types::Types;

pub struct TypeChecker {
    st: SymTable,
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
        let array_type = op.array.visit(&self);
        let expr_type = op.index.visit(&self);

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

    fn visit_proc_call(&self, s: &ProcCall) -> Types {
        Types::Unknown
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
            None => panic!("Missing declaration for {id}"),
        }
        parsed_type
    }
}
