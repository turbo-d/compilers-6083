use crate::ast::{Ast, AstVisitor, RelationOp};
use crate::symtable::SymTable;
use crate::types::Types;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};

pub struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub st: SymTable<(PointerValue<'ctx>, Types), FunctionValue<'ctx>>,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub fn new(context: &'ctx Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> CodeGen<'a, 'ctx> {
        let ret_type = context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(3)
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();
        let fn_type = context.f64_type().fn_type(args_types, false);

        let fn_val = module.add_function("main", fn_type, None);
        let st = SymTable::new(fn_val);

        // TODO: Delete this once runtime is finished.
        // This is just for testing
        module.add_function("getbool", fn_type, None);
        module.add_function("getinteger", fn_type, None);
        module.add_function("getfloat", fn_type, None);
        module.add_function("getstring", fn_type, None);
        module.add_function("putbool", fn_type, None);
        module.add_function("putinteger", fn_type, None);
        module.add_function("putfloat", fn_type, None);
        module.add_function("putstring", fn_type, None);
        module.add_function("sqrt", fn_type, None);

        CodeGen {
            context,
            builder,
            module,
            st,
        }
    }

    pub fn create_entry_block_alloca(&self, fn_val: &FunctionValue<'ctx>, name: &str, ty: Types) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = fn_val.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let ty = match ty {
            Types::Int => BasicTypeEnum::from(self.context.i64_type()),
            Types::Float => BasicTypeEnum::from(self.context.f64_type()),
            //Types::String => BasicTypeEnum::from(self.context.ptr_type(AddressSpace::default())), //TODO
            Types::String => BasicTypeEnum::from(self.context.f64_type()), //TODO
            Types::Bool => BasicTypeEnum::from(self.context.bool_type()),
            Types::Array(size, base_type) => {
                match *base_type {
                    Types::Int => BasicTypeEnum::from(self.context.i64_type().array_type(size)),
                    Types::Float => BasicTypeEnum::from(self.context.f64_type().array_type(size)),
                    //Types::String => cg.context.ptr_type(AddressSpace::default()).array_type(size).fn_type(args_types, false), //TODO
                    Types::String => BasicTypeEnum::from(self.context.f64_type().array_type(size)), //TODO
                    Types::Bool => BasicTypeEnum::from(self.context.bool_type().array_type(size)),
                    _ => panic!("Unexpected base type for array type"),
                }
            }
            _ => panic!("Unexpected procedure return type"),
        };

        builder.build_alloca(ty, name).unwrap()
    }
}

impl<'a, 'ctx> AstVisitor<AnyValueEnum<'ctx>> for CodeGen<'a, 'ctx> {
    fn visit_ast(&mut self, ast: &Ast) -> AnyValueEnum<'ctx> {
        match ast {
            Ast::Program { name, decls, body, } => {
                self.module.set_name(name.as_str());

                let args_types = Vec::<BasicMetadataTypeEnum>::new();
                let args_types = args_types.as_slice();
                let fn_type = self.context.i64_type().fn_type(args_types, false);
                let fn_val = self.module.add_function("main", fn_type, None);

                let entry = self.context.append_basic_block(fn_val, "entry");
                self.builder.position_at_end(entry);

                for decl in decls.iter() {
                    self.visit_ast(decl);
                }
                self.builder.position_at_end(entry);

                for stmt in body.iter() {
                    self.visit_ast(stmt);
                }

                AnyValueEnum::from(self.context.i64_type().const_int(0, false))
            }
            Ast::VarDecl { is_global, name, ty } => {
                let parent_fn = self.st.get_local_proc_data().clone();

                if *is_global {
                    let basic_type = match ty.clone() {
                        Types::Int => BasicTypeEnum::from(self.context.i64_type()),
                        Types::Float => BasicTypeEnum::from(self.context.f64_type()),
                        //Types::String => BasicTypeEnum::from(self.context.ptr_type(AddressSpace::default())), //TODO
                        Types::String => BasicTypeEnum::from(self.context.f64_type()), //TODO
                        Types::Bool => BasicTypeEnum::from(self.context.bool_type()),
                        Types::Array(size, base_type) => {
                            match *base_type {
                                Types::Int => BasicTypeEnum::from(self.context.i64_type().array_type(size)),
                                Types::Float => BasicTypeEnum::from(self.context.f64_type().array_type(size)),
                                //Types::String => self.context.ptr_type(AddressSpace::default()).array_type(size).fn_type(args_types, false), //TODO
                                Types::String => BasicTypeEnum::from(self.context.f64_type().array_type(size)), //TODO
                                Types::Bool => BasicTypeEnum::from(self.context.bool_type().array_type(size)),
                                _ => panic!("Unexpected base type for arrary type"),
                            }
                        }
                        _ => panic!("Unexpected procedure return type"),
                    };
                    let global = self.module.add_global(basic_type, Some(AddressSpace::default()), name.as_str());
                    let _  = self.st.insert_global(name.clone(), (global.as_pointer_value(), ty.clone()));
                    return AnyValueEnum::from(global.as_pointer_value())
                } else {
                    let alloca = self.create_entry_block_alloca(&parent_fn, name.as_str(), ty.clone());
                    let _ = self.st.insert(name.clone(), (alloca, ty.clone()));
                    return AnyValueEnum::from(alloca)
                }
            },
            Ast::ProcDecl { name, ty, params, decls, body, ..} => {
                let ret_type = match ty.clone() {
                    Types::Proc(ret, _) => ret,
                    _ => panic!("Expected Proc type"),
                };

                let args_types = params.iter().map(|p| {
                        let ty = match p {
                            Ast::VarDecl { ty, .. } => ty,
                            _ => panic!("Expected Ast::VarDecl for AST::ProcDecl params"),
                        };

                        match ty.clone() {
                            Types::Int => BasicMetadataTypeEnum::from(self.context.i64_type()),
                            Types::Float => BasicMetadataTypeEnum::from(self.context.f64_type()),
                            //Types::String => BasicMetadataTypeEnum::from(self.context.ptr_type(AddressSpace::default())), //TODO
                            Types::String => BasicMetadataTypeEnum::from(self.context.f64_type()), //TODO
                            Types::Bool => BasicMetadataTypeEnum::from(self.context.bool_type()),
                            Types::Array(size, base_type) => {
                                match *base_type {
                                    Types::Int => BasicMetadataTypeEnum::from(self.context.i64_type().array_type(size)),
                                    Types::Float => BasicMetadataTypeEnum::from(self.context.f64_type().array_type(size)),
                                    //Types::String => self.context.ptr_type(AddressSpace::default()).array_type(size).fn_type(args_types, false), //TODO
                                    Types::String => BasicMetadataTypeEnum::from(self.context.f64_type().array_type(size)), //TODO
                                    Types::Bool => BasicMetadataTypeEnum::from(self.context.bool_type().array_type(size)),
                                    _ => panic!("Unexpected base type for arrary type"),
                                }
                            }
                            _ => panic!("Unexpected procedure return type"),
                        }
                    })
                    .collect::<Vec<BasicMetadataTypeEnum>>();
                let args_types = args_types.as_slice();

                let fn_type = match *ret_type {
                    Types::Int => self.context.i64_type().fn_type(args_types, false),
                    Types::Float => self.context.f64_type().fn_type(args_types, false),
                    //Types::String => self.context.ptr_type(AddressSpace::default()).fn_type(args_types, false), //TODO
                    Types::String => self.context.f64_type().fn_type(args_types, false), //TODO
                    Types::Bool => self.context.bool_type().fn_type(args_types, false),
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => self.context.i64_type().array_type(size).fn_type(args_types, false),
                            Types::Float => self.context.f64_type().array_type(size).fn_type(args_types, false),
                            //Types::String => self.context.ptr_type(AddressSpace::default()).array_type(size).fn_type(args_types, false), //TODO
                            Types::String => self.context.f64_type().array_type(size).fn_type(args_types, false), //TODO
                            Types::Bool => self.context.bool_type().array_type(size).fn_type(args_types, false),
                            _ => panic!("Unexpected base type for arrary type"),
                        }
                    }
                    _ => panic!("Unexpected procedure return type"),
                };

                let fn_val = self.module.add_function(name.as_str(), fn_type, None);

                let entry = self.context.append_basic_block(fn_val, "entry");
                self.builder.position_at_end(entry);

                self.st.enter_scope(fn_val);
                //self.st.reserve(self.params.len());

                for (i, arg) in fn_val.get_param_iter().enumerate() {
                    let arg_name = match &params[i] {
                        Ast::VarDecl { name, .. } => name,
                        _ => panic!("Expected Ast::VarDecl for AST::ProcDecl params"),
                    };
                    arg.set_name(arg_name.as_str());

                    //let alloca = self.create_entry_block_alloca(&fn_val, arg_name);
                    let alloca = match PointerValue::try_from(self.visit_ast(&params[i])) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected PointerValue alloca."),
                    };

                    self.builder.build_store(alloca, arg).unwrap();
                }

                for decl in decls.iter() {
                    self.visit_ast(decl);
                }
                self.builder.position_at_end(entry);

                for stmt in body.iter() {
                    self.visit_ast(stmt);
                }

                self.st.exit_scope();

                if !fn_val.verify(true) {
                    unsafe {
                        fn_val.delete();
                    }

                    panic!("Invalid generated function.")
                }

                AnyValueEnum::from(fn_val)
            },
            Ast::AssignStmt { dest, expr } => {
                let name = match **dest {
                    Ast::Var { ref id } => id,
                    Ast::SubscriptOp { ref array, .. } => {
                        match **array {
                            Ast::Var { ref id } => id,
                            _ => panic!("Expected Ast::Var for AST::SubscriptOp array"),
                        }
                    },
                    _ => panic!("Expected Ast::Var or Ast::SubscriptOp for AST::AssignStmt dest"),
                };

                let alloca = match self.st.get(name) {
                    Some((var, _)) => var.clone(),
                    None => panic!("Identifer name not found"),
                };

                let val = match BasicValueEnum::try_from(self.visit_ast(expr)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected BasicValue in assignment."),
                };

                self.builder.build_store(alloca, val).unwrap();

                AnyValueEnum::from(val)
            },
            Ast::IfStmt { cond, then_body, else_body } => {
                let parent = self.st.get_local_proc_data().clone();

                let cond = match IntValue::try_from(self.visit_ast(cond)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected u1 type expression for if stmt conditional"),
                };

                // build branch
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder.build_conditional_branch(cond, then_bb, else_bb).unwrap();

                // build then block
                self.builder.position_at_end(then_bb);
                for stmt in then_body.iter() {
                    self.visit_ast(stmt);
                }
                self.builder.build_unconditional_branch(cont_bb).unwrap();
                //let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                for stmt in else_body.iter() {
                    self.visit_ast(stmt);
                }
                self.builder.build_unconditional_branch(cont_bb).unwrap();
                //let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);
                //let phi = self.builder.build_phi(self.context.f64_type(), "iftmp").unwrap();
                //phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                //AnyValueEnum::from(phi.as_basic_value().into_float_value())
                AnyValueEnum::from(self.context.i64_type().const_int(0, false))
            },
            Ast::LoopStmt { init, cond, body } => {
                let parent = self.st.get_local_proc_data().clone();

                self.visit_ast(init);

                // go from current block to loop block
                let loop_bb = self.context.append_basic_block(parent, "loop");
                self.builder.build_unconditional_branch(loop_bb).unwrap();
                self.builder.position_at_end(loop_bb);

                // emit body
                for stmt in body.iter() {
                    self.visit_ast(stmt);
                }

                // compile end condition
                let cond = match IntValue::try_from(self.visit_ast(cond)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected u1 type expression for loop stmt conditional"),
                };

                let after_bb = self.context.append_basic_block(parent, "afterloop");
                self.builder.build_conditional_branch(cond, loop_bb, after_bb).unwrap();
                self.builder.position_at_end(after_bb);

                AnyValueEnum::from(self.context.i64_type().const_int(0, false))
            },
            Ast::ReturnStmt { expr } => {
                let val = match BasicValueEnum::try_from(self.visit_ast(expr)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected BasicValue in assignment."),
                };

                AnyValueEnum::from(self.builder.build_return(Some(&val)).unwrap())
            },
            Ast::AndOp { lhs, rhs } => {
                let lhs = match IntValue::try_from(self.visit_ast(lhs)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };
                let rhs = match IntValue::try_from(self.visit_ast(rhs)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };

                AnyValueEnum::from(self.builder.build_and(lhs, rhs, "tmpand").unwrap())
            },
            Ast::OrOp { lhs, rhs } => {
                let lhs = match IntValue::try_from(self.visit_ast(lhs)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };
                let rhs = match IntValue::try_from(self.visit_ast(rhs)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };

                AnyValueEnum::from(self.builder.build_or(lhs, rhs, "tmpor").unwrap())
            },
            Ast::NotOp { operand } => {
                let val = match IntValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };

                AnyValueEnum::from(self.builder.build_not(val, "tmpnot").unwrap())
            },
            Ast::AddOp { lhs, rhs } => {
                let lhs = self.visit_ast(lhs);
                let rhs = self.visit_ast(rhs);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_int_add(lhs, rhs, "tmpadd").unwrap());
                } else if lhs.is_float_value() {
                    let lhs = match FloatValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };
                    let rhs = match FloatValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_float_add(lhs, rhs, "tmpadd").unwrap());
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::SubOp { lhs, rhs } => {
                let lhs = self.visit_ast(lhs);
                let rhs = self.visit_ast(rhs);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_int_sub(lhs, rhs, "tmpsub").unwrap());
                } else if lhs.is_float_value() {
                    let lhs = match FloatValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };
                    let rhs = match FloatValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_float_sub(lhs, rhs, "tmpsub").unwrap());
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::MulOp { lhs, rhs } => {
                let lhs = self.visit_ast(lhs);
                let rhs = self.visit_ast(rhs);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_int_mul(lhs, rhs, "tmpmul").unwrap());
                } else if lhs.is_float_value() {
                    let lhs = match FloatValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };
                    let rhs = match FloatValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_float_mul(lhs, rhs, "tmpmul").unwrap());
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::DivOp { lhs, rhs } => {
                let lhs = match FloatValue::try_from(self.visit_ast(lhs)) {
                    Ok(val) => val,
                    Err(_) => panic!("Arithmetic operations can only be performed on operands of float type"),
                };
                let rhs = match FloatValue::try_from(self.visit_ast(rhs)) {
                    Ok(val) => val,
                    Err(_) => panic!("Arithmetic operations can only be performed on operands of float type"),
                };

                AnyValueEnum::from(self.builder.build_float_div(lhs, rhs, "tmpdiv").unwrap())
            },
            Ast::Relation { op, lhs, rhs } => {
                let lhs = self.visit_ast(lhs);
                let rhs = self.visit_ast(rhs);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    let cmp = match op {
                        RelationOp::LT => self.builder.build_int_compare(IntPredicate::ULT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::LTE => self.builder.build_int_compare(IntPredicate::ULE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GT => self.builder.build_int_compare(IntPredicate::UGT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GTE => self.builder.build_int_compare(IntPredicate::UGE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::NotEq => self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "tmpcmp").unwrap(),
                    };

                    return AnyValueEnum::from(cmp);
                } else if lhs.is_float_value() {
                    let lhs = match FloatValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };
                    let rhs = match FloatValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };

                    let cmp = match op {
                        RelationOp::LT => self.builder.build_float_compare(FloatPredicate::ULT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::LTE => self.builder.build_float_compare(FloatPredicate::ULE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GT => self.builder.build_float_compare(FloatPredicate::UGT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GTE => self.builder.build_float_compare(FloatPredicate::UGE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::Eq => self.builder.build_float_compare(FloatPredicate::UEQ, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::NotEq => self.builder.build_float_compare(FloatPredicate::UNE, lhs, rhs, "tmpcmp").unwrap(),
                    };

                    return AnyValueEnum::from(cmp);
                }
                // TODO: Strings

                panic!("Relational operators not supported on these operands");
            },
            Ast::NegateOp { operand } => {
                let val = self.visit_ast(operand);

                if val.is_int_value() {
                    let val = match IntValue::try_from(val) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_int_neg(val, "tmpneg").unwrap());
                } else if val.is_float_value() {
                    let val = match FloatValue::try_from(val) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_float_neg(val, "tmpneg").unwrap());
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::SubscriptOp { .. } => {
                AnyValueEnum::from(self.context.i64_type().const_int(0, false))
            },
            Ast::ProcCall { proc, args } => {
                let id = match **proc {
                    Ast::Var { ref id } => id,
                    _ => panic!("Expected Ast::Var for proc"),
                };

                match self.module.get_function(id.as_str()) {
                    Some(fun) => {
                        let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

                        for arg in args.iter() {
                            let arg = self.visit_ast(arg);
                            let arg = match BasicMetadataValueEnum::try_from(arg) {
                                Ok(val) => val,
                                Err(_) => panic!("Expected BasicMetadataValueEnum in arg list."),
                            };
                            compiled_args.push(arg);
                        }

                        let argsv: Vec<BasicMetadataValueEnum> =
                            compiled_args.iter().by_ref().map(|&val| val.into()).collect();

                        match self.builder
                            .build_call(fun, argsv.as_slice(), "tmp")
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                        {
                            Some(value) => AnyValueEnum::from(value),
                            None => panic!("Invalid call produced."),
                        }
                    },
                    None => panic!("Unknown function."),
                }
            },
            Ast::IntLiteral { value } => AnyValueEnum::from(self.context.i64_type().const_int(*value as u64, false)),
            Ast::FloatLiteral { value } => AnyValueEnum::from(self.context.f64_type().const_float(*value as f64)),
            Ast::BoolLiteral { value } => AnyValueEnum::from(self.context.bool_type().const_int(*value as u64, false)),
            Ast::StringLiteral { .. } => AnyValueEnum::from(self.context.f64_type().const_float(0.0)),
            Ast::Var { id } => {
                match self.st.get(id) {
                    Some((var, ty)) => {
                        let ty = match ty {
                            Types::Int => BasicTypeEnum::from(self.context.i64_type()),
                            Types::Float => BasicTypeEnum::from(self.context.f64_type()),
                            //Types::String => BasicTypeEnum::from(self.context.ptr_type(AddressSpace::default())), //TODO
                            Types::String => BasicTypeEnum::from(self.context.f64_type()), //TODO
                            Types::Bool => BasicTypeEnum::from(self.context.bool_type()),
                            Types::Array(size, base_type) => {
                                match **base_type {
                                    Types::Int => BasicTypeEnum::from(self.context.i64_type().array_type(*size)),
                                    Types::Float => BasicTypeEnum::from(self.context.f64_type().array_type(*size)),
                                    //Types::String => self.context.ptr_type(AddressSpace::default()).array_type(*size).fn_type(args_types, false), //TODO
                                    Types::String => BasicTypeEnum::from(self.context.f64_type().array_type(*size)), //TODO
                                    Types::Bool => BasicTypeEnum::from(self.context.bool_type().array_type(*size)),
                                    _ => panic!("Unexpected base type for arrary type"),
                                }
                            }
                            _ => panic!("Unexpected procedure return type"),
                        };
                        AnyValueEnum::from(self.builder.build_load(ty, *var, id.as_str()).unwrap())
                    }
                    None => panic!("Identifer name not found"),
                }
            },
        }
    }
}

