use crate::ast::{Ast, AstVisitor, RelationOp};
use crate::symtable::SymTable;
use crate::types::Types;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{
    AnyValueEnum,
    BasicMetadataValueEnum,
    BasicValue,
    BasicValueEnum,
    FloatValue,
    FunctionValue,
    IntValue,
    InstructionValue,
    PointerValue,
    VectorValue
};

pub struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub var_st: SymTable<(PointerValue<'ctx>, Types), FunctionValue<'ctx>>,
    pub fn_st: SymTable<FunctionValue<'ctx>, FunctionValue<'ctx>>,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub fn new(context: &'ctx Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> CodeGen<'a, 'ctx> {
        let args_types = Vec::<BasicMetadataTypeEnum>::new();
        let args_types = args_types.as_slice();
        let fn_type = context.i64_type().fn_type(args_types, false);
        let fn_val = module.add_function("main", fn_type, None);

        let var_st = SymTable::new(fn_val);

        let mut fn_st = SymTable::new(fn_val);

        let args_types = Vec::<BasicMetadataTypeEnum>::new();
        let args_types = args_types.as_slice();

        let fn_type = context.bool_type().fn_type(args_types, false);
        let fn_val = module.add_function("getbool", fn_type, None);
        let _ = fn_st.insert(String::from("getbool"), fn_val);

        let fn_type = context.i64_type().fn_type(args_types, false);
        let fn_val = module.add_function("getinteger", fn_type, None);
        let _ = fn_st.insert(String::from("getinteger"), fn_val);

        let fn_type = context.i8_type().ptr_type(AddressSpace::default()).fn_type(args_types, false);
        let fn_val = module.add_function("getstring", fn_type, None);
        let _ = fn_st.insert(String::from("getstring"), fn_val);

        let fn_type = context.f64_type().fn_type(args_types, false);
        let fn_val = module.add_function("getfloat", fn_type, None);
        let _ = fn_st.insert(String::from("getfloat"), fn_val);

        let args_types = vec![BasicMetadataTypeEnum::from(context.bool_type())];
        let args_types = args_types.as_slice();
        let fn_type = context.bool_type().fn_type(args_types, false);
        let fn_val = module.add_function("putbool", fn_type, None);
        let _ = fn_st.insert(String::from("putbool"), fn_val);

        let args_types = vec![BasicMetadataTypeEnum::from(context.i64_type())];
        let args_types = args_types.as_slice();
        let fn_type = context.bool_type().fn_type(args_types, false);
        let fn_val = module.add_function("putinteger", fn_type, None);
        let _ = fn_st.insert(String::from("putinteger"), fn_val);

        let args_types = vec![BasicMetadataTypeEnum::from(context.i8_type().ptr_type(AddressSpace::default()))];
        let args_types = args_types.as_slice();
        let fn_type = context.bool_type().fn_type(args_types, false);
        let fn_val = module.add_function("putstring", fn_type, None);
        let _ = fn_st.insert(String::from("putstring"), fn_val);

        let args_types = vec![BasicMetadataTypeEnum::from(context.f64_type())];
        let args_types = args_types.as_slice();
        let fn_type = context.bool_type().fn_type(args_types, false);
        let fn_val = module.add_function("putfloat", fn_type, None);
        let _ = fn_st.insert(String::from("putfloat"), fn_val);

        let args_types = vec![BasicMetadataTypeEnum::from(context.i64_type())];
        let args_types = args_types.as_slice();
        let fn_type = context.f64_type().fn_type(args_types, false);
        let fn_val = module.add_function("sqrt", fn_type, None);
        let _ = fn_st.insert(String::from("sqrt"), fn_val);

        let args_types = vec![BasicMetadataTypeEnum::from(context.i8_type().ptr_type(AddressSpace::default())), BasicMetadataTypeEnum::from(context.i8_type().ptr_type(AddressSpace::default()))];
        let args_types = args_types.as_slice();
        let fn_type = context.bool_type().fn_type(args_types, false);
        let fn_val = module.add_function("strcmp", fn_type, None);
        let _ = fn_st.insert(String::from("strcmp"), fn_val);


        CodeGen {
            context,
            builder,
            module,
            var_st,
            fn_st,
        }
    }

    fn create_entry_block_alloca(&self, fn_val: &FunctionValue<'ctx>, name: &str, ty: Types) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = fn_val.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let ty = match ty {
            Types::Int => BasicTypeEnum::from(self.context.i64_type()),
            Types::Float => BasicTypeEnum::from(self.context.f64_type()),
            Types::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default())),
            Types::Bool => BasicTypeEnum::from(self.context.bool_type()),
            Types::Array(size, base_type) => {
                match *base_type {
                    Types::Int => BasicTypeEnum::from(self.context.i64_type().vec_type(size)),
                    Types::Float => BasicTypeEnum::from(self.context.f64_type().vec_type(size)),
                    Types::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default()).vec_type(size)),
                    Types::Bool => BasicTypeEnum::from(self.context.bool_type().vec_type(size)),
                    _ => panic!("Unexpected base type for array type"),
                }
            }
            _ => panic!("Unexpected procedure return type"),
        };

        builder.build_alloca(ty, name).unwrap()
    }

    fn get_default_value(&self, ty: &Types) -> Box<dyn BasicValue<'ctx> + 'ctx> {
        match ty {
            Types::Int => Box::new(self.context.i64_type().const_zero()),
            Types::Float => Box::new(self.context.f64_type().const_zero()),
            Types::String => Box::new(self.context.i8_type().ptr_type(AddressSpace::default()).const_zero()),
            Types::Bool => Box::new(self.context.bool_type().const_zero()),
            Types::Array(size, base_type) => {
                match **base_type {
                    Types::Int => Box::new(self.context.i64_type().vec_type(*size).const_zero()),
                    Types::Float => Box::new(self.context.f64_type().vec_type(*size).const_zero()),
                    Types::String => Box::new(self.context.i8_type().ptr_type(AddressSpace::default()).vec_type(*size).const_zero()),
                    Types::Bool => Box::new(self.context.bool_type().vec_type(*size).const_zero()),
                    _ => panic!("No default value for {ty}"),
                }
            }
            _ => panic!("No default value for {ty}"),
        }
    }
}

impl<'a, 'ctx> AstVisitor<AnyValueEnum<'ctx>> for CodeGen<'a, 'ctx> {
    fn visit_ast(&mut self, ast: &mut Ast) -> AnyValueEnum<'ctx> {
        match ast {
            Ast::Program { name, decls, body, } => {
                self.module.set_name(name.as_str());

                let fn_val = self.module.get_function("main").expect("main function not found");
                let entry = self.context.append_basic_block(fn_val, "entry");
                self.builder.position_at_end(entry);

                for decl in decls.iter_mut() {
                    self.visit_ast(&mut *decl);
                }
                self.builder.position_at_end(entry);

                for stmt in body.iter_mut() {
                    self.visit_ast(&mut *stmt);
                }

                AnyValueEnum::from(self.builder.build_return(Some(&self.context.i64_type().const_int(0, false))).unwrap())
            }
            Ast::VarDecl { is_global, name, ty } => {
                let parent_fn = self.var_st.get_local_proc_data().clone();

                let basic_type = match ty.clone() {
                    Types::Int => BasicTypeEnum::from(self.context.i64_type()),
                    Types::Float => BasicTypeEnum::from(self.context.f64_type()),
                    Types::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default())),
                    Types::Bool => BasicTypeEnum::from(self.context.bool_type()),
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => BasicTypeEnum::from(self.context.i64_type().vec_type(size)),
                            Types::Float => BasicTypeEnum::from(self.context.f64_type().vec_type(size)),
                            Types::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default()).vec_type(size)),
                            Types::Bool => BasicTypeEnum::from(self.context.bool_type().vec_type(size)),
                            _ => panic!("Unexpected base type for arrary type"),
                        }
                    }
                    _ => panic!("Unexpected procedure return type"),
                };

                if *is_global || self.var_st.is_in_global_scope() {
                    let global = self.module.add_global(basic_type, Some(AddressSpace::default()), name.to_lowercase().as_str());
                    global.set_initializer(&(*self.get_default_value(ty)));
                    let _  = self.var_st.insert_global(name.clone().to_lowercase(), (global.as_pointer_value(), ty.clone()));
                    return AnyValueEnum::from(global.as_pointer_value())
                } else {
                    let alloca = self.create_entry_block_alloca(&parent_fn, name.to_lowercase().as_str(), ty.clone());
                    let _ = self.var_st.insert(name.clone().to_lowercase(), (alloca, ty.clone()));
                    return AnyValueEnum::from(alloca)
                }
            },
            Ast::ProcDecl { name, ty, params, decls, body, ..} => {
                let ret_type = match ty.clone() {
                    Types::Proc(ret, _) => ret,
                    _ => panic!("Expected Proc type"),
                };

                let args_types = params.iter().map(|p| {
                        let ty = match **p {
                            Ast::VarDecl { ref ty, .. } => ty,
                            _ => panic!("Expected Ast::VarDecl for AST::ProcDecl params"),
                        };

                        match ty.clone() {
                            Types::Int => BasicMetadataTypeEnum::from(self.context.i64_type()),
                            Types::Float => BasicMetadataTypeEnum::from(self.context.f64_type()),
                            Types::String => BasicMetadataTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default())),
                            Types::Bool => BasicMetadataTypeEnum::from(self.context.bool_type()),
                            Types::Array(size, base_type) => {
                                match *base_type {
                                    Types::Int => BasicMetadataTypeEnum::from(self.context.i64_type().vec_type(size)),
                                    Types::Float => BasicMetadataTypeEnum::from(self.context.f64_type().vec_type(size)),
                                    Types::String => BasicMetadataTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default()).vec_type(size)),
                                    Types::Bool => BasicMetadataTypeEnum::from(self.context.bool_type().vec_type(size)),
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
                    Types::String => self.context.i8_type().ptr_type(AddressSpace::default()).fn_type(args_types, false),
                    Types::Bool => self.context.bool_type().fn_type(args_types, false),
                    Types::Array(size, base_type) => {
                        match *base_type {
                            Types::Int => self.context.i64_type().vec_type(size).fn_type(args_types, false),
                            Types::Float => self.context.f64_type().vec_type(size).fn_type(args_types, false),
                            Types::String => self.context.i8_type().ptr_type(AddressSpace::default()).vec_type(size).fn_type(args_types, false),
                            Types::Bool => self.context.bool_type().vec_type(size).fn_type(args_types, false),
                            _ => panic!("Unexpected base type for arrary type"),
                        }
                    }
                    _ => panic!("Unexpected procedure return type"),
                };

                let fn_val = self.module.add_function(name.to_lowercase().as_str(), fn_type, None);
                let _ = self.fn_st.insert(name.to_lowercase(), fn_val);

                let entry = self.context.append_basic_block(fn_val, "entry");
                self.builder.position_at_end(entry);

                self.var_st.enter_scope(fn_val);
                self.fn_st.enter_scope(fn_val);
                //self.var_st.reserve(self.params.len());

                for (i, arg) in fn_val.get_param_iter().enumerate() {
                    let arg_name = match *params[i] {
                        Ast::VarDecl { ref name, .. } => name,
                        _ => panic!("Expected Ast::VarDecl for AST::ProcDecl params"),
                    };
                    arg.set_name(arg_name.as_str());

                    //let alloca = self.create_entry_block_alloca(&fn_val, arg_name);
                    let alloca = match PointerValue::try_from(self.visit_ast(&mut params[i])) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected PointerValue alloca."),
                    };

                    self.builder.build_store(alloca, arg).unwrap();
                }

                for decl in decls.iter_mut() {
                    self.visit_ast(&mut *decl);
                }
                self.builder.position_at_end(entry);

                for stmt in body.iter_mut() {
                    self.visit_ast(&mut *stmt);
                }

                self.fn_st.exit_scope();
                self.var_st.exit_scope();

                //if !fn_val.verify(true) {
                //    unsafe {
                //        fn_val.delete();
                //    }

                //    panic!("Invalid generated function.")
                //}

                AnyValueEnum::from(fn_val)
            },
            Ast::AssignStmt { dest, expr } => {
                let name = match **dest {
                    Ast::Var { ref id } => id.clone(),
                    Ast::SubscriptOp { ref array, .. } => {
                        match **array {
                            Ast::Var { ref id } => id.clone(),
                            _ => panic!("Expected Ast::Var for AST::SubscriptOp array"),
                        }
                    },
                    _ => panic!("Expected Ast::Var or Ast::SubscriptOp for AST::AssignStmt dest"),
                };

                let (alloca, ty) = match self.var_st.get(&name.to_lowercase()) {
                    Some((var, ty)) => {
                        let ty = match ty {
                            Types::Int => BasicTypeEnum::from(self.context.i64_type()),
                            Types::Float => BasicTypeEnum::from(self.context.f64_type()),
                            Types::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default())),
                            Types::Bool => BasicTypeEnum::from(self.context.bool_type()),
                            Types::Array(size, base_type) => {
                                match **base_type {
                                    Types::Int => BasicTypeEnum::from(self.context.i64_type().vec_type(*size)),
                                    Types::Float => BasicTypeEnum::from(self.context.f64_type().vec_type(*size)),
                                    Types::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default()).vec_type(*size)),
                                    Types::Bool => BasicTypeEnum::from(self.context.bool_type().vec_type(*size)),
                                    _ => panic!("Unexpected base type for arrary type"),
                                }
                            }
                            _ => panic!("Unexpected procedure return type"),
                        };

                        (var.clone(), ty)
                    },
                    None => panic!("Identifier {name} not found"),
                };

                let val = match BasicValueEnum::try_from(self.visit_ast(expr)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected BasicValue in assignment."),
                };

                if let Ast::SubscriptOp { ref mut index, .. } = &mut **dest {
                    let array = AnyValueEnum::from(self.builder.build_load(ty, alloca, name.as_str()).unwrap());
                    let array = match VectorValue::try_from(array) {
                        Ok(val) => val,
                        Err(_) => panic!("Subscript operation can only be performed on array types"),
                    };
                    let index = match IntValue::try_from(self.visit_ast(index)) {
                        Ok(val) => val,
                        Err(_) => panic!("Subscript operation index must be an integer type"),
                    };
                    let vec_val = self.builder.build_insert_element(array, val, index, "tmpsubscript").unwrap();
                    self.builder.build_store(alloca, vec_val).unwrap();
                    AnyValueEnum::from(vec_val)
                } else {
                    self.builder.build_store(alloca, val).unwrap();
                    AnyValueEnum::from(val)
                }
            },
            Ast::IfStmt { cond, then_body, else_body } => {
                let parent = self.var_st.get_local_proc_data().clone();

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
                let mut inst_val: Option<InstructionValue> = None;
                for stmt in then_body.iter_mut() {
                    inst_val = InstructionValue::try_from(self.visit_ast(&mut *stmt)).ok();
                }
                match inst_val {
                    None => self.builder.build_unconditional_branch(cont_bb).unwrap(),
                    Some(iv) if !iv.is_terminator() => self.builder.build_unconditional_branch(cont_bb).unwrap(),
                    Some(iv) => iv,
                };
                //let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let mut inst_val: Option<InstructionValue> = None;
                for stmt in else_body.iter_mut() {
                    inst_val = InstructionValue::try_from(self.visit_ast(&mut *stmt)).ok();
                }
                match inst_val {
                    None => self.builder.build_unconditional_branch(cont_bb).unwrap(),
                    Some(iv) if !iv.is_terminator() => self.builder.build_unconditional_branch(cont_bb).unwrap(),
                    Some(iv) => iv,
                };
                //let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);
                //let phi = self.builder.build_phi(self.context.f64_type(), "iftmp").unwrap();
                //phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                //AnyValueEnum::from(phi.as_basic_value().into_float_value())
                AnyValueEnum::from(self.context.i64_type().const_int(0, false))
            },
            Ast::LoopStmt { init, cond, body } => {
                let parent = self.var_st.get_local_proc_data().clone();

                self.visit_ast(init);

                let loopcond_bb = self.context.append_basic_block(parent, "loopcond");
                let loop_bb = self.context.append_basic_block(parent, "loop");
                let after_bb = self.context.append_basic_block(parent, "afterloop");

                self.builder.build_unconditional_branch(loopcond_bb).unwrap();

                self.builder.position_at_end(loopcond_bb);
                let cond = match IntValue::try_from(self.visit_ast(cond)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected u1 type expression for loop stmt conditional"),
                };
                self.builder.build_conditional_branch(cond, loop_bb, after_bb).unwrap();

                self.builder.position_at_end(loop_bb);
                for stmt in body.iter_mut() {
                    self.visit_ast(&mut *stmt);
                }
                self.builder.build_unconditional_branch(loopcond_bb).unwrap();

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
                let lhs = self.visit_ast(lhs);
                let rhs = self.visit_ast(rhs);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                    };

                    return AnyValueEnum::from(self.builder.build_and(lhs, rhs, "tmpand").unwrap())
                } else if lhs.is_vector_value() {
                    let lhs = match VectorValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };
                    let rhs = match VectorValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_and(lhs, rhs, "tmpand").unwrap())
                }

                panic!("Bitwise operations can only be performed on operands of integer type");
            },
            Ast::OrOp { lhs, rhs } => {
                let lhs = self.visit_ast(lhs);
                let rhs = self.visit_ast(rhs);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                    };

                    return AnyValueEnum::from(self.builder.build_or(lhs, rhs, "tmpor").unwrap());
                } else if lhs.is_vector_value() {
                    let lhs = match VectorValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };
                    let rhs = match VectorValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_or(lhs, rhs, "tmpor").unwrap());
                }

                panic!("Bitwise operations can only be performed on operands of integer type");
            },
            Ast::NotOp { operand } => {
                let operand = self.visit_ast(operand);

                if operand.is_int_value() {
                    let val = match IntValue::try_from(operand) {
                        Ok(val) => val,
                        Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                    };

                    return AnyValueEnum::from(self.builder.build_not(val, "tmpnot").unwrap())
                } else if operand.is_vector_value() {
                    let val = match VectorValue::try_from(operand) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_not(val, "tmpnot").unwrap())
                }

                panic!("Bitwise operations can only be performed on operands of integer type");
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
                } else if lhs.is_vector_value() {
                    let lhs = match VectorValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };
                    let rhs = match VectorValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    let lhs_elem_type = lhs.get_type().get_element_type();
                    let rhs_elem_type = rhs.get_type().get_element_type();
                    if lhs_elem_type.is_int_type() {
                        assert!(rhs_elem_type.is_int_type());
                        return AnyValueEnum::from(self.builder.build_int_add(lhs, rhs, "tmpadd").unwrap());
                    } else if lhs_elem_type.is_float_type() {
                        assert!(rhs_elem_type.is_float_type());
                        return AnyValueEnum::from(self.builder.build_float_add(lhs, rhs, "tmpadd").unwrap());
                    }
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
                } else if lhs.is_vector_value() {
                    let lhs = match VectorValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };
                    let rhs = match VectorValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    let lhs_elem_type = lhs.get_type().get_element_type();
                    let rhs_elem_type = rhs.get_type().get_element_type();
                    if lhs_elem_type.is_int_type() {
                        assert!(rhs_elem_type.is_int_type());
                        return AnyValueEnum::from(self.builder.build_int_sub(lhs, rhs, "tmpsub").unwrap());
                    } else if lhs_elem_type.is_float_type() {
                        assert!(rhs_elem_type.is_float_type());
                        return AnyValueEnum::from(self.builder.build_float_sub(lhs, rhs, "tmpsub").unwrap());
                    }
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
                } else if lhs.is_vector_value() {
                    let lhs = match VectorValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };
                    let rhs = match VectorValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    let lhs_elem_type = lhs.get_type().get_element_type();
                    let rhs_elem_type = rhs.get_type().get_element_type();
                    if lhs_elem_type.is_int_type() {
                        assert!(rhs_elem_type.is_int_type());
                        return AnyValueEnum::from(self.builder.build_int_mul(lhs, rhs, "tmpmul").unwrap());
                    } else if lhs_elem_type.is_float_type() {
                        assert!(rhs_elem_type.is_float_type());
                        return AnyValueEnum::from(self.builder.build_float_mul(lhs, rhs, "tmpmul").unwrap());
                    }
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::DivOp { lhs, rhs } => {
                let lhs = self.visit_ast(lhs);
                let rhs = self.visit_ast(rhs);

                if lhs.is_float_value() {
                    let lhs = match FloatValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };
                    let rhs = match FloatValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_float_div(lhs, rhs, "tmpdiv").unwrap())
                } else if lhs.is_vector_value() {
                    let lhs = match VectorValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };
                    let rhs = match VectorValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    return AnyValueEnum::from(self.builder.build_float_div(lhs, rhs, "tmpdiv").unwrap())
                }

                panic!("Arithmetic operations can only be performed on operands of float type");
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
                        RelationOp::LT => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::LTE => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GT => self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GTE => self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "tmpcmp").unwrap(),
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
                } else if lhs.is_pointer_value() {
                    let lhs = match PointerValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected PointerValue"),
                    };
                    let rhs = match PointerValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected PointerValue"),
                    };

                    match self.module.get_function("strcmp") {
                        Some(fun) => {
                            let compiled_args = vec![
                                BasicMetadataValueEnum::try_from(lhs).expect("Expected BaseMetadataValueEnum in arg list"),
                                BasicMetadataValueEnum::try_from(rhs).expect("Expected BaseMetadataValueEnum in arg list"),
                            ];

                            let argsv: Vec<BasicMetadataValueEnum> =
                                compiled_args.iter().by_ref().map(|&val| val.into()).collect();

                            let ret_val = match self.builder
                                .build_call(fun, argsv.as_slice(), "tmp")
                                .unwrap()
                                .try_as_basic_value()
                                .left()
                            {
                                Some(value) => AnyValueEnum::from(value),
                                None => panic!("Invalid call produced."),
                            };

                            if let RelationOp::NotEq = op {
                                let bool_ret_val = match IntValue::try_from(ret_val) {
                                    Ok(val) => val,
                                    Err(_) => panic!("Expected IntValue"),
                                };
                                return AnyValueEnum::from(self.builder.build_not(bool_ret_val, "tmpnot").unwrap());
                            } else {
                                return ret_val;
                            }
                        },
                        None => panic!("Unknown function strcmp."),
                    }
                } else if lhs.is_vector_value() {
                    let lhs = match VectorValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };
                    let rhs = match VectorValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    let lhs_elem_type = lhs.get_type().get_element_type();
                    let rhs_elem_type = rhs.get_type().get_element_type();
                    if lhs_elem_type.is_int_type() {
                        assert!(rhs_elem_type.is_int_type());
                        let cmp = match op {
                            RelationOp::LT => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "tmpcmp").unwrap(),
                            RelationOp::LTE => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "tmpcmp").unwrap(),
                            RelationOp::GT => self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpcmp").unwrap(),
                            RelationOp::GTE => self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "tmpcmp").unwrap(),
                            RelationOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpcmp").unwrap(),
                            RelationOp::NotEq => self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "tmpcmp").unwrap(),
                        };

                        return AnyValueEnum::from(cmp);
                    } else if lhs_elem_type.is_float_type() {
                        assert!(rhs_elem_type.is_float_type());
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
                }

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
                } else if val.is_vector_value() {
                    let val = match VectorValue::try_from(val) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected VectorValue"),
                    };

                    let val_elem_type = val.get_type().get_element_type();
                    if val_elem_type.is_int_type() {
                        return AnyValueEnum::from(self.builder.build_int_neg(val, "tmpneg").unwrap());
                    } else if val_elem_type.is_float_type() {
                        return AnyValueEnum::from(self.builder.build_float_neg(val, "tmpneg").unwrap());
                    }
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");

            },
            Ast::SubscriptOp { array, index } => {
                let array = match VectorValue::try_from(self.visit_ast(array)) {
                    Ok(val) => val,
                    Err(_) => panic!("Subscript operation can only be performed on array types"),
                };
                let index = match IntValue::try_from(self.visit_ast(index)) {
                    Ok(val) => val,
                    Err(_) => panic!("Subscript operation index must be an integer type"),
                };
                AnyValueEnum::from(self.builder.build_extract_element(array, index, "tmpsubscript").unwrap())
            },
            Ast::ProcCall { proc, args } => {
                let id = match **proc {
                    Ast::Var { ref id } => id,
                    _ => panic!("Expected Ast::Var for proc"),
                };

                let fn_name = match self.fn_st.get(&id.to_lowercase()) {
                    Some(fn_val) => fn_val.get_name().to_str().expect("Unable to parse CStr to Str"),
                    _ => panic!("Function not found in module"),
                };

                match self.module.get_function(fn_name) {
                    Some(fun) => {
                        let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

                        for arg in args.iter_mut() {
                            let arg = self.visit_ast(&mut *arg);
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
                    None => panic!("Unknown function {id}."),
                }
            },
            Ast::IntLiteral { value } => AnyValueEnum::from(self.context.i64_type().const_int(*value as u64, false)),
            Ast::FloatLiteral { value } => AnyValueEnum::from(self.context.f64_type().const_float(*value as f64)),
            Ast::BoolLiteral { value } => AnyValueEnum::from(self.context.bool_type().const_int(*value as u64, false)),
            Ast::StringLiteral { value } => {
                let val = self.context.const_string(value.as_bytes(), true);
                let global = self.module.add_global(val.get_type(), Some(AddressSpace::default()), "str");
                global.set_constant(true);
                global.set_initializer(&val);
                global.set_linkage(Linkage::Private);
                global.set_unnamed_addr(true);
                let base_index = self.context.i64_type().const_zero();
                unsafe {
                    AnyValueEnum::from(self.builder.build_gep(val.get_type(), global.as_pointer_value(), &[base_index], "tmpgep").unwrap())
                }
            },
            Ast::Var { id } => {
                match self.var_st.get(&id.to_lowercase()) {
                    Some((var, ty)) => {
                        let ty = match ty {
                            Types::Int => BasicTypeEnum::from(self.context.i64_type()),
                            Types::Float => BasicTypeEnum::from(self.context.f64_type()),
                            Types::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default())),
                            Types::Bool => BasicTypeEnum::from(self.context.bool_type()),
                            Types::Array(size, base_type) => {
                                match **base_type {
                                    Types::Int => BasicTypeEnum::from(self.context.i64_type().vec_type(*size)),
                                    Types::Float => BasicTypeEnum::from(self.context.f64_type().vec_type(*size)),
                                    Types::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::default()).vec_type(*size)),
                                    Types::Bool => BasicTypeEnum::from(self.context.bool_type().vec_type(*size)),
                                    _ => panic!("Unexpected base type for arrary type"),
                                }
                            }
                            _ => panic!("Unexpected procedure return type"),
                        };
                        AnyValueEnum::from(self.builder.build_load(ty, *var, id.as_str()).unwrap())
                    }
                    None => panic!("Identifier {id} not found"),
                }
            },
            Ast::FloatToInt { operand } => {
                let operand = match FloatValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected FloatValue"),
                };
                AnyValueEnum::from(self.builder.build_float_to_signed_int(operand, self.context.i64_type(), "tmpcast").unwrap())
            },
            Ast::IntToFloat { operand } => {
                let operand = match IntValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected IntValue"),
                };
                AnyValueEnum::from(self.builder.build_signed_int_to_float(operand, self.context.f64_type(), "tmpcast").unwrap())
            },
            Ast::BoolToInt { operand } => {
                let operand = match IntValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected IntValue"),
                };
                AnyValueEnum::from(self.builder.build_int_cast(operand, self.context.i64_type(), "tmpcast").unwrap())
            },
            Ast::IntToBool { operand } => {
                let operand = match IntValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected IntValue"),
                };
                AnyValueEnum::from(self.builder.build_int_cast(operand, self.context.bool_type(), "tmpcast").unwrap())
            },
            Ast::FloatArrayToIntArray { operand } => {
                let operand = match VectorValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected VectorValue"),
                };
                assert!(operand.get_type().get_element_type().is_float_type(), "Expected float vector");
                AnyValueEnum::from(self.builder.build_float_to_signed_int(operand, self.context.i64_type().vec_type(operand.get_type().get_size()), "tmpcast").unwrap())
            },
            Ast::IntArrayToFloatArray { operand } => {
                let operand = match VectorValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected VectorValue"),
                };
                assert!(operand.get_type().get_element_type().is_int_type(), "Expected integer vector");
                AnyValueEnum::from(self.builder.build_signed_int_to_float(operand, self.context.f64_type().vec_type(operand.get_type().get_size()), "tmpcast").unwrap())
            },
            Ast::BoolArrayToIntArray { operand } => {
                let operand = match VectorValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected VectorValue"),
                };
                assert!(operand.get_type().get_element_type().is_int_type(), "Expected bool vector");
                AnyValueEnum::from(self.builder.build_int_cast(operand, self.context.i64_type().vec_type(operand.get_type().get_size()), "tmpcast").unwrap())
            },
            Ast::IntArrayToBoolArray { operand } => {
                let operand = match VectorValue::try_from(self.visit_ast(operand)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected VectorValue"),
                };
                assert!(operand.get_type().get_element_type().is_int_type(), "Expected integer vector");
                AnyValueEnum::from(self.builder.build_int_cast(operand, self.context.bool_type().vec_type(operand.get_type().get_size()), "tmpcast").unwrap())
            },
        }
    }
}

//#[cfg(test)]
//mod tests {
//    use super::*;
//
//    #[test]
//    fn codegen_var_decl() {
//        let mut ast = Box::new(Ast::VarDecl { 
//            is_global: true,
//            name: String::from("a"),
//            ty: Types::Int,
//        });
//        let context = Context::create();
//        let builder = context.create_builder();
//        let module = context.create_module("test");
//        let mut codegen = CodeGen::new(&context, &builder, &module);
//
//        let act_val = ast.accept(&mut codegen);
//
//        let exp_val = Types::Int;
//
//        assert_eq!(act_val, exp_val);
//    }
//}
