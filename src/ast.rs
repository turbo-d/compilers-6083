use crate::codegen::CodeGen;
use crate::symtable::SymTable;
use crate::types::Types;

use inkwell::AddressSpace;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FloatValue, IntValue, PointerValue};
use inkwell::FloatPredicate;
use inkwell::IntPredicate;

#[derive(PartialEq)]
pub enum RelationOp {
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    NotEq,
}

impl SymTable<Types, Types> {
    pub fn new_with_runtime() -> SymTable<Types, Types> {
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

        st
    }
}

pub enum Ast {
    Program {
        name: String,
        decls: Vec<Box<Ast>>,
        body: Vec<Box<Ast>>,
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
        params: Vec<Ast>,
        decls: Vec<Box<Ast>>,
        body: Vec<Box<Ast>>,
    },
    AssignStmt {
        //dest: Box<DestNode>,
        dest: Box<Ast>,
        expr: Box<Ast>,
    },
    IfStmt {
        cond: Box<Ast>,
        then_body: Vec<Box<Ast>>,
        else_body: Vec<Box<Ast>>,
    },
    LoopStmt {
        init: Box<Ast>,
        cond: Box<Ast>,
        body: Vec<Box<Ast>>,
    },
    ReturnStmt {
        expr: Box<Ast>,
    },
    AndOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    OrOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    NotOp {
        operand: Box<Ast>,
    },
    AddOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    SubOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    MulOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    DivOp {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    Relation {
        op: RelationOp,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    NegateOp {
        operand: Box<Ast>,
    },
    SubscriptOp {
        //array: Box<Var>,
        array: Box<Ast>,
        index: Box<Ast>,
    },
    ProcCall {
        //proc: Box<Var>,
        proc: Box<Ast>,
        args: Vec<Box<Ast>>,
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

impl Ast {
    pub fn code_gen<'a, 'ctx>(&self, cg: &mut CodeGen<'a, 'ctx>) -> AnyValueEnum<'ctx> {
        match self {
            Ast::Program { name, decls, body, } => {
                cg.module.set_name(name.as_str());

                let args_types = Vec::<BasicMetadataTypeEnum>::new();
                let args_types = args_types.as_slice();
                let fn_type = cg.context.i64_type().fn_type(args_types, false);
                let fn_val = cg.module.add_function("main", fn_type, None);

                let entry = cg.context.append_basic_block(fn_val, "entry");
                cg.builder.position_at_end(entry);

                for decl in decls.iter() {
                    decl.code_gen(cg);
                }
                cg.builder.position_at_end(entry);

                for stmt in body.iter() {
                    stmt.code_gen(cg);
                }

                AnyValueEnum::from(cg.context.i64_type().const_int(0, false))
            }
            Ast::VarDecl { is_global, name, ty } => {
                let parent_fn = cg.st.get_local_proc_data().clone();

                if *is_global {
                    let basic_type = match ty.clone() {
                        Types::Int => BasicTypeEnum::from(cg.context.i64_type()),
                        Types::Float => BasicTypeEnum::from(cg.context.f64_type()),
                        //Types::String => BasicTypeEnum::from(cg.context.ptr_type(AddressSpace::default())), //TODO
                        Types::String => BasicTypeEnum::from(cg.context.f64_type()), //TODO
                        Types::Bool => BasicTypeEnum::from(cg.context.bool_type()),
                        Types::Array(size, base_type) => {
                            match *base_type {
                                Types::Int => BasicTypeEnum::from(cg.context.i64_type().array_type(size)),
                                Types::Float => BasicTypeEnum::from(cg.context.f64_type().array_type(size)),
                                //Types::String => cg.context.ptr_type(AddressSpace::default()).array_type(size).fn_type(args_types, false), //TODO
                                Types::String => BasicTypeEnum::from(cg.context.f64_type().array_type(size)), //TODO
                                Types::Bool => BasicTypeEnum::from(cg.context.bool_type().array_type(size)),
                                _ => panic!("Unexpected base type for arrary type"),
                            }
                        }
                        _ => panic!("Unexpected procedure return type"),
                    };
                    let global = cg.module.add_global(basic_type, Some(AddressSpace::default()), name.as_str());
                    let _  = cg.st.insert_global(name.clone(), (global.as_pointer_value(), ty.clone()));
                    return AnyValueEnum::from(global.as_pointer_value())
                } else {
                    let alloca = cg.create_entry_block_alloca(&parent_fn, name.as_str(), ty.clone());
                    let _ = cg.st.insert(name.clone(), (alloca, ty.clone()));
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
                            Types::Int => BasicMetadataTypeEnum::from(cg.context.i64_type()),
                            Types::Float => BasicMetadataTypeEnum::from(cg.context.f64_type()),
                            //Types::String => BasicMetadataTypeEnum::from(cg.context.ptr_type(AddressSpace::default())), //TODO
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

                let fn_val = cg.module.add_function(name.as_str(), fn_type, None);

                let entry = cg.context.append_basic_block(fn_val, "entry");
                cg.builder.position_at_end(entry);

                cg.st.enter_scope(fn_val);
                //cg.st.reserve(self.params.len());

                for (i, arg) in fn_val.get_param_iter().enumerate() {
                    let arg_name = match &params[i] {
                        Ast::VarDecl { name, .. } => name,
                        _ => panic!("Expected Ast::VarDecl for AST::ProcDecl params"),
                    };
                    arg.set_name(arg_name.as_str());

                    //let alloca = cg.create_entry_block_alloca(&fn_val, arg_name);
                    let alloca = match PointerValue::try_from(params[i].code_gen(cg)) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected PointerValue alloca."),
                    };

                    cg.builder.build_store(alloca, arg).unwrap();
                }

                for decl in decls.iter() {
                    decl.code_gen(cg);
                }
                cg.builder.position_at_end(entry);

                for stmt in body.iter() {
                    stmt.code_gen(cg);
                }

                cg.st.exit_scope();

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

                let alloca = match cg.st.get(name) {
                    Some((var, _)) => var.clone(),
                    None => panic!("Identifer name not found"),
                };

                let val = match BasicValueEnum::try_from(expr.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected BasicValue in assignment."),
                };

                cg.builder.build_store(alloca, val).unwrap();

                AnyValueEnum::from(val)
            },
            Ast::IfStmt { cond, then_body, else_body } => {
                let parent = cg.st.get_local_proc_data().clone();

                let cond = match IntValue::try_from(cond.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected u1 type expression for if stmt conditional"),
                };

                // build branch
                let then_bb = cg.context.append_basic_block(parent, "then");
                let else_bb = cg.context.append_basic_block(parent, "else");
                let cont_bb = cg.context.append_basic_block(parent, "ifcont");

                cg.builder.build_conditional_branch(cond, then_bb, else_bb).unwrap();

                // build then block
                cg.builder.position_at_end(then_bb);
                for stmt in then_body.iter() {
                    stmt.code_gen(cg);
                }
                cg.builder.build_unconditional_branch(cont_bb).unwrap();
                //let then_bb = cg.builder.get_insert_block().unwrap();

                // build else block
                cg.builder.position_at_end(else_bb);
                for stmt in else_body.iter() {
                    stmt.code_gen(cg);
                }
                cg.builder.build_unconditional_branch(cont_bb).unwrap();
                //let else_bb = cg.builder.get_insert_block().unwrap();

                // emit merge block
                cg.builder.position_at_end(cont_bb);
                //let phi = cg.builder.build_phi(cg.context.f64_type(), "iftmp").unwrap();
                //phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                //AnyValueEnum::from(phi.as_basic_value().into_float_value())
                AnyValueEnum::from(cg.context.i64_type().const_int(0, false))
            },
            Ast::LoopStmt { init, cond, body } => {
                let parent = cg.st.get_local_proc_data().clone();

                init.code_gen(cg);

                // go from current block to loop block
                let loop_bb = cg.context.append_basic_block(parent, "loop");
                cg.builder.build_unconditional_branch(loop_bb).unwrap();
                cg.builder.position_at_end(loop_bb);

                // emit body
                for stmt in body.iter() {
                    stmt.code_gen(cg);
                }

                // compile end condition
                let cond = match IntValue::try_from(cond.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected u1 type expression for loop stmt conditional"),
                };

                let after_bb = cg.context.append_basic_block(parent, "afterloop");
                cg.builder.build_conditional_branch(cond, loop_bb, after_bb).unwrap();
                cg.builder.position_at_end(after_bb);

                AnyValueEnum::from(cg.context.i64_type().const_int(0, false))
            },
            Ast::ReturnStmt { expr } => {
                let val = match BasicValueEnum::try_from(expr.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Expected BasicValue in assignment."),
                };

                AnyValueEnum::from(cg.builder.build_return(Some(&val)).unwrap())
            },
            Ast::AndOp { lhs, rhs } => {
                let lhs = match IntValue::try_from(lhs.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };
                let rhs = match IntValue::try_from(rhs.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };

                AnyValueEnum::from(cg.builder.build_and(lhs, rhs, "tmpand").unwrap())
            },
            Ast::OrOp { lhs, rhs } => {
                let lhs = match IntValue::try_from(lhs.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };
                let rhs = match IntValue::try_from(rhs.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };

                AnyValueEnum::from(cg.builder.build_or(lhs, rhs, "tmpor").unwrap())
            },
            Ast::NotOp { operand } => {
                let val = match IntValue::try_from(operand.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Bitwise operations can only be performed on operands of integer type"),
                };

                AnyValueEnum::from(cg.builder.build_not(val, "tmpnot").unwrap())
            },
            Ast::AddOp { lhs, rhs } => {
                let lhs = lhs.code_gen(cg);
                let rhs = rhs.code_gen(cg);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(cg.builder.build_int_add(lhs, rhs, "tmpadd").unwrap());
                } else if lhs.is_float_value() {
                    let lhs = match FloatValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };
                    let rhs = match FloatValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };

                    return AnyValueEnum::from(cg.builder.build_float_add(lhs, rhs, "tmpadd").unwrap());
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::SubOp { lhs, rhs } => {
                let lhs = lhs.code_gen(cg);
                let rhs = rhs.code_gen(cg);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(cg.builder.build_int_sub(lhs, rhs, "tmpsub").unwrap());
                } else if lhs.is_float_value() {
                    let lhs = match FloatValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };
                    let rhs = match FloatValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };

                    return AnyValueEnum::from(cg.builder.build_float_sub(lhs, rhs, "tmpsub").unwrap());
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::MulOp { lhs, rhs } => {
                let lhs = lhs.code_gen(cg);
                let rhs = rhs.code_gen(cg);

                if lhs.is_int_value() {
                    let lhs = match IntValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };
                    let rhs = match IntValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(cg.builder.build_int_mul(lhs, rhs, "tmpmul").unwrap());
                } else if lhs.is_float_value() {
                    let lhs = match FloatValue::try_from(lhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };
                    let rhs = match FloatValue::try_from(rhs) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected FloatValue"),
                    };

                    return AnyValueEnum::from(cg.builder.build_float_mul(lhs, rhs, "tmpmul").unwrap());
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::DivOp { lhs, rhs } => {
                let lhs = match FloatValue::try_from(lhs.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Arithmetic operations can only be performed on operands of float type"),
                };
                let rhs = match FloatValue::try_from(rhs.code_gen(cg)) {
                    Ok(val) => val,
                    Err(_) => panic!("Arithmetic operations can only be performed on operands of float type"),
                };

                AnyValueEnum::from(cg.builder.build_float_div(lhs, rhs, "tmpdiv").unwrap())
            },
            Ast::Relation { op, lhs, rhs } => {
                let lhs = lhs.code_gen(cg);
                let rhs = rhs.code_gen(cg);

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
                        RelationOp::LT => cg.builder.build_int_compare(IntPredicate::ULT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::LTE => cg.builder.build_int_compare(IntPredicate::ULE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GT => cg.builder.build_int_compare(IntPredicate::UGT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GTE => cg.builder.build_int_compare(IntPredicate::UGE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::Eq => cg.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::NotEq => cg.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "tmpcmp").unwrap(),
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
                        RelationOp::LT => cg.builder.build_float_compare(FloatPredicate::ULT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::LTE => cg.builder.build_float_compare(FloatPredicate::ULE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GT => cg.builder.build_float_compare(FloatPredicate::UGT, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::GTE => cg.builder.build_float_compare(FloatPredicate::UGE, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::Eq => cg.builder.build_float_compare(FloatPredicate::UEQ, lhs, rhs, "tmpcmp").unwrap(),
                        RelationOp::NotEq => cg.builder.build_float_compare(FloatPredicate::UNE, lhs, rhs, "tmpcmp").unwrap(),
                    };

                    return AnyValueEnum::from(cmp);
                }
                // TODO: Strings

                panic!("Relational operators not supported on these operands");
            },
            Ast::NegateOp { operand } => {
                let val = operand.code_gen(cg);

                if val.is_int_value() {
                    let val = match IntValue::try_from(val) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(cg.builder.build_int_neg(val, "tmpneg").unwrap());
                } else if val.is_float_value() {
                    let val = match FloatValue::try_from(val) {
                        Ok(val) => val,
                        Err(_) => panic!("Expected IntValue"),
                    };

                    return AnyValueEnum::from(cg.builder.build_float_neg(val, "tmpneg").unwrap());
                }

                panic!("Arithmetic operations can only be performed on operands of integer and float type");
            },
            Ast::SubscriptOp { .. } => {
                AnyValueEnum::from(cg.context.i64_type().const_int(0, false))
            },
            Ast::ProcCall { proc, args } => {
                let id = match **proc {
                    Ast::Var { ref id } => id,
                    _ => panic!("Expected Ast::Var for proc"),
                };

                match cg.module.get_function(id.as_str()) {
                    Some(fun) => {
                        let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

                        for arg in args.iter() {
                            let arg = arg.code_gen(cg);
                            let arg = match BasicMetadataValueEnum::try_from(arg) {
                                Ok(val) => val,
                                Err(_) => panic!("Expected BasicMetadataValueEnum in arg list."),
                            };
                            compiled_args.push(arg);
                        }

                        let argsv: Vec<BasicMetadataValueEnum> =
                            compiled_args.iter().by_ref().map(|&val| val.into()).collect();

                        match cg.builder
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
            Ast::IntLiteral { value } => AnyValueEnum::from(cg.context.i64_type().const_int(*value as u64, false)),
            Ast::FloatLiteral { value } => AnyValueEnum::from(cg.context.f64_type().const_float(*value as f64)),
            Ast::BoolLiteral { value } => AnyValueEnum::from(cg.context.bool_type().const_int(*value as u64, false)),
            Ast::StringLiteral { .. } => AnyValueEnum::from(cg.context.f64_type().const_float(0.0)),
            Ast::Var { id } => {
                match cg.st.get(id) {
                    Some((var, ty)) => {
                        let ty = match ty {
                            Types::Int => BasicTypeEnum::from(cg.context.i64_type()),
                            Types::Float => BasicTypeEnum::from(cg.context.f64_type()),
                            //Types::String => BasicTypeEnum::from(cg.context.ptr_type(AddressSpace::default())), //TODO
                            Types::String => BasicTypeEnum::from(cg.context.f64_type()), //TODO
                            Types::Bool => BasicTypeEnum::from(cg.context.bool_type()),
                            Types::Array(size, base_type) => {
                                match **base_type {
                                    Types::Int => BasicTypeEnum::from(cg.context.i64_type().array_type(*size)),
                                    Types::Float => BasicTypeEnum::from(cg.context.f64_type().array_type(*size)),
                                    //Types::String => cg.context.ptr_type(AddressSpace::default()).array_type(*size).fn_type(args_types, false), //TODO
                                    Types::String => BasicTypeEnum::from(cg.context.f64_type().array_type(*size)), //TODO
                                    Types::Bool => BasicTypeEnum::from(cg.context.bool_type().array_type(*size)),
                                    _ => panic!("Unexpected base type for arrary type"),
                                }
                            }
                            _ => panic!("Unexpected procedure return type"),
                        };
                        AnyValueEnum::from(cg.builder.build_load(ty, *var, id.as_str()).unwrap())
                    }
                    None => panic!("Identifer name not found"),
                }
            },
        }
    }
}
