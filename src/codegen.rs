use crate::symtable::SymTable;
use crate::types::Types;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

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

