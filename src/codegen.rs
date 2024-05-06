use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

pub struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub fn new(context: &'ctx Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> CodeGen<'a, 'ctx> {
        CodeGen {
            context,
            builder,
            module,
        }
    }
}

