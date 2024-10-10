//! The runtime code function.

use std::marker::PhantomData;

use crate::polkavm::context::code_type::CodeType;
use crate::polkavm::context::function::runtime;
use crate::polkavm::context::Context;
use crate::polkavm::Dependency;
use crate::polkavm::WriteLLVM;

use inkwell::debug_info::AsDIScope;

/// The runtime code function.
/// Is a special function that is only used by the front-end generated code.
#[derive(Debug)]
pub struct RuntimeCode<B, D>
where
    B: WriteLLVM<D>,
    D: Dependency + Clone,
{
    /// The runtime code AST representation.
    inner: B,
    /// The `D` phantom data.
    _pd: PhantomData<D>,
}

impl<B, D> RuntimeCode<B, D>
where
    B: WriteLLVM<D>,
    D: Dependency + Clone,
{
    /// A shortcut constructor.
    pub fn new(inner: B) -> Self {
        Self {
            inner,
            _pd: PhantomData,
        }
    }
}

impl<B, D> WriteLLVM<D> for RuntimeCode<B, D>
where
    B: WriteLLVM<D>,
    D: Dependency + Clone,
{
    fn declare(&mut self, context: &mut Context<D>) -> anyhow::Result<()> {
        let function_type =
            context.function_type::<inkwell::types::BasicTypeEnum>(vec![], 0, false);
        context.add_function(
            runtime::FUNCTION_RUNTIME_CODE,
            function_type,
            0,
            Some(inkwell::module::Linkage::External),
        )?;

        self.inner.declare(context)
    }

    fn into_llvm(self, context: &mut Context<D>) -> anyhow::Result<()> {
        context.set_current_function(runtime::FUNCTION_RUNTIME_CODE)?;

        context.set_basic_block(context.current_function().borrow().entry_block());
        context.set_code_type(CodeType::Runtime);
        if let Some(dinfo) = context.debug_info() {
            let di_builder = dinfo.builder();
            let func_name: &str = runtime::FUNCTION_RUNTIME_CODE;
            let di_file = dinfo.compilation_unit().get_file();
            let di_scope = dinfo.top_scope().expect("expected a debug-info scope");

            let func_value = context
                .current_function()
                .borrow()
                .declaration()
                .function_value();
            let di_func_scope = match func_value.get_subprogram() {
                Some(scp) => scp,
                None => {
                    let di_flags = inkwell::debug_info::DIFlagsConstants::PUBLIC;
                    let ret_type = dinfo.create_word_type(Some(di_flags))?.as_type();
                    let subroutine_type =
                        di_builder.create_subroutine_type(di_file, Some(ret_type), &[], di_flags);
                    let linkage = dinfo.namespace_as_identifier(Some(func_name));
                    di_builder.create_function(
                        di_scope,
                        func_name,
                        Some(linkage.as_str()),
                        di_file,
                        0,
                        subroutine_type,
                        false,
                        true,
                        1,
                        di_flags,
                        false,
                    )
                }
            };

            func_value.set_subprogram(di_func_scope);
            dinfo.push_scope(di_func_scope.as_debug_info_scope());
            let di_loc = di_builder.create_debug_location(
                context.llvm(),
                0,
                0,
                di_func_scope.as_debug_info_scope(),
                None,
            );
            context.builder().set_current_debug_location(di_loc)
        }

        self.inner.into_llvm(context)?;
        if let Some(dinfo) = context.debug_info() {
            let di_loc_scope = dinfo.top_scope().expect("expected a debug-info scope");
            let di_loc =
                dinfo
                    .builder()
                    .create_debug_location(context.llvm(), 0, 0, di_loc_scope, None);
            context.builder().set_current_debug_location(di_loc)
        }
        match context
            .basic_block()
            .get_last_instruction()
            .map(|instruction| instruction.get_opcode())
        {
            Some(inkwell::values::InstructionOpcode::Br) => {}
            Some(inkwell::values::InstructionOpcode::Switch) => {}
            _ => context
                .build_unconditional_branch(context.current_function().borrow().return_block()),
        }

        context.set_basic_block(context.current_function().borrow().return_block());
        context.build_unreachable();

        if let Some(dinfo) = context.debug_info() {
            let _ = dinfo.pop_scope();
        }

        Ok(())
    }
}
