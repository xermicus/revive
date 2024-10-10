//! The immutable data runtime function.

use crate::polkavm::context::address_space::AddressSpace;
use crate::polkavm::context::function::runtime;
use crate::polkavm::context::pointer::Pointer;
use crate::polkavm::context::Context;
use crate::polkavm::WriteLLVM;
use crate::polkavm::{runtime_api, Dependency};

use inkwell::debug_info::AsDIScope;

/// A function for requesting the immutable data from the runtime.
/// This is a special function that is only used by the front-end generated code.
///
/// The runtime API is called lazily and subsequent calls are no-ops.
///
/// The bytes written is asserted to match the expected length.
/// This should never fail; the length is known.
/// However, this is a one time assertion, hence worth it.
#[derive(Debug)]
pub struct ImmutableDataLoad;

impl<D> WriteLLVM<D> for ImmutableDataLoad
where
    D: Dependency + Clone,
{
    fn declare(&mut self, context: &mut Context<D>) -> anyhow::Result<()> {
        context.add_function(
            runtime::FUNCTION_LOAD_IMMUTABLE_DATA,
            context.void_type().fn_type(Default::default(), false),
            0,
            Some(inkwell::module::Linkage::Private),
        )?;

        Ok(())
    }

    fn into_llvm(self, context: &mut Context<D>) -> anyhow::Result<()> {
        context.set_current_function(runtime::FUNCTION_LOAD_IMMUTABLE_DATA)?;
        context.set_basic_block(context.current_function().borrow().entry_block());

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

        let immutable_data_size_pointer = context
            .get_global(revive_runtime_api::immutable_data::GLOBAL_IMMUTABLE_DATA_SIZE)?
            .value
            .as_pointer_value();
        let immutable_data_size = context.build_load(
            Pointer::new(
                context.xlen_type(),
                AddressSpace::Stack,
                immutable_data_size_pointer,
            ),
            "immutable_data_size_load",
        )?;

        let load_immutable_data_block = context.append_basic_block("load_immutables_block");
        let return_block = context.current_function().borrow().return_block();
        let immutable_data_size_is_zero = context.builder().build_int_compare(
            inkwell::IntPredicate::EQ,
            context.xlen_type().const_zero(),
            immutable_data_size.into_int_value(),
            "immutable_data_size_is_zero",
        )?;
        context.build_conditional_branch(
            immutable_data_size_is_zero,
            return_block,
            load_immutable_data_block,
        )?;

        context.set_basic_block(load_immutable_data_block);
        let output_pointer = context
            .get_global(revive_runtime_api::immutable_data::GLOBAL_IMMUTABLE_DATA_POINTER)?
            .value
            .as_pointer_value();
        context.build_runtime_call(
            runtime_api::imports::GET_IMMUTABLE_DATA,
            &[
                context
                    .builder()
                    .build_ptr_to_int(output_pointer, context.xlen_type(), "ptr_to_xlen")?
                    .into(),
                context
                    .builder()
                    .build_ptr_to_int(
                        immutable_data_size_pointer,
                        context.xlen_type(),
                        "ptr_to_xlen",
                    )?
                    .into(),
            ],
        );
        let bytes_written = context.builder().build_load(
            context.xlen_type(),
            immutable_data_size_pointer,
            "bytes_written",
        )?;
        context.builder().build_store(
            immutable_data_size_pointer,
            context.xlen_type().const_zero(),
        )?;
        let overflow_block = context.append_basic_block("immutable_data_overflow");
        let is_overflow = context.builder().build_int_compare(
            inkwell::IntPredicate::UGT,
            immutable_data_size.into_int_value(),
            bytes_written.into_int_value(),
            "is_overflow",
        )?;
        context.build_conditional_branch(is_overflow, overflow_block, return_block)?;

        context.set_basic_block(overflow_block);
        context.build_call(context.intrinsics().trap, &[], "invalid_trap");
        context.build_unreachable();

        context.set_basic_block(return_block);
        context.build_return(None);

        if let Some(dinfo) = context.debug_info() {
            let _ = dinfo.pop_scope();
        }

        Ok(())
    }
}