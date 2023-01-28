// ======================================================================================
// COMPILER =============================================================================
// ======================================================================================

use std::borrow::Borrow;
use std::collections::HashMap;
use tracing::{info, warn};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, FloatValue, FunctionValue, PointerValue};
use inkwell::FloatPredicate;

use crate::types::{Expr, Function, Prototype};

/// Defines the `Expr` compiler.
#[derive(Debug)]
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,

    constants: HashMap<String, PointerValue<'ctx>>,
    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        // Generate .dot files.
        // self.fn_value().view_function_cfg();
        let entry = self.fn_value().get_first_basic_block().unwrap();
        // println!("entry:");
        // println!("{:#?}", entry);

        match entry.get_first_instruction() {
            None => {
                info!("entry doesn't have first instr.");
                // append the whole entry (a basic block) to the builder?
                builder.position_at_end(entry)
            }
            Some(first_instr) => {
                info!("entry has first instr.");
                // append first_instr to the builder?
                builder.position_before(&first_instr)
            }
        }

        let res = builder.build_alloca(self.context.f64_type(), name);
        // println!("res:");
        // println!("{:#?}", res);
        res
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    /// Invariant: self.variables and self.constants are populated.
    fn compile_expr(&mut self, expr: &Expr) -> Result<FloatValue<'ctx>, &'static str> {
        match *expr {
            Expr::Number(nb) => Ok(self.context.f64_type().const_float(nb)),

            Expr::Variable(ref name) => {
                // do this during self.variables population? {
                let float_default_name = "ZERO";
                let float_default_value = match (
                    self.variables.get(float_default_name),
                    self.constants.get(float_default_name),
                ) {
                    (Some(var), _) => {
                        let load_instr = self.builder.build_load(*var, float_default_name);
                        Ok(load_instr.into_float_value())
                    }
                    _ => unreachable!(),
                };
                // }
                match (
                    self.variables.get(name.as_str()),
                    self.constants.get(name.as_str()),
                ) {
                    (Some(var), _) => {
                        let load_instr = self.builder.build_load(*var, name.as_str());
                        Ok(load_instr.into_float_value())
                    }
                    (None, Some(var)) => {
                        let load_instr = self.builder.build_load(*var, name.as_str());
                        Ok(load_instr.into_float_value())
                    }
                    _ => {
                        warn!(name, "Could not find a matching variable.");
                        // trying this idea, see if it's better than panicking.
                        float_default_value
                    }
                }
            }

            Expr::VarIn {
                ref variables,
                ref body,
            } => {
                let mut old_bindings = Vec::new();

                for &(ref var_name, ref initializer) in variables {
                    let var_name = var_name.as_str();

                    let initial_val = match *initializer {
                        Some(ref init) => self.compile_expr(init)?,
                        None => self.context.f64_type().const_float(0.),
                    };

                    let alloca = self.create_entry_block_alloca(var_name);

                    self.builder.build_store(alloca, initial_val);

                    if let Some(old_binding) = self.variables.remove(var_name) {
                        old_bindings.push(old_binding);
                    }

                    self.variables.insert(var_name.to_string(), alloca);
                }

                let body = self.compile_expr(body)?;

                for binding in old_bindings {
                    self.variables
                        .insert(binding.get_name().to_str().unwrap().to_string(), binding);
                }

                Ok(body)
            }

            Expr::Binary {
                op,
                ref left,
                ref right,
            } => {
                if op == '=' {
                    // handle assignement
                    let var_name = match *left.borrow() {
                        Expr::Variable(ref var_name) => var_name,
                        _ => {
                            return Err("Expected variable as left-hand operator of assignement.");
                        }
                    };

                    let var_val = self.compile_expr(right)?;
                    let var = self
                        .variables
                        .get(var_name.as_str())
                        .ok_or("Undefined variable.")?;

                    self.builder.build_store(*var, var_val);

                    Ok(var_val)
                } else {
                    let lhs = self.compile_expr(left)?;
                    let rhs = self.compile_expr(right)?;

                    match op {
                        '+' => Ok(self.builder.build_float_add(lhs, rhs, "tmpadd")),
                        '-' => Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub")),
                        '*' => Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul")),
                        '/' => Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv")),
                        '<' => Ok({
                            let cmp = self.builder.build_float_compare(
                                FloatPredicate::ULT,
                                lhs,
                                rhs,
                                "tmpcmp",
                            );

                            self.builder.build_unsigned_int_to_float(
                                cmp,
                                self.context.f64_type(),
                                "tmpbool",
                            )
                        }),
                        '>' => Ok({
                            let cmp = self.builder.build_float_compare(
                                FloatPredicate::ULT,
                                rhs,
                                lhs,
                                "tmpcmp",
                            );

                            self.builder.build_unsigned_int_to_float(
                                cmp,
                                self.context.f64_type(),
                                "tmpbool",
                            )
                        }),

                        custom => {
                            let mut name = String::from("binary");

                            name.push(custom);

                            match self.get_function(name.as_str()) {
                                Some(fun) => {
                                    match self
                                        .builder
                                        .build_call(fun, &[lhs.into(), rhs.into()], "tmpbin")
                                        .try_as_basic_value()
                                        .left()
                                    {
                                        Some(value) => Ok(value.into_float_value()),
                                        None => Err("Invalid call produced."),
                                    }
                                }

                                None => Err("Undefined binary operator."),
                            }
                        }
                    }
                }
            }

            Expr::Call {
                ref fn_name,
                ref args,
            } => match self.get_function(fn_name.as_str()) {
                Some(fun) => {
                    let mut compiled_args = Vec::with_capacity(args.len());

                    for arg in args {
                        compiled_args.push(self.compile_expr(arg)?);
                    }

                    let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                        .iter()
                        .by_ref()
                        .map(|&val| val.into())
                        .collect();

                    match self
                        .builder
                        .build_call(fun, argsv.as_slice(), "tmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => Ok(value.into_float_value()),
                        None => Err("Invalid call produced."),
                    }
                }
                None => Err("Unknown function."),
            },

            Expr::Conditional {
                ref cond,
                ref consequence,
                ref alternative,
            } => {
                let parent = self.fn_value();
                let zero_const = self.context.f64_type().const_float(0.0);

                // create condition by comparing without 0.0 and returning an int
                let cond = self.compile_expr(cond)?;
                let cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    cond,
                    zero_const,
                    "ifcond",
                );

                // build branch
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                // build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expr(consequence)?;
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.compile_expr(alternative)?;
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);

                let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value().into_float_value())
            }

            Expr::For {
                ref var_name,
                ref start,
                ref end,
                ref step,
                ref body,
            } => {
                let parent = self.fn_value();

                let start_alloca = self.create_entry_block_alloca(var_name);
                let start = self.compile_expr(start)?;

                self.builder.build_store(start_alloca, start);

                // go from current block to loop block
                let loop_bb = self.context.append_basic_block(parent, "loop");

                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(loop_bb);

                let old_val = self.variables.remove(var_name.as_str());

                self.variables.insert(var_name.to_owned(), start_alloca);

                // emit body
                self.compile_expr(body)?;

                // emit step
                let step = match *step {
                    Some(ref step) => self.compile_expr(step)?,
                    None => self.context.f64_type().const_float(1.0),
                };

                // compile end condition
                let end_cond = self.compile_expr(end)?;

                let curr_var = self.builder.build_load(start_alloca, var_name);
                let next_var =
                    self.builder
                        .build_float_add(curr_var.into_float_value(), step, "nextvar");

                self.builder.build_store(start_alloca, next_var);

                let end_cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    end_cond,
                    self.context.f64_type().const_float(0.0),
                    "loopcond",
                );
                let after_bb = self.context.append_basic_block(parent, "afterloop");

                self.builder
                    .build_conditional_branch(end_cond, loop_bb, after_bb);
                self.builder.position_at_end(after_bb);

                self.variables.remove(var_name);

                if let Some(val) = old_val {
                    self.variables.insert(var_name.to_owned(), val);
                }

                Ok(self.context.f64_type().const_float(0.0))
            }
        }
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'ctx>, &'static str> {
        let fn_val = {
            let ret_type = self.context.f64_type();
            let ret_types = std::iter::repeat(ret_type).take(proto.args.len());

            let args_types = ret_types
                .map(|f| f.into())
                .collect::<Vec<BasicMetadataTypeEnum>>();

            let fn_type = self
                .context
                .f64_type()
                .fn_type(args_types.as_slice(), false);

            self.module.add_function(proto.name.as_str(), fn_type, None)
        };

        // Internally calls LLVMSetValueName to mutate arg.value.
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.args[i].as_str());
        }

        // finally return built prototype
        Ok(fn_val)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let proto = &self.function.prototype;
        let function = self.compile_prototype(proto)?;

        // got external function, returning only compiled prototype
        if self.function.body.is_none() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(proto.args.len());

        // self.variables population for a varin program not here...
        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = proto.args[i].as_str();
            let alloca_ptr = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca_ptr, arg);

            self.variables.insert(proto.args[i].clone(), alloca_ptr);
        }

        // compile body
        let body = self.compile_expr(self.function.body.as_ref().unwrap())?;

        self.builder.build_return(Some(&body));

        // return the whole thing after verification and optimization
        if function.verify(true) {
            self.fpm.run_on(&function);

            Ok(function)
        } else {
            unsafe {
                function.delete();
            }

            Err("Invalid generated function.")
        }
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &Function,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        // variables: {
        //     "b": PointerValue {
        //         ptr_value: Value {
        //             name: "b",
        //             address: 0x00005620e8e37970,
        //             is_const: false,
        //             is_null: false,
        //             is_undef: false,
        //             llvm_value: "  %b = alloca double, align 8",
        //             llvm_type: "double*",
        //         },
        //     },
        // },
        let mut compiler = Compiler {
            context,
            builder,
            fpm: pass_manager,
            module,
            function,
            fn_value_opt: None,
            constants: HashMap::new(),
            variables: HashMap::new(),
        };

        compiler.compile_fn()
    }
}
