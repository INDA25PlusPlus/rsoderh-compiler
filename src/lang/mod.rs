//! Contains the parts used during semantic analysis.

use std::sync::Arc;

use crate::{
    error::{Error, SemanticError},
    lang::instruction::{GlobalState, Identifier},
    lex,
    parse::Parser,
    syntax::{self, Expression},
};
use instruction::{
    Instruction, InstructionStack, IntoInstructions, IntoInstructionsReturn, Register,
};

mod instruction;
pub mod render;
#[cfg(test)]
mod test;

#[derive(Debug, Clone)]
pub struct BindingStack {
    bindings: Option<Arc<Binding>>,
}

impl BindingStack {
    pub fn new() -> Self {
        Self { bindings: None }
    }
}

impl BindingStack {
    pub fn lookup(&self, symbol: &syntax::Symbol) -> Option<Arc<BoundValue>> {
        self.items()
            .find(|(bound_symbol, _)| &bound_symbol.0 == &symbol.0)
            .map(|(_, value)| value)
    }

    pub fn items(&self) -> BindingStackItems {
        BindingStackItems {
            inner: self.bindings.clone(),
        }
    }

    pub fn with_value(&self, symbol: Arc<syntax::Symbol>, value: BoundValue) -> Self {
        BindingStack {
            bindings: Some(Arc::new(Binding {
                symbol,
                value: Arc::new(value),
                previous: self.bindings.clone(),
            })),
        }
    }
    pub fn with_register(&self, symbol: Arc<syntax::Symbol>, register: Register) -> Self {
        self.with_value(symbol, BoundValue::Register(register))
    }
    pub fn with_function(
        &self,
        symbol: Arc<syntax::Symbol>,
        function_identifier: Identifier,
    ) -> Self {
        self.with_value(symbol, BoundValue::Function(function_identifier))
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    symbol: Arc<syntax::Symbol>,
    value: Arc<BoundValue>,
    /// The previous identifier in this binding chain. May be from a higher scope.
    previous: Option<Arc<Binding>>,
}

#[derive(Debug, Clone)]
pub struct BindingStackItems {
    inner: Option<Arc<Binding>>,
}

impl Iterator for BindingStackItems {
    type Item = (Arc<syntax::Symbol>, Arc<BoundValue>);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.clone().map(|next| {
            self.inner = next.previous.as_ref().map(|arc| arc.clone());
            (next.symbol.clone(), next.value.clone())
        })
    }
}

#[derive(Debug, Clone)]
pub enum BoundValue {
    Register(Register),
    Function(Identifier),
}

impl IntoInstructions for syntax::Defun {
    fn into_instructions(
        &self,
        instructions: &InstructionStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        // TODO: Add args to binding stack.

        self.body.into_instructions(instructions, bindings, globals)
    }
}

impl IntoInstructions for syntax::Expression {
    fn into_instructions(
        &self,
        instructions: &InstructionStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        match self {
            Expression::Progn(node) => node.into_instructions(instructions, bindings, globals),
            Expression::Application(node) => {
                node.into_instructions(instructions, bindings, globals)
            }
            Expression::Symbol(node) => node.into_instructions(instructions, bindings, globals),
            Expression::Literal(node) => node.into_instructions(instructions, bindings, globals),
        }
    }
}

impl IntoInstructions for syntax::Progn {
    fn into_instructions(
        &self,
        instructions: &InstructionStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        // self.expressions.

        let mut instructions = instructions.clone();
        let mut bindings = bindings.clone();

        for expression in &self.expressions {
            (instructions, bindings) = match expression.as_ref() {
                syntax::VarExpression::Var(node) => {
                    node.into_instructions(&instructions, &bindings, globals)
                }
                syntax::VarExpression::Expression(node) => {
                    node.into_instructions(&instructions, &bindings, globals)
                }
            }?;
        }

        Ok((instructions, bindings))
    }
}

impl IntoInstructions for syntax::Var {
    fn into_instructions(
        &self,
        instructions: &InstructionStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let (instructions, bindings) =
            self.value
                .into_instructions(instructions, bindings, globals)?;

        let register = instructions.current_register();
        Ok((
            instructions,
            bindings.with_register(self.name.clone(), register),
        ))
    }
}

impl IntoInstructions for syntax::Application {
    fn into_instructions(
        &self,
        instructions: &InstructionStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let identifier = match bindings
            .lookup(&self.function)
            .as_ref()
            .map(|arc| arc.as_ref())
        {
            None => {
                return Err(SemanticError::new(
                    format!("cannot find function '{}' in this scope", &self.function.0),
                    self.span.clone(),
                    globals.document(),
                ));
            }
            Some(BoundValue::Register(_)) => {
                return Err(SemanticError::new(
                    format!("'{}' is not a function", &self.function.0),
                    self.span.clone(),
                    globals.document(),
                ));
            }
            Some(BoundValue::Function(identifier)) => identifier.clone(),
        };

        let mut instructions = instructions.clone();
        let mut bindings = bindings.clone();

        let registers = self
            .args
            .iter()
            .map(|expression| {
                (instructions, bindings) =
                    expression.into_instructions(&instructions, &bindings, globals)?;

                Ok(instructions.current_register())
            })
            .collect::<Result<Box<[Register]>, SemanticError>>()?;
        let instruction = Instruction::FunctionCall(identifier, registers);

        Ok((instructions.with_instruction(instruction), bindings))
    }
}

impl IntoInstructions for syntax::Symbol {
    fn into_instructions(
        &self,
        instructions: &InstructionStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let instruction = match bindings.lookup(self).as_ref().map(|x| x.as_ref()) {
            Some(BoundValue::Register(register)) => Instruction::Register(*register),
            Some(BoundValue::Function(identifier)) => {
                Instruction::GlobalAddress(identifier.clone())
            }
            None => {
                return Err(SemanticError::new(
                    format!("cannot find value '{}' in this scope", self.0),
                    self.1.clone(),
                    globals.document(),
                ));
            }
        };

        Ok((instructions.with_instruction(instruction), bindings.clone()))
    }
}

impl IntoInstructions for syntax::Literal {
    fn into_instructions(
        &self,
        instructions: &InstructionStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let instruction = match self {
            syntax::Literal::Int(int) => Instruction::LiteralInt(int.clone()),
            syntax::Literal::String(string_literal) => {
                let identifier = globals.add_string(string_literal.clone());
                Instruction::GlobalAddress(identifier)
            }
        };

        Ok((instructions.with_instruction(instruction), bindings.clone()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    identifier: Identifier,
    num_args: usize,
    instructions: InstructionStack,
}

impl Function {
    pub fn compile(
        function: Arc<syntax::Defun>,
        bindings: &BindingStack,
        state: &mut GlobalState,
    ) -> Result<Self, Error> {
        let identifier = match function.name.0.as_ref() {
            "main" => Identifier::new("_main"),
            _ => Identifier::from_symbol(&function.name),
        };
        Self::compile_with_identifier(identifier, function, bindings, state)
    }

    pub fn compile_with_identifier(
        identifier: Identifier,
        function: Arc<syntax::Defun>,
        bindings: &BindingStack,
        state: &mut GlobalState,
    ) -> Result<Self, Error> {
        let mut bindings = bindings.clone();
        for (i, arg_symbol) in function.arguments.iter().enumerate() {
            bindings = bindings.with_register(arg_symbol.clone(), Register::param(i));
        }

        let (instructions, _) = function
            .into_instructions(&InstructionStack::new(), &bindings, state)
            .map_err(|error| Error::function_error(error, function.clone()))?;

        Ok(Self {
            identifier,
            num_args: function.arguments.len(),
            instructions,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    functions: Box<[Function]>,
    top_level: Function,
    state: GlobalState,
}

impl Program {
    pub fn compile(file: &syntax::File, document: lex::Document) -> Result<Self, Error> {
        let mut state = GlobalState::new(document.clone());
        let mut bindings = BindingStack::new();

        // Hard-coded list of external functions which can be called in Ship. Would be nice to scan
        // this from header files, but that seems like a lot of work.
        let external_functions = [syntax::Symbol::new_static("printf")];

        for symbol in external_functions {
            let identifier = Identifier::from_symbol(&symbol);
            bindings = bindings.with_function(Arc::from(symbol), identifier);
        }

        let compiled_functions = file
            .statements
            .iter()
            .filter_map(|statement| match statement.as_ref() {
                syntax::Statement::Defun(defun) => Some(defun.clone()),
                _ => None,
            })
            .map(|defun| {
                Ok((
                    Function::compile(defun.clone(), &bindings, &mut state)?,
                    defun,
                ))
            })
            .collect::<Result<Box<[_]>, _>>()?;

        for (function, defun) in &compiled_functions {
            if matches!(
                bindings
                    .lookup(&defun.name)
                    .as_ref()
                    .map(|arc| arc.as_ref()),
                Some(BoundValue::Function(_))
            ) {
                return Err(Error::SemanticError(SemanticError::new(
                    format!("function '{}' is already defined", &defun.name.0),
                    defun.name.span().clone(),
                    document.clone(),
                )));
            }

            bindings = bindings.with_function(defun.name.clone(), function.identifier.clone())
        }

        let top_level_body = syntax::Progn {
            expressions: file
                .statements
                .iter()
                .filter_map(|statement| match statement.as_ref() {
                    syntax::Statement::Defun(_) => None,
                    syntax::Statement::Var(var) => {
                        Some(Arc::new(syntax::VarExpression::Var(var.clone())))
                    }
                    syntax::Statement::Expression(expression) => Some(Arc::new(
                        syntax::VarExpression::Expression(expression.clone()),
                    )),
                })
                .collect(),
            span: file.span.clone(),
        };
        let top_level_defun = syntax::Defun {
            // TODO: The 0..0 span is very, ugly. Should probably make this an option...
            name: Arc::new(syntax::Symbol(String::from("<top-level>"), 0..0)),
            arguments: Box::new([]),
            body: Arc::new(syntax::Expression::Progn(Arc::new(top_level_body))),
            span: file.span.clone(),
        };
        let top_level = Function::compile_with_identifier(
            Identifier::new("main"),
            Arc::new(top_level_defun),
            &bindings,
            &mut state,
        )?;

        let functions = compiled_functions
            .into_iter()
            .map(|(function, _)| function)
            .collect();

        Ok(Self {
            functions,
            top_level,
            state,
        })
    }
}
