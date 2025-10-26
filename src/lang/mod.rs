//! Contains the parts used during semantic analysis.

use std::{ops::Range, str::FromStr, sync::Arc};

use crate::{
    error::{Error, SemanticError},
    lang::instruction::{GlobalState, Identifier, JumpInstruction, Value},
    lex,
    parse::Parser,
    syntax::{self, Expression},
};
use instruction::{BlockStack, Instruction, IntoInstructions, IntoInstructionsReturn, Register};
use itertools::{Itertools, Position};

mod instruction;
pub mod render;
#[cfg(test)]
mod test;

pub fn extract_args_exact<const N: usize>(
    args: &[Arc<Expression>],
    function_desc: &str,
    span: &Range<usize>,
    document: lex::Document,
) -> Result<[Arc<Expression>; N], SemanticError> {
    args.try_into()
        .map_err(|_| {
            if args.len() < N {
                SemanticError::new(
                    format!(
                        "function '{}' called with too few argument, expected {}",
                        function_desc, N,
                    ),
                    span.clone(),
                    document,
                )
            } else {
                SemanticError::new(
                    format!(
                        "function '{}' called with too few argument, expected {}",
                        function_desc, N,
                    ),
                    span.clone(),
                    document,
                )
            }
        })
        .map(|args: &[Arc<Expression>; N]| args.to_owned())
}

/// Builtin functions that take an arbitrary number of arguments, which are evaluated by performing
/// the operation with each argument and the previous result.
#[derive(Debug, Clone, Copy, strum_macros::EnumString, strum_macros::IntoStaticStr)]
pub enum BuiltinVariadic {
    #[strum(to_string = "+")]
    Add,
    #[strum(to_string = "-")]
    Sub,
    #[strum(to_string = "*")]
    Mul,
    #[strum(to_string = "//")]
    Div,
    #[strum(to_string = "rem")]
    Rem,
}

impl BuiltinVariadic {
    pub fn identity(&self) -> Option<i64> {
        match self {
            Self::Add => Some(0),
            Self::Sub => None,
            Self::Mul => Some(1),
            Self::Div => None,
            Self::Rem => None,
        }
    }

    pub fn into_instructions(
        &self,
        mut args: &[Arc<Expression>],
        span: &Range<usize>,
        instructions: &BlockStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let mut instructions = instructions.clone();
        let mut bindings = bindings.clone();

        (instructions, bindings, args) = match self.identity() {
            Some(identity) => (
                instructions.with_instruction(Instruction::LiteralInt(identity)),
                bindings,
                args,
            ),
            None => {
                // Builtin isn't symmetric (e.g. `-`), which means that we need to
                // start with the first argument.
                let Some(first) = args.first() else {
                    return Err(SemanticError::new(
                        format!(
                            "function '{}' called with no argument, expected at least one",
                            <&'static str>::from(self),
                        ),
                        span.clone(),
                        globals.document(),
                    ));
                };

                let (instructions, bindings) =
                    first.into_instructions(&instructions, &bindings, globals)?;
                (instructions, bindings, &args[1..])
            }
        };
        let mut acc = instructions.current_register();

        for expression in args {
            (instructions, bindings) =
                expression.into_instructions(&instructions, &bindings, globals)?;

            instructions = instructions.with_instruction(match self {
                BuiltinVariadic::Add => Instruction::Add(acc, instructions.current_register()),
                BuiltinVariadic::Sub => Instruction::Sub(acc, instructions.current_register()),
                BuiltinVariadic::Mul => Instruction::Mul(acc, instructions.current_register()),
                BuiltinVariadic::Div => Instruction::Div(acc, instructions.current_register()),
                BuiltinVariadic::Rem => Instruction::Rem(acc, instructions.current_register()),
            });
            acc = instructions.current_register();
        }

        Ok((instructions, bindings))
    }
}

#[derive(Debug, Clone, Copy, strum_macros::EnumString, strum_macros::IntoStaticStr)]
pub enum BuiltinBinary {
    #[strum(to_string = ">>")]
    ShiftRight,
    #[strum(to_string = "<<")]
    ShiftLeft,
}

impl BuiltinBinary {
    pub fn into_instructions(
        &self,
        args: &[Arc<Expression>],
        span: &Range<usize>,
        instructions: &BlockStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let mut instructions = instructions.clone();
        let mut bindings = bindings.clone();

        let [a, b] =
            extract_args_exact(args, <&'static str>::from(self), span, globals.document())?;

        (instructions, bindings) = a.into_instructions(&instructions, &bindings, globals)?;
        let arg_a = instructions.current_register();

        (instructions, bindings) = b.into_instructions(&instructions, &bindings, globals)?;
        let arg_b = instructions.current_register();

        instructions = instructions.with_instruction(match self {
            BuiltinBinary::ShiftRight => Instruction::ShiftRight(arg_a, arg_b),
            BuiltinBinary::ShiftLeft => Instruction::ShiftLeft(arg_a, arg_b),
        });

        Ok((instructions, bindings))
    }
}

/// Builtin functions that take an arbitrary number of arguments, which are evaluated by combining
/// the results of the operation performed on each pair of argument with logical and.
#[derive(Debug, Clone, Copy, strum_macros::EnumString, strum_macros::IntoStaticStr)]
pub enum BuiltinComparison {
    #[strum(to_string = "=")]
    Equal,
    #[strum(to_string = "!=")]
    NotEqual,
    #[strum(to_string = ">")]
    GreaterThan,
    #[strum(to_string = "<")]
    LessThan,
    #[strum(to_string = ">=")]
    GreaterThanEqual,
    #[strum(to_string = "<=")]
    LessThanEqual,
}

impl BuiltinComparison {
    pub fn into_instructions(
        &self,
        args: &[Arc<Expression>],
        instructions: &BlockStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let mut instructions = instructions.clone();
        let mut bindings = bindings.clone();

        let mut last_arg = None;

        let final_label = globals.next_label("final");
        let done_label = globals.next_label("done_comparison");

        let mut phi_labels = Vec::new();

        for (pos, expression) in args.iter().with_position() {
            (instructions, bindings) =
                expression.into_instructions(&instructions, &bindings, globals)?;
            let arg = instructions.current_register();

            let next_label = match pos {
                Position::Last | Position::Only => final_label.clone(),
                _ => globals.next_label("arg"),
            };

            if let Some(last_arg) = last_arg {
                instructions = instructions.with_instruction(match self {
                    BuiltinComparison::Equal => Instruction::Equal(last_arg, arg),
                    BuiltinComparison::NotEqual => Instruction::NotEqual(last_arg, arg),
                    BuiltinComparison::GreaterThan => Instruction::GreaterThan(last_arg, arg),
                    BuiltinComparison::LessThan => Instruction::LessThan(last_arg, arg),
                    BuiltinComparison::GreaterThanEqual => {
                        Instruction::GreaterThanEqual(last_arg, arg)
                    }
                    BuiltinComparison::LessThanEqual => Instruction::LessThanEqual(last_arg, arg),
                });
                let result = instructions.current_register();

                instructions = instructions.with_jump(JumpInstruction::JumpNotZero(
                    result,
                    next_label.clone(),
                    done_label.clone(),
                ));
            }
            if !matches!(pos, Position::Last | Position::Only) {
                phi_labels.push((next_label.clone(), Value::Literal(0)));
                instructions = instructions.with_label(next_label.clone());
            }

            last_arg = Some(arg);
        }

        instructions = instructions.with_label(final_label.clone());
        phi_labels.push((final_label.clone(), Value::Literal(1)));
        instructions = instructions.with_jump(JumpInstruction::Jump(done_label.clone()));

        instructions = instructions.with_label(done_label.clone());
        instructions = instructions.with_instruction(Instruction::Phi(phi_labels.into()));

        Ok((instructions, bindings))
    }
}

/// Builtin functions with special handling.
#[derive(Debug, Clone, Copy, strum_macros::EnumString, strum_macros::IntoStaticStr)]
pub enum BuiltinSpecial {
    #[strum(to_string = "and")]
    And,
    #[strum(to_string = "or")]
    Or,
    #[strum(to_string = "not")]
    Not,
    #[strum(to_string = "if")]
    If,
}

impl BuiltinSpecial {
    pub fn into_instructions(
        &self,
        args: &[Arc<Expression>],
        span: &Range<usize>,
        instructions: &BlockStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        match self {
            BuiltinSpecial::And => {
                let mut instructions = instructions.clone();
                let mut bindings = bindings.clone();

                let final_label = globals.next_label("final");
                let done_label = globals.next_label("and_done");

                let mut phi_labels = Vec::new();

                let mut last_label = None;
                // Default to true if zero args are given.
                let mut last_value = Value::Literal(1);

                for (pos, arg) in args.iter().with_position() {
                    let label = last_label.unwrap_or_else(|| globals.next_label("arg"));
                    instructions = instructions.with_label(label.clone());

                    (instructions, bindings) =
                        arg.into_instructions(&instructions, &bindings, globals)?;
                    let value = instructions.current_register();

                    let next_label = match pos {
                        Position::Last | Position::Only => final_label.clone(),
                        _ => globals.next_label("arg"),
                    };
                    instructions = instructions.with_jump(JumpInstruction::JumpNotZero(
                        value,
                        next_label.clone(),
                        done_label.clone(),
                    ));

                    phi_labels.push((label.clone(), Value::Literal(0)));

                    last_value = Value::Register(value);
                    last_label = Some(next_label);
                }

                instructions = instructions.with_label(final_label.clone());
                instructions = instructions.with_jump(JumpInstruction::Jump(done_label.clone()));
                phi_labels.push((final_label.clone(), last_value));

                instructions = instructions.with_label(done_label.clone());
                instructions = instructions.with_instruction(Instruction::Phi(phi_labels.into()));

                Ok((instructions, bindings))
            }
            BuiltinSpecial::Or => {
                let mut instructions = instructions.clone();
                let mut bindings = bindings.clone();

                let final_label = globals.next_label("final");
                let done_label = globals.next_label("or_done");

                let mut phi_labels = Vec::new();

                let mut last_label = None;

                for (pos, arg) in args.iter().with_position() {
                    let label = last_label.unwrap_or_else(|| globals.next_label("arg"));
                    instructions = instructions.with_label(label.clone());

                    (instructions, bindings) =
                        arg.into_instructions(&instructions, &bindings, globals)?;
                    let value = instructions.current_register();

                    let next_label = match pos {
                        Position::Last | Position::Only => final_label.clone(),
                        _ => globals.next_label("arg"),
                    };
                    instructions = instructions.with_jump(JumpInstruction::JumpNotZero(
                        value,
                        done_label.clone(),
                        next_label.clone(),
                    ));

                    phi_labels.push((label.clone(), Value::Register(value)));

                    last_label = Some(next_label);
                }

                instructions = instructions.with_label(final_label.clone());
                instructions = instructions.with_jump(JumpInstruction::Jump(done_label.clone()));
                phi_labels.push((final_label.clone(), Value::Literal(0)));

                instructions = instructions.with_label(done_label.clone());
                instructions = instructions.with_instruction(Instruction::Phi(phi_labels.into()));

                Ok((instructions, bindings))
            }
            BuiltinSpecial::Not => {
                let mut instructions = instructions.clone();
                let mut bindings = bindings.clone();

                let [expr] =
                    extract_args_exact(args, <&'static str>::from(self), span, globals.document())?;
                (instructions, bindings) =
                    expr.into_instructions(&instructions, &bindings, globals)?;
                let arg = instructions.current_register();
                let then_label = globals.next_label("then");
                let else_label = globals.next_label("else");
                let not_label = globals.next_label("not");

                instructions = instructions.with_jump(JumpInstruction::JumpNotZero(
                    arg,
                    then_label.clone(),
                    else_label.clone(),
                ));

                instructions = instructions.with_label(then_label.clone());
                instructions = instructions.with_jump(JumpInstruction::Jump(not_label.clone()));

                instructions = instructions.with_label(else_label.clone());
                instructions = instructions.with_jump(JumpInstruction::Jump(not_label.clone()));

                instructions = instructions.with_label(not_label.clone());
                instructions = instructions.with_instruction(Instruction::Phi(
                    [
                        (then_label, Value::Literal(0)),
                        (else_label, Value::Literal(1)),
                    ]
                    .into(),
                ));

                Ok((instructions, bindings))
            }
            BuiltinSpecial::If => {
                let mut instructions = instructions.clone();
                let mut bindings = bindings.clone();

                let [condition_expr, then_expr, else_expr] =
                    extract_args_exact(args, <&'static str>::from(self), span, globals.document())?;

                let then_label = globals.next_label("then");
                let else_label = globals.next_label("else");
                let done_label = globals.next_label("if_done");

                (instructions, bindings) =
                    condition_expr.into_instructions(&instructions, &bindings, globals)?;
                let condition = instructions.current_register();

                instructions = instructions.with_jump(JumpInstruction::JumpNotZero(
                    condition,
                    then_label.clone(),
                    else_label.clone(),
                ));

                instructions = instructions.with_label(then_label.clone());
                (instructions, bindings) =
                    then_expr.into_instructions(&instructions, &bindings, globals)?;
                let then = instructions.current_register();
                instructions = instructions.with_jump(JumpInstruction::Jump(done_label.clone()));

                instructions = instructions.with_label(else_label.clone());
                (instructions, bindings) =
                    else_expr.into_instructions(&instructions, &bindings, globals)?;
                let else_ = instructions.current_register();
                instructions = instructions.with_jump(JumpInstruction::Jump(done_label.clone()));

                instructions = instructions.with_label(done_label.clone());
                instructions = instructions.with_instruction(Instruction::Phi(
                    [
                        (then_label, Value::Register(then)),
                        (else_label, Value::Register(else_)),
                    ]
                    .into(),
                ));

                Ok((instructions, bindings))
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Variadic(BuiltinVariadic),
    Binary(BuiltinBinary),
    Comparison(BuiltinComparison),
    Special(BuiltinSpecial),
}

impl Builtin {
    const VARIANTS: [Self; 17] = [
        Self::Variadic(BuiltinVariadic::Add),
        Self::Variadic(BuiltinVariadic::Sub),
        Self::Variadic(BuiltinVariadic::Mul),
        Self::Variadic(BuiltinVariadic::Div),
        Self::Variadic(BuiltinVariadic::Rem),
        Self::Binary(BuiltinBinary::ShiftRight),
        Self::Binary(BuiltinBinary::ShiftLeft),
        Self::Comparison(BuiltinComparison::Equal),
        Self::Comparison(BuiltinComparison::NotEqual),
        Self::Comparison(BuiltinComparison::GreaterThan),
        Self::Comparison(BuiltinComparison::LessThan),
        Self::Comparison(BuiltinComparison::GreaterThanEqual),
        Self::Comparison(BuiltinComparison::LessThanEqual),
        Self::Special(BuiltinSpecial::And),
        Self::Special(BuiltinSpecial::Or),
        Self::Special(BuiltinSpecial::Not),
        Self::Special(BuiltinSpecial::If),
    ];
}

impl FromStr for Builtin {
    type Err = strum::ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(builtin) = s.parse() {
            Ok(Self::Variadic(builtin))
        } else if let Ok(builtin) = s.parse() {
            Ok(Self::Binary(builtin))
        } else if let Ok(builtin) = s.parse() {
            Ok(Self::Comparison(builtin))
        } else {
            let builtin = s.parse()?;
            Ok(Self::Special(builtin))
        }
    }
}

impl From<&Builtin> for &'static str {
    fn from(value: &Builtin) -> Self {
        match value {
            Builtin::Variadic(builtin) => Self::from(builtin),
            Builtin::Binary(builtin) => Self::from(builtin),
            Builtin::Comparison(builtin) => Self::from(builtin),
            Builtin::Special(builtin) => Self::from(builtin),
        }
    }
}

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
    pub fn with_external_variable(
        &self,
        symbol: Arc<syntax::Symbol>,
        function_identifier: Identifier,
    ) -> Self {
        self.with_value(symbol, BoundValue::ExternalVariable(function_identifier))
    }
    pub fn with_builtin(&self, symbol: Arc<syntax::Symbol>, builtin: Builtin) -> Self {
        self.with_value(symbol, BoundValue::Builtin(builtin))
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
    ExternalVariable(Identifier),
    /// The binding represents a builtin function
    Builtin(Builtin),
}

impl IntoInstructions for syntax::Defun {
    fn into_instructions(
        &self,
        instructions: &BlockStack,
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
        instructions: &BlockStack,
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
        instructions: &BlockStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
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
        instructions: &BlockStack,
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
        instructions: &BlockStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        match bindings
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
            Some(BoundValue::Register(_) | BoundValue::ExternalVariable(_)) => {
                return Err(SemanticError::new(
                    format!("'{}' is not a function", &self.function.0),
                    self.span.clone(),
                    globals.document(),
                ));
            }
            Some(BoundValue::Function(identifier)) => {
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
                let instruction = Instruction::FunctionCall(identifier.clone(), registers);

                Ok((instructions.with_instruction(instruction), bindings))
            }
            Some(BoundValue::Builtin(Builtin::Variadic(builtin))) => {
                builtin.into_instructions(&self.args, &self.span, instructions, bindings, globals)
            }
            Some(BoundValue::Builtin(Builtin::Binary(builtin))) => {
                builtin.into_instructions(&self.args, &self.span, instructions, bindings, globals)
            }
            Some(BoundValue::Builtin(Builtin::Comparison(builtin))) => {
                builtin.into_instructions(&self.args, instructions, bindings, globals)
            }
            Some(BoundValue::Builtin(Builtin::Special(builtin))) => {
                builtin.into_instructions(&self.args, &self.span, instructions, bindings, globals)
            }
        }
    }
}

impl IntoInstructions for syntax::Symbol {
    fn into_instructions(
        &self,
        instructions: &BlockStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let instruction = match bindings.lookup(self).as_ref().map(|x| x.as_ref()) {
            Some(BoundValue::Register(register)) => Instruction::Register(*register),
            Some(BoundValue::Function(identifier)) => {
                Instruction::GlobalAddress(identifier.clone())
            }
            Some(BoundValue::ExternalVariable(identifier)) => {
                Instruction::Global(identifier.clone())
            }
            Some(BoundValue::Builtin(function)) => {
                return Err(SemanticError::new(
                    format!(
                        "cannot dereference builtin function '{}' as a value",
                        <&'static str>::from(function),
                    ),
                    self.1.clone(),
                    globals.document(),
                ));
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
        instructions: &BlockStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError> {
        let instruction = match self {
            syntax::Literal::Int(int) => Instruction::LiteralInt(int.value),
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
    instructions: BlockStack,
}

impl Function {
    /// Get the identifier for a defun value.
    pub fn defun_identifier(function: Arc<syntax::Defun>) -> Identifier {
        match function.name.0.as_ref() {
            "main" => Identifier::new("_main"),
            _ => Identifier::from_symbol(&function.name),
        }
    }
    pub fn compile(
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
            .into_instructions(&BlockStack::new(), &bindings, state)
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

        // Hard-coded list of external functions and variables which can be called in Ship. Would be
        // nice to scan this from header files, but that seems like a lot of work.
        let external_functions = [
            syntax::Symbol::new_static("printf"),
            syntax::Symbol::new_static("strtol"),
            syntax::Symbol::new_static("fgets"),
            syntax::Symbol::new_static("malloc"),
            syntax::Symbol::new_static("free"),
        ];
        let external_variables = [
            syntax::Symbol::new_static("stdin"),
            syntax::Symbol::new_static("stdout"),
            syntax::Symbol::new_static("stderr"),
        ];

        for symbol in external_functions {
            let identifier = Identifier::from_symbol(&symbol);
            bindings = bindings.with_function(Arc::from(symbol), identifier);
        }
        for symbol in external_variables {
            let identifier = Identifier::from_symbol(&symbol);
            bindings = bindings.with_external_variable(Arc::from(symbol), identifier);
        }

        for builtin in Builtin::VARIANTS {
            bindings = bindings.with_builtin(
                Arc::new(syntax::Symbol::new_static((&builtin).into())),
                builtin,
            );
        }

        let defun_identifiers = file
            .statements
            .iter()
            .filter_map(|statement| match statement.as_ref() {
                syntax::Statement::Defun(defun) => Some(defun.clone()),
                _ => None,
            })
            .map(|defun| Ok((defun.clone(), Function::defun_identifier(defun))))
            .collect::<Result<Box<[_]>, _>>()?;

        // Add all function identifiers to the global scope.
        for (defun, identifier) in &defun_identifiers {
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

            bindings = bindings.with_function(defun.name.clone(), identifier.clone())
        }

        let functions = defun_identifiers
            .into_iter()
            .map(|(defun, identifier)| {
                Function::compile(identifier, defun.clone(), &bindings, &mut state)
            })
            .collect::<Result<Box<[_]>, _>>()?;

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
        let top_level = Function::compile(
            Identifier::new("main"),
            Arc::new(top_level_defun),
            &bindings,
            &mut state,
        )?;

        Ok(Self {
            functions,
            top_level,
            state,
        })
    }
}
