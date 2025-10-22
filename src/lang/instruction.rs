use std::{
    fmt::{Display, Write},
    iter,
    sync::Arc,
};

use crate::{error::SemanticError, lang::BindingStack, lex, syntax};

/// Refers to register in the generated QBE code. It's set to the instruction at the given
/// index in the register stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Register(RegisterId);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RegisterId {
    Local(usize),
    Param(usize),
}

impl Register {
    /// Can only be created by this module.
    fn local(id: usize) -> Self {
        Self(RegisterId::Local(id))
    }
    pub fn param(id: usize) -> Self {
        Self(RegisterId::Param(id))
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            RegisterId::Local(id) => write!(f, "%l{}", id),
            RegisterId::Param(id) => write!(f, "%p{}", id),
        }
    }
}

/// Identifier for an object stored in the global scope of the generated QBE code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier(Arc<str>);

impl Identifier {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn new(identifier: &str) -> Self {
        Self(Arc::from(identifier))
    }

    /// Convert Ship symbol to unique valid QBE identifier name.
    ///
    /// QBE allows (i think...) [A-Za-Z0-9_]+
    pub fn from_symbol(symbol: &syntax::Symbol) -> Self {
        let mut identifier = String::new();

        for chr in symbol.0.chars() {
            match chr {
                'A'..='Z' | 'a'..='z' | '0'..='9' => identifier.push(chr),
                '_' => identifier.push_str("__"),
                chr => {
                    write!(identifier, "_u{:x}_", chr as u32)
                        .expect("String::write_fmt is infaliable");
                }
            }
        }

        Self(Arc::from(identifier))
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}

/// Every instruction has a result which is assigned to a new register. Their values are indices
/// into the instruction stack, i.e. previous computations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    FunctionCall(Identifier, Box<[Register]>),
    /// Assigns the given register to the result.
    Register(Register),
    LiteralInt(Arc<syntax::Int>),
    /// Assigns the address of the given global identifier to the result.
    GlobalAddress(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InstructionStackLink {
    instruction: Instruction,
    previous: Option<Arc<InstructionStackLink>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionStack {
    inner: Option<Arc<InstructionStackLink>>,
    len: usize,
}

impl InstructionStack {
    pub fn new() -> Self {
        Self {
            inner: None,
            len: 0,
        }
    }

    pub fn with_instruction(&self, instruction: Instruction) -> Self {
        Self {
            inner: Some(Arc::new(InstructionStackLink {
                instruction: instruction,
                previous: self.inner.clone(),
            })),
            len: self.len + 1,
        }
    }

    /// Get reference to last register in this instruction stack.
    pub fn current_register(&self) -> Register {
        Register::local(self.len)
    }

    pub fn instructions(&self) -> impl ExactSizeIterator<Item = (Register, Instruction)> {
        let mut inner = self.inner.clone();
        let mut index = self.len;
        let reverse_instructions = iter::from_fn(move || match inner.clone() {
            Some(link) => {
                inner = link.previous.clone();

                let returned_index = index;
                index -= 1;

                Some((Register::local(returned_index), link.instruction.clone()))
            }
            None => None,
        });

        reverse_instructions.collect::<Box<_>>().into_iter().rev()
    }
}

pub type IntoInstructionsReturn = (InstructionStack, BindingStack);

pub trait IntoInstructions {
    fn into_instructions(
        &self,
        instructions: &InstructionStack,
        bindings: &BindingStack,
        globals: &mut GlobalState,
    ) -> Result<IntoInstructionsReturn, SemanticError>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Counter(u32);
impl Counter {
    fn new() -> Self {
        Self(0)
    }

    fn next(&mut self) -> u32 {
        let result = self.0;
        self.0 += 1;
        result
    }
}

/// Stores global state used during QBE instructions generation.
/// This includes:
/// - List of all data blobs store in the global scope. (Currently only string literals are stored
///   there.)
/// - The parsed document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalState {
    strings: Vec<(Identifier, Arc<syntax::StringLiteral>)>,
    string_counter: Counter,
    document: lex::Document,
}

impl GlobalState {
    pub fn new(document: lex::Document) -> Self {
        Self {
            strings: Vec::new(),
            string_counter: Counter::new(),
            document,
        }
    }

    pub fn add_string(&mut self, string: Arc<syntax::StringLiteral>) -> Identifier {
        let identifier = self.next_string_identifier();
        self.strings.push((identifier.clone(), string));
        identifier
    }

    pub fn items(&self) -> impl Iterator<Item = &(Identifier, Arc<syntax::StringLiteral>)> {
        self.strings.iter()
    }

    pub fn document(&self) -> lex::Document {
        self.document.clone()
    }

    fn next_string_identifier(&mut self) -> Identifier {
        Identifier(Arc::from(format!("string_{}", self.string_counter.next())))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn instruction_stack_instructions() {
        let stack = InstructionStack::new();
        let stack = stack.with_instruction(Instruction::Register(Register::param(2)));
        let stack = stack.with_instruction(Instruction::Register(Register::param(3)));
        let stack =
            stack.with_instruction(Instruction::GlobalAddress(Identifier::new("my_global")));
        let stack = stack.with_instruction(Instruction::Register(Register::param(4)));

        assert_eq!(
            stack.instructions().collect::<Vec<_>>(),
            vec![
                (
                    Register::local(1),
                    Instruction::Register(Register::param(2))
                ),
                (
                    Register::local(2),
                    Instruction::Register(Register::param(3))
                ),
                (
                    Register::local(3),
                    Instruction::GlobalAddress(Identifier::new("my_global"))
                ),
                (
                    Register::local(4),
                    Instruction::Register(Register::param(4))
                ),
            ]
        )
    }
}
