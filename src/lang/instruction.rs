use std::{
    fmt::{Display, Write},
    iter,
    sync::Arc,
    vec,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label(Arc<str>);

impl Label {
    /// Can only be created by this module.
    fn new(label: Arc<str>) -> Self {
        Self(label)
    }
    #[allow(dead_code)]
    fn from_str(label: &str) -> Self {
        Self(Arc::from(label))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Register(Register),
    Literal(i64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Register(register) => write!(f, "{}", register),
            Value::Literal(value) => write!(f, "{}", value),
        }
    }
}

/// Every instruction has a result which is assigned to a new register. Their values are indices
/// into the instruction stack, i.e. previous computations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    FunctionCall(Identifier, Box<[Register]>),
    /// Assigns the given register to the result.
    Register(Register),
    LiteralInt(i64),
    /// Assigns the address of the given global identifier to the result.
    GlobalAddress(Identifier),
    /// Assigns the value of the given global variable to the result.
    Global(Identifier),
    /// QBE's phi instruction: Assigns a value to the result based on from which label this
    /// instruction was jumped to.
    Phi(Box<[(Label, Value)]>),
    /// Assigns the addition of the two registers to the result.
    Add(Register, Register),
    /// Assigns the subtraction of the two registers to the result.
    Sub(Register, Register),
    /// Assigns the multiplication of the two registers to the result.
    Mul(Register, Register),
    /// Assigns the integer division of the first register by the second to the result.
    Div(Register, Register),
    /// Assigns the remainder of the integer division of the first register by the second to the
    /// result.
    Rem(Register, Register),
    /// Shift the bits of the first register right and assign it to the result.
    ShiftRight(Register, Register),
    /// Shift the bits of the first register left and assign it to the result.
    ShiftLeft(Register, Register),
    /// Assigns 1 to the result if the first register is equal to the second, otherwise 0.
    Equal(Register, Register),
    /// Assigns 1 to the result if the first register is not equal to the second, otherwise 0.
    NotEqual(Register, Register),
    /// Assigns 1 to the result if the first register is greater than the second, otherwise 0.
    GreaterThan(Register, Register),
    /// Assigns 1 to the result if the first register is less than the second, otherwise 0.
    LessThan(Register, Register),
    /// Assigns 1 to the result if the first register is greater than or equal to the second,
    /// otherwise 0.
    GreaterThanEqual(Register, Register),
    /// Assigns 1 to the result if the first register is less than or equal to the second,
    /// otherwise 0.
    LessThanEqual(Register, Register),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JumpInstruction {
    /// Jumps to the specified label.
    Jump(Label),
    /// Jumps to the first label if the register is not zero, otherwise to the second.
    JumpNotZero(Register, Label, Label),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgramLine {
    Instruction(Register, Instruction),
    Label(Label),
    Jump(JumpInstruction),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InstructionStackLink {
    instruction: Instruction,
    previous: Option<Arc<InstructionStackLink>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BlockStackLink {
    /// The label that's attached to the block, if any.
    label: Option<Label>,
    instructions: Option<Arc<InstructionStackLink>>,
    jump: Option<JumpInstruction>,
    previous: Option<Arc<BlockStackLink>>,
}

impl BlockStackLink {
    fn iter_block(&self, register_index: &mut usize) -> BlockStackLinkIter {
        let mut instructions = self.instructions.clone();
        let reverse_instructions = iter::from_fn(move || match instructions.clone() {
            Some(link) => {
                instructions = link.previous.clone();

                let returned_index = *register_index;
                *register_index -= 1;

                Some(ProgramLine::Instruction(
                    Register::local(returned_index),
                    link.instruction.clone(),
                ))
            }
            None => None,
        });

        BlockStackLinkIter {
            label: self.label.clone(),
            instructions: reverse_instructions.collect::<Box<_>>().into_iter().rev(),
            jump: self.jump.clone(),
        }
    }
}

struct BlockStackLinkIter {
    label: Option<Label>,
    instructions: iter::Rev<vec::IntoIter<ProgramLine>>,
    jump: Option<JumpInstruction>,
}

impl Iterator for BlockStackLinkIter {
    type Item = ProgramLine;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(label) = self.label.clone() {
            self.label = None;
            Some(ProgramLine::Label(label))
        } else if let Some(next) = self.instructions.next() {
            Some(next)
        } else if let Some(jump) = self.jump.clone() {
            self.jump = None;
            Some(ProgramLine::Jump(jump))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockStack {
    inner: Arc<BlockStackLink>,
    len: usize,
}

impl BlockStack {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(BlockStackLink {
                label: None,
                instructions: None,
                jump: None,
                previous: None,
            }),
            len: 0,
        }
    }

    pub fn with_instruction(&self, instruction: Instruction) -> Self {
        Self {
            inner: Arc::new(BlockStackLink {
                instructions: Some(Arc::new(InstructionStackLink {
                    instruction: instruction,
                    previous: self.inner.instructions.clone(),
                })),
                ..self.inner.as_ref().clone()
            }),
            len: self.len + 1,
        }
    }

    pub fn with_label(&self, label: Label) -> Self {
        Self {
            inner: Arc::new(BlockStackLink {
                label: Some(label),
                instructions: None,
                jump: None,
                previous: Some(self.inner.clone()),
            }),
            len: self.len,
        }
    }

    pub fn with_jump(&self, jump: JumpInstruction) -> Self {
        Self {
            inner: Arc::new(BlockStackLink {
                label: None,
                instructions: None,
                jump: None,
                previous: Some(Arc::new(BlockStackLink {
                    jump: Some(jump),
                    ..self.inner.as_ref().clone()
                })),
            }),
            len: self.len,
        }
    }

    /// Get reference to last register in this instruction stack.
    pub fn current_register(&self) -> Register {
        Register::local(self.len)
    }

    pub fn lines(&self) -> impl Iterator<Item = ProgramLine> {
        let mut inner = Some(self.inner.clone());
        let mut index = self.len;

        iter::from_fn(move || {
            inner.clone().map(|link| {
                inner = link.previous.clone();

                link.iter_block(&mut index)
            })
        })
        .collect::<Box<[_]>>()
        .into_iter()
        .rev()
        .flatten()
    }
}

pub type IntoInstructionsReturn = (BlockStack, BindingStack);

pub trait IntoInstructions {
    fn into_instructions(
        &self,
        instructions: &BlockStack,
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
    label_counter: Counter,
    document: lex::Document,
}

impl GlobalState {
    pub fn new(document: lex::Document) -> Self {
        Self {
            strings: Vec::new(),
            string_counter: Counter::new(),
            label_counter: Counter::new(),
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

    pub fn next_label(&mut self, prefix: &str) -> Label {
        Label::new(Arc::from(format!(
            "{}{}",
            prefix,
            self.label_counter.next()
        )))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn instruction_stack_instructions() {
        let stack = BlockStack::new();
        let stack = stack.with_instruction(Instruction::Register(Register::param(1)));

        let stack = stack.with_label(Label::from_str("test"));
        let stack = stack.with_instruction(Instruction::Register(Register::param(2)));
        let stack = stack.with_jump(JumpInstruction::Jump(Label::from_str("jump")));

        let stack = stack.with_label(Label::from_str("jump"));
        let stack =
            stack.with_instruction(Instruction::GlobalAddress(Identifier::new("my_global")));
        let stack = stack.with_instruction(Instruction::Register(Register::param(3)));

        assert_eq!(
            stack.lines().collect::<Vec<_>>(),
            vec![
                ProgramLine::Instruction(
                    Register::local(1),
                    Instruction::Register(Register::param(1))
                ),
                ProgramLine::Label(Label::from_str("test")),
                ProgramLine::Instruction(
                    Register::local(2),
                    Instruction::Register(Register::param(2))
                ),
                ProgramLine::Jump(JumpInstruction::Jump(Label::from_str("jump"))),
                ProgramLine::Label(Label::from_str("jump")),
                ProgramLine::Instruction(
                    Register::local(3),
                    Instruction::GlobalAddress(Identifier::new("my_global"))
                ),
                ProgramLine::Instruction(
                    Register::local(4),
                    Instruction::Register(Register::param(3))
                ),
            ]
        )
    }
}
