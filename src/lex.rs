use std::{
    cell::RefCell,
    fmt::{Debug, Display, Write},
    sync::LazyLock,
};

use arc_slice::ArcSlice;
use regex::Regex;
use serde::{Deserialize, Serialize};
use strum_macros::EnumString;

use crate::syntax::{FloatTypeSpecifier, IntTypeSpecifier};

#[derive(Clone, PartialEq, Eq)]
pub struct Document {
    document: ArcSlice<str>,
    /// Byte offset of remaining text in the original document.
    pos: RefCell<usize>,
}

impl Document {
    pub fn new(document: ArcSlice<str>) -> Self {
        Self {
            document: document,
            pos: RefCell::new(0),
        }
    }

    pub fn from_str(string: &str) -> Self {
        Self {
            document: ArcSlice::from_slice(string),
            pos: RefCell::new(0),
        }
    }

    pub fn pos(&self) -> usize {
        *self.pos.borrow()
    }

    pub fn rest(&self) -> &str {
        &self.document[self.pos()..]
    }

    #[deprecated]
    pub fn with_remaining_len<'b>(&'b self, len: usize) -> Self {
        Self {
            document: self.document.clone(),
            pos: RefCell::new(self.document.len() - len),
        }
    }

    #[deprecated]
    pub fn with_consumed<'b>(&'b self, len: usize) -> Self {
        Self {
            document: self.document.clone(),
            pos: RefCell::new(self.pos() + len),
        }
    }

    pub fn set_remaining_len(&self, len: usize) -> () {
        *self.pos.borrow_mut() = self.document.len() - len;
    }

    pub fn consume(&self, len: usize) -> () {
        *self.pos.borrow_mut() += len;
    }

    // /// Update internal position by generating a length of the rest string using the specified
    // /// function. The function takes the current rest and should return the new length of remaining
    // /// bytes.
    // pub fn update_len<F>(&mut self, updater: F)
    // where
    //     F: FnOnce(&str) -> usize,
    // {
    //     self.set_remaining_len(updater(&self.document[self.pos..]));
    // }

    // /// Update internal position by shifting it an amount of bytes generating using the specified
    // /// function. The function takes the current rest and must return the amount of bytes to
    // /// consume.
    // pub fn update_consumed<F>(&mut self, updater: F)
    // where
    //     F: FnOnce(&str) -> usize,
    // {
    //     self.consume(updater(&self.document[self.pos..]));
    // }

    pub fn strip_prefix(&self, prefix: &str) -> Option<()> {
        if self.rest().starts_with(prefix) {
            *self.pos.borrow_mut() += prefix.len();
            Some(())
        } else {
            None
        }
    }

    /// Match pattern at start of `Self::rest()`, returning the match if found, consuming the
    /// matched bytes.
    pub fn strip_prefix_match(&self, pattern: &Regex) -> Option<regex::Match> {
        match pattern.find(&self.document[self.pos()..]) {
            Some(mat) => {
                if mat.start() == 0 {
                    *self.pos.borrow_mut() += mat.len();
                    Some(mat)
                } else {
                    None
                }
            }

            None => None,
        }
    }

    /// Match pattern at start of `Self::rest()`, returning the captures if found, consuming the
    /// matched bytes.
    pub fn strip_prefix_captures(&self, pattern: &Regex) -> Option<regex::Captures> {
        match pattern.captures(&self.document[self.pos()..]) {
            Some(cap) => {
                if cap.get(0).unwrap().start() == 0 {
                    *self.pos.borrow_mut() += cap.get(0).unwrap().len();
                    Some(cap)
                } else {
                    None
                }
            }

            None => None,
        }
    }
}

impl Debug for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Document")
            .field("rest", &self.rest())
            .field("pos", &self.pos())
            .finish()
    }
}

pub trait Tokenizer {
    fn token(document: &mut Document) -> Option<Self>
    where
        Self: Sized;
}

pub enum Token {
    WhiteSpace(WhiteSpace),
    OpenParen(OpenParen),
    CloseParen(CloseParen),
    FloatLiteral(FloatLiteral),
    IntLiteral(IntLiteral),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::WhiteSpace(white_space) => write!(f, "{}", white_space),
            Token::OpenParen(open_paren) => write!(f, "{}", open_paren),
            Token::CloseParen(close_paren) => write!(f, "{}", close_paren),
            Token::FloatLiteral(float_literal) => write!(f, "{}", float_literal),
            Token::IntLiteral(int_literal) => write!(f, "{}", int_literal),
        }
    }
}

impl Tokenizer for Token {
    fn token(document: &mut Document) -> Option<Self> {
        if let Some(token) = WhiteSpace::token(document) {
            return Some(Self::WhiteSpace(token));
        }
        if let Some(token) = OpenParen::token(document) {
            return Some(Self::OpenParen(token));
        }
        if let Some(token) = CloseParen::token(document) {
            return Some(Self::CloseParen(token));
        }
        if let Some(token) = FloatLiteral::token(document) {
            return Some(Self::FloatLiteral(token));
        }
        if let Some(token) = IntLiteral::token(document) {
            return Some(Self::IntLiteral(token));
        }

        None
    }
}

#[derive(Debug)]
pub struct WhiteSpace;

impl Display for WhiteSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("white space")
    }
}

#[derive(Debug)]
pub struct OpenParen;

impl Display for OpenParen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("'('")
    }
}

#[derive(Debug)]
pub struct CloseParen;

impl Display for CloseParen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("')'")
    }
}

#[derive(Debug)]
pub struct FloatLiteral {
    pub sign: Sign,
    pub int: ArcSlice<str>,
    pub fract: ArcSlice<str>,
    pub type_specifier: Option<super::syntax::FloatTypeSpecifier>,
}

impl Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sign == Sign::Negative {
            f.write_char('-')?;
        }
        write!(f, "{}.{}", self.int, self.fract)?;
        if let Some(specifier) = self.type_specifier {
            write!(f, "{}", specifier)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct IntLiteral {
    pub sign: Sign,
    pub int: ArcSlice<str>,
    pub type_specifier: Option<super::syntax::IntTypeSpecifier>,
}

impl Display for IntLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sign == Sign::Negative {
            f.write_char('-')?;
        }
        f.write_str(&self.int)?;
        if let Some(specifier) = self.type_specifier {
            write!(f, "{}", specifier)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Sign {
    Positive,
    Negative,
}

#[derive(Debug)]
pub struct DoubleQuote;

impl Display for DoubleQuote {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("'\"'")
    }
}

#[derive(Debug, EnumString)]
pub enum EscapeSequence {
    #[strum(serialize = "\\n")]
    Newline,
    #[strum(serialize = "\\0")]
    NullByte,
    #[strum(serialize = "\\a")]
    Alert,
    #[strum(serialize = "\\b")]
    Backspace,
    #[strum(serialize = "\\t")]
    Tab,
    #[strum(serialize = "\\v")]
    VerticalTab,
    #[strum(serialize = "\\f")]
    Formfeed,
    #[strum(serialize = "\\r")]
    CarriageReturn,
    #[strum(serialize = "\\e")]
    Escape,
    #[strum(serialize = "\\s")]
    Space,
    #[strum(serialize = "\\\"")]
    DoubleQuote,
    #[strum(serialize = "\\\\")]
    Backslash,
    #[strum(serialize = "\\d")]
    Delete,
}

impl Display for EscapeSequence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Newline => "'\\n'",
            Self::NullByte => "'\\0'",
            Self::Alert => "'\\a'",
            Self::Backspace => "'\\b'",
            Self::Tab => "'\\t'",
            Self::VerticalTab => "'\\v'",
            Self::Formfeed => "'\\f'",
            Self::CarriageReturn => "'\\r'",
            Self::Escape => "'\\e'",
            Self::Space => "'\\s'",
            Self::DoubleQuote => "'\\\"'",
            Self::Backslash => "'\\\\'",
            Self::Delete => "'\\d'",
        })
    }
}

#[derive(Debug)]
pub struct StringFragment(pub ArcSlice<str>);

impl Display for StringFragment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "string fragment '{}'", &self.0)
    }
}

#[derive(Debug)]
pub struct Symbol(pub ArcSlice<str>);

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Tokenizer for WhiteSpace {
    fn token(document: &mut Document) -> Option<Self> {
        static WHITE_SPACE_PATTERN: LazyLock<Regex> =
            LazyLock::new(|| Regex::new("^[ \\t\\r\\n]+").expect("regex is valid"));

        document
            .strip_prefix_match(&WHITE_SPACE_PATTERN)
            .map(|_| Self)
    }
}

impl Tokenizer for OpenParen {
    fn token(document: &mut Document) -> Option<Self> {
        match document.rest().strip_prefix("(") {
            None => None,
            Some(_) => {
                document.consume(1);
                Some(Self)
            }
        }
    }
}

impl Tokenizer for CloseParen {
    fn token(document: &mut Document) -> Option<Self> {
        match document.rest().strip_prefix(")") {
            None => None,
            Some(_) => {
                document.consume(1);
                Some(Self)
            }
        }
    }
}

impl Tokenizer for FloatLiteral {
    fn token(document: &mut Document) -> Option<Self> {
        static PATTERN: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new("^(-)?([0-9]+)\\.([0-9]*)(f32|f64)?").expect("regex is valid")
        });

        document.strip_prefix_captures(&PATTERN).map(|capt| Self {
            sign: capt
                .get(1)
                .map(|_| Sign::Negative)
                .unwrap_or(Sign::Positive),
            int: document.document.subslice_from_ref(
                capt.get(2)
                    .expect("pattern always includes group 2")
                    .as_str(),
            ),
            fract: document.document.subslice_from_ref(
                capt.get(3)
                    .expect("pattern always includes group 3")
                    .as_str(),
            ),
            type_specifier: capt.get(4).map(|match_| match match_.as_str() {
                "f32" => FloatTypeSpecifier::F32,
                "f64" => FloatTypeSpecifier::F64,
                _ => panic!("Can only match f32 or f64"),
            }),
        })
    }
}

impl Tokenizer for IntLiteral {
    fn token(document: &mut Document) -> Option<Self> {
        static PATTERN: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new("^(-)?([0-9]+)(i32|u32|i64|u64)?").expect("regex is valid")
        });

        document.strip_prefix_captures(&PATTERN).map(|capt| Self {
            sign: capt
                .get(1)
                .map(|_| Sign::Negative)
                .unwrap_or(Sign::Positive),
            int: document.document.subslice_from_ref(
                capt.get(2)
                    .expect("pattern always includes group 2")
                    .as_str(),
            ),
            type_specifier: capt.get(3).map(|match_| match match_.as_str() {
                "i32" => IntTypeSpecifier::I32,
                "u32" => IntTypeSpecifier::U32,
                "i64" => IntTypeSpecifier::I64,
                "u64" => IntTypeSpecifier::U64,
                _ => panic!("can only match i32, u32, i64, or u64"),
            }),
        })
    }
}

impl Tokenizer for DoubleQuote {
    fn token(document: &mut Document) -> Option<Self> {
        match document.rest().strip_prefix("\"") {
            None => None,
            Some(_) => {
                document.consume(1);
                Some(Self)
            }
        }
    }
}

impl Tokenizer for EscapeSequence {
    fn token(document: &mut Document) -> Option<Self> {
        let stripped = match document
            .rest()
            .char_indices()
            .map(|(index, _)| index)
            .chain(Some(document.rest().len()))
            .nth(2)
        {
            Some(second_char_index) => &document.rest()[0..second_char_index],
            None => return None,
        };

        match stripped.parse() {
            Ok(sequence) => {
                document.consume(stripped.len());
                Some(sequence)
            }
            Err(_) => None,
        }
    }
}

impl Tokenizer for StringFragment {
    fn token(document: &mut Document) -> Option<Self> {
        static PATTERN: LazyLock<Regex> =
            LazyLock::new(|| Regex::new("^[^\\\\\"]+").expect("regex is valid"));

        document
            .strip_prefix_match(&PATTERN)
            .map(|mat| Self(document.document.subslice_from_ref(mat.as_str())))
    }
}

impl Tokenizer for Symbol {
    fn token(document: &mut Document) -> Option<Self> {
        static PATTERN: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new("^(?:-)?[a-zA-Z+\\-/=<>!?_][0-9a-zA-Z+\\-/=<>!?_]*").expect("regex is valid")
        });

        document
            .strip_prefix_match(&PATTERN)
            .map(|mat| Self(document.document.subslice_from_ref(mat.as_str())))
    }
}
