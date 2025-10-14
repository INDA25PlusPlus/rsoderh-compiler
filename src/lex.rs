use std::{
    cell::RefCell,
    fmt::{Debug, Display, Write},
    fs, io,
    path::PathBuf,
    sync::LazyLock,
};

use arc_slice::ArcSlice;
use regex::Regex;
use serde::{Deserialize, Serialize};
use strum_macros::EnumString;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
    pub path: Option<PathBuf>,
}

impl Display for LineColumn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.path {
            Some(path) => f.write_str(
                &path
                    .file_name()
                    .expect("surely our path isn't a directory")
                    .to_string_lossy(),
            )?,
            None => f.write_str("<anonymous-file>")?,
        }
        write!(f, ":{}:{}", self.line, self.column)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Document {
    document: ArcSlice<str>,
    /// Byte offset of remaining text in the original document.
    pos: RefCell<usize>,
    /// File path which this document represents (if any).
    path: Option<PathBuf>,
}

impl Document {
    pub fn new(document: ArcSlice<str>) -> Self {
        Self {
            document: document,
            pos: RefCell::new(0),
            path: None,
        }
    }

    pub fn from_str(string: &str) -> Self {
        Self {
            document: ArcSlice::from_slice(string),
            pos: RefCell::new(0),
            path: None,
        }
    }

    pub fn load(path: PathBuf) -> io::Result<Self> {
        let content = fs::read_to_string(&path)?;
        Ok(Self {
            document: ArcSlice::from_slice(&content),
            pos: RefCell::new(0),
            path: Some(path),
        })
    }

    pub fn pos(&self) -> usize {
        *self.pos.borrow()
    }

    pub fn rest(&self) -> &str {
        &self.document[self.pos()..]
    }

    /// Calculates the 1-indexed line number and column for the current document position.
    pub fn row_column(&self) -> LineColumn {
        self.row_column_for_offset(self.pos())
    }

    /// Calculates the 1-indexed line number and column for a byte offset.
    pub fn row_column_for_offset(&self, offset: usize) -> LineColumn {
        // All bytes before offset.
        let before = &self.document[..offset];

        let (last_newline_index, last_newline_offset) = before
            .char_indices()
            .filter(|(_, chr)| *chr == '\n')
            .map(|(offset, _)| offset)
            .enumerate()
            .last()
            .unwrap_or((0, 0));

        let line = last_newline_index + 2;
        let column = before.len() - last_newline_offset + 1;

        LineColumn {
            line,
            column,
            path: self.path.clone(),
        }
    }

    pub fn set_remaining_len(&self, len: usize) -> () {
        *self.pos.borrow_mut() = self.document.len() - len;
    }

    pub fn consume(&self, len: usize) -> () {
        *self.pos.borrow_mut() += len;
    }

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

pub enum AnyToken {
    Valid(Token),
    Invalid(InvalidToken),
    Eof(Eof),
}

impl Display for AnyToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Valid(token) => write!(f, "{}", token),
            Self::Invalid(token) => write!(f, "{}", token),
            Self::Eof(token) => write!(f, "{}", token),
        }
    }
}

impl AnyToken {
    pub fn token(document: &mut Document) -> Self {
        match Token::token(document) {
            Some(Token::Eof(eof)) => Self::Eof(eof),
            Some(token) => Self::Valid(token),
            None => Self::Invalid(InvalidToken::token(document)),
        }
    }
}

pub enum Token {
    WhiteSpace(WhiteSpace),
    OpenParen(OpenParen),
    CloseParen(CloseParen),
    IntLiteral(IntLiteral),
    Eof(Eof),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::WhiteSpace(white_space) => write!(f, "{}", white_space),
            Token::OpenParen(open_paren) => write!(f, "{}", open_paren),
            Token::CloseParen(close_paren) => write!(f, "{}", close_paren),
            Token::IntLiteral(int_literal) => write!(f, "{}", int_literal),
            Token::Eof(eof) => write!(f, "{}", eof),
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
        if let Some(token) = IntLiteral::token(document) {
            return Some(Self::IntLiteral(token));
        }
        if let Some(token) = Eof::token(document) {
            return Some(Self::Eof(token));
        }

        None
    }
}

/// Represents some string of characters which couldn't be tokenized into a valid token.
#[derive(Debug)]
pub struct InvalidToken(pub ArcSlice<str>);

impl Display for InvalidToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

pub struct Eof;

impl Display for Eof {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("end of file")
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
pub struct IntLiteral {
    pub sign: Sign,
    pub int: ArcSlice<str>,
}

impl Display for IntLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sign == Sign::Negative {
            f.write_char('-')?;
        }
        f.write_str(&self.int)?;
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

impl InvalidToken {
    pub fn token(document: &mut Document) -> Self {
        let start = document.pos();
        let mut len = 0;

        loop {
            let mut document = document.clone();
            document.consume(len);

            if Token::token(&mut document).is_some() {
                break;
            }

            // Length of the next char at the document's position.
            let Some(char_len) = document
                .rest()
                .char_indices()
                .nth(1)
                .map(|(index, _)| index)
            else {
                // Current position is at EOF.
                break;
            };

            len += char_len;
        }

        document.consume(len);
        Self(document.document.subslice(start..(start + len)))
    }
}

impl Tokenizer for Eof {
    fn token(document: &mut Document) -> Option<Self> {
        match document.rest() == "" {
            true => Some(Self),
            false => None,
        }
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

impl Tokenizer for IntLiteral {
    fn token(document: &mut Document) -> Option<Self> {
        static PATTERN: LazyLock<Regex> =
            LazyLock::new(|| Regex::new("^(-)?([0-9]+)").expect("regex is valid"));

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
