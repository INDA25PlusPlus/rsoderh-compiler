use std::{error, fmt::Display, ops::Range, sync::Arc};

use crate::{
    lex::{self, Document},
    syntax::{self, NodeType},
};

#[derive(Debug, Clone)]
pub enum Error {
    FunctionError(FunctionError),
    SemanticError(SemanticError),
    ParseError(ParseError),
}

impl Error {
    pub fn function_error(inner: SemanticError, function: Arc<syntax::Defun>) -> Self {
        Self::FunctionError(FunctionError::new(inner, function))
    }
    pub fn semantic_error(reason: String, span: Range<usize>, document: lex::Document) -> Self {
        Self::SemanticError(SemanticError::new(reason, span, document))
    }

    pub fn parse_error(document: Document, node_type: NodeType, expected: &'static str) -> Self {
        Self::ParseError(ParseError::new(document, node_type, expected))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Error::FunctionError(error) => write!(f, "{}", error),
            Error::SemanticError(error) => write!(f, "{}", error),
            Error::ParseError(error) => write!(f, "{}", error),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionError {
    inner: SemanticError,
    function: Arc<syntax::Defun>,
}

impl FunctionError {
    pub fn new(inner: SemanticError, function: Arc<syntax::Defun>) -> Self {
        Self { inner, function }
    }
}

impl Display for FunctionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\ninside function {} {}",
            self.inner,
            &self.function.name.0,
            self.inner
                .document
                .row_column_for_offset(self.function.span.start),
        )
    }
}

impl error::Error for FunctionError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&self.inner)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    reason: String,
    // offset: usize,
    span: Range<usize>,
    document: lex::Document,
}

impl SemanticError {
    pub fn new(reason: String, span: Range<usize>, document: lex::Document) -> Self {
        // let document = document.clone();
        // document.set_pos(offset);
        Self {
            reason,
            span,
            document,
        }
    }
}

impl error::Error for SemanticError {}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error: {}\n  --> {}",
            &self.reason,
            self.document.row_column_for_offset(self.span.start),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    document: Document,
    /// The type of node which was being parsed.
    node_type: NodeType,
    expected: &'static str,
}

impl ParseError {
    pub fn new(document: Document, node_type: NodeType, expected: &'static str) -> Self {
        Self {
            document,
            node_type,
            expected,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let found = lex::AnyToken::token(&mut self.document.clone());

        write!(f, "Expected {}, but", self.expected,)?;

        match found {
            lex::AnyToken::Valid(token) => write!(f, " encountered token {}", token)?,
            lex::AnyToken::Invalid(invalid_token) => {
                write!(f, " encountered invalid token {}", invalid_token)?
            }
            lex::AnyToken::Eof(_) => f.write_str(" reached end of file")?,
        }

        write!(f, " at {}", self.document.row_column(),)
    }
}
