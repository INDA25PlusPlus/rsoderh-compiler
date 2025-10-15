use std::{fmt::Display, ops::Range};

use crate::{
    lex::{self, Document, Tokenizer},
    syntax::{self, NodeType},
};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Eq)]
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

pub trait Parser {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized;

    /// Get the range this node occupies in the source document as a pair of byte offsets.
    fn span(&self) -> &Range<usize>;
}

impl Parser for syntax::File {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();
        let start = parsed_document.pos();

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let mut statements = Vec::new();

        loop {
            if let Some(arg) = syntax::Statement::parse_from(&mut parsed_document)? {
                statements.push(arg);
                _ = lex::WhiteSpace::token(&mut parsed_document);
            } else {
                break;
            }
        }

        let end = parsed_document.pos();

        *document = parsed_document;
        Ok(Some(Self {
            statements: statements.into(),
            span: start..end,
        }))
    }

    fn span(&self) -> &Range<usize> {
        &self.span
    }
}

impl Parser for syntax::Statement {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        if let Some(node) = syntax::Defun::parse_from(document)? {
            Ok(Some(syntax::Statement::Defun(node)))
        } else if let Some(node) = syntax::Var::parse_from(document)? {
            Ok(Some(syntax::Statement::Var(node)))
        } else if let Some(node) = syntax::Expression::parse_from(document)? {
            Ok(Some(syntax::Statement::Expression(node)))
        } else {
            Ok(None)
        }
    }

    fn span(&self) -> &Range<usize> {
        match self {
            syntax::Statement::Defun(node) => node.span(),
            syntax::Statement::Var(node) => node.span(),
            syntax::Statement::Expression(node) => node.span(),
        }
    }
}

impl Parser for syntax::Defun {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();
        let start = parsed_document.pos();

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Ok(None);
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(symbol) = syntax::Symbol::parse_from(&mut parsed_document)? else {
            return Ok(None);
        };

        if &symbol.0 != "defun" {
            return Ok(None);
        }

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(name) = syntax::Symbol::parse_from(&mut parsed_document)? else {
            return Err(ParseError::new(parsed_document, NodeType::Defun, "symbol"));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Err(ParseError::new(parsed_document, NodeType::Defun, "'('"));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let mut arguments = Vec::new();
        loop {
            if let Some(arg) = syntax::Symbol::parse_from(&mut parsed_document)? {
                arguments.push(arg);
                _ = lex::WhiteSpace::token(&mut parsed_document);
            } else {
                break;
            }
        }

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(ParseError::new(parsed_document, NodeType::Defun, "')'"));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(body) = syntax::Expression::parse_from(&mut parsed_document)? else {
            return Err(ParseError::new(
                parsed_document,
                NodeType::Defun,
                "expression",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(ParseError::new(parsed_document, NodeType::Defun, "')'"));
        };

        let end = parsed_document.pos();
        *document = parsed_document;
        Ok(Some(Self {
            name,
            arguments: arguments.into(),
            body,
            span: start..end,
        }))
    }

    fn span(&self) -> &Range<usize> {
        &self.span
    }
}

impl Parser for syntax::Var {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();
        let start = parsed_document.pos();

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Ok(None);
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(symbol) = syntax::Symbol::parse_from(&mut parsed_document)? else {
            return Ok(None);
        };

        if &symbol.0 != "var" {
            return Ok(None);
        }

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(name) = syntax::Symbol::parse_from(&mut parsed_document)? else {
            return Err(ParseError::new(parsed_document, NodeType::Var, "symbol"));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(value) = syntax::Expression::parse_from(&mut parsed_document)? else {
            return Err(ParseError::new(
                parsed_document,
                NodeType::Var,
                "expression",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(ParseError::new(parsed_document, NodeType::Var, "')'"));
        };

        let end = parsed_document.pos();
        *document = parsed_document;
        Ok(Some(Self {
            name,
            value,
            span: start..end,
        }))
    }

    fn span(&self) -> &Range<usize> {
        &self.span
    }
}

impl Parser for syntax::Expression {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        if let Some(node) = syntax::Progn::parse_from(document)? {
            Ok(Some(syntax::Expression::Progn(node)))
        } else if let Some(node) = syntax::Application::parse_from(document)? {
            Ok(Some(syntax::Expression::Application(node)))
        } else if let Some(node) = syntax::Symbol::parse_from(document)? {
            Ok(Some(syntax::Expression::Symbol(node)))
        } else if let Some(node) = syntax::Literal::parse_from(document)? {
            Ok(Some(syntax::Expression::Literal(node)))
        } else {
            Ok(None)
        }
    }

    fn span(&self) -> &Range<usize> {
        match self {
            syntax::Expression::Progn(node) => node.span(),
            syntax::Expression::Application(node) => node.span(),
            syntax::Expression::Symbol(node) => node.span(),
            syntax::Expression::Literal(node) => node.span(),
        }
    }
}

impl Parser for syntax::Application {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();
        let start = parsed_document.pos();

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Ok(None);
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(function) = syntax::Symbol::parse_from(&mut parsed_document)? else {
            return Ok(None);
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let mut args = Vec::new();

        loop {
            if let Some(arg) = syntax::Expression::parse_from(&mut parsed_document)? {
                args.push(arg);
                _ = lex::WhiteSpace::token(&mut parsed_document);
            } else {
                break;
            }
        }

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(ParseError::new(
                parsed_document,
                NodeType::Application,
                "')'",
            ));
        };

        let end = parsed_document.pos();
        *document = parsed_document;
        Ok(Some(Self {
            function,
            args: args.into(),
            span: start..end,
        }))
    }

    fn span(&self) -> &Range<usize> {
        &self.span
    }
}

impl Parser for syntax::Progn {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();
        let start = parsed_document.pos();

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Ok(None);
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(symbol) = lex::Symbol::token(&mut parsed_document) else {
            return Ok(None);
        };

        if &symbol.0 != "progn" {
            return Ok(None);
        }

        let mut expressions = Vec::new();

        loop {
            _ = lex::WhiteSpace::token(&mut parsed_document);

            if let Some(var) = syntax::Var::parse_from(&mut parsed_document)? {
                expressions.push(syntax::VarExpression::Var(var));
            } else if let Some(expression) = syntax::Expression::parse_from(&mut parsed_document)? {
                expressions.push(syntax::VarExpression::Expression(expression));
            } else {
                break;
            }
        }

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(ParseError::new(parsed_document, NodeType::Progn, "')'"));
        };

        let end = parsed_document.pos();
        *document = parsed_document;
        Ok(Some(Self {
            expressions: expressions.into(),
            span: start..end,
        }))
    }

    fn span(&self) -> &Range<usize> {
        &self.span
    }
}

impl Parser for syntax::Literal {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        if let Some(int) = syntax::Int::parse_from(document)? {
            Ok(Some(syntax::Literal::Int(int)))
        } else if let Some(string) = syntax::StringLiteral::parse_from(document)? {
            Ok(Some(syntax::Literal::String(string)))
        } else {
            Ok(None)
        }
    }

    fn span(&self) -> &Range<usize> {
        match self {
            syntax::Literal::Int(node) => node.span(),
            syntax::Literal::String(node) => node.span(),
        }
    }
}

impl Parser for syntax::Int {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        let start = document.pos();

        match lex::IntLiteral::token(document) {
            Some(int) => Ok(Some(Self {
                sign: int.sign,
                digits: int.int.as_ref().into(),
                span: start..document.pos(),
            })),
            None => Ok(None),
        }
    }

    fn span(&self) -> &Range<usize> {
        &self.span
    }
}

impl Parser for syntax::StringLiteral {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();
        let start = parsed_document.pos();

        let Some(_quote) = lex::DoubleQuote::token(&mut parsed_document) else {
            return Ok(None);
        };

        let mut string = String::new();

        loop {
            if let Some(fragment) = lex::StringFragment::token(&mut parsed_document) {
                string += &fragment.0;
            } else if let Some(escape) = lex::EscapeSequence::token(&mut parsed_document) {
                string.push(match escape {
                    lex::EscapeSequence::Newline => '\n',
                    lex::EscapeSequence::NullByte => '\0',
                    lex::EscapeSequence::Alert => '\x07',
                    lex::EscapeSequence::Backspace => '\x08',
                    lex::EscapeSequence::Tab => '\t',
                    lex::EscapeSequence::VerticalTab => '\x0b',
                    lex::EscapeSequence::Formfeed => '\x0c',
                    lex::EscapeSequence::CarriageReturn => '\r',
                    lex::EscapeSequence::Escape => '\x1b',
                    lex::EscapeSequence::Space => ' ',
                    lex::EscapeSequence::DoubleQuote => '\"',
                    lex::EscapeSequence::Backslash => '\\',
                    lex::EscapeSequence::Delete => '\x7f',
                })
            } else {
                break;
            }
        }

        let Some(_quote) = lex::DoubleQuote::token(&mut parsed_document) else {
            return Err(ParseError::new(
                parsed_document,
                NodeType::StringLiteral,
                "'\"' or string fragment",
            ));
        };

        let end = parsed_document.pos();
        *document = parsed_document;
        Ok(Some(Self(string, start..end)))
    }

    fn span(&self) -> &Range<usize> {
        &self.1
    }
}

impl Parser for syntax::Symbol {
    fn parse_from(document: &mut lex::Document) -> Result<Option<Self>, ParseError>
    where
        Self: Sized,
    {
        let start = document.pos();

        match lex::Symbol::token(document) {
            Some(symbol) => Ok(Some(Self(
                symbol.0.as_ref().to_owned(),
                start..document.pos(),
            ))),
            None => Ok(None),
        }
    }

    fn span(&self) -> &Range<usize> {
        &self.1
    }
}
