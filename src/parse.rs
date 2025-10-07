use std::fmt::Display;

use crate::{
    lex::{self, Document, Tokenizer},
    syntax::{self, NodeType},
};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Eq)]
pub struct GenericParseError {
    document: Document,
    /// The type of node which was being parsed.
    node_type: NodeType,
    expected_token: &'static str,
}

impl GenericParseError {
    pub fn new(document: Document, node_type: NodeType, expected_token: &'static str) -> Self {
        Self {
            document,
            node_type,
            expected_token,
        }
    }
}

impl Display for GenericParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(found) = lex::Token::token(&mut self.document.clone()) {
            write!(
                f,
                "Invalid token {} at {}, expected {}",
                found,
                self.document.pos(),
                self.expected_token
            )
        } else {
            write!(
                f,
                "Expected {} token at {}",
                self.expected_token,
                self.document.pos()
            )
        }
    }
}

pub trait Parser {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized;
}

impl Parser for syntax::File {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let mut statements = Vec::new();

        loop {
            if let Ok(arg) = syntax::Statement::parse_from(&mut parsed_document) {
                statements.push(arg);
                _ = lex::WhiteSpace::token(&mut parsed_document);
            } else {
                break;
            }
        }

        *document = parsed_document;
        Ok(Self {
            statements: statements.into(),
        })
    }
}

impl Parser for syntax::Statement {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        if let Ok(node) = syntax::Defun::parse_from(document) {
            Ok(syntax::Statement::Defun(node))
        } else if let Ok(node) = syntax::Var::parse_from(document) {
            Ok(syntax::Statement::Var(node))
        } else if let Ok(node) = syntax::Expression::parse_from(document) {
            Ok(syntax::Statement::Expression(node))
        } else {
            Err(GenericParseError::new(
                document.clone(),
                NodeType::Statement,
                "defun or var or expression",
            ))
        }
    }
}

impl Parser for syntax::Defun {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Defun,
                "'('",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Ok(symbol) = syntax::Symbol::parse_from(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Defun,
                "symbol `defun`",
            ));
        };

        if &symbol.0 != "defun" {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Defun,
                "symbol `defun`",
            ));
        }

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Ok(name) = syntax::Symbol::parse_from(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Defun,
                "symbol",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Defun,
                "'('",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let mut arguments = Vec::new();
        loop {
            if let Ok(arg) = syntax::Symbol::parse_from(&mut parsed_document) {
                arguments.push(arg);
                _ = lex::WhiteSpace::token(&mut parsed_document);
            } else {
                break;
            }
        }

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Defun,
                "')'",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Ok(body) = syntax::Expression::parse_from(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Defun,
                "expression",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Defun,
                "')'",
            ));
        };

        *document = parsed_document;
        Ok(Self {
            name,
            arguments: arguments.into(),
            body,
        })
    }
}

impl Parser for syntax::Var {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Var,
                "'('",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Ok(symbol) = syntax::Symbol::parse_from(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Var,
                "symbol `var`",
            ));
        };

        if &symbol.0 != "var" {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Var,
                "symbol `var`",
            ));
        }

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Ok(name) = syntax::Symbol::parse_from(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Var,
                "symbol",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Ok(value) = syntax::Expression::parse_from(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Var,
                "expression",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Var,
                "')'",
            ));
        };

        *document = parsed_document;
        Ok(Self { name, value })
    }
}

impl Parser for syntax::Expression {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        if let Ok(node) = syntax::Progn::parse_from(document) {
            Ok(syntax::Expression::Progn(node))
        } else if let Ok(node) = syntax::Application::parse_from(document) {
            Ok(syntax::Expression::Application(node))
        } else if let Ok(node) = syntax::Symbol::parse_from(document) {
            Ok(syntax::Expression::Symbol(node))
        } else if let Ok(node) = syntax::Literal::parse_from(document) {
            Ok(syntax::Expression::Literal(node))
        } else {
            Err(GenericParseError::new(
                document.clone(),
                NodeType::Expression,
                "progn or application or symbol or literal",
            ))
        }
    }
}

impl Parser for syntax::Application {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Application,
                "'('",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Ok(function) = syntax::Symbol::parse_from(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Application,
                "function symbol",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let mut args = Vec::new();

        loop {
            if let Ok(arg) = syntax::Expression::parse_from(&mut parsed_document) {
                args.push(arg);
                _ = lex::WhiteSpace::token(&mut parsed_document);
            } else {
                break;
            }
        }

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Application,
                "')'",
            ));
        };

        *document = parsed_document;
        Ok(Self {
            function,
            args: args.into(),
        })
    }
}

impl Parser for syntax::Progn {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();

        let Some(_) = lex::OpenParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Progn,
                "'('",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(symbol) = lex::Symbol::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Progn,
                "symbol `progn`",
            ));
        };

        if &symbol.0 != "progn" {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Progn,
                "symbol `progn`",
            ));
        }

        let mut expressions = Vec::new();

        loop {
            _ = lex::WhiteSpace::token(&mut parsed_document);

            if let Ok(var) = syntax::Var::parse_from(&mut parsed_document) {
                expressions.push(syntax::VarExpression::Var(var));
            } else if let Ok(expression) = syntax::Expression::parse_from(&mut parsed_document) {
                expressions.push(syntax::VarExpression::Expression(expression));
            } else {
                break;
            }
        }

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::Progn,
                "')'",
            ));
        };

        *document = parsed_document;
        Ok(Self {
            expressions: expressions.into(),
        })
    }
}

impl Parser for syntax::Literal {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        if let Ok(int) = syntax::Int::parse_from(document) {
            Ok(syntax::Literal::Int(int))
        } else if let Ok(float) = syntax::Float::parse_from(document) {
            Ok(syntax::Literal::Float(float))
        } else if let Ok(string) = syntax::StringLiteral::parse_from(document) {
            Ok(syntax::Literal::String(string))
        } else {
            Err(GenericParseError::new(
                document.clone(),
                NodeType::Literal,
                "int or float or string",
            ))
        }
    }
}

impl Parser for syntax::Int {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        match lex::IntLiteral::token(document) {
            Some(int) => Ok(Self {
                sign: int.sign,
                digits: int.int.as_ref().into(),
                type_specifier: int.type_specifier,
            }),
            None => Err(GenericParseError::new(
                document.clone(),
                NodeType::Int,
                "float",
            )),
        }
    }
}

impl Parser for syntax::Float {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        match lex::FloatLiteral::token(document) {
            Some(float) => Ok(Self {
                sign: float.sign,
                fract: float.fract.as_ref().into(),
                int: float.int.as_ref().into(),
                type_specifier: float.type_specifier,
            }),
            None => Err(GenericParseError::new(
                document.clone(),
                NodeType::Float,
                "float",
            )),
        }
    }
}

impl Parser for syntax::StringLiteral {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        let mut parsed_document = document.clone();

        let Some(_quote) = lex::DoubleQuote::token(&mut parsed_document) else {
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::StringLiteral,
                "'\"'",
            ));
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
            return Err(GenericParseError::new(
                parsed_document,
                NodeType::StringLiteral,
                "'\"' or string fragment",
            ));
        };

        *document = parsed_document;
        Ok(Self(string))
    }
}

impl Parser for syntax::Symbol {
    fn parse_from(document: &mut lex::Document) -> Result<Self, GenericParseError>
    where
        Self: Sized,
    {
        match lex::Symbol::token(document) {
            Some(symbol) => Ok(Self(symbol.0.as_ref().to_owned())),
            None => Err(GenericParseError::new(
                document.clone(),
                NodeType::Symbol,
                "symbol",
            )),
        }
    }
}
