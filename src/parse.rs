use std::{ops::Range, sync::Arc};

use crate::{
    error::ParseError,
    lex::{self, Tokenizer},
    syntax::{self, NodeType},
};

#[cfg(test)]
mod tests;

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
                statements.push(Arc::new(arg));
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
            Ok(Some(syntax::Statement::Defun(node.into())))
        } else if let Some(node) = syntax::Let::parse_from(document)? {
            Ok(Some(syntax::Statement::Let(node.into())))
        } else if let Some(node) = syntax::Expression::parse_from(document)? {
            Ok(Some(syntax::Statement::Expression(node.into())))
        } else {
            Ok(None)
        }
    }

    fn span(&self) -> &Range<usize> {
        match self {
            syntax::Statement::Defun(node) => node.span(),
            syntax::Statement::Let(node) => node.span(),
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
                arguments.push(arg.into());
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
            name: name.into(),
            arguments: arguments.into(),
            body: body.into(),
            span: start..end,
        }))
    }

    fn span(&self) -> &Range<usize> {
        &self.span
    }
}

impl Parser for syntax::Let {
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

        if &symbol.0 != "let" {
            return Ok(None);
        }

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(name) = syntax::Symbol::parse_from(&mut parsed_document)? else {
            return Err(ParseError::new(parsed_document, NodeType::Let, "symbol"));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(value) = syntax::Expression::parse_from(&mut parsed_document)? else {
            return Err(ParseError::new(
                parsed_document,
                NodeType::Let,
                "expression",
            ));
        };

        _ = lex::WhiteSpace::token(&mut parsed_document);

        let Some(_) = lex::CloseParen::token(&mut parsed_document) else {
            return Err(ParseError::new(parsed_document, NodeType::Let, "')'"));
        };

        let end = parsed_document.pos();
        *document = parsed_document;
        Ok(Some(Self {
            name: name.into(),
            value: value.into(),
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
            Ok(Some(syntax::Expression::Progn(node.into())))
        } else if let Some(node) = syntax::Application::parse_from(document)? {
            Ok(Some(syntax::Expression::Application(node.into())))
        } else if let Some(node) = syntax::Symbol::parse_from(document)? {
            Ok(Some(syntax::Expression::Symbol(node.into())))
        } else if let Some(node) = syntax::Literal::parse_from(document)? {
            Ok(Some(syntax::Expression::Literal(node.into())))
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
                args.push(Arc::new(arg));
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
            function: function.into(),
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

            if let Some(let_) = syntax::Let::parse_from(&mut parsed_document)? {
                expressions.push(syntax::LetExpression::Let(let_.into()).into());
            } else if let Some(expression) = syntax::Expression::parse_from(&mut parsed_document)? {
                expressions.push(syntax::LetExpression::Expression(expression.into()).into());
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
            Ok(Some(syntax::Literal::Int(int.into())))
        } else if let Some(string) = syntax::StringLiteral::parse_from(document)? {
            Ok(Some(syntax::Literal::String(string.into())))
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
            Some(int) => {
                // Yes, it's really stupid that we separate the sign, only to reassemble a new
                // string for parsing, but I don't care enough to change it.
                let Ok(value) = format!(
                    "{}{}",
                    match int.sign {
                        lex::Sign::Positive => "",
                        lex::Sign::Negative => "-",
                    },
                    int.int
                )
                .parse::<i64>() else {
                    // Note: We've already checked that int.sign is a valid int, so the only
                    // possible error is if it's out of range.
                    let document = document.clone();
                    document.set_pos(start);
                    return Err(ParseError::new(
                        document,
                        NodeType::Int,
                        "integer to be within -9_223_372_036_854_775_808 to 9_223_372_036_854_775_807",
                    ));
                };

                Ok(Some(Self {
                    value,
                    span: start..document.pos(),
                }))
            }
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
