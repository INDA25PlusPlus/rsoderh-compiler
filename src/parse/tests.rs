use crate::lex::Sign;

use super::*;

macro_rules! assert_parse {
    ($node_type:ty, $document:expr, $result:expr $(,)?) => {
        assert_eq!(<$node_type>::parse_from(&mut $document), $result,);
        assert_eq!($document.rest().len(), 0);
    };
}

#[test]
fn parse_file() {
    let mut document = Document::from_str(" 1\t 1  \n\n");
    assert_parse!(
        syntax::File,
        document,
        Ok(Some(syntax::File {
            statements: vec![
                syntax::Statement::Expression(
                    syntax::Expression::Literal(
                        syntax::Literal::Int(
                            syntax::Int {
                                sign: Sign::Positive,
                                digits: "1".into(),
                                span: 1..2,
                            }
                            .into()
                        )
                        .into()
                    )
                    .into()
                )
                .into(),
                syntax::Statement::Expression(
                    syntax::Expression::Literal(
                        syntax::Literal::Int(
                            syntax::Int {
                                sign: Sign::Positive,
                                digits: "1".into(),
                                span: 4..5,
                            }
                            .into()
                        )
                        .into()
                    )
                    .into()
                )
                .into(),
            ]
            .into(),
            span: 0..9,
        }))
    );
}

#[test]
fn parse_defun() {
    let mut document = Document::from_str("(\ndefun my-fun()nil)");
    assert_parse!(
        syntax::Defun,
        document,
        Ok(Some(syntax::Defun {
            name: syntax::Symbol("my-fun".into(), 8..14).into(),
            arguments: vec![].into(),
            body: syntax::Expression::Symbol(syntax::Symbol("nil".into(), 16..19).into()).into(),
            span: 0..20,
        }))
    );
}

#[test]
fn parse_var() {
    let mut document = Document::from_str("( \tvar  a(fun)   )");
    assert_parse!(
        syntax::Var,
        document,
        Ok(Some(syntax::Var {
            name: syntax::Symbol("a".into(), 8..9).into(),
            value: syntax::Expression::Application(
                syntax::Application {
                    function: syntax::Symbol("fun".into(), 10..13).into(),
                    args: vec![].into(),
                    span: 9..14,
                }
                .into()
            )
            .into(),
            span: 0..18
        }))
    );
}

#[test]
fn parse_expression() {
    let mut document = Document::from_str("(  progn 3  )");
    assert_parse!(
        syntax::Expression,
        document,
        Ok(Some(
            syntax::Expression::Progn(
                syntax::Progn {
                    expressions: vec![
                        syntax::VarExpression::Expression(
                            syntax::Expression::Literal(
                                syntax::Literal::Int(
                                    syntax::Int {
                                        sign: Sign::Positive,
                                        digits: "3".into(),
                                        span: 9..10,
                                    }
                                    .into()
                                )
                                .into()
                            )
                            .into(),
                        )
                        .into()
                    ]
                    .into(),
                    span: 0..13,
                }
                .into()
            )
            .into()
        ))
    );
}

#[test]
fn parse_application() {
    let mut document = Document::from_str("(\tfun 1 2 3 4 5\t)");
    assert_parse!(
        syntax::Application,
        document,
        Ok(Some(syntax::Application {
            function: syntax::Symbol("fun".into(), 2..5).into(),
            args: vec![
                syntax::Expression::Literal(
                    syntax::Literal::Int(
                        syntax::Int {
                            sign: Sign::Positive,
                            digits: "1".into(),
                            span: 6..7,
                        }
                        .into()
                    )
                    .into()
                )
                .into(),
                syntax::Expression::Literal(
                    syntax::Literal::Int(
                        syntax::Int {
                            sign: Sign::Positive,
                            digits: "2".into(),
                            span: 8..9,
                        }
                        .into()
                    )
                    .into()
                )
                .into(),
                syntax::Expression::Literal(
                    syntax::Literal::Int(
                        syntax::Int {
                            sign: Sign::Positive,
                            digits: "3".into(),
                            span: 10..11,
                        }
                        .into()
                    )
                    .into()
                )
                .into(),
                syntax::Expression::Literal(
                    syntax::Literal::Int(
                        syntax::Int {
                            sign: Sign::Positive,
                            digits: "4".into(),
                            span: 12..13,
                        }
                        .into()
                    )
                    .into()
                )
                .into(),
                syntax::Expression::Literal(
                    syntax::Literal::Int(
                        syntax::Int {
                            sign: Sign::Positive,
                            digits: "5".into(),
                            span: 14..15,
                        }
                        .into()
                    )
                    .into()
                )
                .into(),
            ]
            .into(),
            span: 0..17,
        }))
    );
}

#[test]
fn parse_progn() {
    let mut document = Document::from_str("(  progn( var a 4 )a )");
    assert_parse!(
        syntax::Progn,
        document,
        Ok(Some(syntax::Progn {
            expressions: vec![
                syntax::VarExpression::Var(
                    syntax::Var {
                        name: syntax::Symbol("a".into(), 14..15).into(),
                        value: syntax::Expression::Literal(
                            syntax::Literal::Int(
                                syntax::Int {
                                    sign: Sign::Positive,
                                    digits: "4".into(),
                                    span: 16..17,
                                }
                                .into()
                            )
                            .into()
                        )
                        .into(),
                        span: 8..19,
                    }
                    .into()
                )
                .into(),
                syntax::VarExpression::Expression(
                    syntax::Expression::Symbol(syntax::Symbol("a".into(), 19..20,).into()).into()
                )
                .into(),
            ]
            .into(),
            span: 0..22,
        }))
    );
}

#[test]
fn parse_literal() {
    let mut document = Document::from_str("5");
    assert_parse!(
        syntax::Literal,
        document,
        Ok(Some(syntax::Literal::Int(syntax::Int {
            sign: Sign::Positive,
            digits: "5".into(),
            span: 0..1,
        }.into())))
    );
}

#[test]
fn parse_int() {
    let mut document = Document::from_str("-5");
    assert_parse!(
        syntax::Int,
        document,
        Ok(Some(syntax::Int {
            sign: Sign::Negative,
            digits: "5".into(),
            span: 0..2,
        }))
    );
}

#[test]
fn parse_string_literal_simple() {
    let mut document = Document::from_str("\"string litteräl\"");
    assert_parse!(
        syntax::StringLiteral,
        document,
        Ok(Some(syntax::StringLiteral("string litteräl".into(), 0..18)))
    );
}

#[test]
fn parse_string_literal_escapes() {
    let mut document = Document::from_str("\"string\\n\\t\\afragment\\0\\\"\\\\\\s\\b\\e\\d\"");
    assert_parse!(
        syntax::StringLiteral,
        document,
        Ok(Some(syntax::StringLiteral(
            "string\n\t\x07fragment\x00\"\\ \x08\x1b\x7f".into(),
            0..36,
        )))
    );
}

#[test]
fn parse_symbol_1() {
    let mut document = Document::from_str("defun!?_");
    assert_parse!(
        syntax::Symbol,
        document,
        Ok(Some(syntax::Symbol("defun!?_".into(), 0..8)))
    );
}

#[test]
fn parse_symbol_2() {
    let mut document = Document::from_str("_");
    assert_parse!(
        syntax::Symbol,
        document,
        Ok(Some(syntax::Symbol("_".into(), 0..1)))
    );
}

#[test]
fn parse_symbol_consumed() {
    let mut document = Document::from_str("(defun");
    document.consume(1);
    assert_parse!(
        syntax::Symbol,
        document,
        Ok(Some(syntax::Symbol("defun".into(), 1..6)))
    );
}
