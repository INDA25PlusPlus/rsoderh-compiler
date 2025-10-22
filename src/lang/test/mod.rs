use crate::{
    lang::{Program, instruction::Identifier, render::QbeRenderable},
    lex,
    parse::Parser,
    syntax,
};
use similar_asserts;

#[test]
fn compile_simple_program() {
    let source = include_str!("simple_source.ship");

    let expected = include_str!("simple_result.ssa");

    let mut document = lex::Document::from_str(source);
    let file = syntax::File::parse_from(&mut document).unwrap().unwrap();

    let compiled = Program::compile(&file, document.clone()).unwrap();

    let mut buf = Vec::new();
    compiled.render_qbe(&mut buf).unwrap();
    let result = String::from_utf8(buf).expect("render_qbe writes utf-8");

    similar_asserts::assert_eq!(result, expected);
}

#[test]
fn identifier_from_symbol() {
    let symbol = syntax::Symbol(String::from("hello-hi_emoji🔔"), 0..0);
    assert_eq!(
        Identifier::from_symbol(&symbol),
        Identifier::new("hello_u2d_hi__emoji_u1f514_"),
    );
}
