use std::{fs, process};

use crate::{
    cli,
    lang::{Program, instruction::Identifier, render::QbeRenderable},
    lex,
    parse::Parser,
    syntax,
};
use anyhow::anyhow;
use similar_asserts;
use tempfile::TempDir;

/// Compiles source and executes it, asserting that stdout is equal to the expected string.
macro_rules! assert_compiled_stdout_eq {
    ($source:expr, $expected:expr $(,)?) => {
        let stdout = compile_and_capture_stdout($source).unwrap();
        similar_asserts::assert_eq!(stdout, $expected);
    };
}

fn compile_and_capture_stdout(source: &str) -> anyhow::Result<String> {
    let mut document = lex::Document::from_str(source);

    let ast = syntax::File::parse_from(&mut document)
        .map_err(|error| anyhow!("{}", error))?
        .unwrap();

    let program = Program::compile(&ast, document).map_err(|error| anyhow!("{}", error))?;

    let directory = TempDir::new()?;

    let exe_path = directory.path().join("exe");
    let exe = fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(&exe_path)?;

    cli::compile(&program, exe.into())?;

    let output = process::Command::new(&exe_path)
        .stdin(process::Stdio::null())
        .stdout(process::Stdio::piped())
        .output()?;

    String::from_utf8(output.stdout)
        .map_err(|error| anyhow!("Program didn't output utf8: {}", error))
}

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

#[test]
fn program_output_string() {
    assert_compiled_stdout_eq!("(printf \"%s\" \"hello åäö 🔔\")", "hello åäö 🔔",);
}

#[test]
fn program_output_function_literal() {
    assert_compiled_stdout_eq!(
        "\
        (defun fun () 6)\n\
        (printf \"%d\" (fun))\n\
        ",
        "6",
    );
}
