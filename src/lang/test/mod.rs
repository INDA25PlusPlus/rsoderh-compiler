use std::{fs, process};

use crate::{
    cli,
    lang::{Builtin, Program, instruction::Identifier, render::QbeRenderable},
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
fn builtin_enum_strings() {
    let strings = [
        "+", "-", "*", "//", "rem", ">>", "<<", "=", "!=", ">", "<", ">=", "<=", "and", "or", "not",
    ];

    assert_eq!(
        strings
            .map(|string| string.parse::<Builtin>().unwrap())
            .map(|builtin| -> &'static str { (&builtin).into() }),
        strings,
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

#[test]
fn program_output_add() {
    assert_compiled_stdout_eq!("(printf \"%d\" (+ 1 2 3))", "6",);
}

#[test]
fn program_output_sub() {
    assert_compiled_stdout_eq!("(printf \"%d\" (- 1 2 3))", "-4",);
}

#[test]
fn program_output_mul() {
    assert_compiled_stdout_eq!("(printf \"%d\" (* 1 2 3))", "6",);
}

#[test]
fn program_output_div() {
    assert_compiled_stdout_eq!("(printf \"%d\" (// 10 2 2))", "2",);
}

#[test]
fn program_output_rem() {
    assert_compiled_stdout_eq!(
        "(printf \"%d\\n%d\\n%d\" (rem 11 4) (rem -11 4) (rem -11 -4))",
        "3\n-3\n-3",
    );
}

#[test]
fn program_output_shift_right() {
    assert_compiled_stdout_eq!("(printf \"%d\\n%d\" (>> 8 3) (>> -10 2))", "1\n-3",);
}

#[test]
fn program_output_shift_left() {
    assert_compiled_stdout_eq!("(printf \"%d\\n%d\" (<< 8 3) (<< -10 2))", "64\n-40",);
}

#[test]
fn program_output_equal() {
    assert_compiled_stdout_eq!("(printf \"%d\\n%d\" (= -10 -10 -10) (= 8 8 2))", "1\n0",);
}

#[test]
fn program_output_not_equal() {
    assert_compiled_stdout_eq!(
        "(printf \"%d\\n%d\\n%d\" (!= -10 -10 -8) (!= 8 8 2) (!= 8 7 6))",
        "0\n0\n1",
    );
}

#[test]
fn program_output_greater_than() {
    assert_compiled_stdout_eq!("(printf \"%d\\n%d\" (> 30 10 -10) (> -15 -10))", "1\n0",);
}

#[test]
fn program_output_less_than() {
    assert_compiled_stdout_eq!("(printf \"%d\\n%d\" (< 10 10) (< -15 -10))", "0\n1",);
}

#[test]
fn program_output_greater_than_equal() {
    assert_compiled_stdout_eq!(
        "(printf \"%d\\n%d\" (>= 45 10 10) (>= -10 -15 -15))",
        "1\n1",
    );
}

#[test]
fn program_output_less_than_equal() {
    assert_compiled_stdout_eq!("(printf \"%d\\n%d\" (<= 10 10) (<= -10 -15))", "1\n0",);
}

#[test]
fn program_output_and() {
    assert_compiled_stdout_eq!("(printf \"%d\\n%d\" (and 1 15 -1) (and 1 0 10))", "-1\n0",);
}

#[test]
fn program_output_or() {
    assert_compiled_stdout_eq!("(printf \"%d\\n%d\" (or 0 0 0) (or 0 -10 0))", "0\n-10",);
}

#[test]
fn program_output_not() {
    assert_compiled_stdout_eq!(
        "(printf \"%d\\n%d\\n%d\" (not 0) (not 1) (not 123))",
        "1\n0\n0",
    );
}

#[test]
fn program_output_if() {
    assert_compiled_stdout_eq!(
        "(printf \"%d\\n%d\\n%d\" (if 1 2 3) (if 0 2 3) (if -10 2 3) (if -10 2 (printf \"garage\")))",
        "2\n3\n2",
    );
}
