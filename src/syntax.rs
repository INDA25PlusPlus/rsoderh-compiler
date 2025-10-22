use std::{ops::Range, sync::Arc};

use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq)]
pub enum NodeType {
    File,
    Statement,
    Defun,
    Var,
    Expression,
    Application,
    Progn,
    VarExpression,
    Literal,
    Int,
    Float,
    StringLiteral,
    Symbol,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct File {
    pub statements: Box<[Arc<Statement>]>,
    pub(crate) span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Statement {
    Defun(Arc<Defun>),
    Var(Arc<Var>),
    Expression(Arc<Expression>),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Defun {
    pub name: Arc<Symbol>,
    pub arguments: Box<[Arc<Symbol>]>,
    pub body: Arc<Expression>,
    pub(crate) span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Var {
    pub name: Arc<Symbol>,
    pub value: Arc<Expression>,
    pub(crate) span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Expression {
    Progn(Arc<Progn>),
    Application(Arc<Application>),
    Symbol(Arc<Symbol>),
    Literal(Arc<Literal>),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Application {
    pub function: Arc<Symbol>,
    pub args: Box<[Arc<Expression>]>,
    pub(crate) span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Progn {
    pub expressions: Box<[Arc<VarExpression>]>,
    pub(crate) span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum VarExpression {
    Var(Arc<Var>),
    Expression(Arc<Expression>),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Literal {
    Int(Arc<Int>),
    String(Arc<StringLiteral>),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Int {
    pub value: i64,
    pub(crate) span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StringLiteral(pub String, pub(crate) Range<usize>);

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Symbol(pub String, pub(crate) Range<usize>);
