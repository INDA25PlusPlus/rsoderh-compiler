use serde::{Deserialize, Serialize};

use crate::lex::Sign;

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
    pub statements: Box<[Statement]>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Statement {
    Defun(Defun),
    Var(Var),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Defun {
    pub name: Symbol,
    pub arguments: Box<[Symbol]>,
    pub body: Expression,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Var {
    pub name: Symbol,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Expression {
    Progn(Progn),
    Application(Application),
    Symbol(Symbol),
    Literal(Literal),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Application {
    pub function: Symbol,
    pub args: Box<[Expression]>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Progn {
    pub expressions: Box<[VarExpression]>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum VarExpression {
    Var(Var),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Literal {
    Int(Int),
    String(StringLiteral),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Int {
    pub sign: Sign,
    pub digits: String,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StringLiteral(pub String);

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Symbol(pub String);
