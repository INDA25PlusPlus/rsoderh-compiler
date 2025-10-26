//! Contains logic for rendering QBE text.
// spell-checker: words nicode

use std::io;

use itertools::{Itertools, Position};

use crate::{
    lang::{
        Function, Program,
        instruction::{Instruction, JumpInstruction, ProgramLine, Register},
    },
    syntax,
};

pub trait QbeRenderable {
    fn render_qbe(&self, writer: &mut impl io::Write) -> io::Result<()>;
}

impl QbeRenderable for Program {
    fn render_qbe(&self, writer: &mut impl io::Write) -> io::Result<()> {
        for (identifier, string) in self.state.items() {
            write!(writer, "data {} = ", identifier)?;
            string.render_qbe(writer)?;
            write!(writer, "\n")?;
        }

        write!(writer, "\n")?;

        for function in &self.functions {
            function.render_qbe(writer)?;
            write!(writer, "\n")?;
        }

        write!(writer, "export ")?;
        self.top_level.render_qbe(writer)?;

        Ok(())
    }
}

impl QbeRenderable for syntax::StringLiteral {
    fn render_qbe(&self, writer: &mut impl io::Write) -> io::Result<()> {
        // writer.write("")
        writer.write_all(b"{ ")?;

        // Iterates over segments of string which are either entirely valid QBE literal characters
        // or entirely not valid.
        for (printable, string) in self
            .0
            .char_indices()
            .chunk_by(|(_, chr)| {
                // QDE doesn't specify which characters are allowed within string literals, so this is
                // just a guess...
                (chr.is_ascii_graphic() || *chr == ' ') && *chr != '"'
            })
            .into_iter()
            .map(|(printable, group)| {
                let (start, end) = group.fold((None, 0), |(start, _), (index, chr)| {
                    (start.or(Some(index)), index + chr.len_utf8())
                });
                let start = start.expect("group has at least one character");

                (printable, &self.0[start..end])
            })
        {
            match printable {
                true => {
                    write!(writer, "b \"{}\", ", string)?;
                }
                false => {
                    for byte in string.bytes() {
                        write!(writer, "b {}, ", byte)?;
                    }
                }
            }
        }

        // Strings are zero terminated in Ship.
        writer.write_all(b"b 0 }")?;

        Ok(())
    }
}

impl QbeRenderable for Function {
    fn render_qbe(&self, writer: &mut impl io::Write) -> io::Result<()> {
        write!(writer, "function l {}(", self.identifier)?;

        for (pos, index) in (0..self.num_args).with_position() {
            if !matches!(pos, Position::First | Position::Only) {
                write!(writer, ", ")?;
            }
            write!(writer, "l {}", Register::param(index))?;
        }
        write!(writer, ") {{\n@start\n")?;

        for line in self.instructions.lines() {
            match line {
                ProgramLine::Instruction(register, instruction) => {
                    write!(writer, "    {} =l ", register)?;
                    instruction.render_qbe(writer)?;
                    write!(writer, "\n")?;
                }
                ProgramLine::Label(label) => {
                    write!(writer, "{}\n", label)?;
                }
                ProgramLine::Jump(jump) => {
                    write!(writer, "    ")?;
                    jump.render_qbe(writer)?;
                    write!(writer, "\n")?;
                }
            }
        }
        write!(writer, "\n")?;
        write!(writer, "    ret {}\n", self.instructions.current_register())?;

        write!(writer, "}}\n")?;

        Ok(())
    }
}

impl QbeRenderable for Instruction {
    fn render_qbe(&self, writer: &mut impl io::Write) -> io::Result<()> {
        match self {
            Instruction::FunctionCall(identifier, registers) => {
                write!(writer, "call {}(", identifier)?;

                let mut registers = registers.iter().with_position();

                if identifier.as_str() == "printf" {
                    // Call printf specifically as a rest-args function. Yes this is super ugly :)
                    let fmt = registers.next().expect(
                        "Oh nose, you called printf with no arguments, and the shitty design of \
                        Ship can't handle that better than panicking. :)"
                    ).1;

                    write!(writer, "l {}, ...", fmt)?;
                }

                for (pos, register) in registers {
                    if !matches!(pos, Position::First | Position::Only) {
                        write!(writer, ", ")?;
                    }
                    write!(writer, "l {}", register)?;
                }
                write!(writer, ")")?;

                Ok(())
            }
            Instruction::Register(register) => {
                write!(writer, "copy {}", register)
            }
            Instruction::LiteralInt(int) => {
                write!(writer, "copy {}", int)
            }
            Instruction::GlobalAddress(identifier) => {
                write!(writer, "copy {}", identifier)
            }
            Instruction::Phi(label_registers) => {
                write!(writer, "phi")?;
                for (pos, (label, register)) in label_registers.iter().with_position() {
                    write!(writer, " {} {}", label, register)?;
                    if !matches!(pos, Position::Last | Position::Only) {
                        write!(writer, ",")?;
                    }
                }

                Ok(())
            }
            Instruction::Add(a, b) => {
                write!(writer, "add {}, {}", a, b)
            }
            Instruction::Sub(a, b) => {
                write!(writer, "sub {}, {}", a, b)
            }
            Instruction::Mul(a, b) => {
                write!(writer, "mul {}, {}", a, b)
            }
            Instruction::Div(a, b) => {
                write!(writer, "div {}, {}", a, b)
            }
            Instruction::Rem(a, b) => {
                write!(writer, "rem {}, {}", a, b)
            }
            Instruction::ShiftRight(a, b) => {
                write!(writer, "shr {}, {}", a, b)
            }
            Instruction::ShiftLeft(a, b) => {
                write!(writer, "shl {}, {}", a, b)
            }
            Instruction::Equal(a, b) => {
                write!(writer, "ceql {}, {}", a, b)
            }
            Instruction::NotEqual(a, b) => {
                write!(writer, "cnel {}, {}", a, b)
            }
            Instruction::GreaterThan(a, b) => {
                write!(writer, "csgtl {}, {}", a, b)
            }
            Instruction::LessThan(a, b) => {
                write!(writer, "csltl {}, {}", a, b)
            }
            Instruction::GreaterThanEqual(a, b) => {
                write!(writer, "csgel {}, {}", a, b)
            }
            Instruction::LessThanEqual(a, b) => {
                write!(writer, "cslel {}, {}", a, b)
            }
        }
    }
}

impl QbeRenderable for JumpInstruction {
    fn render_qbe(&self, writer: &mut impl io::Write) -> io::Result<()> {
        match self {
            Self::Jump(label) => {
                write!(writer, "jmp {}", label)
            }
            Self::JumpNotZero(register, a, b) => {
                write!(writer, "jnz {}, {}, {}", register, a, b)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lang::render::QbeRenderable, syntax};

    #[test]
    fn qbe_render_string_literal() {
        let literal = syntax::StringLiteral(
            String::from("a long ünicode string with escapes!\nBell sound: 🔔\x07"),
            0..0,
        );

        let mut buffer = Vec::new();
        literal.render_qbe(&mut buffer).unwrap();
        let result = String::from_utf8(buffer).unwrap();

        assert_eq!(
            &result,
            "{ b \"a long \", b 195, b 188, b \"nicode string with escapes!\", b 10, b \"Bell sound: \", b 240, b 159, b 148, b 148, b 7, b 0 }"
        )
    }
}
