use std::{
    fs,
    io::{self, Write},
    path,
    process::{self, exit},
};

use anyhow::anyhow;
use clap::Parser as ClapParser;
use tempfile::TempDir;

use crate::{
    lang::{self, render::QbeRenderable},
    lex,
    parse::Parser,
    syntax,
};

pub fn assemble(program: &lang::Program, result: process::Stdio) -> anyhow::Result<()> {
    let mut child = process::Command::new("qbe")
        .arg("-")
        .arg("-o")
        .arg("-")
        .stdin(process::Stdio::piped())
        .stdout(result)
        .spawn()?;

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        program.render_qbe(stdin)?;
    }

    let output = child.wait_with_output()?;

    if !output.status.success() {
        return Err(anyhow!("qbe exited with error {}", output.status));
    }

    Ok(())
}

pub fn compile(program: &lang::Program, result: process::Stdio) -> anyhow::Result<()> {
    let mut qbe = process::Command::new("qbe")
        .arg("-")
        .arg("-o")
        .arg("-")
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .spawn()?;
    let mut gcc = process::Command::new("gcc")
        .arg("-x")
        .arg("assembler")
        .arg("-")
        .arg("-o")
        .arg("/dev/stdout")
        .stdin(qbe.stdout.expect("qbe stdout is captured"))
        .stdout(result)
        .spawn()?;

    {
        let mut qbe_stdin = qbe.stdin.take().expect("failed to open stdin");
        program.render_qbe(&mut qbe_stdin)?;
    }

    let status = gcc.wait()?;
    if !status.success() {
        return Err(anyhow!("gcc exited with error {}", status));
    }

    Ok(())
}

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File with the Ship code to process. Specify '-' to read from stdin.
    file: path::PathBuf,
    /// Write the program's AST as a JSON object to this file. Specify '-' for stdout.
    #[arg(long)]
    ast: Option<path::PathBuf>,
    /// Write the program's QBE intermediate representation to this file. Specify '-' for stdout.
    #[arg(long)]
    qbe: Option<path::PathBuf>,
    /// Assemble the program and write the result to this file. Specify '-' for stdout.
    #[arg(long)]
    assemble: Option<path::PathBuf>,
    /// Compile the program and write the binary to this file. Specify '-' for stdout.
    #[arg(long)]
    compile: Option<path::PathBuf>,
    /// Compile the program and run the result.
    #[arg(long)]
    run: bool,
}

pub fn cli() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut document = lex::Document::load(args.file.clone())?;

    let ast = syntax::File::parse_from(&mut document)
        .map_err(|error| anyhow!("{}", error))?
        .unwrap();

    if document.rest() != "" {
        // TODO: Actually helpful error messages.
        eprintln!("Syntax error: Parsing didn't consume entire program.");
        eprintln!("remaining: {}", document.rest());
        process::exit(1);
    }

    let program = lang::Program::compile(&ast, document).map_err(|error| anyhow!("{}", error))?;

    let mut did_anything = false;

    if let Some(ast_path) = args.ast {
        did_anything = true;
        let mut file: Box<dyn io::Write> = if ast_path.to_str() == Some("-") {
            Box::new(io::stdout())
        } else {
            Box::new(
                fs::OpenOptions::new()
                    .write(true)
                    .truncate(true)
                    .create(true)
                    .open(ast_path)?,
            )
        };

        serde_json::to_writer_pretty(&mut file, &ast)?;
        file.write(b"\n")?;
    }

    if let Some(path) = args.qbe {
        did_anything = true;

        let mut file: Box<dyn io::Write> = if path.to_str() == Some("-") {
            Box::new(io::stdout())
        } else {
            Box::new(
                fs::OpenOptions::new()
                    .write(true)
                    .truncate(true)
                    .create(true)
                    .open(path)?,
            )
        };

        program.render_qbe(&mut file)?;
    }

    if let Some(path) = args.assemble {
        did_anything = true;

        let file: process::Stdio = if path.to_str() == Some("-") {
            io::stdout().into()
        } else {
            fs::OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(path)?
                .into()
        };

        assemble(&program, file)?;
    }

    if let Some(path) = args.compile {
        did_anything = true;

        let file: process::Stdio = if path.to_str() == Some("-") {
            io::stdout().into()
        } else {
            fs::OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(path)?
                .into()
        };

        compile(&program, file)?;
    }

    if args.run {
        did_anything = true;

        let dir = TempDir::new()?;
        let path = dir.path().join(
            args.file
                .file_stem()
                .expect("the filepath has been opened successfully already"),
        );

        let file = fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(&path)?
            .into();

        compile(&program, file)?;

        let status = process::Command::new(&path).status()?;

        if let Some(code) = status.code() {
            if code != 0 {
                exit(code);
            }
        }

        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt;

            const SIGABRT: i32 = 6;
            const SIGSEGV: i32 = 11;
            const SIGFPE: i32 = 8;
            const SIGILL: i32 = 4;

            if let Some(signal) = status.signal() {
                eprintln!(
                    "[1] {} {} {}{:?}",
                    process::id(),
                    match signal {
                        SIGABRT => "IOT instruction",
                        SIGSEGV => "Segmentation fault",
                        SIGFPE => "Floating point exception",
                        SIGILL => "Illegal instruction",
                        _ => "Terminated by signal",
                    },
                    if status.core_dumped() {
                        "(core dumped) "
                    } else {
                        ""
                    },
                    &path
                );

                exit(1);
            }
        }
    }

    if !did_anything {
        println!("Nothing to be done. See `--help` for the available operations.")
    }

    Ok(())
}
