use std::{fs, io, path, process};

use anyhow::anyhow;
use clap::Parser as ClapParser;

use crate::{lex, parse::Parser, syntax};

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File with the Ship code to process. Specify '-' to read from stdin.
    file: path::PathBuf,
    #[arg(long)]
    /// Write the program's AST as a JSON object to this file. Specify '-' for stdout.
    ast: Option<path::PathBuf>,
}

pub fn cli() -> anyhow::Result<()> {
    let args = Args::parse();

    let content = fs::read_to_string(args.file)?;
    let mut document = lex::Document::from_str(&content);

    let program = syntax::File::parse_from(&mut document).map_err(|error| anyhow!("{}", error))?;

    if document.rest() != "" {
        // TODO: Actually helpful error messages.
        eprintln!("Syntax error: Parsing didn't consume entire program.");
        eprintln!("remaining: {}", document.rest());
        process::exit(1);
    }

    if let Some(ast_path) = args.ast {
        let mut file: Box<dyn io::Write> = if ast_path.to_str() == Some("-") {
            Box::new(io::stdout())
        } else {
            Box::new(
                fs::OpenOptions::new()
                    .write(true)
                    .create(true)
                    .open(ast_path)?,
            )
        };

        serde_json::to_writer_pretty(&mut file, &program)?;
        file.write(b"\n")?;
    }

    Ok(())
}
