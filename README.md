# Ship
Compiler for the language Ship, a shitty LISP.

## Usage

The CLI is used by giving it a path to a file containing Ship code as the first
argument, and then specifying what you want to do with it using the following
arguments.

You can export the AST JSON representation using `--ast <file>`:
```bash
cargo run -- example.ship --ast ast.json
```

If you add `--ast -`, the result is printed to stdout.
