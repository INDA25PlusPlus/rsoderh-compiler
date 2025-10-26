# Ship
Compiler for the language Ship, a shitty LISP.

## Documentation

Language reference: [reference.md](./docs/reference.md)

## CLI

The CLI is used by giving it a path to a file containing a Ship program as the
first argument, and then specifying the operations to perform on it.

### Operations

You can pass `-` to any filepath to print the result to stdout.

- `--run`
    
    Compiles and runs the program.
    
    Example:
    ```shell
    cargo run -- examples/fib.ship --run
    ```

- `--compile <file>`
    
    Compiles the program and export it as an executable binary. 
    
    Example:
    ```shell
    cargo run -- examples/fib.ship --compile fib
    ```

- `--assemble <file>`
    
    Exports the program's assembly reprenstation.
    
    Example:
    ```shell
    cargo run -- examples/fib.ship --assemble fib.s
    ```

- `--qbe <file>`
    
    Exports the program's QBE intermidate representation.
    
    Example:
    ```shell
    cargo run -- examples/fib.ship --qbe ast.ssa
    ```

- `--ast <file>`
    
    Exports the AST JSON representation.
    
    Example:
    ```shell
    cargo run -- examples/fib.ship --ast fib.json
    ```
