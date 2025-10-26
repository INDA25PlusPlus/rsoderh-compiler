# Ship

Ship, a shitty LISP, is a compiled functional-ish language with a LISPy syntax. It combines the wonderfully problematic syntax of LISP with C72's lack of modern ergonomics and safety. It's also mostly functional (except when interacting with standard C functions), mainly because implementing mutation would be more work...

## Type System

Ship's type system is just singed 64-bit integers: after all it's the only type you need. 😊 String literals just evaluate to an integer containing it's address, functions also just evaluate to an address. There are no cons cells, meaning there's no linked lists. Functions are entirely static, so no dynamically created functions. And there are also no way of interacting with arrays.

## Constructs

In Ship, an expression consists of a pair of parentheses, starting with a function symbol, followed by a list of arguments separated by spaces. The file's top level content can contain expressions and `defun` statements. The expressions are collected into a top level function when compiled which is called automatically once the program is ran. No other functions are called automatically.

### `defun`

```
(defun <name-symbol> (..<argument-symbol>) <body-expression>)
```

Defun statements define a function which can be used in other parts of the program. Once called, the body expression is evaluated with the argument symbols bound to the values the function was called with, with it's value being returned. All functions return an integer. There is also no mechanism for early returns.

Example:
```clj
(defun add-numbers (a b)
  (+ a b))
```

### `progn`

```
(progn ..<let-or-expression>)
```

Progn expressions evaluate their expressions in series evaluating to the last expression. They also allow you to create temporary bindings via `let` statements. These bindings are available until the end of the progn block.

Example:
```clj
(defun complex-calculation (arg-a arg-b)
  (progn
    (let first-part (rem (* arg-a arg-b) 42))
    (printf "First part of the calculation: %d" first-part)
    (let result (<< first-part 10))
    result))
```

#### `let`

```
(let <symbol> <expression>)
```

Let statements create a new local binding which last until the end of their scope. They can help with readability. You can also shadow previous bindings

Example:
```clj
(progn
  (let a 1)
  (let b (+ a 2))
  (+ b 3))

(progn
  (let a 1)
  (let a (+ a 2))
  (+ b 3))
```

### `if`

```
(if <condition-expression> <then-expression> <else-expression>)
```

If expressions evaluate one of two branches based on an condition. The then branch is evaluated if the condition expression evaluates to a truthy value (i.e. not zero), otherwise the else branch is evaluated. The if expression itself evaluates to the result of the evaluated branch.

Example
```clj
(if 1
  (printf "true\n")
  (printf "false\n"))
```

### `and`

```
(and ..<expression>)
```

And expressions evalutate to the result of last expression if every expression is truthy (i.e. not zero). Otherwise it evaluates to false (i.e. `0`). It's short-circuiting, meaing that the later expressions aren't evaluated once an expression evaluates to false.

Evaluates to true if called with no expressions.

### `or`

```
(or ..<expression>)
```

Or expressions evalutate to the result of the first expression which is truthy (i.e. not zero). Otherwise it evaluates to false (i.e. `0`). It's short-circuiting, meaing that the later expressions aren't evaluated once an expression evaluates to true.

Evaluates to false if called with no expressions.

### `not`

```
(not <expression>)
```

Evalutes to `1` if the expression is `0`, otherwise evaluates to `0`.

## Builtins

### Arithmetic

- ```
  (+ ..<expression>)
  ```
  
  Calculates the sum of the values. Evaluates to `0` if no arguments are given.
- ```
  (- <expression> ..<subtrahend-expression>)
  ```
  
  Calculates the expression repeatedly subtracted by the remaining expressions.
- ```
  (* ..<expression>)
  ```
  
  Calculates the product of the values. Evaluates to `1` if no arguments are given.
- ```
  (// <expression> ..<divisor-expression>)
  ```
  
  Calculates the expression repeatedly integer-divided by the remaining expressions.
- ```
  (rem <expression> ..<divisor-expression>)
  ```
  
  Calculates the remainder of expression divided by the remaining expressions.
  
  **Note:** Calling this operator with negative values is undefined behavior because I'm too lazy to figure out the actual behavior. 😎
- ```
  (>> <expression> <expression>)
  ```
  
  Calculates the result of shifting the bits of the first expression to the right by an amount of places determined by the second argument.
- ```
  (<< <expression> <expression>)
  ```
  
  Calculates the result of shifting the bits of the first expression to the left by an amount of places determined by the second argument.

### Comparison

- ```
  (= ..<expression>)
  ```
  
  Evaluates to `1` if all the arguments evaluate to the same value. Otherwise evaluates to `0`.

- ```
  (!= ..<expression>)
  ```
  
  Evaluates to `1` if the expressions evaluate to different values. Otherwise evaluates to `0`. **Note:** The expression is considered "not equal" if every consecutive argument is not equal, meaning that `(!= 1 1 2)` is false, while `(!= 2 1 2)` is true.
- ```
  (< ..<expression>)
  ```
  
  Evaluates to `1` if every expression is smaller than the next one. Otherwise evaluates to `0`.
- ```
  (> ..<expression>)
  ```
  
  Evaluates to `1` if every expression is greater than the next one. Otherwise evaluates to `0`.
- ```
  (<= ..<expression>)
  ```
  
  Evaluates to `1` if every expression is smaller than or equal to the next one. Otherwise evaluates to `0`.
- ```
  (>= ..<expression>)
  ```
  
  Evaluates to `1` if every expression is greater than or equal to the next one. Otherwise evaluates to `0`.

### Standard library functions

You can use a small subset of C standard library functions. The arguments you pass to it are passed along directly with no checking for the number of arguments passed. There are also no abstractions for pointers or anything, so if you pass zero to the first argument of `fgets`, it will get a null pointer!

These functions can be called
- `printf`
- `strtol`
- `fgets`
- `malloc`
- `free`,

and these variables can be used
- `stdin`
- `stdout`
- `stderr`.
