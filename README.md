# Scalino

Scalino is a small imperative language (IMP with subtraction, integer division and `print`)
together with two ways of running it:

- a **reference interpreter** that follows the big-step semantics rule by rule;
- a **compiler** to a small stack-machine bytecode, and a **VM** that executes it.

Integers are unbounded. The test suite checks that the interpreter and the
compiled code agree: same final store, same output, and the same failure when a program
gets stuck.

## Requirements

- JDK 17 or newer (developed with JDK 25)
- [sbt](https://www.scala-sbt.org/download/) — the project pins sbt 2.1.0 and Scala 3.10.1,
  both downloaded automatically on the first run

## Build and test

```sh
sbt compile
sbt test
```

## Run

The `scalino` command has three modes:

```sh
sbt "run examples/Main.scalino"                 # interpret a program
sbt "run -compile examples/Squares.scalino"     # compile it to examples/Squares.scalinobc
sbt "run -vm examples/Squares.scalinobc"        # run the bytecode on the VM
```

Pass one `run` per sbt invocation. To run several commands in a row without restarting
sbt each time, open the sbt shell with `sbt` and type them there, e.g.
`run examples/Main.scalino`.

Running `examples/Main.scalino` prints the values one per line:

```
1
2
6
24
12
72
504
```

The bytecode is a plain text file with one instruction per line. Here is
`examples/Squares.scalinobc`:

```
PUSH 0
STORE i
LOAD i
PUSH 3
LT
JMPZ 15
LOAD i
LOAD i
MUL
PRINT
LOAD i
PUSH 1
ADD
STORE i
JMP 2
```

### Errors and exit codes

Values printed by the program go to stdout and errors go to stderr.

| Exit code | Meaning |
| --- | --- |
| 0 | the program ran to completion |
| 1 | nothing was run: bad arguments, missing file, or a static error (syntax, undeclared or redeclared variable) |
| 2 | the program got stuck (division by zero, unbound variable); what it printed before is still shown |

```
$ sbt "run bad.scalino"
bad.scalino:2:7: error: variable 'y' is not declared

$ sbt "run div.scalino"
1
error: stuck: division by zero
```

## The language

```
// Prints 0, 1, 4.
var i = 0;
while (i < 3) {
    print(i * i);
    i = i + 1;
}
```

- Statements: `var x = a;`, `x = a;`, `print(a);`, `if (b) { ... } else { ... }`
  (the `else` is optional) and `while (b) { ... }`.
- Arithmetic expressions: integer literals, variables, `+`, `-`, `*`, `/` and parentheses.
  `*` and `/` bind tighter than `+` and `-`, and all are left-associative. `/` truncates
  towards zero, and dividing by zero gets the program stuck.
- Boolean expressions: `true`, `false` and `a1 < a2`.
- A variable must be declared with `var` before it is used, and only once.
- `//` starts a comment that runs to the end of the line.

## Layout

```
src/main/scala/scalino/
  Lexer.scala, Parser.scala   concrete syntax → abstract syntax (with the declaration check)
  Syntax.scala                abstract syntax: AExp, BExp, Com
  Domains.scala               values, stores, outputs, and the ways a program can get stuck
  Interpreter.scala           big-step semantics
  Bytecode.scala              instruction set and the .scalinobc text format
  Compiler.scala              compilation of programs to bytecode
  VM.scala                    the stack machine
  Main.scala                  command-line interface
src/test/scala/scalino/       munit test suites
examples/                     sample programs
```
