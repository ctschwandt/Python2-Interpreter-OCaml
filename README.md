# Python2 Interpreter (OCaml)

A Python-like interpreter implemented in OCaml. This project implements a subset
of Python with functions, control flow, data structures, and an interactive REPL.

## Build

Requires OCaml and Dune:

```bash
dune build
```

## Interactive REPL

With no script filename, the interpreter starts the existing REPL:

```bash
dune exec ./bin/main.exe
```

## Execute a source file

Pass the path to a Python source file to parse and execute all top-level
statements in order:

```bash
dune exec ./bin/main.exe -- examples/hello.py
```

For example, save the following as `hello.py`:

```python
def square(x):
    return x * x

for n in range(3):
    print(square(n))
```

Then run:

```bash
dune exec ./bin/main.exe -- hello.py
```

Output:

```
0
1
4
```

## Play Tic-Tac-Toe

The `examples/tic_tac_toe.py` sample is an interactive N x N Tic-Tac-Toe game
adapted for this interpreter's supported Python subset. It includes optional
blocked squares (`@`), a human player (`X`), and an OCaml-controlled opponent
(`O`) that prioritizes winning moves, blocks immediate human wins, and otherwise
uses the first available square.

Run the game:

```bash
dune exec ./bin/main.exe -- examples/tic_tac_toe.py
```

Enter the board size (3 is recommended), the number of blocked squares (0 for a
standard game), and your move coordinates using zero-based row and column
indices. The interpreter includes minimal `input()` and `int()` builtins to
support the game. No separate test suite is included.
