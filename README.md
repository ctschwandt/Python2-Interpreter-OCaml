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
