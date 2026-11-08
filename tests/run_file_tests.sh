#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
dune build bin/main.exe
interpreter="$(pwd)/_build/default/bin/main.exe"
workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT

cat > "$workdir/program.py" <<'PY'
# Whole-file execution: top-level statements, functions and blocks.
x = 2  # Inline comments are ignored.

def square(n):
    return n * n

if x == 2:
    print(square(x))
else:
    print(0)

for i in range(2):
    print(i)
PY

expected=$'4\n0\n1'
actual="$("$interpreter" "$workdir/program.py")"
if [[ "$actual" != "$expected" ]]; then
    printf 'Expected:\n%s\nGot:\n%s\n' "$expected" "$actual" >&2
    exit 1
fi

printf 'print(7)' > "$workdir/no-final-newline.py"
[[ "$("$interpreter" "$workdir/no-final-newline.py")" == '7' ]]

printf '# Empty script\n\n' > "$workdir/comments-only.py"
[[ -z "$("$interpreter" "$workdir/comments-only.py")" ]]

if "$interpreter" "$workdir/missing.py" >"$workdir/out" 2>"$workdir/err"; then
    echo 'Expected a missing-file error' >&2
    exit 1
fi
grep -q 'File error:' "$workdir/err"

printf 'print(undefined_name)\n' > "$workdir/runtime-error.py"
if "$interpreter" "$workdir/runtime-error.py" >"$workdir/out" 2>"$workdir/err"; then
    echo 'Expected a runtime error' >&2
    exit 1
fi
grep -q 'Runtime error:' "$workdir/err"

printf 'if True:\n' > "$workdir/parse-error.py"
if "$interpreter" "$workdir/parse-error.py" >"$workdir/out" 2>"$workdir/err"; then
    echo 'Expected a parse error' >&2
    exit 1
fi
grep -q 'Parse error:' "$workdir/err"

printf 'print(9)\n' | "$interpreter" | grep -q '9'

echo 'File execution and REPL smoke tests passed.'
