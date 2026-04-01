#!/usr/bin/env bash
set -euo pipefail

run_case() {
  local name="$1"
  local program="$2"
  local expected="$3"

  local actual
  actual=$(printf "%s" "$program" | dune exec ./bin/main.exe | sed -E 's/^>>> ?//; s/^\.\.\. ?//')

  if [[ "$actual" != "$expected" ]]; then
    echo "FAIL: $name"
    echo "Expected:"
    printf '%s\n' "$expected"
    echo "Actual:"
    printf '%s\n' "$actual"
    exit 1
  fi

  echo "PASS: $name"
}

run_case \
  "dict iterates over keys" \
  $'d = {"a": [1, 2], "b": "c"}\nfor key in d:\n    print(key)\n\n' \
  $'"a"\n"b"'

run_case \
  "empty dict iterates zero times" \
  $'d = {}\nfor key in d:\n    print(key)\n\nprint(999)\n' \
  $'999'

run_case \
  "loop variable reassignment does not affect loop progression" \
  $'d = {"x": 1}\nfor k in d:\n    print(k)\n    k = "changed"\n\nprint(123)\n' \
  $'"x"\n123'

run_case \
  "non-iterable runtime error preserved" \
  $'for x in 42:\n    print(x)\n\n' \
  $'Runtime error: value is not iterable: 42'

