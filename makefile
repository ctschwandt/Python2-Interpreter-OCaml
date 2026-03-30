.PHONY: build run clean utop

b build:
	dune build

r run:
	dune exec ./bin/main.exe

c clean:
	dune clean
	rm -f src/lexer/lexer.ml src/lexer/lexer.mli

u utop:
	dune utop
