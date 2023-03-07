run:
	./_build/default/bin/main.exe

print:
	./_build/default/bin/main.exe --notify print

install:
	dune build
	dune build @install
	dune install

install-deps:
	dune build 2>&1 || opam install . --deps-only
	dune build

odigdoc: install
	odig doc -u whatwhat

dunedoc:
	dune clean
	dune build @doc
	open ./_build/default/_doc/_html/index.html
