run:
	dune exec -- whatwhat

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
