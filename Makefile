run:
	dune exec -- whatwhat

install:
	dune build
	dune build @install
	dune install

install-deps:
	dune build 2>/dev/null 1>/dev/null || true
	opam install . --deps-only -y
	dune build

odigdoc: install
	odig doc -u whatwhat

dunedoc:
	dune clean
	dune build @doc
	open ./_build/default/_doc/_html/index.html
