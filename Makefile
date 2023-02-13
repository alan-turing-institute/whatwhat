install:
	dune build
	dune build @install
	dune install

odigdoc: install
	odig doc -u whatwhat

dunedoc:
	dune clean
	dune build @doc
	open ./_build/default/_doc/_html/index.html
