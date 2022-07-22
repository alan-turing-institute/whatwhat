## WhatWhat

A reimagining of `nowwhat` in OCaml.

### Setup for MacOS

[Instructions from the official website](https://ocaml.org/docs/up-and-running)

[Instructions from Real World OCaml](https://dev.realworldocaml.org/install.html)

(Fill this in when the github workflow is working.)

(For new projects): `dune init proj <project_name>`. 

For existing projects, git pulled:
```
opam switch create .
opam install merlin odoc utop dune
eval $(opam env)
```

The following is a bit like `poetry`: It creates an `opam` environment (called a
"switch") in the current directory in the current directory. 
```
opam switch create .
```


### What are these things?

`opam` :: Package manager 

`dune` :: A build tool

`odoc` :: The recommended documentation generator

`odig` :: For reading local documentation

`ocamldoc` :: A format for documentation, and the old documentation generator.


