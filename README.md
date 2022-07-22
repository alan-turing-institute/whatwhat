## WhatWhat

A reimagining of `nowwhat` in OCaml.

### Setup for MacOS

1. `brew install opam` (The package manager.)

2. `opam init` (in $HOME)

   ```eval `opam env` ```

3. `opam switch create` ("Switch" means something like "virtual environment")

   `eval $(opam env)` (I don't know why it's different or whether it needs to be.)

4. `opam install dune utop merlin` 

5. (For VS Code) `opam install ocaml-lsp-server`

6. (For Emacs) `opam install tuareg`

7. (For Emacs and Vim) `opam user-setup install`

### Project setup (existing project)

1. In `whatwhat/`...

2. I think ... `opam switch .` (Then `eval $(opam env)`)


### Resources

#### Official

Main website: https://ocaml.org/

[Installation instructions from the official website](https://ocaml.org/docs/up-and-running)



#### Learning and getting started

[Merlin: set up for Emacs and VIM](https://ocaml.github.io/merlin/)

[OCamlverse](https://ocamlverse.github.io/) is a community wiki

[Instructions from Real World OCaml](https://dev.realworldocaml.org/install.html)

[OCaml discourse](https://discuss.ocaml.org/)

[OCaml PRO](https://ocamlpro.com/)

#### Books and documentation

[Real World OCaml (Jane Street)](https://dev.realworldocaml.org/index.html)

[Developing Applications with Objective Caml (Book)](https://caml.inria.fr/pub/docs/oreilly-book/html/index.html)


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

`utop` :: The recommended REPL

`odoc` :: The recommended documentation generator

`odig` :: For reading local documentation

`ocamldoc` :: A format for documentation, and the old documentation generator.


