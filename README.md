## WhatWhat

A reimagining of `nowwhat` in OCaml.

**Contents:**

1. [Usage](#1-usage)
2. [Setting up OCaml on MacOS](#2-setting-up-ocaml-on-macos)
3. [Setting up whatwhat development environment](#3-setting-up-whatwhat-development-environment)
4. [Resources for getting started with OCaml](#4-resources-for-getting-started-with-ocaml)
5. [Glossary](#5-glossary)

----

### 1. Usage

(Prefix command with `dune exec` in a build environment, or `dune exec --` if you are passing
command-line arguments.)

```sh
whathat
```
Currently, report errors found when parsing the github metadata.

```sh
whathat --target=github
```
Post a comment to each GitHub issue in the Project Tracker where a problem was discovered.

---

### 2. Setting up OCaml on MacOS

1. `brew install opam` (The package manager.)

2. `opam init` (in `$HOME`)

    This creates `~/.ocaml`, which contains all your local libraries and
    binaries. `opam` doesn't write anywhere else apart from `/tmp`.

    `eval $(opam env)`

3. `opam install dune utop merlin odig` 

    (And I think you will need to do this again if you create a new
    switch. Maybe there's some way to say "this switch builds on this other
    one?")

4. (For VS Code) `opam install ocaml-lsp-server`

5. (For Emacs) `opam install tuareg`

6. (For Emacs and Vim) `opam user-setup install` (I don't have an example yet of a successful Vim set
   up. Someone **has** used `ocaml-lsp-server` with Vim, however.)

---

### 3. Setting up whatwhat development environment

```
cd whatwhat
opam switch create .
eval $(opam env)
```

To run `whatwhat`, you will need a secrets file with authentication tokens, and a config file with details of the Forecast and GitHub projects you want to work with.
For instructions on setting this up, see the [Secrets and config file wiki page](https://github.com/alan-turing-institute/whatwhat/wiki/Secrets-and-config-file).

Then, follow these instructions:

    1. To build: `dune build`
    2. To build the docs: see the [Documentation wiki page](https://github.com/alan-turing-institute/whatwhat/wiki/Documentation)
    3. To run: `dune exec whatwhat`
    4. To run with command-line arguments: `dune exec -- whatwhat <flags>`
    5. For help with the arguments see `dune exec -- whatwhat --help`

---

### 4. Resources for getting started with OCaml

#### Official

- Main website: https://ocaml.org/
- [Installation instructions from the official website](https://ocaml.org/docs/up-and-running)

#### Learning and getting started

- [Merlin: set up for Emacs and VIM](https://ocaml.github.io/merlin/)
- [OCamlverse](https://ocamlverse.github.io/) is a community wiki
- [Instructions from Real World OCaml](https://dev.realworldocaml.org/install.html)
- [OCaml discourse](https://discuss.ocaml.org/)
- [OCaml PRO](https://ocamlpro.com/)

#### Books and documentation

- [OCaml Programming: Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/cover.html) (A nice textbook with accompanying lecture series.)
- [Real World OCaml (Jane Street)](https://dev.realworldocaml.org/index.html)
- [Developing Applications with Objective Caml (Book)](https://caml.inria.fr/pub/docs/oreilly-book/html/index.html)

---

### 5. Glossary 

`opam` :: A package manager and virtual environment manager

`dune` :: A build tool. Created by Jane Street, so can be idiosyncratic

`utop` :: The recommended REPL

`odoc` :: The recommended documentation generator

`odig` :: Tool for reading local documentation

`ocamldoc` :: A format for documentation, and the old documentation generator.

