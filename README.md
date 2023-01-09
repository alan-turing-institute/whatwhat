## WhatWhat

A reimagining of `nowwhat` in OCaml.

1. [Usage]("Usage")
2. [Setting up OCaml on MacOS](Setting up OCaml on MacOS)
3. [Setting up whatwhat development environment](Setting up whatwhat development environment)
4. [Resources for getting started with OCaml](Resources for getting started with OCaml)
5. [Glossary](Glossary)


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


### 2. Setting up OCaml on MacOS

1. `brew install opam` (The package manager.)

2. `opam init` (in `$HOME`)

    This creates `~/.ocaml`, which contains all your local libraries and
    binaries. `opam` doesn't write anywhere else apart from `/tmp`.

   ```eval `opam env` ```

3. `opam install dune utop merlin odig` 

    (And I think you will need to do this again if you create a new
    switch. Maybe there's some way to say "this switch builds on this other
    one?")

4. (For VS Code) `opam install ocaml-lsp-server`

5. (For Emacs) `opam install tuareg`

6. (For Emacs and Vim) `opam user-setup install` (I don't have an example yet of a successful Vim set
   up. Someone **has** used `ocaml-lsp-server` with Vim, however.)

### 3. Setting up whatwhat development environment

#### Project setup (existing project)

1. In `whatwhat/`...

2. I think ... `opam switch create .` (Then `eval $(opam env)`)

#### You will need a secrets file with authentication tokens

Put this in `~/.config/nowwhat/secrets.json` (note the different name!): 
```json
{
    "githubToken"    : "<yours here>",
    "githubBotToken" : "<get from the Shared Drive>",
    "forecastId"     : "<check the URL when you connect to Forecast>",
    "forecastToken"  : "<yours here>",
    "slackToken"     : "<get from the Shared Drive>"
}
```

#### Building and running

1. To build: `dune build`
2. To build the docs: `dune build @doc`
3. To run: `dune exec whatwhat`
4. To run with command-line arguments: `dune exec -- whatwhat <flags>`



### 4. Resources for getting started with OCaml

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

[OCaml Programming: Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/cover.html) (A nice textbook with accompanying lecture series.)

[Real World OCaml (Jane Street)](https://dev.realworldocaml.org/index.html)

[Developing Applications with Objective Caml (Book)](https://caml.inria.fr/pub/docs/oreilly-book/html/index.html)



### 5. Glossary 

`opam` :: Package manager and virtual environment manager

`dune` :: A build tool

`utop` :: The recommended REPL

`odoc` :: The recommended documentation generator

`odig` :: For reading local documentation

`ocamldoc` :: A format for documentation, and the old documentation generator.


