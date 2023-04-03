# WhatWhat

`whatwhat` is an OCaml command-line tool to help monitor project status and allocations in the [Research Engineering Group](https://www.turing.ac.uk/research-engineering), aka Hut23.
It is the successor to [NowWhat](https://github.com/alan-turing-institute/nowwhat) (F#) and [WhatNow](https://github.com/alan-turing-institute/whatnow) (Racket).

## Contents

1. [Setting up OCaml on macOS](#setting-up-ocaml-on-macos)
1. [Installation](#installation)
1. [Usage](#-usage)
1. [Resources for getting started with OCaml](#4-resources-for-getting-started-with-ocaml)
1. [Glossary](#5-glossary)

## Setting up OCaml on macOS

```sh
brew install opam
opam init
opam install dune utop odig ocaml-lsp-server -y
```

`opam` is the OCaml package manager.
It installs all packages into `~/.opam`.
Note: If you allow `opam` to install its 'shell hook' during installation, you shouldn't need to ever do `eval $(opam env)$`.
Most online advice is outdated in this respect.

Setup for some editors (others can be set up in an analogous way):

 - (Emacs) `opam install tuareg`
 - (Neovim) The easiest way is to use
   [`lspconfig`](https://github.com/neovim/nvim-lspconfig), and add something like
   `require'lspconfig'.ocamllsp.setup{}` to your config.

## Installation and configuration

First, check that you have the `dune` executable available on your `$PATH`:

```sh
which dune
```

`dune` is the 'official' OCaml build system, and is used throughout this README.

```sh
git clone git@github.com:alan-turing-institute/whatwhat.git
cd whatwhat
make install-deps
dune build
```

The `make install-deps` command takes care of installing project dependencies.
See [the wiki]( https://github.com/alan-turing-institute/whatwhat/wiki/Installing-dependencies) for more explanation of what goes on inside there.

`dune` places all its build artifacts in the `_build` folder.
If you got to this point without errors, you should be able to run (the local version of) `whatwhat`:

```sh
dune exec -- whatwhat
```

It will most likely complain about a missing configuration or missing secret.
To get around this, you will have to set up two files, one containing secrets and one containing other non-sensitive info.

**Secrets.** Put this in `~/.config/whatwhat/secrets.json` (and **make sure that it is not stored in source control**, e.g. in your personal dotfiles / config!):

```json
{
    "githubToken"    : "<yours here>",
    "githubBotToken" : "<get from the Shared Drive>",
    "forecastToken"  : "<yours here>",
    "slackToken"     : "<get from the Shared Drive>"
}
```

 - `githubToken` here refers to a "classic" personal access token, which you can generate at https://github.com/settings/tokens.
   The token will need to have the permissions: `read:user`, `repo`, and `user:email`.
 - `forecastToken` can be obtained from https://id.getharvest.com/oauth2/access_tokens/new.
 - To obtain `githubBotToken` and `slackToken`, you need to be added to the `hut23-1206-nowwhat@turing.ac.uk` group
   (ask someone else on the `whatwhat` developer team to add you, e.g. the person who most recently committed to `main`).
   However, for initial development purposes, it suffices to use an empty string, because these tokens are only used if you wish to actually post to GitHub or Slack respectively.

**Config.** Separately, place this in `~/.config/whatwhat/config.json`:

```json
{
    "githubProjectName"       : "Project Tracker",
    "githubProjectColumns"    : ["Active", "Awaiting start", "Finding people", "Awaiting go/no-go"],
    "forecastId"              : "<yours here>",
    "forecastUrl"             : "https://api.forecastapp.com",
    "githubRepoName"          : "Hut23",
}
```

 - `forecastID` can be obtained by opening [Forecast](https://forecastapp.com).
   It will redirect you to another URL that contains your ID, which is a series of several digits.
   Paste the ID in as a string.

You shouldn't need to change any of the other settings here.

## Usage

Note that, to compile and execute the source code *in your working directory*, `whatwhat` should always be run using `dune exec -- whatwhat [options]`.
Just running `whatwhat [options]` will use the installed version inside `~/.opam` (if it exists).
(You can create a shell alias if you're lazy: something like `alias dew='dune exec -- whatwhat'`).

For extensive usage options, do `dune exec -- whatwhat --help`, or `dune exec -- whatwhat <COMMAND> --help` for the subcommands.
However, this should be enough to get started.

 - `dune exec --whatwhat`: Report errors for projects on the four main columns of the issue tracker. Print output to terminal.
 - `dune exec --whatwhat --notify github`: Same as above, but additionally post GitHub comments on all of those issues.
 - `dune exec --whatwhat export-{project,team}`: Create Forecast project or team export CSV files. Useful for [other reporting purposes](https://github.com/alan-turing-institute/Hut23/issues/1354).

## Documentation

Online documentation for the `main` branch of `whatwhat` can be viewed at:

https://alan-turing-institute.github.io/whatwhat/whatwhat/index.html

Documentation is written using [odoc syntax](https://ocaml.github.io/odoc/).
To build documentation locally, there are a couple of options:
1. `make dunedoc`

   Uses `dune` to build docs based on the development version of `whatwhat` (i.e. the actual source code on your computer).
   This documentation does not contain links to types, functions, etc. from other modules, and the build process can emit a few spurious warnings
   (see [#64](https://github.com/alan-turing-institute/whatwhat/issues/64)).

2. `make odigdoc`

   Uses `odig` to build docs based on the installed version of `whatwhat` (i.e. the version inside `~/.opam`).
   This documentation does link to that of other modules.
   However, you do have to install `whatwhat` first before this can be done; the `Makefile` takes care of this automatically.

See the [Documentation wiki page](https://github.com/alan-turing-institute/whatwhat/wiki/Documentation) for more description of what's going on with these commands.

## Resources for getting started with OCaml

### Official

- Main website: https://ocaml.org/
- [Installation instructions from the official website](https://ocaml.org/docs/up-and-running)

### Learning and getting started

- [Merlin: set up for Emacs and VIM](https://ocaml.github.io/merlin/)
- [OCamlverse](https://ocamlverse.github.io/) is a community wiki
- [OCaml Discourse](https://discuss.ocaml.org/)
- [OCaml PRO](https://ocamlpro.com/)

### Books and documentation

- [OCaml Programming: Correct + Efficient +
  Beautiful](https://cs3110.github.io/textbook/cover.html) (A nice textbook
  with accompanying lecture series.)
- [Real World OCaml (Jane Street)](https://dev.realworldocaml.org/index.html)
  (Goes faster and is more in-depth, so quite good if you have some experience
  with functional programming. However, note that it relies on Jane Street's
  alternative standard library, which `whatwhat` doesn't use.)
- [Developing Applications with Objective Caml
  (Book)](https://caml.inria.fr/pub/docs/oreilly-book/html/index.html)
