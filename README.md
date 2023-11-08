# WhatWhat

`whatwhat` is an OCaml command-line tool to help monitor project status and allocations in the [Research Engineering Group](https://www.turing.ac.uk/research-engineering), aka Hut23.
It is the successor to [NowWhat](https://github.com/alan-turing-institute/nowwhat) (F#) and [WhatNow](https://github.com/alan-turing-institute/whatnow) (Racket); the latter contains [an illuminating backbrief](https://github.com/alan-turing-institute/whatnow/blob/main/backbrief/backbrief.org) describing the history of project allocations in REG.

## Brew install WhatWhat
The simplest way to use `whatwhat` is to brew install it. It is hosted in our local [Hut23 tap](https://github.com/alan-turing-institute/homebrew-hut23). You will need to run the following commands:

```sh
brew update
brew tap alan-turing-institute/hut23
brew install alan-turing-institute/hut23/whatwhat
```
This is likely to install `whatwhat` from the pre-compiled Homebrew bottle (associated with a GitHub release), which will make the installation quicker. However, if there is not a bottle that matches your computer build it will automatically build from the source.

Once you have installed it, you will need to create a secrets.json file to store your personal access tokens, which you can do
by running `whatwhat init` then populating the file with your github and forecast tokens.

These tokens can be found here:
 - `githubToken` here refers to a "classic" personal access token, which you can generate at https://github.com/settings/tokens.
   The token will need to have the permissions: `repo` (top level), `read:user` and `user:email` (both within `user`).
 - `forecastToken` can be obtained from https://id.getharvest.com/oauth2/access_tokens/new (log in if necessary).

`whatwhat` should now work, the following examples can get you started.

For extensive usage options, do `whatwhat --help`, or `whatwhat <COMMAND> --help` for the subcommands.

 - `whatwhat`: Report errors for projects on the four main columns of the issue tracker. Print output to terminal.
 - `whatwhat project [NUM|NAME]`: Print an overview of a project, as specified by its GitHub issue number, or a (sub)string of its title.
 - `whatwhat person [NAME]`: Print an overview of a person, as specified by their name or GitHub username (a substring is fine).
 - `whatwhat --notify github`: Same as `whatwhat`, but additionally post GitHub comments on all of those issues.
 - `whatwhat export-{project,team}`: Create Forecast project or team export CSV files. Useful for [other reporting purposes](https://github.com/alan-turing-institute/Hut23/issues/1354).
 - `whatwhat open [NUM]`: Open a GitHub issue in a browser (macOS only, as this uses `open(1)`.

### Lookup table for GitHub usernames

`whatwhat` attempts to match people's full names on Forecast with GitHub usernames.
It is quite successful at doing this, but there are a few edge cases where people's GitHub profiles do not have enough data.
To get around this, you can manually add a mapping inside the file `~/.config/whatwhat/users`.
Each line in this file should look like:

    {Full name on Forecast}:{GitHub username}

(without the curly braces).
If `whatwhat`'s automatic username detection is turning up false positives for a person, you can also override it by entering their info in this file.


## Developers Contents

1. [Setting up OCaml on macOS](#setting-up-ocaml-on-macos)
1. [Installation and configuration](#installation-and-configuration)
1. [Usage](#usage)
1. [Resources for getting started with OCaml](#resources-for-getting-started-with-ocaml)
1. [New Tags and Releases](#new-tags-and-releases)

## Setting up OCaml on macOS

`opam` is the OCaml package manager, which can be installed using Homebrew.
It installs all packages into `~/.opam`.
Most online instructions suggest that you need to run `eval $(opam env)`, but with the invocation of `opam init` below you should not need to.

```sh
brew install opam
opam init --shell-setup -y
opam install dune utop odig ocaml-lsp-server ocamlformat -y
```

Setup for some editors (others can be set up in an analogous way):

 - (Emacs) `opam install tuareg`
 - (Neovim) The easiest way is to use
   [`lspconfig`](https://github.com/neovim/nvim-lspconfig), and add something like
   `require'lspconfig'.ocamllsp.setup{}` to your config.
 - (VSCode) There is a VS Code extension for OCaml called [OCaml Platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform). This includes features for syntax highlighting, editing, navigation and snippets. 

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

### Secrets

Put this in `~/.config/whatwhat/secrets.json` (and **make sure that it is not stored in source control**, e.g. in your personal dotfiles / config!):

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

### Configuration

Separately, place this in `~/.config/whatwhat/config.json`:

```json
{
    "githubProjectName"    : "Project Tracker",
    "githubProjectColumns" : ["Active", "Awaiting start", "Finding people", "Awaiting go/no-go"],
    "forecastId"           : "<yours here>",
    "githubRepoName"       : "Hut23",
    "githubRepoOwner"      : "alan-turing-institute"
}
```

 - `forecastID` can be obtained by opening [Forecast](https://forecastapp.com).
   It will redirect you to another URL that contains your ID, which is a series of several digits.
   Paste the ID in as a string.

You shouldn't need to change any of the other settings here.

### Lookup table for GitHub usernames

`whatwhat` attempts to match people's full names on Forecast with GitHub usernames.
It is quite successful at doing this, but there are a few edge cases where people's GitHub profiles do not have enough data.
To get around this, you can manually add a mapping inside the file `~/.config/whatwhat/users`.
Each line in this file should look like:

    {Full name on Forecast}:{GitHub username}

(without the curly braces).
If `whatwhat`'s automatic username detection is turning up false positives for a person, you can also override it by entering their info in this file.

## Usage

Note that, to compile and execute the source code *in your working directory*, `whatwhat` should always be run using `dune exec -- whatwhat [options]`.
Just running `whatwhat [options]` will use the installed version inside `~/.opam` (if it exists).
(You can create a shell alias if you're lazy: something like `alias dew='dune exec -- whatwhat'`).

The following examples can get you started.
For extensive usage options, do `dune exec -- whatwhat --help`, or `dune exec -- whatwhat <COMMAND> --help` for the subcommands.

 - `dune exec -- whatwhat`: Report errors for projects on the four main columns of the issue tracker. Print output to terminal.
 - `dune exec -- whatwhat init`: Sets up the secrets and config files from a template.
 - `dune exec -- whatwhat --notify github`: Same as above, but additionally post GitHub comments on all of those issues.
 - `dune exec -- whatwhat project [NUM|NAME]`: Print an overview of a project, as specified by its GitHub issue number, or a (sub)string of its title.
 - `dune exec -- whatwhat person [NAME]`: Print an overview of a person, as specified by their name or GitHub username (a substring is fine).
 - `dune exec -- whatwhat export-{project,team}`: Create Forecast project or team export CSV files. Useful for [other reporting purposes](https://github.com/alan-turing-institute/Hut23/issues/1354).
 - `dune exec -- whatwhat open [NUM]`: Open a GitHub issue in a browser (macOS only, as this uses `open(1)`.

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

- [OCaml Programming: Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/cover.html)
  (A nice textbook with accompanying lecture series.)
- [Real World OCaml (Jane Street)](https://dev.realworldocaml.org/index.html)
  (Goes faster and is more in-depth, so quite good if you have some experience with functional programming.
  However, note that it relies on Jane Street's alternative standard library, which `whatwhat` doesn't use.)
- [Developing Applications with Objective Caml (Book)](https://caml.inria.fr/pub/docs/oreilly-book/html/index.html) 

## New Tags and Releases

When you make changes to the `whatwhat` code, the following steps will ensure that the Homebrew-installed version of `whatwhat` is up-to-date.

**These steps are now mostly automated.**
In this repository, run **`dune exec update_whatwhat`** and it will do all of the following steps for you:

> 1. To push the tag to the repo, you need to run `git push` followed by `git push --tags`
> 1. Once this is pushed to the remote, it will trigger a github action in the `whatwhat` repo that opens a pull request to bump the homebrew formula in the [homebrew-hut23](https://github.com/alan-turing-institute/homebrew-hut23/) repo. These steps will take a little while - you can see their progress in the action tab. You need to merge this pull request with the `main` branch before moving to the next step.
> 1. In the terminal navigate to a custom directory. You now need to type `brew update` to make sure you have the latest `whatwhat` formula. If you already have `whatwhat` brew installed, you'll need to uninstall it `brew uninstall whatwhat`. Then type
>       ```sh
>       brew install --build-bottle --verbose whatwhat
>       brew bottle whatwhat --no-rebuild
>       ```
>    This will create a bottle (a prebuilt binary) in that directory and will print a new `bottle do` block to your terminal
> 1. Copy that code snippet, go to the [ruby file](https://github.com/alan-turing-institute/homebrew-hut23/blob/main/whatwhat.rb) in the [homebrew-hut23](https://github.com/alan-turing-institute/homebrew-hut23/) and paste it to overwrite the previous `bottle do` instructions.
> 1. Commit the changes to main

**The following steps must still be done manually.**
The eventual aim is that the `update_whatwhat` executable will perform all of them for you as well:

1. Rename the bottle file to remove one of the dashes (i.e. from `whatwhat--` to `whatwhat-`)
1. Going back to the [whatwhat repo](https://github.com/alan-turing-institute/whatwhat/) go to releases -> tags tab and click on the latest tag.
1. Click create release from tag
1. Upload the binary from your computer
1. Click 'publish release'
1. Copy the url of the release and paste this as the first line within `bottle do` in the [ruby file](https://github.com/alan-turing-institute/homebrew-hut23/blob/main/whatwhat.rb), in the form of `root_url "https://github.com/alan-turing-institute/whatwhat/releases/download/<VERSION>"`. Note this should be the same as the url, but with 'download' rather than 'tag'. Make sure you commit this change.
1. This should now work - users will now be able to `brew install` the latest version of Whatwhat.
