# WhatWhat

`whatwhat` is an OCaml command-line tool to help monitor project status and allocations in the [Research Engineering Group](https://www.turing.ac.uk/research-engineering), aka Hut23.

## Install

`whatwhat` is available via Homebrew:

```sh
brew update
brew tap alan-turing-institute/hut23
brew install alan-turing-institute/hut23/whatwhat
```

Once installation is complete, run `whatwhat init`.
This will set up some configuration files in `~/.config/whatwhat`.
You will now have to edit them to add your personal tokens, as described below.

## Configure

In `~/.config/whatwhat/secrets.json`, you should see the following JSON template:

```json
{
    "githubToken"    : "<yours here>",
    "forecastToken"  : "<yours here>",

    // other stuff can be ignored
}
```

`githubToken` can be obtained from https://github.com/settings/tokens.
**Note that you have to use the "classic" tokens, not the "fine-grained, repo-scoped" tokens.**
The token will need to have the permissions: `read:user`, `repo`, and `user:email`.

`forecastToken` can be obtained from https://id.getharvest.com/oauth2/access_tokens/new.

The other fields in this file can be ignored.

<details><summary>Extra: creating a lookup table for GitHub usernames</summary>

`whatwhat` attempts to match people's full names on Forecast with GitHub usernames using some heuristics.
It is generally quite successful at doing this, but there are a few edge cases where people's GitHub profiles do not have enough data.
To see what `whatwhat`'s automatic username detection is doing, you can run `whatwhat dump-users`.

If you find that the automatic detection for a person is wrong or missing, you can override it by manually add a mapping inside the file `~/.config/whatwhat/users`.
Each line in this file should look like:

    {Full name on Forecast}:{GitHub username}

(without the curly braces).
</details>

## Usage

Once you have added the two tokens, you should be able to run `whatwhat` from the command line.

The main commands you are likely to use are shown here.
For extensive usage options, you can run `whatwhat --help`, or `whatwhat <subcommand> --help`.

| Command | Description |
| ------  | ----------- |
| `whatwhat` | Fetch all projects, detecting inconsistencies and errors |
| `whatwhat project [NUM\|NAME]` | Print information about a project, as specified by its GitHub issue number, or (a substring of) its title |
| `whatwhat person [NAME]` | Print information about a person, as specified by their name, Turing email username, or GitHub username (a substring is fine) |
| `whatwhat overview` | Print a TL;DR of what's happening in REG |
| `whatwhat export-project` | Create a Forecast project export CSV file (useful for [other reporting purposes](https://github.com/alan-turing-institute/Hut23/issues/1354)) |
| `whatwhat export-team` | Create a Forecast team export CSV file |
| `whatwhat open [NUM]` | Open a GitHub issue in a browser (macOS only, as this uses `open(1)`) |

## A historical note

Whatwhat is the successor to [NowWhat](https://github.com/alan-turing-institute/nowwhat) (F#) and [WhatNow](https://github.com/alan-turing-institute/whatnow) (Racket).

The latter contains [an illuminating backbrief](https://github.com/alan-turing-institute/whatnow/blob/main/backbrief/backbrief.org) describing the history of project allocations in REG.

----------------------------------------------------------

## Developers' Contents

1. [Setting up OCaml on macOS](#setting-up-ocaml-on-macos)
1. [Installation and configuration](#installation-and-configuration)
1. [Resources for getting started with OCaml](#resources-for-getting-started-with-ocaml)
1. [Creating a new release](#creating-a-new-release)

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

If it complains about a missing configuration or missing secret, you will have to run the initialisation as described above.

## Extra configuration for developers

The secrets file also contains `githubBotToken` and `slackToken`.
To obtain this, you need to be added to the `hut23-1206-nowwhat@turing.ac.uk` group (ask someone else on the `whatwhat` developer team to add you, e.g. the person who most recently committed to `main`).
However, for initial development purposes, it suffices to use an empty string, because these tokens are only used if you wish to actually post to GitHub or Slack respectively.

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

## Creating a new release

When you make changes to the `whatwhat` code, the Homebrew tap of whatwhat needs to be updated.
To do this, run **`dune exec update_whatwhat`** from the top level of the git repository.
It takes a few minutes to run.

If it fails for any reason, fix the underlying error, then just rerun it and increment the version number one more time.

The update process used to be manual. It involved the following steps, which are saved here for posterity:

<details>
<summary>Steps for manually updating Homebrew release</summary>

1. Update the `version` in the `dune-project` file and commit that.
1. Create a new git tag using `git tag -a vX.Y.Z -m vX.Y.Z` (replace X, Y, Z with the appropriate numbers.
1. To push the tag to the repo, run `git push` followed by `git push --tags`
1. Edit the `whatwhat.rb` file in the [homebrew-hut23](https://github.com/alan-turing-institute/homebrew-hut23/) repo. In this instance, you only need to edit the line beginning `url "...", tag "vX.Y.Z", revision "..."`.
   Change the `tag` string to the new version number, and replace the revision string with the *full* git commit SHA corresponding to the tag you just added. You can obtain this from the output of `git log`.
    <img src="https://github.com/alan-turing-institute/whatwhat/assets/22414895/8db017b3-74d1-4644-b3d0-9aa2e1551281" alt="Commit SHA" />  
    This single step used to be done via a GitHub Action. The action yaml has since been removed from the repository, but you can see the [file in a previous commit here](https://github.com/alan-turing-institute/whatwhat/blob/68ebc36d55864baccaedab7ea928ad722493907a/.github/workflows/bump-brew-formula.yaml).
1. In the terminal navigate to a custom directory. You now need to type `brew update` to make sure you have the latest `whatwhat` formula. If you already have `whatwhat` brew installed, you'll need to uninstall it `brew uninstall whatwhat`. Then type
      ```sh
      brew install --build-bottle --verbose whatwhat
      brew bottle whatwhat --no-rebuild
      ```
   This will create a bottle (a prebuilt binary) in that directory and will print a new `bottle do` block to your terminal
1. Copy that code snippet, go to the [ruby file](https://github.com/alan-turing-institute/homebrew-hut23/blob/main/whatwhat.rb) in the [homebrew-hut23](https://github.com/alan-turing-institute/homebrew-hut23/) and paste it to overwrite the previous `bottle do` instructions.
1. Commit the changes to main
1. Rename the bottle file to remove one of the dashes (i.e. from `whatwhat--` to `whatwhat-`)
1. Going back to the [whatwhat repo](https://github.com/alan-turing-institute/whatwhat/) go to releases -> tags tab and click on the latest tag.
1. Click create release from tag
1. Upload the binary from your computer
1. Click 'publish release'
1. Copy the url of the release and paste this as the first line within `bottle do` in the [ruby file](https://github.com/alan-turing-institute/homebrew-hut23/blob/main/whatwhat.rb), in the form of `root_url "https://github.com/alan-turing-institute/whatwhat/releases/download/<VERSION>"`. Note this should be the same as the url, but with 'download' rather than 'tag'. Make sure you commit this change.
1. This should now work - users will now be able to `brew install` the latest version of Whatwhat.
</details>
