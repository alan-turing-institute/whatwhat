# Developer notes

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
