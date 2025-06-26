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

## Developer notes

See [`CONTRIBUTING.md`](https://github.com/alan-turing-institute/whatwhat/blob/main/CONTRIBUTING.md) for information on how to contribute to `whatwhat`.
