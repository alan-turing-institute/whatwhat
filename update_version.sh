#!/usr/bin/env bash

# Script to automatically update version number for whatwhat.
#
# This edits the dune-project file in the top-level directory, creates a new
# git commit, and a new git tag.
#
# Usage:
#
#   ./update_version.sh <NEW_VERSION>
#
# The old version number is read automatically from the output of `git tag`.

# cd to the correct directory
cd $(dirname "$0")

# Get the current version number from git tag
git_vno=$(git for-each-ref --sort=creatordate --format '%(refname)' refs/tags | sed 's:refs/tags/::' | grep "^v[[:digit:]]\{1,\}\.[[:digit:]]\{1,\}\.[[:digit:]]\{1,\}" | tail -n 1 | sed 's/v//')

usage() {
    cat << EOM
Usage:
  $0 <NEW_VERSION>

(The current version number is $git_vno)
EOM
}

git_dirty() {
    cat << EOM
There are uncommitted changes or untracked files present in the working directory.
Please commit or remove these before running this script.
EOM
}

# Check arguments
if [ "$1" = "-h" ]; then
    usage
    exit 0
elif [ $# -lt 1 ]; then
    usage
    exit 1
elif [ $# -gt 1 ]; then
    usage
    exit 1
fi

# Check if git index or working directory has new stuff
if ! [ -z "$(git status --porcelain=v1 2>/dev/null)" ]; then
    git_dirty
    exit 1
fi

# Escape the dots in version numbers
old_vno=${git_vno//./\\.}
new_vno=$1

# Check version of sed as GNU and BSD versions differ in sed -i behaviour
sed --version >/dev/null 2>&1
if [ $? -eq 0 ]; then
    sed_command=("sed" "-i")        # GNU
else
    sed_command=("sed" "-i" "")     # BSD
fi

# Edit dune-project
"${sed_command[@]}" "s/${old_vno}/${new_vno}/g" "./dune-project"

# git add and commit
git add -A
git commit -m "Update version number to v${new_vno}"
git tag "v${new_vno}"

echo ""
echo "Version numbers updated from v${git_vno} to v${new_vno}."
