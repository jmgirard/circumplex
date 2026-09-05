#!/usr/bin/env bash
# Run ONE test file against the installed package under linux-arm64 -- the
# fast loop while chasing a failure check.sh has already found.
#
#   usage: tools/arm64/testfile.sh <repo-root> <test-file-basename>
set -euo pipefail

IMAGE="${CIRCUMPLEX_ARM64_IMAGE:-circumplex-arm64check:latest}"
REPO="${1:?usage: testfile.sh <repo-root> <test-file>}"
FILE="${2:?usage: testfile.sh <repo-root> <test-file>}"

if ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  echo "image $IMAGE not built; see tools/arm64/README.md" >&2
  exit 2
fi

# The repo is mounted READ-ONLY and copied inside the container before the
# install: `R CMD INSTALL` compiles in place, and compiling here would leave
# aarch64-Linux src/*.o and src/*.so in a macOS working tree, where the next
# `devtools::load_all()` fails on them. The test-file name is passed as a
# positional argument, never interpolated into the `bash -c` string, so a
# filename carrying a quote breaks nothing. Install output is NOT discarded:
# a failed compile has to say why.
exec docker run --rm --platform linux/arm64 \
  -v "$(cd "$REPO" && pwd)":/src:ro \
  "$IMAGE" \
  bash -c 'set -euo pipefail
           cp -a /src /build
           cd /build
           R CMD INSTALL --no-docs --no-byte-compile .
           cd tests/testthat
           Rscript -e "library(testthat); library(circumplex); test_file(commandArgs(TRUE)[[1]])" "$1"' _ "$FILE"
