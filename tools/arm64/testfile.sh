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
#
# THE COPY IS SWEPT of build products first (M122). A macOS working tree that
# has been through `devtools::load_all()` carries Mach-O `src/*.o` and a
# `src/circumplex.so`; `make` inside the container finds them newer than the
# sources, reports "Nothing to be done", and links the macOS objects into an
# aarch64 library. The install then fails at load with "invalid ELF header" --
# which reads like a broken image rather than what it is. `check.sh` builds
# from a tarball and never sees this; only this working-tree path does.
#
# `package = "circumplex"` is what puts the package's INTERNALS in scope
# (M122). `library(circumplex)` attaches the exports only, and the helper
# files this suite sources call unexported functions at load time, so the run
# died in `source_test_helpers()` before reaching a single test. `R CMD check`
# never hits this either: `tests/testthat.R` calls `test_check()`, which
# evaluates in the package namespace. NOT_CRAN is passed through so a caller
# can reach the blocks CRAN skips.
exec docker run --rm --platform linux/arm64 \
  -v "$(cd "$REPO" && pwd)":/src:ro \
  -e "NOT_CRAN=${NOT_CRAN:-}" \
  "$IMAGE" \
  bash -c 'set -euo pipefail
           cp -a /src /build
           cd /build
           rm -f src/*.o src/*.so src/*.dll src/*.dylib
           R CMD INSTALL --no-docs --no-byte-compile .
           cd tests/testthat
           Rscript -e "library(testthat); test_file(commandArgs(TRUE)[[1]], package = \"circumplex\")" "$1"' _ "$FILE"
