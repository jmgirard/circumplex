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

exec docker run --rm --platform linux/arm64 \
  -v "$(cd "$REPO" && pwd)":/src -w /src \
  "$IMAGE" \
  bash -c "R CMD INSTALL --no-docs --no-byte-compile . >/dev/null 2>&1 && Rscript -e 'library(testthat); library(circumplex); setwd(\"tests/testthat\"); test_file(\"$FILE\")'"
