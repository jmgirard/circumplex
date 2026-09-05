#!/usr/bin/env bash
# Check a built source tarball under CRAN's linux-arm64 additional-check
# flavor. See README.md for what the image is and what it does not cover.
#
#   usage: tools/arm64/check.sh /path/to/circumplex_X.Y.Z.tar.gz
#
# Writes, beside the tarball (both removed before the run, so nothing an
# earlier run left behind can be read as this one's result):
#   circumplex.Rcheck/   the check directory, 00check.log included
#   arm64-platform.txt   the R version, platform, LAPACK and BLAS of the
#                        CONTAINER the check ran in -- a separate R process
#                        from `R CMD check`, so container identity, not
#                        process identity
#
# Exit status: 0 only when 00check.log reports `Status: OK`. `R CMD check`
# itself exits 0 on WARNINGs and NOTEs, which CRAN rejects on, so its status
# is not forwarded unchanged.
#
# _R_CHECK_FORCE_SUGGESTS_=false because the image deliberately omits the
# heavyweight Suggests (brms, OpenMx, glmmTMB, vdiffr); their tests self-skip.
# _R_CHECK_TESTS_NLINES_=0 because the default 13 lines of failing-test output
# routinely fill with vdiffr snapshot notices, leaving the actual failure --
# file, line and message -- out of 00check.log entirely.
set -euo pipefail

IMAGE="${CIRCUMPLEX_ARM64_IMAGE:-circumplex-arm64check:latest}"
TARBALL="${1:?usage: check.sh <tarball>}"
[ -f "$TARBALL" ] || { echo "no such tarball: $TARBALL" >&2; exit 2; }
DIR="$(cd "$(dirname "$TARBALL")" && pwd)"
BASE="$(basename "$TARBALL")"

if ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  echo "image $IMAGE not built; see tools/arm64/README.md" >&2
  exit 2
fi

# Docker Desktop shares only a configured set of host paths, and a bind mount
# of a path outside that set silently becomes an EMPTY directory: R CMD check
# then warns that the tarball "is neither a file nor directory", skips it, and
# EXITS 0. A green harness that ran nothing is worse than no harness, so the
# tarball's visibility inside the container is proven before anything else.
if ! docker run --rm --platform linux/arm64 -v "$DIR":/pkg -w /pkg "$IMAGE" \
     test -f "$BASE"; then
  echo "$BASE is not visible inside the container." >&2
  echo "$DIR is probably outside Docker Desktop's shared paths;" >&2
  echo "move the tarball under your home directory, or add the path in" >&2
  echo "Docker Desktop > Settings > Resources > File sharing." >&2
  exit 2
fi

# 00check.log names no LAPACK or BLAS path, so it cannot say which linear
# algebra the check ran against -- and on this package that is the whole
# question. The probe therefore runs in the SAME CONTAINER as the check, one
# `docker run`. It is still a separate R process from `R CMD check`, so what
# it establishes is the container's R installation, not the checking
# process's own: container identity, not process identity. The file is
# stamped with the tarball and the UTC time and removed before the run, so a
# previous run's answer can neither survive beside a new log nor be mistaken
# for this one's. The check directory goes the same way: `R CMD check` unlinks
# only the directory its own tarball names, so a run that dies before the
# check starts would otherwise leave a previous log for the Status: guard
# below to read.
rm -f "$DIR/arm64-platform.txt"
rm -rf "$DIR/circumplex.Rcheck"

PROBE='cat("R: ", R.version.string, "\nplatform: ", R.version$platform, "\nLAPACK: ", La_library(), "\nBLAS: ", extSoftVersion()[["BLAS"]], "\n", sep = "")'

set +e
docker run --rm --platform linux/arm64 \
  -e _R_CHECK_FORCE_SUGGESTS_=false \
  -e _R_CHECK_TESTS_NLINES_=0 \
  -e PROBE="$PROBE" \
  -v "$DIR":/pkg -w /pkg \
  "$IMAGE" \
  bash -c 'set -euo pipefail
           printf "tarball: %s\ndate: %s\n" "$1" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > arm64-platform.txt
           Rscript -e "$PROBE" >> arm64-platform.txt
           cat arm64-platform.txt
           # --platform does not guarantee an arm64 IMAGE: an image built on an
           # amd64 host, or a CIRCUMPLEX_ARM64_IMAGE pointed at one, runs under
           # emulation and reports x86_64. A green from that machine is not
           # evidence about the flavor this harness exists to reproduce.
           grep -q "^platform: aarch64" arm64-platform.txt || {
             echo "not an aarch64 container -- see arm64-platform.txt" >&2
             exit 3
           }
           set +e
           R CMD check --no-manual --no-vignettes "$1"
           exit 0' _ "$BASE"
STATUS=$?
set -e

# The container exits 0 once `R CMD check` has run at all; anything else is the
# harness failing to get that far (an unbuilt image, a non-aarch64 one, a dead
# probe), never a package result.
if [ "$STATUS" -ne 0 ]; then
  echo "the harness did not complete (container exit $STATUS)" >&2
  exit 2
fi

# A check that produced no verdict is a harness failure, not a package result.
LOG="$DIR/circumplex.Rcheck/00check.log"
if [ ! -f "$LOG" ] || ! grep -q '^Status:' "$LOG"; then
  echo "no Status: line in $LOG -- the check did not complete" >&2
  exit 2
fi

echo
grep '^Status:' "$LOG"
echo "platform record: $DIR/arm64-platform.txt"
echo "check directory: $DIR/circumplex.Rcheck"

# CRAN rejects on WARNINGs, and `R CMD check` exits 0 on them, so the verdict
# is read from the log rather than taken from the exit status.
grep -q '^Status: OK$' "$LOG" || exit 1
