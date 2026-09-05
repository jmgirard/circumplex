#!/usr/bin/env bash
# Check a built source tarball under CRAN's linux-arm64 additional-check
# flavor. See README.md for what the image is and what it does not cover.
#
#   usage: tools/arm64/check.sh /path/to/circumplex_X.Y.Z.tar.gz
#
# Writes, beside the tarball:
#   circumplex.Rcheck/   the check directory, 00check.log included
#   arm64-platform.txt   the platform and LAPACK the check ran on
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

# Recorded from inside the container: R CMD check prints no platform or LAPACK
# line of its own, so 00check.log cannot answer what the check actually ran on.
docker run --rm --platform linux/arm64 "$IMAGE" \
  Rscript -e 'cat("R: ", R.version.string, "\nplatform: ", R.version$platform, "\nLAPACK: ", La_library(), "\nBLAS: ", extSoftVersion()[["BLAS"]], "\n", sep = "")' \
  | tee "$DIR/arm64-platform.txt"

set +e
docker run --rm --platform linux/arm64 \
  -e _R_CHECK_FORCE_SUGGESTS_=false \
  -e _R_CHECK_TESTS_NLINES_=0 \
  -v "$DIR":/pkg -w /pkg \
  "$IMAGE" \
  R CMD check --no-manual --no-vignettes "$BASE"
STATUS=$?
set -e

echo
echo "platform record: $DIR/arm64-platform.txt"
echo "check directory: $DIR/circumplex.Rcheck"
exit "$STATUS"
