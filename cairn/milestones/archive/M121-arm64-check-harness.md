# M121: A local reproduction of CRAN's linux-arm64 check flavor

**Status:** done (2026-09-05, PR #154 https://github.com/jmgirard/circumplex/pull/154)

**Goal:** A one-command local check reproducing CRAN's linux-arm64 flavor, so a
platform-specific failure is found before submission rather than by rejection.

**Outcome:** `tools/arm64/` — a `linux/arm64` image pinned at its base by digest
(`r-base@sha256:41d55643…`, R 4.6.1, OpenBLAS 0.3.33+ds-3), `check.sh`, `testfile.sh`.
`check.sh` proves the tarball visible in the container, records R/platform/LAPACK/BLAS
from that container to `arm64-platform.txt`, refuses one not reporting `aarch64`,
clears both outputs per run, and exits 0 only on `Status: OK` — `R CMD check` itself
exits 0 on the WARNINGs CRAN rejects on. `PROFILE.md`'s release-walk blocks the CRAN
handoff without a green run's dated log in `cran-comments.md`; `CLAUDE.md` carries the
command; `^tools$` keeps it out of the built package. It reproduces 2.0.1's rejection
at `test-axes-certificate.R:544:3`, `PASS 2399` against CRAN's 2410.

**Decisions:** none cross-cutting. Locally: pin by digest and lag CRAN, since a
harness failing to build on submission day is worse than one a month behind.

**Review:** three-lens fan-out twice; the first returned AC4 for a gated amendment (its
"11 assertions they cost" clause was false). Prior-review lens: no evidence, zero
findings. Second pass: 17 findings — 14 fixed at the gate (the four false-green defects
in `check.sh`, each guard shown red on a planted fault first; `testfile.sh`, README and
ignore-file fixes; the menu restored), 2 to candidate rows, 1 noted, 4 rejected.
