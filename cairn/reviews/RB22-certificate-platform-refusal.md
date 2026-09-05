# RB22: A platform-dependent refusal at the certificate's counterexample B (M121/M122)

- **Date:** 2026-09-05
- **Output required:** write findings to `cairn/reviews/RR22-certificate-platform-refusal.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package for circumplex data analysis, on CRAN, currently
at 2.0.0 published. Version 2.0.1 is a patch release that exists solely to
clear test failures that 2.0.0 shows on CRAN's check farm. **It has now been
rejected twice at CRAN's incoming pre-test**, each time for a test failure on
a platform the maintainer cannot run locally. The maintainer is blocked.

`axes_reliability()` reports standard errors for the reliability of the
circumplex axes. Those SEs are computed through a chain of two matrix
inversions (`axes_pricing_core()`), and because that chain can lose accuracy
at ill-conditioned inputs, the package ships an **accuracy certificate**: a
per-fit estimate, computed by replaying the same arithmetic in compensated
double-double precision, of the relative error in the reported figures.

`tests/testthat/test-axes-certificate.R` is the suite that validates the
certificate. Its core claim is that at six committed geometries — five
"anchor" matrices built from `cos()` calls in the test file, plus
"counterexample B" read from a committed fixture — the certificate's estimate
*brackets* the machine's own true relative error, where "true" means measured
against exact quadratic forms committed as hexadecimal literals from an
exact-rational oracle.

Counterexample B is deliberately the worst case in the set: the one committed
matrix on which the shipped corrected SEs are measurably wrong while the
package's pre-2026 criterion reported them without complaint.

### The failure under review

CRAN's linux-arm64 additional check on 2.0.1 (commit `ecb06de7`) reports:

```
── Failure ('test-axes-certificate.R:544:3'): AC2/AC3: at counterexample B
   the estimate brackets a 3.4%-wrong SE ──
the shipped pricing REFUSES at case 'cxb' (unidentified, unidentified) --
an admitted geometry, so this is a regression, not a platform difference

[ FAIL 1 | WARN 4 | SKIP 540 | PASS 2410 ]
```

Full log:
<https://win-builder.r-project.org/incoming_pretest/circumplex_2.0.1_20260904_173758/specialChecks/linux-arm64/00check.log>

**Root cause, measured 2026-09-05 and reproducible.** `axes_pricing_core()`
inverts `sigma`, builds the ML information matrix `info`, and inverts that.
At counterexample B:

| platform | `rcond(info)` | `solve(info)` |
|---|---|---|
| macOS aarch64, R's reference BLAS/LAPACK | 2.6008e-16 | succeeds |
| aarch64 Linux, OpenBLAS 0.3.33 | 2.0494e-16 | throws "system is computationally singular" |

`solve()`'s default tolerance is `.Machine$double.eps` = 2.22e-16. The two
platforms straddle it by 17%. On the Linux side `axes_pricing_core()` returns
the string `"unidentified"`, `axes_v_pricing()` and `axes_u_pricing()` forward
it, and the test's refusal branch calls that a regression and fails.

For scale: `kappa(sigma)` = 6.65e6 and `kappa(info)` = 3.0e15 at this matrix.
Both are properties of the committed fixture, identical on both platforms —
the fixture is read from a `.rds`, so no matrix-construction difference is
involved. Only the inversion's outcome differs.

The failure has been reproduced locally in a `linux/arm64` Docker container
(`r-base`, R 4.6.1, `aarch64-unknown-linux-gnu`, OpenBLAS): same file, same
line, same message, same skip count, full `R CMD check` in 1 minute 27
seconds. So any proposed fix can be verified against the failing platform
before resubmission. That harness is not yet committed to the repo; landing it
is the other half of this planning round (Question 4 below).

### What the suite does with a refusal today, and why

`cert_true_error()` (`tests/testthat/test-axes-certificate.R:348-400`) has two
exits before it prices anything:

1. **The matrix check.** If this machine does not build the anchor matrix bit
   for bit, the case `skip()`s — the committed exact quadratic forms are not a
   yardstick for a different matrix.
2. **The refusal branch.** If the shipped pricing returns a string, the case
   `fail()`s, on the stated grounds that all six geometries are *admitted*
   (`axes_sigma_degenerate()` passes on each), so a refusal must be a
   regression in `axes_pricing_core()` rather than a platform difference.

That split is deliberate and hard-won. A predecessor of this file gated the
comparison behind the matrix check alone, and on ubuntu and windows **all six
cases skipped silently** — `skip()` is green to `testthat` and to `R CMD
check`, so an all-skip run looked like an all-pass run and the suite asserted
nothing at all. The repo's recorded lesson from that episode is that a
precondition gating a frozen comparison can swallow the regression it guards,
and that a refusal must be discriminated from a non-reproduction. **The
refusal branch exists because of that lesson.** A fix must not undo it.

A file-local environment records each case's disposition as it runs
(`cert_record(id, "priced")` and similar), and a test at the end of the file —
the "all-skip detector" — reddens if no case was priced.

### The complication a criteria audit surfaced

The obvious repair — admit the refusal at counterexample B and move on —
has a cost that must be weighed rather than assumed away. If
`cert_true_error()` returns `NULL` there, the cxb test block returns early
and **five assertions never run**: three `cert_bracket()` calls, and the two
`expect_gt(true_rel$…, axes_degeneracy_delta_star)` assertions at lines
565-566 that are the file's only claim that no machine reaches the accuracy
target on this matrix. A green `FAIL 0` on arm64 would then be compatible
with counterexample B asserting nothing beyond its condition number, with
nothing in the suite saying so.

## Materials

Read, in this repo:

- `tests/testthat/test-axes-certificate.R` — the whole file (~1200 lines).
  Key sites: `cert_anchors()` at line 50; `cert_rel()` / `cert_root_rel()` /
  `cert_floor` at 255-270; the safety-factor assertion at 311;
  `cert_record()` / `cert_disposition()` at 332-344; `cert_true_error()` at
  348-400 (matrix check 364-375, refusal branch 381-393, `cert_record(id,
  "priced")` at 398); `cert_bracket()` at 428-448; the five generated
  per-anchor tests at 334-353 of the test section; the counterexample-B block
  at 526-570; the all-skip detector at the end of the file.
- `R/axes_corrected_se.R` — `axes_pricing_core()` at 164-181 (the
  `"singular"` return at 166, the `"unidentified"` return at 179),
  `axes_v_pricing()` from 188.
- `R/axes_certificate.R` — `axes_certificate_sentinel()` at 436 and
  `axes_accuracy_certificate()` from 438, including every early return to the
  sentinel.
- `tests/testthat/fixtures/rb18-counterexample-b.rds` — the fixture. Its
  provenance is documented at its first read site in
  `tests/testthat/test-axes-scaled-fit.R`. Note it is read at six further
  sites beyond the one under review (`test-axes-certificate-refusal.R:193`,
  `:661`, `:734`; `test-axes-scaled-fit.R:1674`, `:1740`, `:2197`) — whether
  any of those is also exposed to the arm64 refusal is an open question you
  may answer.
- `cairn/DESIGN.md` — the numbered principles, and the "Known fragilities"
  paragraph added 2026-08-31, which lists six latent defects in this
  certificate's validation layers.
- `cairn/reviews/archive/RR21-axes-degeneracy-per-fit-certificate.md` — the
  review that specified this certificate. Its §2 states the n-free
  construction the bracket claim rests on.
- `cairn/DECISIONS.md` — scan the `### D-` headings; D-048, D-049, D-050,
  D-051, D-053 and D-054 concern this certificate and its accuracy target.
- `cairn/ROADMAP.md` — the candidate row titled "The all-skip detector's two
  open edges", whose full text matters to Question 5.

To reproduce the failure, or to test a proposed fix, on an arm64 host with
Docker:

```
docker run --rm --platform linux/arm64 -v "$PWD":/src -w /src \
  r-base:latest bash -c 'Rscript -e "..."'
```

The maintainer's working harness builds an image with the package's Imports
plus `testthat`, then runs `R CMD check --no-manual --no-vignettes` with
`_R_CHECK_FORCE_SUGGESTS_=false`.

## Questions

1. **What must remain asserted at counterexample B on a platform where the
   shipped pricing refuses to price it, for the certificate suite to still be
   doing its job there?** Answer in terms of specific assertions, not
   principles. Consider at minimum: the identity of the refusal (the
   `"unidentified"` literal from the `acov` inversion, as against `"singular"`
   from the `sigma` inversion or `"indefinite"` from elsewhere); the fixture's
   unchanged condition number; the certificate's own sentinel return; and
   whether any part of the bracket claim survives without a priced value.

2. **How should the suite tell a platform-dependent refusal apart from a
   regression?** Three candidates were weighed before this brief was written,
   and you are not limited to them:
   (A) admit a refusal at counterexample B only, assert its identity and the
   unchanged geometry, record a distinct disposition, and make the all-skip
   detector redden when every case refuses;
   (B) give `axes_pricing_core()`'s `solve()` an explicit tolerance so the
   outcome stops depending on the platform's LAPACK — this changes when the
   *shipped* package refuses to price real user data, near the boundary,
   and would need its own oracle work;
   (C) something else.
   Say which you recommend and what would falsify the choice. If (A), state
   how the admission is scoped so that it cannot silently widen to the five
   anchor cases. If (B), state what the tolerance should be derived from —
   the repo's recorded lesson is that a tolerance calibrated on one machine is
   not a tolerance — and what oracle would validate it.

3. **Is a refusal at this matrix the *correct* behavior of the shipped
   package, on either platform?** `rcond(info)` = 2.05e-16 and 2.60e-16 both
   sit at the edge of double precision. The question is not which platform is
   right but whether a package that prices this matrix on one machine and
   refuses it on another is reporting honestly in both cases, and whether the
   certificate's sentinel (all fields 1, meaning "no reported number left to
   certify") is the right answer at a matrix this ill-conditioned. If the
   honest answer is that the package should refuse this matrix *everywhere*,
   say so — that changes the test's job entirely.

4. **The certificate's validation layer is on its fifth escalation** (RB18,
   RB19, RB20, RB21, and this brief). It has generated ten milestones, six
   recorded latent defects that no passing test run can reach, and now a
   share of two CRAN rejections. **Weigh retiring it** — wholly, or the
   counterexample-B layer specifically, or the exact-rational bracket
   apparatus while keeping a coarser check — against keeping and repairing
   it. State what verification capability would actually be lost, and whether
   that capability is load-bearing for any claim the package makes to users.
   A blunt retirement outperforming an amended keep is a live possibility
   here, not a formality.

5. **Which already-recorded weaknesses in this file should travel with the
   fix rather than being deferred again?** Three are on record:
   (a) the roadmap's stated premise that "the `cxb` case … always prices",
   which this session proved false — its recorded repair is to require that at
   least one `cos()`-built anchor is priced, accepting a red on any math
   library that rounds `cos(225°)` differently;
   (b) the disposition vocabulary is an unpinned string contract —
   `cert_record(id, "priced")` sits after the three `cert_rel()` calls by
   comment alone, so moving it or mistyping either site makes the detector
   green-on-nothing or red-always with no other signal; it is also why the
   fix's own evidence is unreadable today, since nothing prints which cases
   were actually checked;
   (c) the six latent defects under `DESIGN.md` "Known fragilities".
   For each, say travel-with-the-fix or defer, with a reason.

6. **How firmly should the arm64 reproduction harness be wired in?** Two
   sub-questions. First, pin the container's base image to an exact digest,
   or track a rolling tag — the rolling tag is closer to what CRAN's runners
   upgrade to, but the maintainer's first build today failed on Debian
   unstable's package index having drifted out of sync with the image.
   Second, should running the arm64 check become a *required* step before any
   CRAN submission, or an advisory one? Note the image deliberately omits
   `brms`, `OpenMx`, `glmmTMB` and `vdiffr` (hours to build, or system font
   headers), so it runs 2399 passing assertions against CRAN's 2410 — say
   whether that gap undermines the harness's value as a pre-submission gate.

## Constraints

Fixed; flag disagreement explicitly rather than working around it.

- **IP1: statistical correctness outranks all other concerns**, release
  timing included. That the maintainer is blocked on a CRAN resubmission is
  context, not an argument. If the right answer is slower, say so.
- **IP3: every shipped numeric result is validated against at least two
  independent oracle types**, and the oracles are recorded at the asserting
  tests. A fix that narrows the exact-rational oracle's reach on a platform
  must say so out loud.
- The recorded lesson behind the refusal branch — an all-skip run reads as an
  all-pass run, and a refusal must be discriminated from a non-reproduction —
  stands unless you argue explicitly for superseding it.
- D-048/D-049 fix the certificate's accuracy target and its derivation;
  D-051 and D-053 fix its estimand. Do not relitigate those numbers.
- `axes_accuracy_certificate()`'s sentinel contract (all fields 1 at a
  refusal) is D-051's; changing it is a decision, not an implementation
  detail.
- The repo is base R plus a small dependency set (rlang, ggplot2, boot,
  Rcpp/RcppArmadillo). No new package dependency without its own gate.

## Output format

In `RR22-certificate-platform-refusal.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately
under "Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason. Your report is advisory: emit a
`## Binding criteria` section ONLY if this brief's header slot says
`requested`. It says `not requested`.
