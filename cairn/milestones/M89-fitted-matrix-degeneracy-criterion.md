# M89: One degeneracy criterion for the two fitted-matrix consumers

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP4
- **Branch/PR:** `m89-fitted-matrix-degeneracy` / https://github.com/jmgirard/circumplex/pull/117

## Goal

Give `axes_reliability()`'s two consumers of `lavaan::fitted(fit)$cov` one
stated criterion for when that matrix is too degenerate to price, so a user
never receives NA corrected SEs beside silently scaled fit statistics derived
from the same matrix.

## Scope

Surface tier: **user-facing** — `axes_reliability()`'s `$fit`, `$components`
and printed output are exported surfaces a user reads, and the reason literals
are documented and printed verbatim.

**In:** one stated degeneracy criterion on the fitted covariance matrix,
recorded beside its rationale and applied at both consumers
(`R/axes_reliability.R:1715`, `:1824`); relaxing `axes_corrected_se()`'s
emergent `solve()`-based refusals so the stated criterion is what gates;
one shared reason vocabulary across both surfaces, including the `+Inf` case;
what `axes_reliability()` reports when the criterion fires; regression probes
at the boundary; the documented reason enumerations and a NEWS entry.

**Out:** the upstream input-side positive-definiteness gate at
`R/axes_reliability.R:1428`, which prices the user's input R rather than the
fitted matrix → stays unowned, no candidate row (no defect is known in it).
Any change to the scaling arithmetic itself → D-036 stands. A new oracle for
the scaled statistic → M68 already carries one.

## Acceptance criteria

- [x] **AC1** — A single stated degeneracy criterion on the fitted covariance
      matrix is recorded in code beside its rationale, and is applied at both
      consumers: the `corrected <- axes_corrected_se(...)` expression and the
      `scaling <- axes_scaling_factor(...)` expression in
      `R/axes_reliability.R`. *(RB tripwire: no-oracle — escalation offered and
      declined at the plan gate; see work log.)*
- [x] **AC2** — The two surfaces agree, in both senses, over the probe grid the
      AC4 test enumerates (both diagonal positions × k = 0..16 × the forms in
      AC4) plus the `+Inf` and `-Inf` cases: they return a non-NULL `reason` at
      exactly the same grid points, and wherever both refuse they name the same
      reason literal. `axes_corrected_se()` is the surface that adopts
      `"infinite_diagonal"` for `+Inf`; `axes_scaling_factor()`'s literal is
      unchanged. Asserted by a test that runs that grid and compares the two
      `reason` fields pairwise.
- [x] **AC3** — The new criterion is evaluated after the existing `<= 0` and
      `is.infinite()` diagonal guards in `axes_scaling_factor()`
      (`R/axes_scaled_fit.R:147-148`), and the M71 AC1/AC2 block in
      `tests/testthat/test-axes-scaled-fit.R:1258-1300` passes byte-unchanged.
- [x] **AC4** — Regression probes fail against the pre-milestone code at ≥2
      distinct diagonal positions, and include ≥1 non-inflation form (a
      near-collinear item pair or a near-zero positive diagonal) that drives
      the divergence in the opposite direction — `axes_scaling_factor()`
      refusing while the raw-priced branch survives.
- [x] **AC5** — The documented reason enumerations (`R/axes_reliability.R`
      roxygen and the regenerated `man/axes_reliability.Rd`) and `NEWS.md`
      name the new literal and the new NA condition.
- [x] **AC6** — On a constructed fitted matrix that trips the criterion inside
      `axes_reliability()`, the corrected component SEs and the four scaled
      statistics D-036 scales (`chisq`, `pvalue`, `rmsea`, `cfi`) are all NA,
      each surface warning names the shared reason, and `df` and `srmr` are
      unaffected.
- [x] **AC7** — `devtools::test()` clean and `devtools::check(args =
      "--no-manual")` clean, with a warning-free `devtools::document()` and a
      diff-free `man/`/`NAMESPACE` beyond AC6's intended change.

## Coverage

- AC1 → T3, T4
- AC2 → T1, T4, T5
- AC3 → T5
- AC4 → T1, T2
- AC5 → T7
- AC6 → T6
- AC7 → T8

## Tasks

- [x] **T1** — Test-first: the AC2 grid as a failing test — both diagonal
      positions × k = 0..16 at the octant probe, comparing the two `reason`
      decisions pairwise. Red against HEAD from k = 7 up.
- [x] **T2** — Add the non-inflation probe form (near-collinear pair or
      near-zero positive diagonal) and confirm by measurement that it drives
      the divergence in the opposite direction. Red against HEAD.
- [x] **T3** — Choose the stated criterion and record its rationale in code.
      It must price the raw Σ̂: `cov2cor()` of an inflated matrix stays at
      condition 10.45, so a correlation-metric test cannot see this at all.
- [x] **T4** — Apply the criterion at both consumers; relax
      `axes_corrected_se()`'s emergent `solve()`-based refusals
      (`R/axes_corrected_se.R:162-163`) so the stated criterion is the gate.
- [x] **T5** — Unify the reason vocabulary; confirm the M71 block passes
      byte-unchanged.
- [x] **T6** — Assembly-level test through `axes_reliability()` on a
      constructed fitted matrix.
- [x] **T7** — Roxygen reason enumeration, `devtools::document()`, NEWS entry.
- [x] **T8** — Full `devtools::check()`.
- [x] **T9** — Requested at the 2026-08-15 review gate: NEWS names two of the
      four user-visible reason-literal changes. Add the other two — an exactly
      singular fitted matrix moves `"singular"` → `"ill_conditioned"` and an
      indefinite one moves `"indefinite"` → `"ill_conditioned"`, on both
      surfaces — then re-run `devtools::test()` and `devtools::check()`.
- [x] **T10** — Requested at the same gate: no test asserts `"unidentified"` or
      `"indefinite"` as a *returned* reason from either fitted-matrix surface
      (review finding O3, scored 65). Fire what is reachable with its condition
      asserted and a passing control, and correct the reachability claim in
      `R/axes_reliability.R` for whatever is not.
- [ ] **T11** — Requested at the round-2 review gate: escalate the criterion's
      metric choice to a Fable review via `/milestone-brief`. The question is
      which matrix the degeneracy criterion should price — raw Sigma-hat, as
      shipped, or the `cov2cor(Sigma-hat)` both surfaces actually invert — given
      two verified counterexamples pointing in opposite directions (O1/RS5, 85;
      RO2, 70). Ingest the RR, then re-review.

## Work log

- 2026-08-15: created by /milestone-plan. Graduates two ROADMAP candidate rows — the finite-degenerate scaling row and the `+Inf` reason-label row, the latter taken in by the plan gate's wide-scope choice.
- 2026-08-15: criteria audit ([O], fresh context) returned seven findings; six fixed before the gate (numbering as shipped): AC1's `grep` procedure selected four lines of which two were comments, replaced with expression-pinned sites; AC3's ordering constraint stated; AC6 restated at the helper-plus-assembly boundary with a constructed matrix, no real fit being known to reach the regime, and narrowed to the four statistics D-036 scales; AC4 widened past one exemplar to two positions plus a non-inflation form; AC5 added for the documented enumerations the user-facing tier obliges. The two judgment calls went to the gate as one scope-width question; the draft's AC2 and AC3 were merged after the gate to hold the criteria count under the split tripwire.
- 2026-08-15: plan gate chose the wide scope — one stated criterion gating both surfaces, emergent `solve()`-based refusals relaxed, one shared reason vocabulary — over the narrow scope that adds the criterion and leaves the existing refusals and labels alone, because the narrow version leaves the `+Inf` case still disagreeing and meets the Goal only half. Falsified by evidence that relaxing the emergent refusals lets a genuinely unpriceable matrix through, which would argue those guards were load-bearing.
- 2026-08-15: plan gate chose to decide the cutoff in the build over escalating it to a written Fable review, because the choice is a numerical-conditioning call rather than a new statistical quantity and the build can justify it in code. Falsified by the build finding the cutoff turns on a statistical property of the estimator rather than on conditioning.
- 2026-08-15: T1–T2 red against HEAD as planned — the pairwise reason grid splits from k = 7 at both positions (se "unidentified"/"singular" against sf NULL), and the AC4 opposite-direction form is the near-zero positive diagonal with off-diagonals kept (raw pricing survives, both correlation-metric surfaces refuse); near-collinear pairs fail both surfaces at the same eps in every scan, so they cannot serve as the opposite-direction probe.
- 2026-08-15: T3–T5 done — criterion `axes_sigma_degenerate()` chosen and recorded beside its rationale (see Decisions), applied at both consumers ahead of any pricing; se relabels "nonpositive_diagonal" → "singular" and adopts "infinite_diagonal" for +Inf; M71 block byte-unchanged (diff hunks fall at lines 359, 1228, and 1301 only); full suite clean (7215 pass, 0 fail). Also added `axes_fitted_cov()` as the single seam both consumer call sites read, for T6's constructed-matrix injection.

- 2026-08-15: T6 done — assembly test injects a constructed degenerate matrix at the `axes_fitted_cov()` seam via `local_mocked_bindings()`; both surfaces warn with the shared literal, component SEs and the four D-036 statistics are NA together, df/srmr and the point estimates unaffected.

- 2026-08-15: T7 done — roxygen names the criterion and the shared literal at the scaled-fit details and the return-value enumeration; NEWS bullet added; `devtools::document()` warning-free, regenerating only `man/axes_reliability.Rd` (plus the roxygen2 8.1.0 version stamp in DESCRIPTION, a generated line from the updated local toolchain).

- 2026-08-15: T8 done — `devtools::check(args = "--no-manual")` clean, 0 errors / 0 warnings / 0 notes. All tasks complete; status → review.

- 2026-08-15: /milestone-review — PR #117 opened draft; all seven criteria verified with fresh evidence (AC4's pre-milestone reds re-measured against a scratch `origin/master` checkout); consistency gate green (`cairn_validate` exit 0, no DESIGN principle change so `cairn_impact` skipped, `document()` warning-free and diff-free, check Status OK). Three-lens review returned 21 findings, 1 actioned at >= 80 (O1, 85 — the criterion is not invariant under a pure diagonal rescaling), routed to the graduated ROADMAP row rather than patched: no criterion fails, it is unreachable through the exported API, and the repair re-opens the criterion's design as an `no-oracle` tripwire. 20 below-bar findings logged in the Review section.

- 2026-08-15: merge approval WITHHELD at the review gate — Jeff chose to close the NEWS gap first (finding O5, scored 75, below the action bar and so logged rather than actioned by the review). Logged as T9; status review → in-progress. Not a defect return: no acceptance criterion failed and no finding met the return floor, so this does not count toward the thrash rule.

- 2026-08-15: T9 done — the NEWS vocabulary sentence widened from two literal changes to all four, each verified by measurement against `origin/master` rather than composed: an exactly singular fitted matrix moves `"singular"` → `"ill_conditioned"` and an indefinite one `"indefinite"` → `"ill_conditioned"`, both on both surfaces (measured at λmin = −9.32e−16 and −0.382 on the octant probe). All four are pinned by named tests. `devtools::test()` FAIL 0 / PASS 7231 and `devtools::check(args = "--no-manual")` Status OK. All tasks complete; status → review. Note for the next review pass: the recorded AC5 and AC7 evidence predates this NEWS edit and needs re-gathering.

- 2026-08-15: T10 done — `"unidentified"` is fired as a returned reason on BOTH surfaces by a new test each, with its condition asserted and a passing control that passes for the claim's reason (dropping `fit_zeta1` is the only change). The probe is a degenerate Δ rather than a degenerate Σ̂: a single-scale map makes `zeta1` identical to the all-ones `xi2`, which the criterion cannot see (`axes_sigma_degenerate()` returns NULL on that Σ̂, asserted in both tests). `"indefinite"` has no construction left and the reachability claim at `R/axes_reliability.R` is corrected in place rather than tested: measured, an indefinite Σ̂ (λmin = −0.382) answered `"indefinite"` on both surfaces pre-M89 and answers `"ill_conditioned"` now, and 1500 random PD correlation matrices returned c ∈ [0.94, 1.29] with no refusal. Suite FAIL 0 / PASS 7250 (up 19); check Status OK; `document()` warning-free, no `man/` diff. Status → review.

- 2026-08-15: /milestone-review round 2 — all seven criteria re-verified at 8778ae06, consistency gate green. 23 findings, 2 actioned: RO1 (92) falsified the reachability comment T10 added and is fixed here (with the same wording in both tests and RO5's dead assertion); RS5 (85) is round 1's O1, disposition unchanged. RO2 (70), the mirror-image half of O1, is recorded on the ROADMAP row — verified, helper-boundary only. Not a defect return: no acceptance criterion failed, and RO1 is an internal comment rather than a defect in what the package does for users.

- 2026-08-15: merge approval WITHHELD at the round-2 gate — Jeff chose a Fable review of the criterion's metric choice over merging as it stands, the per-instance escalation approval D-004 requires. Logged as T11; status review → in-progress. Not a defect return: no acceptance criterion failed and nothing met the return floor, so this does not count toward the thrash rule.

## Decisions

- 2026-08-15 — **The stated criterion is a relative smallest-eigenvalue floor on the raw fitted matrix: refuse as `"ill_conditioned"` when λmin(Σ̂) ≤ λmax(Σ̂)·sqrt(p·eps), evaluated after each surface's diagonal guards.** (≈ κ ≥ 1.4e7 at p = 24.) Grounds: both consumers build the information matrix from Σ̂⁻¹ twice, so its entries carry relative error growing like p·κ²·eps, and the floor is exactly where that bound reaches 1. Measured fit: every pre-M89 divergence point sits at or above it (the inflation grid splits at κ = 2.1e7; the emergent near-collinear failures begin at κ = 7.9e8) and every measured accurately-computing point sits below it (κ ≤ 8.6e6 on the probe grids), so the criterion refuses nothing the surfaces were pricing accurately. One inequality also covers indefinite and exactly singular matrices (λmin ≤ 0) — needed: an indefinite Σ̂ (λmin = −0.11) sailed through both surfaces with reason NULL and scale 0.95 before M89. Rejected: any correlation-metric test (cov2cor of the inflated matrix stays at condition 10.45 at every magnitude 10⁰–10¹⁶ — blind, the plan's T3 note); the bare eps^(−1/2) ≈ 6.7e7 cutoff without the dimension factor (leaves the measured k = 7 divergence point, κ = 2.1e7, computing on one surface while emergently refused on the other — the exact disagreement M89 exists to remove). Recorded in code beside `axes_sigma_degenerate()` (R/axes_corrected_se.R).

## Review

### Round 2 — 2026-08-15, at 8778ae06, PR #117

Re-reviewed after T9 and T10 landed. `origin/master` has not moved since the
branch was cut (0 behind, nothing unpushed), so no sync merge; all evidence
below is re-gathered at this HEAD, superseding round 1's.

- **AC1** — `axes_sigma_degenerate()` defined once (`R/axes_corrected_se.R:315`)
  beside its rationale, called at both named consumers' helpers
  (`R/axes_corrected_se.R:259`, `R/axes_scaled_fit.R:149`) for the
  `corrected <- axes_corrected_se(...)` and `scaling <- axes_scaling_factor(...)`
  expressions (`R/axes_reliability.R:1727`, `:1836`). One definition, two sites.
- **AC2** — the grid test passes (78 assertions, 0 failures); pre-milestone the
  same grid diverges at 20 of 68 points. `+Inf` → `"infinite_diagonal"` on both;
  `-Inf` → `"singular"` on both.
- **AC3** — guards precede the criterion in both files
  (`R/axes_scaled_fit.R:139,140,149`; `R/axes_corrected_se.R:244,253,259`);
  master's `test-axes-scaled-fit.R:1258-1300` block is present verbatim at
  HEAD:1262 and passes.
- **AC4** — re-measured against a scratch `origin/master` checkout: the
  inflation form diverges from k = 7 at **both** positions 4 and 20
  (`"unidentified"` against `NULL`); the non-inflation form (`sigma[4,4] <-
  1e-3`) has the raw-priced branch surviving with finite naive SEs while
  `axes_scaling_factor()` refuses — AC4's stated direction — and its
  `"ill_conditioned"` pin fails pre-milestone (both answer `"indefinite"`).
- **AC5** — `"ill_conditioned"` in roxygen (5 sites), `man/axes_reliability.Rd`
  (2), and `NEWS.md` (3). After T9 the NEWS entry names all six literals in
  play: `"nonpositive_diagonal"`, `"singular"`, `"unidentified"`,
  `"infinite_diagonal"`, `"indefinite"`, `"ill_conditioned"`.
- **AC6** — the assembly test passes (16 assertions): both warnings name the
  shared reason, `components$SE` and all four scaled statistics NA together,
  `df`/`srmr` finite and equal to `details$fit_uncorrected`.
- **AC7** — `devtools::test()` **FAIL 0 | WARN 5 | SKIP 3 | PASS 7250**;
  `devtools::check(args = "--no-manual")` **Status: OK** (0/0/0, test phase OK
  at 401s). `document()` warning-free, `man/`/`NAMESPACE` diff-free.

### Consistency gate (round 2)

- `cairn_validate` exit 0, all 16 CHECKs PASS; advisory WARNs are M7 work-log
  lines only.
- No `DESIGN.md` principle changed → `cairn_impact` skipped.
- `r-package` `consistency-gate` slot: `document()` emits 0 `resolve link`
  lines and no diff; `NEWS.md` entry present; no new top-level files or exports.

### Independent fresh-context review (round 2)

Three distinct-evidence reviewers over the updated branch, then a fresh scorer.
23 findings: 10 from the [O] diff-bug lens, 8 from the [S] blame lens, 5 from
the [S] prior-review lens. Round-1 findings re-reported by the lenses were
re-scored on their own merits. Two actioned at >= 80.

**RO1 (92) — the `"indefinite"` reachability comment T10 added was FALSE, and
is corrected.** It claimed "No construction reaching it has been found since",
resting on 1500 random PD matrices at p = 24 only. The criterion admits kappa up
to 1/sqrt(p*eps), which is 3.8e7 at p = 3, and c goes negative in that admitted
band. Verified two ways by this review: deterministically, a saturated model
(p = 3, df = 0) makes `R/axes_scaled_fit.R:217` divide by zero, giving
`cval = Inf` -> `"indefinite"` on a matrix `axes_sigma_degenerate()` returns
NULL for; and on a captured exemplar at kappa = 6.65e6, two orders below the
p = 3 cutoff. **Fixed now**: the comment no longer claims unreachability. It
states both measured routes, names the assembly gates that stop them
(`axes_reliability()` refuses < 4 scales; `axes_design()` drops a collinear
component), and says explicitly that this is a fact about the assembly and not
about the criterion. The same falsified "only" wording in both T10 tests is
corrected with it, and RO5's vacuous `expect_false(any(is.nan(...)))` — dead by
construction after `na_out()` — is removed.

**RS5 (85) — round 1's O1, restated by the blame lens.** Disposition unchanged:
follow-up on the graduated ROADMAP row, not fixed here.

**Below the action bar (21), logged not actioned.** Highest first:

- RO4 (75) / RS3 (74) — T10 fires `"unidentified"` through a single-scale map,
  which `axes_reliability()` refuses upstream, so the literal is fired only at
  the helper contract boundary. Both tests now say so in as many words.
- RO2 (70) — **the mirror of RS5/O1, and the more serious half.** Because the
  criterion prices raw Sigma-hat while both surfaces price `cov2cor(Sigma-hat)`,
  a matrix well conditioned raw and degenerate in the correlation metric passes
  the door and fails later: at p = 3 over ~8,200 criterion-accepted draws, 36
  returned finite corrected SEs with `se_correction_failed = NULL` beside
  `fit_scaling_failed = "indefinite"` — this milestone's Goal failure mode with
  the roles swapped. Reproduced by this review at kappa = 6.65e6. Scored 70
  because `axes_reliability()` refuses fewer than 4 scales and the reviewer
  found 0 disagreements at p = 4, 5, 6, 8, so no user path reaches it. Recorded
  on the ROADMAP row beside O1: together the two say the raw-vs-correlation
  metric choice is the open question, not the cutoff value.
- RO3 (68) — the T10 tests' "only a degenerate Delta" prose was the same
  overstatement that produced RO1; corrected with it.
- RP1 (66) — the ROADMAP row's round-1 note went stale when T9 and T10 closed
  two of the three findings it listed; corrected in place and marked.
- RO7 (65) / RS8 (62) — the load-bearing `p` factor is exercised at p = 24 only;
  sharpened by round 2, whose counterexamples sit at p = 3 where that factor
  makes the cutoff loosest. Carried on the ROADMAP row.
- RP5 (62) — the sibling `se_correction_failed` comment still carries a
  "never fired" caveat that round 2 shows was already stale on master.
- RS1 (55) / RP3 (55) — M70's declined reason-code parity overturned with no
  recorded supersession.
- RS2 (55) / RP4 (55) — M71's declined `+Inf` label, likewise.
- RO5 (52) — vacuous NaN assertion in the new SE test; removed with RO1's fix.
- RS6 (50) — T4's "relax the emergent refusals" never happened; they are
  shadowed, and round 2 shows they remain reachable.
- RS7 (48) — the NA/NaN warning-count collapse is still absent from NEWS.
- RO8 (40) — all four NEWS literal claims verified accurate; two wording gaps.
- RO9 (38) — `p` used before it is defined in the generated Rd prose.
- RS4 (35) — the rewritten `na.rm` comment drops M70's attribution.
- RO6 (34) — `eigen(symmetric = TRUE)` folds to the symmetric part.
- RO10 (20) — the `Config/roxygen2/version` stamp.
- RP2 (5) — explicitly not a finding (records that no regression was found).

**Re-verified after the RO1 fix:** `devtools::test()` FAIL 0 | PASS 7249;
`devtools::check(args = "--no-manual")` Status OK; `document()` 0 `resolve link`
lines, no `man/` diff.

### Round 1 — 2026-08-15, at 416cb655, PR #117.
Branch 5 commits ahead of `origin/master`, 0 behind — no sync merge needed.

### Acceptance-criterion evidence

- **AC1** — `axes_sigma_degenerate()` is defined once, at
  `R/axes_corrected_se.R:315`, beside the rationale block at `:274-314`
  (criterion, cutoff derivation, why it prices the raw matrix, return
  vocabulary). It is called at both named consumers' helpers ahead of any
  pricing: `R/axes_corrected_se.R:259` for the `corrected <-
  axes_corrected_se(...)` expression (`R/axes_reliability.R:1727`) and
  `R/axes_scaled_fit.R:149` for the `scaling <- axes_scaling_factor(...)`
  expression (`R/axes_reliability.R:1836`). One definition, two call sites,
  no second criterion anywhere in `R/`.
- **AC2** — the AC2 grid test (`tests/testthat/test-axes-scaled-fit.R:1330`)
  runs both diagonal positions × k = 0..16 × both forms and compares the two
  `reason` fields pairwise; it passes on HEAD with 0 failures. The same grid
  measured against pre-milestone code diverges at 20 of its 68 points. `+Inf`
  now returns `"infinite_diagonal"` on both surfaces (pre-milestone:
  `"unidentified"` / `"infinite_diagonal"`); `-Inf` returns `"singular"` on
  both (pre-milestone: `"nonpositive_diagonal"` / `"singular"`).
- **AC3** — ordering measured by grep: in both files the `<= 0` and
  `is.infinite()` diagonal guards precede the criterion
  (`R/axes_scaled_fit.R:139,140,149`; `R/axes_corrected_se.R:244,253,259`).
  The M71 AC1/AC2 block at master's `test-axes-scaled-fit.R:1258-1300` is
  present byte-for-byte in HEAD's file (verified by exact substring match; it
  sits at HEAD:1262 after an upstream hunk's +4 shift) and passes.
- **AC4** — the probes were run against a scratch checkout of `origin/master`
  with only the new test files copied in. Inflation form: the pre-milestone
  surfaces diverge from k = 7 at **both** diagonal positions 4 and 20
  (`se = "unidentified"` against `sf = NULL`), so the AC2 pins fail
  pre-milestone at two distinct positions. Non-inflation form
  (`sigma[4,4] <- 1e-3`, off-diagonals kept): measured pre-milestone, the
  raw-priced branch **survives** — `axes_se_pricing()` returns finite naive
  SEs, not a failure string — while the correlation-metric surface refuses,
  the opposite direction from the inflation form; the AC4 test's
  `"ill_conditioned"` pin fails pre-milestone (both surfaces answer
  `"indefinite"`). Whole-file run against pre-milestone code: red, capped at
  testthat's 10-failure limit with 23 more reported.
- **AC5** — `"ill_conditioned"` appears in the roxygen at
  `R/axes_reliability.R` (4 sites: the scaled-fit details block, the
  `se_correction_failed` and `fit_scaling_failed` return enumerations, and
  the internal enumeration comment), in the regenerated
  `man/axes_reliability.Rd` (2 sites), and in `NEWS.md`, whose bullet names
  the literal, the NA condition, and both relabelings
  (`"nonpositive_diagonal"` → `"singular"`, `"unidentified"` →
  `"infinite_diagonal"`).
- **AC6** — the assembly test (`tests/testthat/test-axes-reliability.R:3083`)
  injects a constructed degenerate matrix at the `axes_fitted_cov()` seam and
  passes: both warnings name `"ill_conditioned"`, `se_correction_failed` and
  `fit_scaling_failed` both carry it, `components$SE` and all four of
  `chisq`/`pvalue`/`rmsea`/`cfi` are NA, and `df`/`srmr` match
  `details$fit_uncorrected` and stay finite.

- **AC7** — `devtools::test()`: **FAIL 0 | WARN 5 | SKIP 3 | PASS 7231** (the
  5 warnings are in `test-ci_accuracy.R` and `test-ssm_sem.R`, files this
  branch does not touch). `devtools::check(args = "--no-manual")`:
  **Status: OK** — 0 errors, 0 warnings, 0 notes, 8m45s, test phase OK.
  `devtools::document()` warning-free and `man/`/`NAMESPACE` diff-free beyond
  the intended `man/axes_reliability.Rd` regeneration.

### Consistency gate

- `cairn_validate` exit 0 — all 16 CHECKs PASS. 47 advisory WARNs, every one a
  `work-log format` line in **M7**'s hard-wrapped log; none in M89.
- No `DESIGN.md` principle changed on this branch → `cairn_impact` skipped.
- Toolchain (`r-package` profile `consistency-gate` slot):
  `options(cli.width = 500); devtools::document()` emits **0** lines matching
  `resolve link` and leaves `man/` and `NAMESPACE` diff-free. `NEWS.md`
  carries the user-visible entry. No new top-level files, no new exports.

### Independent fresh-context review

Three distinct-evidence reviewers, then a fresh scorer that generated none of
the findings. 21 findings reported: 14 from the [O] diff-bug lens, 7 from the
[S] blame-history lens, 0 from the [S] prior-review lens — whose GitHub
inline-comment probe returned empty, so it read the archived `## Review`
sections on the touched files and found no point this diff reintroduces or
contradicts.

**Actioned (≥80): one.**

- **O1 (85) — `axes_scaling_factor()` refuses matrices it can price exactly;
  the criterion measures a property that surface never uses.** The rationale
  recorded at `R/axes_corrected_se.R:274-314` says both consumers build the
  information matrix from Σ̂⁻¹, so their error grows like p·κ(Σ̂)²·eps. That is
  false for `axes_scaling_factor()`: it runs `cov2cor()` first and every
  quantity it computes is a function of `cov2cor(Σ̂)` alone, so its error is
  governed by κ(cov2cor(Σ̂)). Reproduced independently at this review on HEAD:
  Σ̂ = D S D with D = diag(1e4, 1, …, 1) leaves `cov2cor(Σ̂)` identical to the
  well-conditioned S (κ = 10.4) while κ(raw) = 2.13e8 — pre-M89 the surface
  returned `scale = 0.9563346` (correct), HEAD returns `NA` /
  `"ill_conditioned"`. A pure diagonal rescaling, which the estimand is exactly
  invariant under, is now refused.
  **Triaged: follow-up, not fixed here.** Not an acceptance-criterion failure —
  AC1 asks that a single criterion be recorded and applied, AC2's grid
  agreement holds, and the Goal is met. It is also unreachable through the
  exported API today: every `axes_reliability()` path fits a correlation
  matrix, so `axes_fitted_cov(fit)` is near-unit-diagonal (κ measured 3.6–21.4
  on the probe fits, six orders below the 1.4e7 floor). And the repair is not a
  patch: pricing `cov2cor(Σ̂)` instead is exactly what the plan's T3 note ruled
  out as blind to the inflation regime M89 exists to close, so choosing a
  scale-invariant degeneracy measure re-opens the criterion's design — an
  `no-oracle` RB-tripwire question, not review-side work. Routed to the
  ROADMAP row this milestone graduated.

**Below the action bar (20), logged not actioned.** Highest first:

- O5 (75) — NEWS names two of the four user-visible literal changes; the
  exactly-singular (`"singular"` → `"ill_conditioned"`) and indefinite
  (`"indefinite"` → `"ill_conditioned"`) relabelings are unmentioned.
- O3 (65) — after the criterion runs first, no test asserts `"unidentified"`
  or `"indefinite"` as a returned reason from either surface; the two that did
  were flipped to `"ill_conditioned"`.
- O7 (60) — the load-bearing `p` factor in the cutoff is exercised only at
  p = 24; `probe_six()` and `probe_single()` go unused, so dropping `p` or
  writing `p^2` passes the whole M89 suite.
- S1 (55) — M70's declined reason-code parity is overturned with no recorded
  supersession link.
- S2 (55) — M71's declined `+Inf` relabel likewise.
- S7 (55) — the cutoff is analytically derived and self-validated, with no
  DECISIONS.md entry and no independent oracle.
- O6 (50) / S3 (50) — T4's "relax the emergent `solve()`-based refusals" never
  happened; `axes_se_pricing()` is byte-identical to master and the guards are
  merely made unreachable.
- O11 (50) — the NA/NaN diagonal now emits one warning instead of two; a
  user-visible console change absent from NEWS.
- O8 (45) — 64 of the AC2 grid's 68 points are non-discriminating.
- O10 (45) — the `df_mismatch` guard runs before the criterion on one surface
  only; pre-existing and unreachable through `axes_reliability()`.
- O4 (40) — `"ill_conditioned"` conflates indefinite, singular and ill-scaled;
  the milestone's Decision calls that one inequality carrying three cases.
- S5 (35) — a rewritten comment drops M70's `na.rm` fix attribution.
- O9 (35) — `eigen(symmetric = TRUE)` silently reads only the lower triangle.
- O14 (25) — `eigen()` runs twice per call on the same matrix.
- O12 (25) — the `Config/roxygen2/version` bump is a local-toolchain artifact.
- S4 (25) — the reviewer's own conclusion is that M69's contract is preserved.
- O2 (25) — claims AC4 unmet; AC4's own gloss ("`axes_scaling_factor()`
  refusing while the raw-priced branch survives") is satisfied as measured.
- S6 (20) — an M69 attribution comment now points at a moved route.
- O13 (15) — a cosmetic line-wrap artifact in the generated Rd.
