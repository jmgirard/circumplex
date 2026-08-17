# M90: Say which degeneracy happened, and stop saying it when it didn't

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M89
- **Driving RR:** RR18
- **Principles touched:** GP2, GP4
- **Branch/PR:** m90-degeneracy-reason-vocabulary · https://github.com/jmgirard/circumplex/pull/118

## Goal

Split the fitted-matrix refusal vocabulary so an indefinite model-implied
matrix -- a statement about the user's model -- stops sharing one word with
mere ill-conditioning, give a saturated model its own refusal instead of
letting it reach `cval = Inf` and report `"indefinite"`, and stop the
`cval <= 0` branch from claiming indefiniteness it can never actually
diagnose.

## Scope

Surface tier: **user-facing** -- these are the literals `axes_reliability()`
prints and documents in `details$se_correction_failed` and
`details$fit_scaling_failed`. One reachability nuance recorded rather than
implied: AC1's `"saturated"` is helper-boundary-only today (df = 0 needs
p = 3 with ζ1 fitted; `axes_reliability()` refuses fewer than four scales),
like the documented siblings at `R/axes_reliability.R:1951`.

**In:** the `df == 0` guard and its `"saturated"` literal; the
`"indefinite"`/`"ill_conditioned"` partition of the eigenvalue refusal; the
`cval <= 0` relabel; the cross-surface literal-precedence contract (cov2cor
arm evaluated first, with the raw-matrix finiteness hoist that requires);
and the documentation every user-visible literal owes, enumerated by AC4's
grep procedure.

**Out:** the metric move and the tau floor -> **M89**, this milestone's
dependency, where RR18's BC1/BC2/BC3/BC7/BC8 are met. RR18 rec 7's
decoupling of `naive` -> **M91**, split out at the 2026-08-16 replan gate
(size tripwire); D-044 routes rec 7 onward and M91 carries its lineage. The
a-posteriori cancellation guard on `cval` (RR18 rec 6, "consider") ->
candidate row, since M89's tightened floor closes every measured case and
RR18 asks for its constant to be calibrated against the oracle first.

## Acceptance criteria

- [x] **AC1 (BC4, narrowed)** — `axes_scaling_factor()` refuses `df == 0` with the literal
      `"saturated"`, after the two df-consistency guards and before any matrix computation; the
      deterministic p = 3 construction (S = {1,.5,.3;.5,1,.4;.3,.4,1}, scales A/A/B, `fit_zeta1 =
      TRUE`, df = 0) returns `"saturated"`; the guard's position upstream of the `cval` division —
      asserted by code order and the construction's literal, not a claim over all paths — makes
      `df` nonzero wherever `cval` is computed.
- [x] **AC2 (BC5, anchors restated)** — Within the refusal region the criterion returns
      `"indefinite"` iff λmin < −λmax·sqrt(p·ε), else `"ill_conditioned"`; the comment beside the
      partition states its rationale as a convergence-noise band (fitted-matrix entries carry
      optimizer error ~sqrt(ε), so eigenvalues within ~λmax·sqrt(p·ε) of zero are not confident
      statements about the user's model) — if implementation cannot defend it, escalate rather than
      silently change the constant (RB tripwire: no-oracle). Anchors by construction and metric:
      the M89 nestedness-grid indefinite probe (`dd %*% sigma %*% dd`, cov2cor, p = 24, λmin =
      −0.5) returns `"indefinite"` on both surfaces; the near-singular probe (cov2cor, p = 24, λmin
      = −9.32e-16) returns `"ill_conditioned"` on both.
- [x] **AC3 (BC6)** — The `cval ≤ 0` (or non-finite) refusal at the end of `axes_scaling_factor()`
      no longer returns `"indefinite"`; it returns `"ill_conditioned"`, with the tr(UΓ) ≥ 0
      rationale recorded in a comment beside it.
- [x] **AC4 (procedure-based)** — A repo-wide grep for each pre-M90 refusal literal
      (`"indefinite"`, `"ill_conditioned"`, `"saturated"`) over `R/`, `man/`, `NEWS.md`,
      `vignettes/` enumerates the doc surfaces; every hit either already describes the post-M90
      vocabulary or is updated here — including the inline comments at
      `R/axes_reliability.R:1897-1950`, whose df = 0 claim (`"unidentified"`) the replan audit
      falsified (measured: `"indefinite"`), the roxygen enumerations, `man/axes_reliability.Rd`,
      and NEWS. Documented: the three printed-output changes — AC1's `"saturated"`, AC2's
      partition, AC3's relabel (GP4).
- [x] **AC5 (amended at review round 1)** — The `cval ≤ 0` branch's four-arm predicate is
      dispositioned arm by arm on recorded evidence (recorded in this file's work log). The two
      `cb` arms are settled by argument in the comment (cb = Σ(1−ρ²)²/baseline_df ≥ 0, equality
      only at |ρ| = 1, refused upstream; baseline_df > 0 wherever the line runs). For the two
      `cval` arms: either a test reaches the branch on an unmocked input — AC1's guard and the
      degeneracy criterion both live — and asserts `"ill_conditioned"`, or the recorded search —
      near-floor draws plus adversarial near-cancellation constructions targeting tr_vg −
      sum(acov·bmat) ≈ 0, ≥1e4 draws per p ∈ {3, 8, 24} — finds no reaching input, the branch is
      marked a defensive backstop (not "unreachable"), and three tests pin the disposition: a
      seeded smoke tier re-runs the family with the smallest cval that search recorded (p = 3 / df
      = 1, min +1.2e-5), asserting every accepted draw computes with cval > 0; the upstream
      criterion — not the backstop — is asserted to refuse the one matrix on record (RR18 exemplar
      B, `cairn/reviews/rb18-counterexample-b.rds`) measured to compute cval < 0 in doubles, pinned
      at the criterion itself; and a mocked-criterion test asserts the backstop's own literal is
      `"ill_conditioned"` (the mock is the upstream criterion, never the branch's condition; the
      test claims only the emitted literal, never that any unmocked input reaches the branch —
      M62). AC3's comment covers all four arms.
- [x] **AC6 (probe family widened)** — At p = 24 and at least one of p ∈ {8, 12}, tests construct
      matrices with λmin just inside and just outside −λmax·sqrt(p·ε) (factors ≈ 0.5 and ≈ 2,
      computed per p), at two λmax scales (≈ 1, ≈ 1e3) and two construction forms (rank-one
      negative perturbation; eigen-recomposition Q diag(λ) Qᵀ), asserting the literal flips across
      the boundary in every cell; the drop-p, squared-p, and drop-λmax partition mutants each
      verifiably redden. p = 3 excluded deliberately: there the ×2 factor no longer separates the
      squared-p mutant.
- [x] **AC7 (reworded at the replan)** — Whenever both of `axes_corrected_se()`'s arms refuse, the
      reported literal is the one its cov2cor arm produces — requiring the arm order at
      `R/axes_corrected_se.R:264-268` inverted (cov2cor first) and the raw-matrix finiteness check
      `!all(is.finite(sigma))` hoisted ahead of both arms (mirroring
      `R/axes_scaled_fit.R:149-154`), so `cov2cor()` never runs on an NA/NaN diagonal and M71's
      one-warning-per-refusal contract holds — and M89's nestedness grid is re-run with expected
      literals updated to the new vocabulary, asserting the nesting relation pointwise: every cell
      where the scaling surface refuses, the SE helper refuses with the same literal.

### Deviations from RR18

| BC/rec | Departure | Why |
|---|---|---|
| BC1, BC3, BC7, BC8 | Met in M89 | The metric move, τ floor, oracle, and metric-contract docs are all M89's; this milestone depends on it. |
| BC2 | Met in M89 | AC2 here changes the literal BC2 asserts agreement on, which is why it sequences after. Its retained cost (unit refusal) is M91's. |
| BC4 | "no path reaches `cval = Inf`" narrowed to a guard-order claim | Bounded-promise rule: no named procedure enumerates all paths; the guard's position plus the construction's literal is what a procedure settles. |
| BC5 | Anchor restated by construction and metric (λmin = −0.5, cov2cor, p = 24); partition constant kept, rationale demanded in-code | The brief's bare −0.382 named neither construction nor metric; it is the d44 probe's raw-metric λmin (measured at review round 1 — the replan audit's "matches no committed probe" was false, corrected here). The constant's original rationale (the refusal floor) was superseded by M89's τ floor. |
| BC6 | Met as AC3 | — |
| rec 6 | Deferred to the τ-calibration candidate row | "Consider"-level; M89's floor closes every measured case; RR18 asks for oracle calibration first. |
| rec 7 | Split to M91 (planned 2026-08-16, depends on M90) | Size tripwire at the replan gate; D-044 routes it onward. |

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T6
- AC4 → T7
- AC5 → T6
- AC6 → T4
- AC7 → T5

## Tasks

- [x] **T1** — Test-first: the deterministic saturated construction, red
      against M89's merged code (measured at the replan audit: it returns
      `"indefinite"` today).
- [x] **T2** — The `df == 0` guard and its `"saturated"` literal.
- [x] **T3** — Test-first: the indefinite/near-singular partition at the
      nestedness-grid probes, red where the two currently share one literal.
- [x] **T4** — The partition in `axes_sigma_degenerate()`, plus the AC6
      battery: two p values, two λmax scales, two construction forms, three
      partition mutants verified to redden.
- [x] **T5** — The cov2cor-arm-first inversion and finiteness hoist in
      `axes_corrected_se()`; M89's nestedness grid re-expected under the new
      vocabulary and re-run (AC7).
- [x] **T6** — The `cval ≤ 0` relabel, the four-arm comment, and AC5's
      arm-by-arm record (`cb` by argument; `cval` by reaching test or
      recorded search).
- [x] **T7** — AC4's grep sweep: roxygen enumerations, the
      `R/axes_reliability.R:1897-1950` comment block (falsified df = 0
      claim), `devtools::document()`, NEWS.
- [x] **T8** — Full `devtools::check()`.

## Work log

- 2026-08-16: created by /milestone-plan, in the same run as M89's re-cut. Takes RR18's BC4-BC6, which M89's Deviations table defers here, plus three ingest-audit findings that RR18's own criteria left uncovered.
- 2026-08-16: replanned under the current rulebook (/milestone-plan). Full-mode criteria audit ([O] fresh-context reader, two passes): round 1 returned 1 blocker (AC7's "grid still passes" unsatisfiable — the grid pins the literals AC2/AC8 change) plus 10 findings; round 2 on the final wordings returned clean except the AC7 finiteness hoist, added. AC1/AC4/AC5 narrowed to named procedures, AC2 anchors restated by construction+metric, AC6 gains the λmax/form axes and the drop-λmax mutant, the arm-order task added; AC8 split to M91 at the gate.
- 2026-08-16: alternative rejected at the gate — keeping AC8 (naive decoupling) in M90; lost to the size tripwire (8 criteria, ~12 tasks after repairs); falsified if the split forces `axes_corrected_se()`'s return shape to be reopened twice across the M90/M91 seam.
- 2026-08-16: alternative rejected at the gate — tightening the AC2 partition to the eigensolver-noise band (~p·ε); the reviewed BC5 constant kept with a demanded convergence-noise rationale; falsified by implementation finding no defensible rationale, in which case escalate via RB (no-oracle) rather than silently change the constant.

- 2026-08-16: T1+T2 — red premise re-measured on the branch (construction returns "indefinite" via cval = Inf pre-guard; q = 6, df = 0 verified); guard added after the df-consistency guards with a df_mismatch-ordering control; suite 7423 pass / 0 fail.

- 2026-08-16: T3+T4 — anchors+battery written first, red on exactly the 10 "indefinite" expectations; partition landed with the convergence-noise rationale comment (one-directional claim, so no escalation needed); grid dd-probe, check_nested set, and d44 cells re-expected ("indefinite" measured λmin −0.56 raw / −48 cor); suite 7444/0. Mutant verification follows this commit.

- 2026-08-16: T4 mutants — drop-p 8 fails, squared-p 8 fails, drop-λmax 5 fails (the λmax-scale cells), each applied/restored with blob-hash verification; first perl attempt silently no-opped (interpolated `$double` in \Q…\E), caught by the hash check before any green was trusted.
- 2026-08-16: T5 — arm-disagreement probe measured red first (cov2cor arm "indefinite", raw arm "ill_conditioned", reported = raw's); finiteness hoisted, cov2cor arm now consulted first, AC7 pin test asserts both surfaces say "indefinite" on the split probe; suite 7448/0.

- 2026-08-16: T6 (AC5 search record) — families: Wishart correlations at 3 concentrations + spectrum-surgery draws pinned 1.05–3× above the refusal floor + 1000-step adversarial hill-climbs from the minimum-cval draw; 10,000 accepted draws per p ∈ {3, 8, 24} (30,000 total; script `scratchpad/ac5-search.R`, seed 20260816). Branch reached 0 times; min cval by map: +1.2e-5 (p=3, df=1), +0.813 (p=8), +0.956 (p=24). Disposition: defensive backstop, relabeled "ill_conditioned"; cb arms settled analytically; seeded smoke tier + exemplar-B upstream-guard identity in test. Suite 8250/0.

- 2026-08-16: T7 (AC4 grep disposition) — repo-wide grep over R/, man/, NEWS.md, vignettes/: vignettes 0 hits; updated the two roxygen passages (refusal criterion, se_correction_failed), both inline reason-list comments (falsified df=0 "unidentified" claim corrected to the measured "indefinite"-via-cval=Inf history; enumeration now 8 literals incl. "saturated"), NEWS M89 bullet's now-false indefinite→ill_conditioned sentence fixed in place, M90 bullet added; document() warning-free, no DESCRIPTION drift; suite 8250/0.

- 2026-08-16: T8 — devtools::check() 0 errors / 0 warnings / 0 notes (9m24s). PDF manual unbuildable on this machine (no TeX; environment fact, M82-family); the class it screens for is ruled out directly — 0 non-ASCII bytes in every line the branch adds to R/ and man/. All tasks done; status → review.

- 2026-08-16: review round 1, defect return #1 — AC4 failed inside its grep domain (R/axes_corrected_se.R:293-296 still describes the single-literal criterion; blame lens). 18 findings triaged: 13 fix-now on this return, 1 routed to M91, 2 rejected with reason, 1 dissolved on measurement, plus the AC5 amendment on its own track. Status → in-progress.
- 2026-08-16: correction — the T3+T4 work-log line's measured numbers were false: the d44 probe measures λmin −0.3819 raw / −38.25 cov2cor and the dd grid probe −0.1407 raw / −0.5000 cov2cor (the −0.56/−48 came from an ad-hoc reproduction with wrong population parameters; found by the diff lens, re-measured with the test file's own probe).

- 2026-08-16: amendment return: AC5 — "either a test reaches the branch on an unmocked input — AC1's guard and the degeneracy criterion both live — and asserts \"ill_conditioned\", or the recorded search … finds no reaching input, the branch is marked a defensive backstop (not \"unreachable\"), and three tests pin the disposition: a seeded smoke tier re-runs the family with the smallest cval that search recorded (p = 3 / df = 1, min +1.2e-5), asserting every accepted draw computes with cval > 0; the upstream criterion — not the backstop — is asserted to refuse the one matrix on record (RR18 exemplar B) measured to compute cval < 0 in doubles, pinned at the criterion itself; and a mocked-criterion test asserts the backstop's own literal is \"ill_conditioned\" (the mock is the upstream criterion, never the branch's condition — M62)". Amended text audited by a fresh [O] reader (PASS with 4 wording repairs, all applied: disjunct made disjoint, both superlatives replaced by citations, mock kind corrected); user approved the amendment at the round-1 gate.

- 2026-08-16: review round 2 — delta reviewer's 7 findings all fixed same-round (search-script detector, false p=1 argument, NEWS/roxygen qualifiers, comment homonymy, test-block split, fresh check); all 7 AC boxes ticked against recorded evidence; suite 8257/0 on the final tree.

## Decisions

## Review

### Round 1 (2026-08-16, PR #118)

Evidence per criterion, fresh this session on the branch head:

- **AC1** ✓ — suite green (fresh run, 0 fail) incl. the M90 AC1 test (6
  assertions): the deterministic construction returns `"saturated"`; the
  df_mismatch-ordering control passes; guard position verified in the diff
  (after both df-consistency guards, before any matrix computation).
- **AC2** ✓ — anchors test green; re-measured with the test file's own
  probe: dd grid probe cov2cor λmin −0.5000 → `"indefinite"` on both
  surfaces (grid cells); exactly-singular probe λmin −9.322e-16, and the
  probe is unit-diagonal so `cov2cor()` of it is identical — the criterion
  literally evaluates the cov2cor metric, `"ill_conditioned"` on both
  surfaces (grid + AC1-refusal tests). Rationale comment present beside the
  partition; one-directional, no escalation.
- **AC3** — relabel and four-arm comment verified in source; tick HELD for
  round 2: no test pinned the branch's literal (finding F1), so the NEWS
  claim rests on source-reading alone until the wiring test lands.
- **AC4** ✗ FAILED in-domain — re-running the grep enumerates
  `R/axes_corrected_se.R:293-296` ("THE CRITERION" block) still describing
  the pre-M90 single-literal semantics (blame-lens finding). Defect return #1.
- **AC5** ✗ criterion wrong as written — its "a test asserts the upstream
  guard fires on the search's nearest miss" clause is unsatisfiable against
  the recorded search's outcome: the nearest miss (cval +1.2e-5) COMPUTES,
  so no guard fires on it. Amendment return (see work log).
- **AC6** ✓ — battery green (16 cells); three mutants verified red this
  session with blob-hash-verified apply/restore (work log).
- **AC7** ✓ — split-probe test green (arms measured disagreeing; reported
  literal = cov2cor arm's = scaling surface's); nestedness grid re-run green.

Driving RR (projection vs outcome): RR18 BC5's indefinite-probe anchor
projected λmin = −0.382; measured −0.3819 on the committed d44 probe's raw
metric (the replan's "matches no committed probe" was false — see triage
F3) and −0.5000 on the grid dd probe the criterion text now names.
Near-singular: projected −9.32e-16, measured −9.322e-16. BC4/BC6 carry no
further numerics; the deterministic construction behaves as projected.

Consistency gate: `cairn_validate` PASS (all checks); no principle change
(impact skipped); `document()` warning-free, no diff, no DESCRIPTION drift;
NEWS entry present; no new top-level files; full `devtools::check()`
0e/0w/0n (T8, this session, identical code tree); PDF manual unbuildable
locally (no TeX — environment fact), branch R/man additions verified pure
ASCII. pkgdown check deferred to round 2 with the doc fixes.

Findings (3 lenses: [O] diff-bug 17, [S] blame-history 1, [S] prior-review 0;
ranked by the reviewers). Dispositions:

- FIX on this return: F1 (cval-relabel literal untested — add
  mocked-criterion wiring test), F2 (test comment + work-log measured
  numbers false — corrected, see work-log correction line), F3 (Deviations
  BC5 "Why" cell false — −0.382 IS the d44 probe's raw λmin; cell
  corrected, minor amendment), F4 (roxygen places `"saturated"` inside the
  NA-together paragraph, implying SEs also NA — reworded), F5 (comment
  overclaims "df > 0"; guard establishes df ≠ 0 — reworded), F7 (no
  one-warning-count test on the SE surface's NA-diagonal route — added),
  F8 (AC5 search script cited from untracked scratchpad — committed to
  devel/m90-ac5-search/), F9 (NEWS presents helper-boundary literals as
  user-reachable — reachability qualifier added), F10 (partition threshold
  never stated numerically user-facing — formula added to both roxygen
  passages), F12 (cb comment silently assumes baseline_df > 0 — p = 1
  clause added), F13 ("hoist above this comment" — it is below; fixed),
  F14 (NEWS "four reported literals change" now miscounts — count dropped),
  BH1 = the AC4 failure above.
- ROUTED: F11 (noise-band rationale derived in the correlation metric but
  applied to the raw arm too; not empirically reachable) → M91 work-log
  note, whose decoupling reopens that surface.
- REJECTED: F16 (equality-case probe at the partition boundary —
  fp-unstable to construct portably; measure-zero point with no statistical
  content), F17 (double `cov2cor()` on the success path — perf nitpick,
  negligible beside the eigendecomposition).
- DISSOLVED on measurement: F15 (AC2's near-singular anchor "mislabeled
  cov2cor" — the probe is unit-diagonal, `cov2cor()` of it is identical,
  label accurate; clarifying assertion added to the test).

### Round 2 (2026-08-16)

The round-1 fixes plus the audited AC5 amendment landed (commit f832c982
and the round-2 delta). A fresh [O] reviewer over the fix delta returned 7
findings, all fixed in the delta's follow-up: (R1) the committed search
script's reach detector still read the pre-relabel literal, so a re-run
could never fire — detector corrected to the post-relabel identity
("ill_conditioned" on an accepted draw = the backstop, since the criterion
already returned NULL); (R2) the cb-arm p = 1 argument was false (q = 3,
df = −2 passes the df guards; what actually refuses p = 1 is the singular
information matrix, "unidentified", measured) — comment corrected; (R3)
NEWS qualifier said "two literals" where the claim is about two CHANGES,
and implied unreachability — reworded; (R4) the roxygen `"saturated"`
guarantee lacked the boundary caveat NEWS carries — added; (R5) THE
CRITERION comment's case names collided with the literals — reworded to
"decided by depth, not by case"; (R6) AC5 says three tests, delta had two
blocks — smoke tier and exemplar-B pin split into their own `test_that`
blocks; (R7) check evidence predated the delta — full `devtools::check()`
re-run on the final tree (below). The reviewer verified clean: the wiring
test genuinely lands on the cval arm (cval = −0.2160593 measured under the
mock), is site-exclusive and mutation-sensitive; the one-warning test
reddens on hoist-revert; the corrected numbers (−0.3819 raw / −38.25
cov2cor) reproduce; roxygen/Rd threshold text matches the code verbatim.

Evidence closing the held criteria:
- **AC3** ✓ — relabel + four-arm comment in source; the wiring test pins
  the branch's literal, and the relabel-revert mutant reddens exactly that
  test (hash-verified apply/restore).
- **AC4** ✓ — the missed THE CRITERION block updated; grep procedure
  re-run over `R/`, `NEWS.md`, `vignettes/`: zero surfaces describing the
  pre-M90 semantics remain (`man/` regenerates from the roxygen).
- **AC5 (amended)** ✓ — all three named tests exist as their own blocks
  and pass; the search record, corrected script (devel/m90-ac5-search/),
  backstop marking, and cb-arm argument (corrected per R2) are in place.

Final tree: suite 8257 pass / 0 fail; `document()` 0 link warnings, Rd in
sync, no DESCRIPTION drift; `pkgdown::check_pkgdown()` no problems;
`cairn_validate` all checks pass; full `devtools::check()` on this tree —
see the gate presentation.
