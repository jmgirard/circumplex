# M110: Correct the calibration-domain claim in the accuracy target and its shipped surfaces

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m110-calibration-domain-claim` — https://github.com/jmgirard/circumplex/pull/140

## Goal

State the accuracy target's calibration domain at the worst measured geometry
as well as at the anchor, so no shipped surface implies the domain reaches
1.7 decades past the published sample-size ceiling at every geometry.

## Scope

Surface tier: **user-facing** — it corrects text in `man/axes_reliability.Rd`
and in `NEWS.md`, both of which ship.

The defect, recorded at M106's fourth review round and descoped with AC1: the
calibration-domain paragraph closes on the anchor's endpoint (`n = 5e5`,
"1.7 decades past the n ~ 1e4 ceiling") one sentence after putting the worst
measured geometry's endpoint at `n = 2.0e3` — a decade *below* that ceiling.
Three shipped surfaces repeat the anchor figure; one of them qualifies it as
typical, two do not.

**In:**
- The paragraph's closing sentence, `R/axes_corrected_se.R:484-485`.
- The two roxygen sites `R/axes_reliability.R:731` and `:1045`, and the
  regenerated `man/axes_reliability.Rd`.
- `NEWS.md:79`, which spells the same figure `500,000`.
- A test pinning the corrected help-page sentences at the installed Rd.

**Out:**
- Certifying the derivation block's prose in general — M106 descoped it after
  three review rounds and it stays descoped; this repairs one named defect.
- Moving the constants → refused by D-048 and D-049.
- The `WHY THE LIMB EXISTS AT ALL` section's stale claims → M111.

## Acceptance criteria

- [x] AC1: The calibration-domain paragraph closes by stating the domain
      endpoint at the worst measured coefficient (a = 0.045, tenth-margin
      n = 2.0e3) alongside the anchor's (a = 1/sqrt(2), n = 5.0e5), and says
      which of the two lies below the n ~ 1e4 ceiling of published circumplex
      correlation matrices. The source's own spelling of the coefficient
      (0.045) is used; the archived review spells it 0.046 and is not edited.
- [x] AC2: Every line that `grep -rnE '5e5|5\.0e5|500,000' R/ man/ NEWS.md`
      returns either names the coefficient its figure belongs to, or does not
      state that figure as the calibration domain.
- [x] AC3: `Rscript -e 'options(cli.width = 500); devtools::document()'`
      produces no diff and emits no line matching `resolve link`;
      `Rscript -e 'devtools::test()'` and
      `Rscript -e 'devtools::check(args = "--no-manual")'` clean.

## Coverage

- AC1 → T1
- AC2 → T1, T2, T3
- AC3 → T4

## Tasks

- [x] T1: Rewrite the closing sentence at `R/axes_corrected_se.R:484-485`. The
      arithmetic is already in the block at `:469-473`: the tenth-margin is
      `n = 1e6 * a^2`, so a = 0.045 gives 2.0e3 and a = 1/sqrt(2) gives 5.0e5.
- [x] T2: Correct the two roxygen sites — `R/axes_reliability.R:1045`
      ("calibrated for `n` up to about `5e5`", unqualified) and `:731`
      ("for a typical design at `n` up to about `5e5`", which names the typical
      case but never says the domain ends below the published ceiling at the
      worst measured geometry). Regenerate the Rd.
- [x] T3: Correct `NEWS.md:79`, which carries the same claim spelled
      `500,000` and so escapes a grep for the source's spelling.
- [x] T4: Add a test reading the two calibration-domain sentences from the
      installed help page via `tools::Rd_db("circumplex")`, failing if either
      states its sample-size figure without its coefficient; prove it able to
      fail by restoring the current wording. Nothing in `tests/` pins these
      sentences today, while this same Rd page is fenced claim-by-claim at four
      other sites. Then run document, verify and check.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan gate chose a bounded repair of one named sentence over reopening general prose certification, because the general form failed three review rounds in M106 and was descoped there; falsified by a second defect of the same class turning up in the block.
- 2026-08-24: criteria audit (FULL mode, [O], fresh context) returned three findings here, all fixed before writing. The sweep's literal `5e5` missed `5.0e5`, which the target file itself uses at `R/axes_corrected_se.R:472`, and its `R/ man/` directory set missed `NEWS.md:79`, which ships and carries the same unqualified claim — both verified against the repo and the sweep widened. A drafted criterion binding the existence of a pinning test was an instrument promise rather than a property of the deliverable; it moved to T4, leaving AC2 to bind the shipped text itself.
- 2026-08-24: T1 — the closing sentence now states both tenth-margin endpoints with their coefficients and which falls below the n ~ 1e4 published ceiling; the channel-3 line at :501 gained the anchor's value so its 5e5 names its coefficient too. Full suite clean (FAIL 0, PASS 8619).
- 2026-08-24: T2 — both roxygen sites now carry the anchor and worst-measured endpoints with their coefficients, per the gate's choice to name both rather than drop the figures; `document()` regenerated `man/axes_reliability.Rd` and emitted no `resolve link` line.
- 2026-08-24: T3 — the NEWS entry now gives both endpoints with their coefficients, and the paragraph is wrapped so the `500,000` figure and its `1/sqrt(2)` share a line; the AC2 sweep returns 9 lines and every one names the coefficient its figure belongs to.
- 2026-08-24: T4 — a help-page guard at `tests/testthat/test-axes-reliability.R:3235+` cuts the Rd into sentences and requires each of the two `5e5` sentences to name `a = 1/sqrt(2)` and each `2e3` sentence to name `a = 0.045`, using the dual-source man/-or-`Rd_db()` read the file's other Rd guards use (under check only the `Rd_db()` arm runs). Proved able to fail on two planted defects: reverting both sites reddens 4 of 6 assertions, reverting only the unqualified `\value` site reddens 2 of 7 — the sentence count alone stays green on the first defect, which is why the per-sentence match carries the claim.
- 2026-08-24: verification — `document()` no diff and no `resolve link` line; `devtools::test()` FAIL 0, PASS 8627 (8619 before, the 8 added being this guard's); `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes. Status → review.
- 2026-08-24: review opened — master in sync, branch pushed, draft PR #140; evidence gathering under way.
- 2026-08-24: review round 1 — AC1 and AC2 verified with fresh evidence and ticked; `devtools::test()` FAIL 0, PASS 8627; AC3's check still running.
- 2026-08-24: review round 1 — AC3 verified and ticked (`check(args = "--no-manual")` Status OK); consistency gate clean; three fresh-context lenses returned 12 findings, triage pending at the gate.

**Independent review — three fresh-context lenses, 12 findings.**

[O] diff-bug, [S] blame-history, [S] prior-review. Ranked as reported; text
verbatim where quoted, disposition and reason per finding.

- **[O] F1 — the `\value`-site half of the new guard is not sentence-local.**
  "its 'sentence' is a 2,635-character block spanning the tail of
  `\arguments` plus the entire `\value` section." Re-verified by running the
  test's own splitter over `man/axes_reliability.Rd`: the two `5e5` chunks are
  2,635 and 464 characters, so at the `\value` site the guard only requires
  the coefficient to appear somewhere in that block, not beside its figure;
  `expect_identical(length(anchor), 2L)` also counts chunks, not occurrences.
  → FIX NOW.
- **[O] F2 — the NEWS insertion leaves three unpaired em-dashes**, so "divided
  by the factor of `10`" no longer reliably attaches to "the accuracy target
  `1e-4`"; a reader can close the appositive early and read `1e-5` as being
  asserted below published sample sizes. → FIX NOW.
- **[O] F3 — `R/axes_corrected_se.R:537` computes its `6.5e-6` corner from
  a = 0.046, not the 0.045 the block states.** Confirmed pre-existing (present
  in `origin/master`, untouched by this diff) and confirmed arithmetically:
  0.1*0.045/sqrt(5e5) = 6.36e-6, 0.1*0.046/sqrt(5e5) = 6.51e-6.
  → FOLLOW-UP (the diff makes it more reachable but did not introduce it).
- **[O] F4 — NEWS's "(coefficient `1/sqrt(2)`)" is unglossed**: NEWS never
  says what the number is a coefficient of, so a NEWS-only reader gets a token
  rather than the information. → FIX NOW, folded into F2's rewrite.
- **[O] F5 — "The two tenth-margin endpoints" reifies a two-point set out of a
  measured continuum** whose upper end (a = 1.38) reaches n = 1.9e6.
  → REJECT: the phrase is anaphoric to the two endpoints the preceding
  sentence computes, not a claim that only two exist.
- **[O] F6 — the `\value` site newly calls a = 1/sqrt(2) "a typical design"**
  when the source derives it analytically. → REJECT: the source itself calls
  it "a fair TYPICAL value", so the roxygen matches its own authority.
- **[O] F7 — no prose surface carries the "no theorem bounds a away from
  zero" caveat**, so `2e3` can read as a floor rather than the worst
  *measured* endpoint. → REJECT: general certification of the derivation
  block's prose is explicitly Out of scope, and "measured" is stated at every
  surface.
- **[O] F8 / [S] blame-history F2 (same finding) — "Above the endpoint the
  guarantee is the fixed target alone" is ambiguous** now that two endpoints
  are in scope; the pre-diff text read "Above that" with one antecedent.
  → FIX NOW.
- **[O] F9 — NEWS drops the "up to about" both roxygen sites keep**, stating a
  rounded model-derived boundary as if exact. → FIX NOW, folded into F2's
  rewrite.
- **[S] blame-history F1 — the AC checkboxes were unticked at review entry.**
  → REJECT (no change needed): that is the AC-fencing protocol — review ticks
  each box against its own recorded evidence, which this section does.
- **[S] prior-review F1 — the new test's "(M106 review round 4, F4)" citation
  is mislabeled**, the lens holding that F4 names a round-3 print()/summary()
  finding. → REJECT: refuted against the record. `cairn/ROADMAP.md:29` states
  "M110 repairs M106's parked round-4 F4 defect: the calibration-domain
  paragraph closes on the anchor's endpoint a sentence after putting the worst
  geometry's a decade below the published ceiling" — the citation in the test
  matches the repo's own record of what F4 is.
- **[S] prior-review, [S] blame-history — no regression found.** Both lenses
  confirmed M106's round-2 F13 (the coverage-units fix) and round-3 F2 (the
  paired-endpoint fix) survive untouched, and that no D-entry constant moved.
  The GitHub inline-comment probe returned an empty array, so that surface was
  skipped.

## Decisions

## Review

Round 1. PR #140 (draft at evidence time). Master in sync at `9374d4e8`;
branch 4 ahead, 0 behind, no merge needed.

**Acceptance criteria — fresh evidence.**

- AC1 ✓ (2026-08-24). Read `R/axes_corrected_se.R:519-527`: the paragraph
  closes "The two tenth-margin endpoints fall on opposite sides of the
  n ~ 1e4 ceiling of published circumplex correlation matrices: the anchor's
  n = 5.0e5 (a = 1/sqrt(2)) is 1.7 decades past that ceiling, while the worst
  measured geometry's n = 2.0e3 (a = 0.045) is below it". Both endpoints
  carry their coefficients, and it says which lies below the ceiling.
  Arithmetic re-derived from the block's own `n = 1e6 * a^2`: a = 0.045 gives
  2025, a = 1/sqrt(2) gives 5.0e5, log10(5e5/1e4) = 1.699. `grep -rn 0.046 R/
  man/ NEWS.md` returns nothing — the source's own `0.045` is used throughout.
- AC2 ✓ (2026-08-24). `grep -rnE '5e5|5\.0e5|500,000' R/ man/ NEWS.md`
  returns 9 lines: `R/axes_reliability.R` 731, 1049; `R/axes_corrected_se.R`
  501, 508, 522, 539; `man/axes_reliability.Rd` 88, 233; `NEWS.md` 79. Each
  names the coefficient its figure belongs to (`a = 1/sqrt(2)` at all nine).
- AC3 ✓ (2026-08-24). `Rscript -e 'options(cli.width = 500); devtools::document()'`
  exit 0, zero lines matching `resolve link`, and `git status --porcelain`
  showed no generated-file diff. `Rscript -e 'devtools::test()'`:
  FAIL 0 | WARN 5 | SKIP 1 | PASS 8627.
  `Rscript -e 'devtools::check(args = "--no-manual")'`: exit 0, Status: OK
  (0 errors, 0 warnings, 0 notes).

**Consistency gate.**

- `cairn_validate.py` exit 0 — every check PASS, including `scaffold present`
  and `coverage complete`; `release window` advisory silent. 47 `work-log
  format` advisory WARNs, all pre-existing M7 lines, no gate failure.
- `cairn_impact.py` skipped: the diff changes no `DESIGN.md` principle (IP1 is
  touched, not amended; `DESIGN.md` is not in the diff).
- Toolchain checks (`r-package` profile `consistency-gate`): `document()` no
  diff and no unresolved-link warning (above); generated files unedited by
  hand (same no-diff check); README.md/README.Rmd both untouched by the diff
  and in sync at `7d258248`; `pkgdown::check_pkgdown()` "No problems found";
  `NEWS.md` carries this milestone's user-visible change and names no
  milestone number; no new top-level files, so no `.Rbuildignore` entry owed;
  full `devtools::check()` Status OK (devtools defaults to `--no-manual`,
  so this is the same invocation AC3 names; the `manual = TRUE` release check
  is not owed here). Master watches: newest
  push-run verdict on `R-CMD-check.yaml` and on `test-coverage.yaml` is
  `success` at `096b7cda` (master's tip `9374d4e8` is docs-only and triggers
  no run). `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R`
  and `tools/check-branch-protection.R` all exit clean.
