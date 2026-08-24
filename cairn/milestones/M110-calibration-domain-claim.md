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

- [ ] AC1: The calibration-domain paragraph closes by stating the domain
      endpoint at the worst measured coefficient (a = 0.045, tenth-margin
      n = 2.0e3) alongside the anchor's (a = 1/sqrt(2), n = 5.0e5), and says
      which of the two lies below the n ~ 1e4 ceiling of published circumplex
      correlation matrices. The source's own spelling of the coefficient
      (0.045) is used; the archived review spells it 0.046 and is not edited.
- [ ] AC2: Every line that `grep -rnE '5e5|5\.0e5|500,000' R/ man/ NEWS.md`
      returns either names the coefficient its figure belongs to, or does not
      state that figure as the calibration domain.
- [ ] AC3: `Rscript -e 'options(cli.width = 500); devtools::document()'`
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

## Decisions

## Review
