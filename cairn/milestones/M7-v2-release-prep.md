# M7: v2.0.0 CRAN release preparation

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M25, M26, M27, M31, M32, M33, M34, M35, M36, M37, M38
- **Branch/PR:** `m7-v2-release-prep`

## Goal

Ship the accumulated M2–M5 work (inference, visualization, Browne model + CI
trustworthiness, structure tests, SEM) plus the CIRCUM free-scaling family
(M17/M18, per D-008) and the longitudinal SSM builds (M25–M27, per D-012 +
the 2026-07-16 plan gate) plus the visualization expansion (M30 design → M31
build, M32 ergonomics, M33 trajectory viz, M34 docs, M35 model-based
trajectories, per D-018) to CRAN as one
v2.0.0 release. **No target date** — the release ships when its bundle is
complete and validated (D-008).

## Scope

**In:**
- Version bump to 2.0.0; NEWS.md development heading → 2.0.0.
- Refresh `cran-comments.md` (test environments, clean check (0 errors / 0 warnings / 0 notes), no revdeps, the
  Moss DOI 403 = SAGE bot-block note from urlchecker).
- Second independent human re-read of the Grassi et al. (2010) CircE and
  Zimmermann & Wright (2017) transcriptions against their primary sources
  (a pre-submission oracle gate; absorbed from a ROADMAP candidate 2026-07-12).
- win-builder / R-devel checks; then hand `submit_cran()` to Jeff (never submit
  autonomously).

**Out:**
- New features beyond the bundled milestones (longitudinal deferrals stay
  ROADMAP candidates; D-012 governs any late-merging build).
- The billed `/code-review ultra` unless Jeff asks (legacy CLAUDE workflow).

## Acceptance criteria

- [ ] DESCRIPTION at 2.0.0; NEWS.md heading renamed; `cran-comments.md` accurate.
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes) and
      win-builder / R-devel green across platforms.
- [ ] Second independent human re-read of the Grassi et al. (2010) CircE and
      Zimmermann & Wright (2017) norm/structure transcriptions against their
      primary sources completed before submission, with any discrepancies
      resolved (Jeff-attested in the work log).
- [ ] Release handed to Jeff for `submit_cran()` (not self-submitted).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4

## Tasks

- [x] **T1** — Version bump + NEWS heading + refresh `cran-comments.md`
      (groundwork staged 2026-07-08; the accurate summary + urlchecker DOI note
      already written per legacy MILESTONES.md R2).
- [ ] **T2** — Full `check()` + win-builder / R-devel.
- [ ] **T3** — Second independent human re-read of the Grassi et al. (2010) and
      Zimmermann & Wright (2017) transcriptions vs primary sources; gates
      submission. Human task (Jeff); discrepancies resolved before T4.
- [ ] **T4** — Hand `submit_cran()` to Jeff.

## Work log

- 2026-07-12: created by /cairn-init migration from circumplex's legacy
  MILESTONES.md active unit ("v2.0.0 release preparation", task R2). R1
  (`cpm_pack` β-boundary fix, the last cross-platform CI red) is DONE — verified
  green 2026-07-08 (PR #29, all 7 checks). No-invention: criteria/tasks
  translated from R2's written accept text, not inferred.
- 2026-07-12: BLOCKED — held for the CRAN cadence window. v1.2.0 was
  CRAN-approved 2026-07-02; CRAN wants ~1 month between submissions, so the
  version bump / NEWS rename / win-builder / submit are deliberately deferred
  until ~2026-08-02 (freeze ~2026-07-26). Repo stays at 1.3.0.9002 until then
  (legacy MILESTONES.md R2).
- 2026-07-12: AMENDED (gated) — absorbed the "v2.0.0 pre-release oracle
  re-reads" ROADMAP candidate as AC3/T3 (second independent human re-read of
  the Grassi 2010 + Zimmermann & Wright 2017 transcriptions, gating submission);
  old submit-handoff task renumbered T3→T4. Candidate row retired. Scope
  unchanged otherwise; still blocked on the CRAN cadence window.
- 2026-07-12: gained `Depends on: M16` (/milestone-plan) — the v2.0.0 bundle
  now includes the print-independent certification-rule replacement, which the
  user placed before the ~2026-07-26 freeze. No release-prep task change.
- 2026-07-12: dependency M16 is now **done** (PR #40 merged, squash `cd0c140`).
  M7 stays `blocked` on the external CRAN cadence window (~2026-08-02), not on
  any remaining milestone.
- 2026-07-13: dependency re-pointed M18→M19 (/milestone-plan). M18 is done; the
  free-family analytic-CI **coverage** claim is not yet settled. Per D-009's
  "mandatory pre-ship gate before any analytic-CI-trust claim", M7 now depends on
  M19 (free-family coverage oracle + caution calibration). No release-prep task
  change.
- 2026-07-12: AMENDED (gated, /milestone-plan) — **the CRAN cadence window is no
  longer a constraint (D-008).** Jeff confirmed there is no release-time
  pressure: v2.0.0 has no target date and ships when its bundle is complete and
  validated. All the ~2026-08-02 / ~07-26 framing in the entries above is
  superseded (left as the historical record). Status blocked→planned. Dependency
  re-pointed M16→M18: the v2.0.0 bundle now includes the CIRCUM free-scaling
  family (M17 design → M18 build, per D-008). No release-prep task change.

- 2026-07-16: dependency re-pointed M19→{M20, M21} (/milestone-plan). M19 is
  done; Jeff routed two pre-release items into the v2.0.0 bundle at the
  /milestone status gate: M20 (pole CI-endpoint alignment — an exported print
  change cheapest bundled into the major release) and M21 (T_diag-vs-T_free
  inference-default decision + application, superseding D-009's deferral). No
  release-prep task change.

- 2026-07-16: dependency re-pointed {M20, M21}→M22 (/milestone-plan, gated).
  Both are done; Jeff routed one more pre-release item into the v2.0.0 bundle
  at the plan gate: M22 (free-engine multi-start nesting seed, RR05 B2/R5 —
  an exported-results improvement cheapest shipped before the free family's
  CRAN debut). No release-prep task change.

- 2026-07-16: AMENDED (gated, /milestone-plan) — dependency re-pointed
  M22→{M25, M26, M27}. M22 is done; Jeff chose "all builds before M7" at the
  longitudinal plan gate, so the v2.0.0 bundle now includes the longitudinal
  SSM builds (one submission carries everything, per D-001's anti-churn
  rationale + D-012). Goal/Out wording updated to match; the stale "M6
  longitudinal → its own ~v2.1.0" Out clause is superseded. No release-prep
  task change.
- 2026-07-18: AMENDED (gated, /milestone-plan) — dependency gains M35, split out
  of M33 at that milestone's re-plan gate (the growth vignette's trajectory
  figure is model-based, not an occasions object). Extends the D-018 viz
  expansion to M30–M35. No release-prep task change.
- 2026-07-18: gated amendment — `Depends on:` gains M36 (viz polish: legend key
  glyph + non-finite guards) and M37 (static on-circle movement paths), the
  M31–M35 viz-track remainders. Jeff's plan-gate decision: both ship in v2.0.0
  under D-018's fold-in. No release-prep task change.
- 2026-07-18: gated amendment — `Depends on:` gains M38 (guaranteed rim ring for the circumplex canvas), spun out of the PR #62 hotfix. Jeff's plan-gate decision: the rim is visible in every figure the CRAN debut ships, so it goes in the bundle. No release-prep task change.

- 2026-07-18: started (/milestone-implement). Branch `m7-v2-release-prep` cut from master at 3d2d9a76; all 11 dependencies verified done. Status planned→in-progress.
- 2026-07-18: T1 done. DESCRIPTION 1.3.0.9002→2.0.0; NEWS dev section retitled to 2.0.0 and consolidated from 47 flat bullets into 8 thematic `##` groups (matching the 1.1.0/1.0.0 heading style) per the profile's release-walk slot — Jeff's gate choice "consolidate and group". Fixes to code that never shipped (the rim-ring omission, the trajectory legend key, the non-finite amax/center guard — all against features new in 2.0.0) were folded into their feature descriptions rather than listed as fixes; the two Advanced Visualization vignette bullets (added then rewritten in-cycle) merged into one. `cran-comments.md` refreshed: added the longitudinal and viz families, corrected "four"→five new vignettes, added brms/glmmTMB to the new-Suggests list, added a dependency note for the ggplot2 3.3.0→4.0.0 floor and the ggforce drop, and replaced the stale "one user-visible API tightening" claim with the three actual behavior changes (certification rule, pole labeling, argument validation). `devtools::test()`: 0 failures, 2986 passing, 0 skipped under NOT_CRAN=true; 4 pre-existing warnings in test-ci_accuracy.R (the diagnostic's own cautions, no code touched by T1).
- 2026-07-18: T3 aid written — `devel/m7-transcription-reread-checklist.md` (Jeff's gate choice "I prepare a checklist for you"). Enumerates every transcribed Grassi 2010 value (Table 1 matrix + N, the Appendix A full-precision block, Table 2/3 fit measures, the constrained-model rows, three quoted textual claims, the Listing 7-8 secondary fixture) and the Zimmermann & Wright subset that reaches shipped user-facing output (the vignette's Studies 1-2 accuracy table, Note 3 population matrices, the Eq. A6/A7/Eq. 3 scaling formulas, Study 5 + Table 4, Figure 1A octant angles), each with its repo location and its table/page anchor. Flags two things for the reader: the Appendix-A-vs-Table-2 mirror direction, and the one channel discrepancy `m4-zw-transcription.md` resolved by reasoning rather than a clean second read (Eq. A7's radicand). Instructs source-first reading. **T3 itself remains open** — the checklist is an aid, not the re-read, and AC3 needs Jeff's attestation.
- 2026-07-18: T2 partial. Local `devtools::check(args = "--no-manual")` on 2.0.0: **Status OK, 0 errors / 0 warnings / 0 notes** (5m10s; tests 171s, vignettes rebuilt clean) — `cran-comments.md`'s existing clean-check assertion holds as written, no correction needed. win-builder R-devel uploaded (Jeff's gate approval was conditional on the local check being clean; precondition met); results due to me@jmgirard.com in 15-30 min. T2 stays open pending those results. **Finding at build time (not a check NOTE):** `R CMD build` warns that the package now depends on R >= 3.5.0 because of the serialized `vignettes/bayesian_ssm_draws.rds` fixture (D-015), while DESCRIPTION declares `Depends: R (>= 3.4)`. Verified the real floor is higher still: ggplot2 (>= 4.0.0) and htmlTable both declare `Depends: R (>= 4.1)`, so the effective install floor is R >= 4.1 — exactly what D-014/D-019 recorded without DESCRIPTION ever being updated. Re-pinning the R floor is a dependency change (tracking-rules: question gate + D-entry, never unilateral), so it is gated to Jeff rather than applied.
- 2026-07-18: D-021 applied (user-approved): `Depends: R (>= 3.4)`→`R (>= 4.1)`, plus NEWS and cran-comments dependency notes. Re-ran the full local check on the corrected tarball: **Status OK, 0/0/0** (5m07s), and the `R CMD build` R>=3.5.0 serialization warning is gone as D-021 predicted. win-builder re-upload **failed: FTP 550** — most likely win-builder still holds the identically-named `circumplex_2.0.0.tar.gz` from the earlier (pre-D-021) upload and refuses to overwrite until that run clears. Not retried in a loop. T2 remains open: AC2 needs win-builder evidence for the *corrected* tarball, so the earlier in-flight run does not satisfy it.
- 2026-07-19: win-builder re-upload **succeeded** on retry (Jeff's gate choice "retry now"). The corrected post-D-021 tarball `circumplex_2.0.0.tar.gz` is now with win-builder R-devel; results due to me@jmgirard.com ~08:50. The retry succeeding after the first run had cleared corroborates the FTP-550 duplicate-filename reading (supports, does not prove). T2 stays open until those results are read: AC2 is fenced to win-builder evidence for the corrected tarball, and the earlier pre-D-021 run does not supply it.
- 2026-07-19: **win-builder returned 1 ERROR + 1 WARNING** on the corrected tarball: the PDF manual failed to build — `Unicode character θ (U+03B8) not set up for use with LaTeX`, same for `ζ (U+03B6)`. Root cause: literal Greek in a roxygen block, `R/cpm_oop.R:276`, reaching `man/plot.circumplex_cpm.Rd`. **Both clean local 0/0/0 runs were structurally blind to it** — the repo's check command passes `--no-manual` (CLAUDE.md), which skips PDF manual generation entirely, so no local gate can see this failure class. Fixed at the roxygen source as Rd math (`\eqn{\theta}{theta}`, `\eqn{\zeta^2}{zeta^2}`), which typesets in the PDF and degrades to ASCII in text/HTML; `man/*.Rd` regenerated via `document()`, never hand-edited. Regression guard added, `tests/testthat/test-rd-latex-safe.R`: scans `man/*.Rd` for Greek, math-operator, superscript/subscript, and legacy Latin-1 superscript characters. Verified it has teeth by running it against the pre-fix `man/` — it failed, naming `plot.circumplex_cpm.Rd:30: θ ζ`. It initially missed the `²` (U+00B2 is Latin-1, not the superscripts block); range widened to catch ¹²³ before regenerating. Fix verified directly rather than inferred: `R CMD Rd2pdf` now builds the manual, 70 pages, no Unicode errors. Remaining non-ASCII in `man/` is en/em dashes and one curly apostrophe, which inputenc handles and win-builder did not flag.
- 2026-07-19: LESSON for review's post-merge hygiene (not added to LESSONS.md here — lessons are captured at milestone end, and the file is already over its record-density threshold): a toolchain gate that is *routinely invoked with a skip flag* defines a blind spot the whole repo inherits. `--no-manual` made two consecutive 0/0/0 local checks unable to see a CRAN-blocking ERROR. Where a check command carries a skip flag by convention, something must exercise the skipped path before release — here, a cheap `R CMD Rd2pdf` or a full `check()` without the flag. Same shape as the M31 vdiffr auto-skip and the M16 `skip_on_cran()` trap: the run reports green because it never looked.
- 2026-07-19: **correction to the previous entry's verification claim.** A `devtools::check()` run was reported here as covering the manual; it did not — `devtools::check()` defaults to `manual = FALSE`, and that log contained zero occurrences of "PDF version of manual". Same failure mode as the bug being fixed (a green check that silently skipped the relevant step), caught by grepping the log for the step rather than trusting `Status: OK`. Re-ran as `devtools::check(manual = TRUE)`: **Status OK, 0/0/0, with `* checking PDF version of manual ... OK` present in the log** — this is the run AC2's local half rests on. Also swept the rest of the surface, since LaTeX aborts after a couple of errors and win-builder naming only θ/ζ did not rule out more: no non-ASCII in DESCRIPTION, and no Greek/math/superscript characters left in any roxygen block in `R/`. `cran-comments.md` test-environment line updated from `check(args = "--no-manual")` to `check(manual = TRUE)`, since advertising the flag that hid the ERROR would misdescribe the verification to a CRAN reviewer.
- 2026-07-19: **T3 section A complete** — Jeff's second independent human re-read of the Grassi et al. (2010) transcription against the primary source. **Every transcribed value confirmed**; the four findings were all errors in records *about* the values, not in the values, and are applied: (A2) Appendix A prints its blocks in its own variable order (Health, Social, BusinessContact, BusinessOperations, Trades, Technology, Science — ascending in the mirrored angle), not Table 1's; re-mapped by scale the communality indices and all seven CIs agree exactly, so no fixture changed, but the checklist's flat row order invited a false mismatch and both it and `helper-cpm-oracles.R` now state the order explicitly. (A3) the unconstrained m = 1 fit measures come from Appendix A pp. 70-71, **not** Table 3 p. 60 — the provenance header claimed Table 3 for all fit measures and now splits unconstrained (Appendix A) from constrained F (Table 3). (A4) Table 2/3 label the `.87`/`.88`/`(.96,.83,1,...)` column ρ̂ (communality index, Browne 1992 Eq. 4), not ζ; the assertions were already right (that column *is* our `Zeta`, design sec. 6.5) — three code comments corrected. (A5) the published statement is that the **nonsymmetric** CIs on ρ(x_i,c_i) are obtained from **symmetric** CIs on ln v_ii (Browne, 1982, pp. 95-96); the comment had put the symmetry on the communality CIs themselves, though the decoding arithmetic was already correct. (A6) one genuine transcription error: the sixth verbal-ability scale is **ForeignLanguage**, not ForeignLiterature (`helper-cpm-oracles.R:61`) — a Listing 7-8 fixture used for input-refusal behavior only, so no numeric result moved, and it is referenced nowhere else in the repo. The `pending (Jeff)` marker in `test-cpm_oracles.R`/`helper-cpm-oracles.R` now records the completed re-read and what it corrected. `devtools::test()`: 0 failures, 2988 passing, 0 skipped; the 4 warnings are the pre-existing test-ci_accuracy.R diagnostics. **T3 stays open**: section B (Zimmermann & Wright) is not yet re-read, and AC3 covers both sources.
- 2026-07-19: T3 section B worksheet refreshed before Jeff's read (his gate choice "sharpen the section B worksheet"), on the reasoning that section A's four findings were *all* in the label/anchor layer rather than in any value, so the B rows were re-derived against the current sources instead of trusted as cut. Every vignette line anchor had drifted since 2026-07-18 and was re-derived. Substantive gaps closed: a new **B1a** covering six shipped numbers that sit in the vignette's prose bullets rather than its table (`:203-222`) and so escaped the "user-facing output" sweep entirely — including the `15.5%` that `m4-zw-transcription.md` flags as occurring twice in the paper for two different quantities, a flag that was sitting in the record where this pass would never have met it; **B4** split, because its single row had conflated OCPD's *amplitude* (.012, `:237-238`) with PARPD's *elevation* (.250, `:572-573`) and cited a stale line for each, plus the Study 5 CircE fit indices it also ships (`:151-157`); **B3** given Eq. A7's leading ½, which the first cut never listed (only the radicand had ever been checkable); **B2** given the IIP-C ρ values (.683/.500/.345/.288) instead of "per the record". Numeric self-check re-run today: the transcribed ρ matrices and Eq. A6/A7/Eq. 3 reproduce all eight published constants (f_e .7369/.2398; f_a .5454/.8452/.6246; Eq. 3 .1077/.0279/.0292), and the no-√2 variant misses every one — recorded on the worksheet as a *consistency* check with its limit stated (same-pass transcription, so compensating errors would still close), never as an independent oracle. No package code touched; T3 still open pending Jeff's section B read.
- 2026-07-19: **A6 retracted — supersedes the "T3 section A complete" entry above on that one point.** Jeff withdrew the finding the same day as his own slip: the sixth verbal-ability scale in Grassi's Listing 7 **is** `ForeignLiterature`, exactly as the fixture had it since 2026-07-06. `helper-cpm-oracles.R:61` reverted to `ForeignLiterature`; the checklist's A6 note and the `test-cpm_oracles.R` provenance header both corrected in place (current knowledge, not history) to stop claiming a scale-name fix was made. The corrected standing account of section A: **every transcribed value confirmed, no fixture changed**, and three corrections all in the records *about* the values — the A3 page anchor, the A4 column label, and the A5 comment wording. That is a cleaner result than the original entry recorded, not a worse one: the transcription's numeric content came through the re-read untouched. The A6 revert is a no-op for behavior (input-refusal fixture only, referenced nowhere else); `devtools::test()` re-run to confirm.
- 2026-07-19: T3 section B restructured after Jeff reported that many of its rows gave him no way to verify them. Diagnosis: section A was uniform (nearly every row a printed number) but B's rows silently mixed three kinds of claim, so rows needing a *judgment* sat in a checklist shaped for *lookups*. Every B row is now tagged **[VALUE]** (printed, read it off the page), **[FINDING]** (a threshold the authors assert in a sentence, with the page and section named), or **[FIDELITY]** (our own paraphrase, which no page can settle) — and the [FIDELITY] rows are pulled out into a new **B0** that asks the question that actually applies, "does this overclaim?", with the published basis beside each. A locator pass against the shelf PDF pinned the printed page for every [VALUE]/[FINDING] row (PDF and printed pagination coincide); recorded on the worksheet as confirming **anchors, not values**, since it used the same extraction family as the original channel 2 and is therefore not an independent read.
- 2026-07-19: **⚠ potential discrepancy in shipped output, surfaced by that locator pass — needs Jeff's decision at T3.** Zimmermann & Wright state the amplitude/displacement sample-size thresholds **twice, with different numbers**: Study 2 *Results* (p. 10) gives amplitude n ≥ 75 / n ≥ 150 and displacement n ≥ 100 / n > 200; the Study 2 *Discussion*, same page, summarizes amplitude **and** displacement together as 100/200. The vignette's accuracy table (`evaluating-circumplex-structure.Rmd:190-199`) follows the Results, so its amplitude row (75/150) is **looser than the authors' own summary sentence** (100/200). Defensible — the Results are the more precise source — but it is a choice rather than a transcription, and it is exactly section A's error class (right paper, wrong section), here caught before shipping rather than after. Not changed unilaterally: the vignette is user-facing guidance about sample sizes, so whether to keep 75/150, switch to 100/200, or keep 75/150 with a note that the discussion states a stricter figure is Jeff's call. Logged on the worksheet at B1.
- 2026-07-19: T3 section B partly complete (Jeff, on the page). **Confirmed:** B2 (both Note 3 population matrices), B4 (all four Study 5 / Table 4 rows), B5 (Figure 1A octant angles), and — closing the record's longest-standing soft spot — **Eq. A7 including its leading ½**. That equation's √2 radicand had been resolved in 2026-07-07 by *reasoning* (only the √2 form reproduces the published scaling factors) rather than by a clean read, and the ½ had never been checked by any pass at all; both are now confirmed against the page, so `m4-zw-transcription.md` no longer rests an inferential resolution. Jeff also supplied a reason the record lacked: OCPD's absent a/δ CIs in Table 4 are the authors' **deliberate** omission (R² = .117 makes those parameters uninterpretable), not a transcription gap — the same logic the vignette leans on when it picks OCPD as its cautionary near-flat case. **Two worksheet defects found, both the same defect:** B1a's f_a row and B3's worked-values row each presented *our computed* numbers as though printed — B3 listed `.108`/`.028` with the published `.11`/`.03` in parentheses, exactly backwards, and B1a implied the unrounded f_a values sit near Eq. 3 when p. 12 prints only `.545` (IAS `.845` is p. 9, IIP-SC `.625` is p. 14; no page carries all three). Both rewritten with per-value page anchors, and a fourth tag **[DERIVED]** added to the section-B scheme so the printed/computed distinction is structural rather than a thing each row has to remember. Jeff's B1 note — these thresholds are about **95% percentile bootstrap CI coverage accuracy**, not point estimates and not CIs generally — added as scope text on that whole block.
- 2026-07-19: **correction, same day, superseding the OCPD reason in the entry above.** The trigger for Table 4 withholding OCPD's amplitude and displacement CIs is its low **Prob (.130)**, **not** its low R² (.117). Table 4's own note settles it — it defines Prob as "probability of accurate confidence intervals for amplitude and angular displacement", exactly the two parameters withheld — and the authors' rule is not to interpret a/δ CIs when that estimate is < .50 (pp. 12–13, 16). R² is a separate quantity and governs nothing about which intervals print. Corrected in `m4-zw-transcription.md` (Table 4 block, in place; change log gets a superseding line) and in the checklist's B4 note. **Checked whether the conflation reached shipped output: it did not.** The vignette's only statement about OCPD is that Table 4 showed the profile nearly flat, amplitude .012 (`:237-238`) — an amplitude claim, which is both correct and the actual upstream cause of the low Prob, so no vignette or package change is needed. The distinction is worth the care: Prob is a claim about whether an interval is trustworthy, R² about how well the model fits, and collapsing the two is the exact error this vignette's guidance exists to prevent.
- 2026-07-19: **two T3 decisions now pending for Jeff, both user-facing.** (1) The Study 2 threshold-source conflict logged above: keep the vignette's 75/150 amplitude row (Results, p. 10), switch to the authors' own summary 100/200 (Discussion, same page), or keep 75/150 with a note. (2) The accuracy table's header reads `95% CI accurate when…` while the claim is specifically about *bootstrap* CI coverage; the prose at `:184-188` says so but tables get read without their preamble, so the header may want "bootstrap". Neither changed unilaterally — both are user-facing sample-size guidance.
- 2026-07-19: **the Study 2 threshold conflict is resolved in the vignette's favour — Figure 5 (p. 12) supports 75/150.** Jeff asked whether the figures/tables back the Results interpretation; Table 3 carries only η² effect sizes, so Figure 5 is the only per-n evidence, and it settles it. Reading each panel for the smallest n at which the *worst-case* A = .10 curve drops below the 2.5% band (the operative reading, matching the A ≥ .10 precondition): **Panel B (amplitude)** — without GF ≈ 4.2% at n = 100 and ≈ 2.45% at n = 150, first below at **150**; with GF ≈ 3.5% at n = 50 and ≈ 2.1% at n = 75, first below at **75**. **Panel C (displacement)** — without GF still ≈ 2.65% at n = 200, so genuinely **> 200**; with GF ≈ 2.55% at n = 100, sitting *on* the line. Two conclusions: the 75/150 amplitude figures are what the figure shows rather than a loose reading, and **the Discussion's 100/200 is Panel C's displacement thresholds applied to both parameters** — a conservative simplification taking the worse of the two, not a measurement contradicting Panel B. The figure additionally explains two pieces of the authors' wording that had looked like sloppiness: "greater than 200" rather than "at least 200" (at n = 200 the curve has not crossed) and the "close to or dropped below" hedge on the displacement sentence (at n = 100 with GF the curve is on the line). **Recommendation: keep 75/150** — switching to 100/200 would import the Discussion's collapse of two parameters into one number. Caveat recorded on the worksheet: these are values read off a rendered figure, since no per-condition deviances were ever published (the record's no-supplement finding), so the near-threshold with-GF displacement point is not precise; the calls the recommendation rests on are well clear of eyeball error. Decision (1) of the two pending is now evidence-backed; decision (2), the "bootstrap" table header, is untouched.
- 2026-07-19: both pending T3 decisions taken (Jeff, at the gate). **(1) Keep the vignette's 75/150 amplitude thresholds as-is** — no change; Figure 5's Panel B shows exactly those, and the worksheet + `m4-zw-transcription.md` now carry the figure evidence and the reading that produced it, so the choice is documented rather than implicit. **(2) The accuracy table header changed** from `95% CI accurate when…` to `95% bootstrap CI accurate when…` (`evaluating-circumplex-structure.Rmd:190`) — the prose at `:184-188` already scoped it, but a table gets read without its preamble and the column is specifically about percentile-bootstrap coverage, not point estimates and not CIs in general. **First package-file change of T3** (everything prior was worksheet, record, and tracking). Markdown-only edit inside the table header; the table's column structure verified intact (all rows 4 pipes) and `devtools::test()` re-run clean — 0 failures, 2988 passing, 4 pre-existing test-ci_accuracy.R warnings. Note the vignette is not *rebuilt* by `test()`; the knit is exercised by `devtools::check()`, which T2 re-runs before submission, and this edit touches no R chunk.
- 2026-07-19: **win-builder returned OK** on the Rd-fixed tarball — the LaTeX ERROR and WARNING from the earlier run are cleared, confirming the `\eqn{}` fix at `R/cpm_oop.R:276` reached the PDF manual. Together with the local `devtools::check(manual = TRUE)` 0/0/0 (which included `checking PDF version of manual ... OK`), AC2's substance is met. **Not checking AC2 off yet, for a scoping reason worth stating:** that tarball is commit `a731862d`, and three package files have changed since — `tests/testthat/helper-cpm-oracles.R` and `test-cpm_oracles.R` (the A6 revert and the T3 comment/provenance corrections) and one line of `vignettes/evaluating-circumplex-structure.Rmd` (the bootstrap header). All are comment, fixture-name, or prose changes with no R-code semantics, and `devtools::test()` is clean at 2988 on the current tree, so the risk that win-builder would now differ is very low — but AC2 is fenced to evidence for the submitted tarball, and the same reasoning that made the post-D-021 re-upload necessary applies in miniature. One `check(manual = TRUE)` on the final pre-submission tree closes it; whether that also warrants a fresh win-builder upload is Jeff's call at T4.
- 2026-07-19: T3 section B — **all [VALUE] rows now confirmed** (Jeff: "i checked all the B1a/B3 values"), joining B2/B4/B5 and Eq. A7. Worksheet checkboxes ticked to match, including all 33 section-A rows, which had been confirmed in prose since the morning but never actually ticked — the boxes were lagging the attestation. Remaining for AC3: B0's five fidelity judgments (whether our paraphrases overclaim — not lookups), B1's six [FINDING] threshold rows (the Figure 5 work established the numbers, but Jeff has not said the rows themselves are settled), Jeff's work-log attestation, and then the three `pending (Jeff)` markers. T2 stays open until they are read — AC2 needs win-builder green on the *fixed* tarball, and the run that flagged the LaTeX ERROR tested the pre-fix one.

## Decisions

## Review
