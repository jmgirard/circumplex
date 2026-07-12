<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M15: Contrast certification-conditional reporting consistency (ci_accuracy ↔ print)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** m15-contrast-cert-consistency · https://github.com/jmgirard/circumplex/pull/39

## Goal

Make `ssm_ci_accuracy()` report a contrast row's displacement coverage
unconditionally — matching `print.circumplex_ssm()`'s profiles-only
certification stance — resolving the split treatment left by the M4
milestone-close review.

## Scope

Object contract settled by RR02 (M15-D1): **measured quantities stay in the
object; the contrast's interpretive/presentation surfaces (verdict,
print/summary, plot) follow `print.circumplex_ssm()`'s profiles-only stance.**
Concrete sites in Tasks below.

**In:**
- **Verdict + print/summary + plot** made unconditional for the contrast
  (Parameter `"d"`, no "when certified"/"certified displacement"; excluded from
  the "Displacement (certified)" plot panel — the fourth surface RR02 found).
  Profiles unchanged.
- **Object measurements retained** (not dropped): contrast
  `coverage$Coverage_conditional`/`N_conditional` and `guardrail$Cert_rate`
  (`Caution` NA) kept as documented descriptives; three stale comments rewritten.
- Supersede "Milestone-close review #3" (`test-ci_accuracy.R:221-250`); re-pin
  the `ci_accuracy` snapshot (profiles byte-identical); roxygen `@return` + NEWS.

**Out:** `print.circumplex_ssm()` (already correct; B/C rejected at plan gate);
NA'ing the contrast's conditional *measurement* fields (RR02 rejects option b);
guardrail certification-**rule** replacement → its own ROADMAP candidate.

## Acceptance criteria

- [x] For a contrast object, `print()`/`summary()` of `ssm_ci_accuracy()`
      report the contrast's displacement coverage unconditionally — no "when
      certified" framing on the line and no "certified displacement" wording in
      the verdict paragraph. Evidence: updated `ci_accuracy` snapshot + a test.
- [x] The returned object follows M15-D1: the contrast's `verdict` displacement
      row is recomputed unconditionally with `Parameter == "d"` and
      `N_reps == reps`, while its `coverage$Coverage_conditional`/`N_conditional`
      and `guardrail$Cert_rate` are retained (populated, `Caution` NA). Evidence:
      regression tests pin each field; profiles keep `Parameter == "d_conditional"`.
- [x] `plot.circumplex_ci_accuracy()` excludes the contrast series from the
      "Displacement (certified)" panel. Evidence: a test on the built plot data.
- [x] Profile-side output is byte-unchanged (`ssm_certified()` and profile
      reporting untouched). Evidence: profile portions of the snapshot identical.
- [x] Roxygen `@return` documents the contrast rule on all surfaces; the
      `NEWS.md` development bullet gains the contrast clause; `devtools::check()`
      clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T3, T5
- AC2 → T2, T3
- AC3 → T5
- AC4 → T3, T5
- AC5 → T5

## Tasks

- [x] **T1** — RB02 drafted + RR02 ingested: object contract settled (M15-D1);
      unconditional-only supersedes Milestone-close review #3. Done 2026-07-12.
- [x] **T2** — Regression tests first (red before the change): supersede
      `test-ci_accuracy.R:221-250` — pin contrast verdict `Parameter == "d"`
      with `N_reps == reps`, profiles keep `"d_conditional"`, contrast printed
      displacement line has no "when certified", contrast `Coverage_conditional`
      still populated; keep the surviving assertions (contrast `Caution` all-NA,
      finite `Cert_rate`, wording bars).
- [x] **T3** — Verdict + wording: recompute the contrast displacement Class on
      unconditional coverage and relabel `Parameter` → `"d"`
      (`ssm_ci_verdict()`); key the contrast to `"d"` in `ssm_ci_verdict_blocks()`
      and use plain "displacement" wording in `ssm_ci_verdict_text()`; make the
      not-assessable fallback unreachable for the contrast. Profiles unchanged.
- [x] **T4** — Object comments + retention: rewrite the three stale comments
      (`R/ssm_ci_accuracy.R:546-551`, `:690-696`; `R/ssm_ci_oop.R:110-114`) to
      say the joint-cert rate is provenance for the retained object columns, not
      a conditioning device for any displayed line; add object-contract tests.
- [x] **T5** — Plot fix (exclude contrast from the `d_cert` panel,
      `R/ssm_ci_oop.R:501-527`); re-pin the `ci_accuracy` snapshot; roxygen
      `@return` + `NEWS.md` clause; `devtools::document()`; `devtools::check()`
      clean.

## Work log

- 2026-07-12: created by /milestone-plan (promoted from the "statistical
  follow-ups" grouped ROADMAP candidate — contrast-cert-consistency sub-item).
  Direction A (ci_accuracy matches print) and a Fable RB review both chosen at
  the plan gate; T1/AC3 carry the no-oracle tripwire. Reconciliation reverses
  the certification-conditional half of Milestone-close review #3
  (`test-ci_accuracy.R:221`), a deliberate prior decision — flagged for the
  blame-history reviewer.
- 2026-07-12: blocked on RB02 (contrast certification-conditional object
  contract) — T1 escalation drafted; awaiting RR02.
- 2026-07-12: RR02 ingested → back to planned. Object contract settled
  (M15-D1); scope amended — verdict recompute+relabel, plot fourth surface
  added (AC3/T5), measurement fields retained (RR02 rejects the drop/NA
  option). RB02/RR02 archived. T1 done.
- 2026-07-12: in-progress on m15-contrast-cert-consistency (cut from synced
  master); no open implementation gate (design fully settled by M15-D1).
- 2026-07-12: T2–T5 done → review. Contrast verdict now classified on
  unconditional coverage (Parameter "d"); print/summary/plot follow print's
  profiles-only stance; conditional measurement columns + Cert_rate retained;
  3 stale comments rewritten; roxygen + NEWS updated. Full suite FAIL 0 /
  PASS 1881; `check()` 0/0/0. Added a contrast print snapshot + a data-level
  plot-exclusion test (existing snapshot/plot fixtures were profile-only).
- 2026-07-12: review consistency gate found a Coverage mis-map (AC3 pointed at
  T4, but the plot-exclusion work is T5). Gated Coverage amendment: AC3 → T5
  (authoring slip from RR-ingest). No code/criteria change; back to review.

## Decisions

### M15-D1 (2026-07-12, from RR02): contrast ci_accuracy object contract

Under Direction A, the rule is **measured quantities stay in the returned
object; interpretive/presentation surfaces follow `print.circumplex_ssm()`'s
profiles-only certification stance.** Concretely, for the contrast row:
`verdict` displacement Class is recomputed on the *unconditional* coverage and
its `Parameter` relabeled `"d_conditional"` → `"d"` (print/summary/plot render
from it, so it must move); `coverage$Coverage_conditional`/`N_conditional` and
`guardrail$Cert_rate` are *retained* as documented joint-certification
descriptives (they measure a real selection-effect quantity — P(Δd CI covers |
both rows certified) — that no display consumes). Dropping/NA'ing them
(option b) is rejected: destroys information, buys no consistency suppression
doesn't already give. Supersedes "Milestone-close review #3". Milestone-local
(display-layer contract of one function family); promote to DECISIONS.md only
if the "presentation follows print" rule recurs. Source: RR02.

## Review

**AC evidence (fresh, 2026-07-12, PR #39).** All five verified by command:
contrast displacement unconditional in print (`... 91.7% -- borderline`, no
"when certified"; snapshot+test); `verdict` `Parameter=="d"` `N_reps==12` with
`Coverage_conditional`/`Cert_rate` retained (`Caution` NA); plot excludes
contrast from the certified panel; profiles byte-identical; `document()` no
diff; `check()` 0/0/0; full suite FAIL 0 / PASS 1881. Consistency gate PASS (one catch: Coverage AC3→T4 fixed to →T5, gated).

**Independent review (3 lenses, zero findings → nothing to score).** Diff-bug
(Opus): verdict `k`/`n`, `dkey` wording gating, plot label lookup correct.
Blame-history (Sonnet): MCR#3 reversal complete, profiles byte-unchanged.
Prior-PR (Sonnet): no GH-comment evidence.
