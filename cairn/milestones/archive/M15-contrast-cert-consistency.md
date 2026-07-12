# M15: Contrast certification-conditional reporting consistency — done 2026-07-12

- **Outcome:** `ssm_ci_accuracy()` now reports a contrast row's displacement
  coverage and verdict **unconditionally**, matching `print.circumplex_ssm()`'s
  profiles-only certification stance (a contrast's Δamplitude is a signed
  difference, not a prototypicality measure). No profile-side change. PR #39,
  squash `24d8bde`; `check()` 0/0/0; full suite FAIL 0 / PASS 1881.
- Contrast `verdict` classified on unconditional coverage, `Parameter`
  relabeled `"d_conditional"` → `"d"` (profiles keep `"d_conditional"`);
  print/summary drop "when certified" for the contrast; `plot()` excludes it
  from the "Displacement (certified)" panel (a fourth surface RR02 surfaced).
- **Object contract retained (M15-D1):** contrast
  `coverage$Coverage_conditional`/`N_conditional` and `guardrail$Cert_rate`
  (`Caution` NA) stay populated as documented joint-certification descriptives
  (a selection-effect quantity no display consumes); dropping them (RR02 option
  b) rejected. Supersedes the M4 "Milestone-close review #3" split; design
  settled by independent Fable review (RB02 → RR02, archived).

## Review
- Three-lens fresh-context review: **zero findings** (diff-bug Opus,
  blame-history + prior-PR Sonnet). One gate catch: Coverage AC3→T4 → T5
  (gated amendment, no code impact).
