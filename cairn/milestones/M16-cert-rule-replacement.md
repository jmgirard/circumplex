<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M16: Print-independent, scale-free displacement-certification rule

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** m16-cert-rule-replacement   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Replace the display-coupled `round(a_lci, digits) > 0` displacement-interpretability
guardrail with a print-independent, scale-free certification rule — its form
decided via a Fable Review Brief seeded by `ssm_ci_accuracy()`'s false-certification
output — propagated consistently across every certification surface, in time for v2.0.0.

## Scope

**In:**
- Redesign `ssm_certified()` (`R/ssm_oop.R:122`) as a print-independent,
  scale-free rule. The form is left open at plan time (property-based criteria)
  and decided by a Fable RB seeded by the diagnostic's amplitude-ladder
  false-certification output; the leading candidate family is a relative rule
  (`a_lci` as a fraction of the amplitude CI width — spec §12.5).
- Keep every certification surface consistent under the new rule (M15
  invariant): `print.circumplex_ssm()` note, `ssm_ci_accuracy()`
  certification-conditional coverage + false-certification, verdict wording,
  guardrail `Cert_rate`, and the plot "Displacement (certified)" panel.
- Update `ssm_ci_accuracy()`'s guardrail `digits` argument and the output text
  echoing the old scale-dependent threshold (`0.5·10^−digits` amplitude units),
  both meaningless under a print-independent rule.
- NEWS entry for the (major-version) behavior change; docs/pkgdown consistent.

**Out:**
- The amplitude/displacement estimators and CI methods — untouched; the rule
  reads `a_lci` and the amplitude CI, it does not recompute them.
- The model-fit (R² < .70) guardrail — unchanged; only the amplitude ⇒
  displacement certification rule is in scope.
- Z&W option (b) lookup module (spec §6) — stays a requirements record.
- Per-group CPM structure / `ssm_ci_accuracy.circumplex_cpm` (spec §12.3/§12.4)
  → a separate future cut, not this milestone.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] **AC1 (print-independence).** `ssm_certified()` no longer consumes a
      display `digits` argument, and certification of a given fit is invariant
      to print's `digits`. Evidence: property test — one fit certified
      identically across `digits ∈ {2, 3, 5}`.
- [ ] **AC2 (scale-invariance).** Certification is unchanged under a positive
      rescaling of the score metric (amplitude and its CI scale together).
      Evidence: property test — rescaling input scores by `k > 0` flips no
      profile's certification.
- [ ] **AC3 (form decided by review).** The rule's form is decided by a Fable
      RB seeded by `ssm_ci_accuracy()`'s amplitude-ladder false-certification
      output and recorded as a D-entry citing the RR
      (RB tripwire: no-oracle | irreversible-api). Evidence: RB/RR pair in
      `cairn/reviews/` + a `cairn/DECISIONS.md` entry naming the chosen form
      and the false-certification target.
- [ ] **AC4 (calibration).** Under the new rule the false-certification rate at
      a truly-zero amplitude (`c = 0`), measured by `ssm_ci_accuracy()`, meets
      the target set in the AC3 D-entry, and certification-conditional
      displacement coverage stays adequate when certified. Evidence:
      simulation-coverage run via `ssm_ci_accuracy()` at `c = 0` and `c > 0`
      (oracle type: simulation-coverage; spec §4.3).
- [ ] **AC5 (all surfaces consistent).** All five certification surfaces reflect
      the new rule and stay mutually consistent (M15 invariant): print note,
      `ssm_ci_accuracy()` coverage-conditional/false-cert, verdict wording,
      `Cert_rate`, plot "Displacement (certified)" panel. Evidence: refreshed
      snapshots + a cross-surface consistency test.
- [ ] **AC6 (diagnostic API + docs).** `ssm_ci_accuracy()`'s guardrail `digits`
      argument and threshold-echo output are updated/removed to match the
      print-independent rule; NEWS documents the print behavior change; docs
      and `_pkgdown.yml` consistent. Evidence: NEWS entry, `grep` shows no
      stale `digits`/threshold surface.
- [ ] **AC7.** `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T3, T4
- AC2 → T3, T4
- AC3 → T1, T2
- AC4 → T5
- AC5 → T6
- AC6 → T5, T7
- AC7 → T7

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1** — Assemble the RB seed: collect `ssm_ci_accuracy()`
      false-certification output across the §4 amplitude ladder and ≥2 score
      metrics (correlation vs raw), tabulating how the current rule behaves;
      draft the candidate rule (relative-to-CI-width leading family) and a
      proposed false-certification target. `(RB tripwire: no-oracle | irreversible-api)`
      → `devel/m16-cert-rule-seed.{R,rds,md}`.
- [x] **T2** — Escalate via `/milestone-brief` (RB → RR) to decide the rule's
      final form + false-certification target; ingest the RR and record a
      `cairn/DECISIONS.md` entry. `(RB tripwire: no-oracle)` → RB03/RR03,
      **D-007**: `r = a_lci/(a_uci − a_lci) ≥ 0.35`.
- [x] **T3** — Test-first (red before the change): print-independence (AC1,
      verdict identical across `digits`), scale-invariance (AC2, ×1000 rescale
      identical), plus the boundary/invariance battery from RR03 §Beyond-4 —
      profile peaking at 0°/360° (angle-blind), flat-profile `NA` → FALSE,
      degenerate zero-width CI → FALSE (fail-closed), and the *intended*
      regression: a near-zero-amplitude fixture (COR_nearzero-style) flips
      certified → not-certified.
- [x] **T4** — Implement D-007 in `ssm_certified()` (`R/ssm_oop.R:122`):
      `ssm_certified(a_lci, a_uci, k = 0.35)` returning
      `is.finite(r) & r >= k`, `r = a_lci/(a_uci-a_lci)`; drop the `digits`
      param. Update the `print.circumplex_ssm()` call site (`R/ssm_oop.R:183`)
      to pass `dat$a_uci`, and rewrite the note wording (`R/ssm_oop.R:185`)
      from "amplitude CI includes zero" to a lower-bound-relative-to-width
      phrasing (never a significance-test framing).
- [x] **T5** — Verify via `ssm_ci_accuracy()` (AC4, D-007 two-part gate):
      false-cert@c=0 ≤ 0.05 AND Wilson-LCI `Caution` not firing, at reps=1000
      across COR_healthy / COR_nearzero / RAW_means + one small-n (≈100) config;
      record the c>0 power curve. Cross-check against the closed-form
      Rayleigh-tail oracle `exp(−t*²/2)` (2nd oracle type). **Remove** the now-
      vestigial `digits` arg + `Threshold` column from `ssm_ci_accuracy()`
      (unreleased → clean removal, not deprecation; D-007), replacing the
      threshold echo with a `k` echo (`R/ssm_ci_accuracy.R`, `R/ssm_ci_oop.R`).
      Extend the seed generator into a committed `devel/` verification script.
- [ ] **T6** — Propagate to the remaining surfaces (verdict wording,
      `Cert_rate`, guardrail text `R/ssm_ci_oop.R:127-142`, plot "Displacement
      (certified)" panel) and the vignette wording
      (`vignettes/evaluating-circumplex-structure.Rmd:245,313,359`); refresh
      snapshots; add the cross-surface consistency test (AC5).
- [ ] **T7** — NEWS entry (behavior change + the near-zero flip), docs/pkgdown
      consistency, a doc sentence pinning k=0.35 to interval=0.95 (k(interval)
      generalization noted-but-deferred, D-007), full `devtools::check()`
      (AC6, AC7).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: T5 done. AC4 two-part gate PASSES at reps=1000 across all five
  configs (COR_healthy/nearzero, RAW_means, RAW/COR small-n): false-cert@c=0
  0.008–0.023 (all ≤0.05, below the 0.025 benchmark), Caution never fires;
  the closed-form Rayleigh oracle (0.0039) agrees as the 2nd type
  (`devel/m16-cert-rule-verify.{R,rds,md}`). Removed the vestigial `digits`
  arg + `Threshold` column from `ssm_ci_accuracy()` (clean removal — unreleased),
  echo the dimensionless `k` in the summary header, rewrote the guardrail
  caution wording off the "amplitude CI excludes zero" phrasing. Caught+fixed a
  scope bug: my `cert_k` echo constant collided with the loop's certified-count
  `cert_k` (renamed `rule_k`); affected only the cosmetic echo, not the
  certification, so the verification numbers stand. Refreshed the `ci_accuracy`
  snapshot (all changes explained by the rule); `test-ssm_oop`/`test-ci_accuracy`
  green. Two behavior tests re-pointed from the old over-certification pathology
  to the fixed behavior.
- 2026-07-12: T4 done. `ssm_certified(a_lci, a_uci, k = 0.35)` implements
  D-007 (`is.finite(r) & r >= k`, `r = a_lci/(a_uci-a_lci)`); print note
  reworded to the CI-lower-bound-vs-width phrasing; both call sites
  (`R/ssm_oop.R:195`, `R/ssm_ci_accuracy.R:564`) moved to the new signature.
  `test-ssm_oop.R` green; diagnostic smoke shows OCPD false-cert@c=0
  1.000→0.033. Snapshot/vignette/diagnostic-arg cleanup deferred to T5/T6.
- 2026-07-12: T3 done (tests-first, red before T4). Added to
  `test-ssm_oop.R`: unit tests of `ssm_certified(a_lci, a_uci, k)` (threshold,
  scale-invariance, vectorized, NA/degenerate fail-closed), AC1
  print-independence + AC2 scale-invariance end-to-end, the D-007 near-zero
  certified→not-interpretable regression, and 0/360 angle-blindness. Confirmed
  red: 5 unit + AC1/AC2 + regression fail under the current rule (angle-blind
  already green as a structural guard).
- 2026-07-12: T2 done — RR03 ingested. Fable review resolved the crux
  affirmatively: a lower-bound ratio `r = a_lci/(a_uci−a_lci)` is asymptotically
  pivotal at c=0 (Rayleigh null), so `r ≥ 0.35` drives false-cert to ≈α/2 where
  the shipped rule sits at 1.000. Decisions → D-007. RB03/RR03 archived; status
  back to in-progress. Next: T3 (tests-first).
- 2026-07-12: T2 — blocked on RB03 (`cairn/reviews/RB03-cert-rule-form.md`),
  Fable escalation for the rule form (no-oracle | irreversible-api). Deviation
  from /milestone-brief's default (commit brief to master): kept the RB/RR
  cycle on branch `m16-cert-rule-replacement` because T1 is already committed
  there — committing to master would diverge the milestone file. Tracking
  stays with code.
- 2026-07-12: T1 done. Seed (`devel/m16-cert-rule-seed.{R,rds,md}`, reps=500,
  seed 2026) shows the current rule false-certifies a *zero* population
  amplitude 100% of the time across correlation-healthy, correlation-nearzero,
  and raw metrics (structural: c=0 amplitude coverage is theorem-zero, so
  a_lci>0 always clears the 5e-4 threshold). Scale-free `a_lci/CI_width`
  separates signal (2.58, 6.24) from near-zero noise (0.10) where the current
  rule cannot. Leading candidate: relative rule `a_lci/CI_width ≥ k`; proposed
  target false-cert@c=0 ≤ α/2.
- 2026-07-12: created by /milestone-plan. Promotes the "guardrail
  certification-rule replacement (print-independent, scale-free)" ROADMAP
  candidate (Statistical follow-ups line). Targets v2.0.0 (freeze ~2026-07-26)
  per user decision — M7 gains `Depends on: M16`. Rule form left open
  (property-based ACs), decided via a Fable RB seeded by the diagnostic's
  output; both `no-oracle` and `irreversible-api` tripwires apply. Grounded in
  `devel/m4-ci-accuracy-spec.md` §3.4 (current rule), §4.3 (operating
  characteristics), §12.5/§13 (the deferral decision, Jeff 2026-07-03).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

- 2026-07-12 (RR03 ingest): rule form + target decided → **D-007** (promoted
  cross-cutting). Rule `r = a_lci/(a_uci − a_lci) ≥ 0.35`, CI-pair-pure,
  contrast rows ungated (M15-D1).
- 2026-07-12 (RR03 triage): RR03 recommended *deprecating*
  `ssm_ci_accuracy(digits=)`; **corrected to clean removal** — the diagnostic
  is unreleased (new in the dev line toward v2.0.0; latest CRAN v1.2.0), so no
  lifecycle shim is owed. Confirms the implement-gate default.
- 2026-07-12 (RR03 triage): all other RR03 recommendations applied
  (form (a)/k=0.35, two-part target, wording rewrite, boundary tests,
  Rayleigh-tail oracle cross-check); replicate-vector/ROPE and form (b)
  rejected with RR03's reasons; k pinned to interval=0.95 with the
  k(interval) generalization deferred (noted in T7/D-007).

## Review
<!-- owner: review · exclusive -->
