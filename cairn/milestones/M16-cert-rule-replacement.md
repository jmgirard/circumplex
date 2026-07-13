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

- [ ] **T1** — Assemble the RB seed: collect `ssm_ci_accuracy()`
      false-certification output across the §4 amplitude ladder and ≥2 score
      metrics (correlation vs raw), tabulating how the current rule behaves;
      draft the candidate rule (relative-to-CI-width leading family) and a
      proposed false-certification target. `(RB tripwire: no-oracle | irreversible-api)`
- [ ] **T2** — Escalate via `/milestone-brief` (RB → RR) to decide the rule's
      final form + false-certification target; ingest the RR and record a
      `cairn/DECISIONS.md` entry. `(RB tripwire: no-oracle)`
- [ ] **T3** — Test-first: write the print-independence (AC1) and
      scale-invariance (AC2) property tests, red before the change; add a
      regression test superseding the pre-change behavior it replaces.
- [ ] **T4** — Implement the decided rule in `ssm_certified()`
      (`R/ssm_oop.R:122`); drop its `digits` param; update the note call site
      in `print.circumplex_ssm()` (`R/ssm_oop.R:183`).
- [ ] **T5** — Recalibrate/verify via `ssm_ci_accuracy()` (AC4): false-cert at
      `c = 0` hits the target, conditional coverage adequate; update the
      diagnostic's guardrail `digits` arg + threshold-echo text
      (`R/ssm_ci_accuracy.R`, `R/ssm_ci_oop.R`).
- [ ] **T6** — Propagate to the remaining surfaces (verdict wording,
      `Cert_rate`, plot panel), refresh snapshots, add the cross-surface
      consistency test (AC5).
- [ ] **T7** — NEWS entry, docs/pkgdown consistency, full `devtools::check()`
      (AC6, AC7).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

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

## Review
<!-- owner: review · exclusive -->
