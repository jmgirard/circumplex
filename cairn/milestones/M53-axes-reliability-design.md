# M53: Axes-reliability (Strack 2013) — design spec + GO/NO-GO

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m53-axes-reliability-design

## Goal

Turn Strack et al. (2013)'s tau-equivalent CFA axes-reliability model into a
build-ready spec with a validation strategy and a Fable-reviewed GO/NO-GO on
shipping the feature in v2.0.0.

## Scope

**In:** A build-ready design spec (`devel/m53-axes-reliability-spec.md`) for a
circumplex axes-reliability estimator: the restricted tau-equivalent CFA
(item-level, item→axis loadings fixed to cosine weights of each scale's
`Angle`), the five variance components (general/axes/scale/block/item), axes
reliability (Spearman–Brown from the axes variance ξ1) + SEm, the
Nunnally–Bernstein comparison, block handling, and the supported circumplex
type(s). Plus: the proposed exported API surface; the oracle/validation
strategy (no published-data oracle exists); a `cairn/references/strack2013.md`
source note; a Fable escalation (RB09→RR09) and a GO/NO-GO D-entry.

**Out:** Implementation, tests, oracle runs → the axes-reliability *build*
candidate (ROADMAP; planned post-GO, will gate M7). Circumplex types the spec
excludes → named as deferred in the spec. Quasi-circumplex cosine-weight
adaptation (Strack's own future-work) → out; a candidate row if wanted. The
GO/NO-GO itself may retire the whole feature (no build).

## Acceptance criteria

- [ ] AC1 — the spec fully specifies the restricted CFA model (fixed
      cosine-weight loadings derived from scale `Angle`; the five variance
      components; the lavaan engine, reusing the `R/ssm_sem.R` `lavaan::cfa`
      chokepoint pattern) AND the reliability = Spearman–Brown
      `(item_n·ξ1)/(1+(item_n−1)·ξ1)` with `item_n = Σwᵢ²`, SEm = `SD·√(1−Rel)`,
      and the Nunnally–Bernstein comparison — each element anchored to
      `strack2013` (fig/eq/p). `(RB tripwire: ip-touching)`
- [ ] AC2 — the spec specifies a validation strategy meeting the ≥2
      independent-oracle-types bar despite no published-data oracle: synthetic
      recovery of a known ξ1 + a cross-engine lavaan/OpenMx check, including a
      failure-expecting cell (high scale-specificity → N–B overestimates, the
      paper's headline + the M23 lesson). `(RB tripwire: no-oracle)`
- [ ] AC3 — the spec specifies the exported API: function name, signature,
      accepted inputs (item data + instrument/angles, or an item correlation
      matrix + weights), outputs (per-axis reliability, SEm, variance
      components with SEs), and the refuse-don't-coerce contract for
      unsupported inputs (non-circumplex, unequal spacing, missing item→scale
      map) per the M18 lesson. `(RB tripwire: irreversible-api)`
- [ ] AC4 — `cairn/references/strack2013.md` is authored from
      `templates/source-note.md`, INDEX-listed, with the reliability/SEm/N–B
      formulas and Table 3 anchors extracted and a provenance block whose
      extraction status carries its own dated re-check.
- [ ] AC5 — a Fable-reviewed (RB09→RR09) GO/NO-GO on building the feature in
      v2.0.0 is recorded as a D-entry enumerating the load-bearing findings; on
      GO the build candidate is promoted and M7 gains the dependency, on NO-GO
      the feature is dropped/deferred with rationale.
      `(RB tripwire: no-oracle, irreversible-api, ip-touching)`

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1 — Draft `devel/m53-axes-reliability-spec.md`: the restricted
      tau-equivalent CFA (fixed cosine-weight loadings from scale `Angle`; five
      variance components; lavaan engine per the `R/ssm_sem.R:736` chokepoint),
      each element anchored to `strack2013`. Decide + document the supported
      circumplex type(s) — type a / 8-octant primary, others deferred-in-spec.
- [x] T2 — Spec section: reliability (Spearman–Brown from ξ1), SEm, and the
      Nunnally–Bernstein comparison, formulas verbatim-anchored to `strack2013`
      (pp. 3–4).
- [x] T3 — Spec section: the validation/oracle strategy — synthetic ξ1
      recovery + cross-engine lavaan/OpenMx check + a failure-expecting cell
      (high scale-specificity), stating no published-data oracle exists.
- [x] T4 — Spec section: the proposed exported API (name, signature, inputs,
      outputs, refuse-don't-coerce contract).
- [x] T5 — Author `cairn/references/strack2013.md` from the source-note
      template; extract the reliability/SEm/N–B formulas + Table 3 anchors;
      write the provenance block with a dated extraction status; add the INDEX
      line.
- [ ] T6 — Escalate to Fable: write RB09 (self-contained: the model, the
      oracle-absence, the API, the angle-invariant touch), ingest RR09, record
      the GO/NO-GO D-entry with load-bearing findings; on GO promote the build
      candidate and note the M7 dependency add.

## Work log

- 2026-07-23: created by /milestone-plan. Design-first into v2.0.0 (D-025); strack2013 candidate (M48) promoted; build stays a candidate pending GO/NO-GO.
- 2026-07-23 (T1–T4): drafted `devel/m53-axes-reliability-spec.md` — model (§2, 4 Fable points), reliability/SEm/N–B (§3), two-layer oracle (§4, Layer A = published Table-3 formula oracle, Layer B = synthetic+cross-engine ξ1), API (§5). Plan-gate directions: standalone fn, octant MVP, item+instrument input.
- 2026-07-23 (T5): authored `cairn/references/strack2013.md` (born-digital extraction, verified; Table-3 formula-oracle rows + all formulas anchored) + INDEX line; validate green (references index<->disk PASS, staleness OK).
- 2026-07-23 (T6): drafted RB09 (8 questions: model faithfulness, F-1…F-4, oracle sufficiency, API, GO/NO-GO); blocked on RB09 pending Fable review.
- 2026-07-23 (T6): ingested RR09 — verdict GO, BC1–BC13 (promoted D-026). Applied spec §2/§4 corrections; updated the build candidate (GO'd, Driving RR: RR09); archived RB09/RR09. Back to in-progress.

## Decisions

- 2026-07-23 (T6, RR09 ingested): Fable **GO** on building `axes_reliability()`
  in v2.0.0, conditional on BC1–BC13 (promoted to D-026). Design holdings:
  model faithful (the flat implemented form ≡ Figure 2's hierarchical drawing,
  all intermediate paths fixed); identified (moment structure linear in the
  components → parameter-free rank condition, rank 3 with ≥2 items/scale,
  df = p(p+1)/2 − p − 3, verified by exact population recovery); Layer-A
  Table-3 oracle genuine (four anchors reproduced independently); Layer-B needs
  a population-matrix cell (BC5) + `(N−1)/N` handling; the N–B col-14 is NOT a
  printed oracle → own code-independent oracle (BC8). **BC1–BC13 bind the
  BUILD** (its `Driving RR: RR09`), not this design milestone. Spec §2/§4
  corrected; RB09/RR09 archived.

## Review
