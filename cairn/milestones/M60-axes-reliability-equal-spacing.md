# M60: Any equally spaced angle set for `axes_reliability()`

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m60-axes-reliability-equal-spacing`

## Goal

Let `axes_reliability()` estimate from any equally spaced set of scale angles at
any rotation, instead of only the canonical octant set.

## Scope

**In:**
- Replace the `octants()` set-identity refusal (`R/axes_reliability.R:485-506`)
  with a genuine modular equal-spacing predicate: k ≥ 4 scales, constant
  successive gap of 360/k including the wrap-around gap, tolerance-based,
  pole-aware (LM = 360 ≡ 0). Unequal spacing, duplicate angles and NA angles
  stay refused with a message naming the offender. k = 3 is refused as
  *unidentified*, not merely unsupported: three equally spaced scales give every
  cross-scale pair the same cos Δ = −0.5, collapsing the moment-structure design
  (cos Δ, 1, same-scale) from rank 3 to rank 2 at any number of items per scale
  (measured 2026-07-25; D-026 holding 2). The spacing tolerance admits
  floating-point representation error only, never a near-equal (quasi-circumplex)
  set — RR09 §4.
- The ≥ 2-items-per-scale refusal (`:507-510`) stays, so ζ1 remains identified.
- Pin the rotation-invariance that keeps the equal-axis-variance restriction
  substantively innocuous: per-axis Σw² = k/2 at any rotation, for every
  accepted k.
- Oracles: the four CV-LI type-b rows of Strack Table 3 (Layer A), plus the
  existing population-matrix / synthetic-recovery / cross-engine cells re-run at
  a rotated and a non-octant configuration (Layer B).
- D-entry admitting this to v2.0.0 (narrow D-001 supersession, D-030 pattern);
  roxygen, `man/`, vignette, NEWS.

**Out:**
- One-item-per-scale positions (Strack types e/f, ζ1 dropped) → M61.
- Blockwise ζ2 (Strack type d) → ROADMAP candidate row; blocked on a data-model
  decision, since no bundled instrument records block membership
  (`R/instrument_oop.R:1-11`).
- FIML on items → ROADMAP candidate row.
- Unequal spacing / quasi-circumplex — stays refused; RR09 §4 holds the refusal
  is scope-correct, and nothing here touches it.

## Acceptance criteria

- [ ] AC1: `axes_reliability()` estimates a rotated equally spaced set (the
      type-b interstitial angles 22.5…337.5) and a k ≠ 8 set (k = 6 and k = 12),
      returning finite equal per-axis reliability; the octant-only error no
      longer fires.
- [ ] AC2: each refusal still fires with a message naming the offender —
      unequal spacing, duplicate angle, NA angle, k < 4 (naming identification
      as the reason at k = 3), and < 2 items on any scale.
- [ ] AC3: the spacing test is modular — a set expressed with LM = 360 is
      accepted identically to the same set using 0, and a set carrying both 0
      and 360 is refused as a duplicate.
- [ ] AC4: per-axis item_n equals `n_items × k/2` for every accepted
      configuration at any rotation, at a tolerance set from the discrimination
      required rather than from one machine's printed value (M59, M20).
- [ ] AC5: Layer A — all four CV-LI type-b rows of Strack (2013) Table 3
      (%axes 3.5 / 2.7 / 1.9 / 7.6 at item_n 16 → .37 / .31 / .24 / .57;
      `strack2013` p. 7) reproduce within ±.01.
- [ ] AC6: Layer B at a rotated-octant and a non-octant configuration —
      population-matrix recovery exact to numerical tolerance, synthetic
      recovery, and cross-engine lavaan/OpenMx agreement.
- [ ] AC7: no doc still claims octant-only — roxygen, regenerated `man/`,
      vignette and NEWS updated, and the type-b oracle rows banked in
      `cairn/references/strack2013.md` with a provenance re-verification mark.
- [ ] AC8: `devtools::check()` clean and the PDF manual actually built
      (`R CMD Rd2pdf`; `check()` skips it by default — M7/M57).

## Coverage

- AC1 → T1, T3
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T5, T7
- AC8 → T8

## Tasks

- [x] T1: tests first — a rotated type-b set and k = 6 / k = 12 sets currently
      error at `R/axes_reliability.R:495-506`; pin that as the failing fence,
      and pin every refusal that must survive
      (`tests/testthat/test-axes-reliability.R:442-476`).
- [x] T2: extract a modular equal-spacing predicate (pole-aware, wrap-around
      gap, tolerance-based); no such helper exists anywhere in `R/`. Unit-test
      at the pole and against near-miss spacings.
- [x] T3: replace the refusal block with count / spacing / duplicate checks and
      informative messages; keep the ≥ 2-items-per-scale gate.
- [x] T4: rotation-invariance tests for `axis_item_n()`
      (`R/axes_reliability.R:31-34`) — `n × k/2`; keep the exact-equality octant
      assertion at `test-axes-reliability.R:5` exact, since exactness there is
      an octant accident (16 scales at 22.5° measured `y = 31.999999999999996`).
- [x] T5: bank the four CV-LI type-b Table 3 rows in
      `cairn/references/strack2013.md` and add the ±.01 sweep test.
- [x] T6: re-run the population-matrix, synthetic-recovery and cross-engine
      OpenMx cells at a rotated-octant and a k ≠ 8 configuration.
- [ ] T7: roxygen (`R/axes_reliability.R:316-317,343,395`), `man/` regeneration,
      vignette and NEWS.
- [ ] T8: D-entry for the v2.0.0 admission; full check plus the PDF manual.

## Work log

- 2026-07-25: created by /milestone-plan.
- 2026-07-25: in-progress on `m60-axes-reliability-equal-spacing`.
- 2026-07-25: amended Scope + AC2 at the implement gate — scale-count floor 3 → 4, measured: the moment design (cos Δ, 1, same-scale) is rank 2 at k = 3 for any items/scale, rank 3 from k = 4. Jeff accepted.
- 2026-07-25: implement gate — spacing tolerance is float-representation only, never near-equal spacing (Jeff; keeps RR09 §4's quasi-circumplex refusal intact).
- 2026-07-25: T1 fence added (4 test blocks, rotated/k!=8/refusals/pole); red for the right reason — the octant-only refusals at :485-506, not an incidental error. Suite stays red by design until T3.
- 2026-07-25: T2 `angles_spacing_status()` (modular, tol = 1e-8 = float noise only) + 15 unit assertions incl. k = 4:24 at two rotations; T3 wired it into the refuse contract and re-pointed the three stale BC12 message expectations. Suite green (232).
- 2026-07-25: mutation-tested the 5 guards. 4 had teeth (floor→3: 1 fail; tol 1e-8→0.5: 3; skip duplicate: 6; accept any spacing: 7). The `%% 360` reduction did NOT redden — for in-range sets the wrap term compensates; it bites only on angles outside [0, 360), now asserted (`c(10, 100, 190, 640)`), and the mutation reddens. Full suite 3394 pass / 0 fail; the 4 warnings are pre-existing in test-ci_accuracy.R.
- 2026-07-25: T4 item_n = n*k/2 pinned over k = 4:16 x 4 rotations x 3 item counts, plus an unbalanced set giving legitimately unequal per-axis values; stale octant-only comment on axis_item_n() corrected.
- 2026-07-25: T5 banked Table 3's `Type` column + the four type-b (CV-LI) and one type-c (MEIL) rows in `references/strack2013.md`, re-verified in two channels (text layer + 200-dpi page image) agreeing on every value; provenance status re-marked inline. Sweep test added, incl. a discrimination check that the wrong item_n fails it. MEIL's components sum to 74.4 (a second source defect, RR10-corroborated) so it anchors reliability only, never a sum guard.
- 2026-07-25: T6 Layer-B at 4 new geometries (type-b rotated octants, k=6, k=12, k=5 at a 13.7° rotation): exact population recovery + chisq<1e-6, MC recovery within 2 MCSE at the rotated set, lavaan/OpenMx agreement <1e-3. Also hardened two test helpers whose `split()` on a numeric group vector would misorder scales at k>=10 — a no-op at k=8, and the production path (`axes_resolve_map()`) was already positional and safe.
- 2026-07-25: refusals use `stop(call. = FALSE)` matching all 26 in the file; the profile's cli_abort slot would need cli as a new dependency (gate + D-entry) for no gain.

## Decisions

- 2026-07-25 (T6): a GLOBAL rotation of the syntax weights is undetectable by any oracle here, and correctly so — Σ depends only on cos(θi−θj), which is invariant to adding a constant to every angle, which is the model's own "no preferred rotation" axiom (p. 4). Weight mutations must break RELATIVE geometry to be valid; perturbing one scale's angle reddens 19 tests, perturbing all of them reddens none. Recorded so a later session does not read the null result as missing coverage.

- 2026-07-25 (T5): `cairn_validate`'s sizing advisory flags M60 and M61 at 8 acceptance criteria (>7 tripwire). Not split: the 8th in each is the template-mandated "profile verify/check clean" criterion every code milestone carries, so the substantive count is 7. Splitting on a hygiene criterion would cut a coherent unit for a counting artifact.

- 2026-07-25 (T4): non-octant item_n is compared with a tolerance set from discrimination (1e-8 absolute; one item = 1.0, so it fences at 1e8x and sits ~6 orders above the ~1e-14 float noise). The octant `expect_identical()` in BC3 stays exact — exactness there is a real property of that set, not an artifact to weaken.

- 2026-07-25 (T2): the wrap-around gap in `angles_spacing_status()` is kept for symmetry with the modular reading but is NOT load-bearing — all gaps sum to 360, so k-1 interior gaps of 360/k force it. Mutation-verified: removing the term changes no test. The comment says so rather than claiming a mechanism it does not have (M36).

## Review
