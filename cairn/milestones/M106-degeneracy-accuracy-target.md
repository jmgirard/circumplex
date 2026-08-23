# M106: Price the degeneracy refusal region on a stated statistical argument

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2, GP4
- **Branch/PR:** —

## Goal

`axes_degeneracy_tau <- 1e-6` (`R/axes_corrected_se.R:398`) caps the relative
error a reported corrected SE may carry at ~1e-5, but that cap is calibrated
numerically only — against machine epsilon and the oracle's measured
`p·κ²·ε` bound — so nothing states what error a reported SE can actually
tolerate and no argument distinguishes τ = 1e-6 from τ = 1e-3. Give τ a stated
statistical footing, set it from that footing, and fence the κ band it governs.
**User-facing tier:** `axes_reliability()` is exported and τ decides whether a
user is handed numbers or NA.

## Scope

**In:** A stated derivation of the largest relative error a reported corrected
SE may carry, priced against the SE's own sampling variability rather than
machine epsilon alone, written beside the constant and restated at the two
exported roxygen sites. τ set from that derivation. Tests across
κ ∈ [1.4e4, 1.4e7] — the band M89's tightening newly refused — straddling the
committed threshold, with p varied. The `cormat` near-duplicate path as a named
case. A superseding `DECISIONS.md` entry if τ moves (D-044 states τ = 1e-6 and
its thresholds in its own text; history is superseded, never edited).

The measured motivation (M89 round-3 review, F3 at 68): an item pair at
r = .9999 fits cleanly, reaches κ(cov2cor(Σ̂)) = 3.3e4, and is refused, though
its error bound is ~5.8e-6. Over the two recorded exemplars, τ ∈ (6e-6, 3e-2)
— less the oracle's factor-of-10 slack — computes the near-duplicate while
still refusing RR18's 3.4%-wrong exemplar (κ = 6.65e6, p = 3). τ = 1e-6 sits
below that window; the derivation, not the window, decides where τ lands, and
τ may legitimately stay where it is.

**Out:** Which matrix is priced (`cov2cor(Σ̂)` vs raw) — settled by D-044, not
reopened. The separate `sqrt(p·ε)` indefinite/`ill_conditioned` band constant →
stays M90's, its own escalation. Making τ user-settable → candidate row if the
derivation shows one target cannot serve every design. Release sequencing →
M7's own gate.

## Acceptance criteria

- [ ] AC1 — The comment block beside `axes_degeneracy_tau` derives the largest
      relative error a reported corrected SE may carry from the SE's own
      sampling relative standard error, stating every premise the derivation
      rests on; each published result it uses is cited `citekey (p. N)`, or the
      block states in so many words that the derivation uses no published
      source. The same target is restated at `R/axes_reliability.R:720` and
      `:1030`. *(RB tripwire: no-oracle)*
- [ ] AC2 — `axes_degeneracy_tau`'s committed value equals AC1's derived target
      or differs from it by at most one decade, with any deviation stated
      beside the constant.
- [ ] AC3 — `axes_sigma_degenerate()` returns `NULL` at 1.05× the committed
      floor and `"ill_conditioned"` at 0.95×, at p = 3, 12 and 24, and across
      three spectral forms at 0.95×: positive λmin, λmin negative but inside
      `-λmax·sqrt(p·ε)` (still `"ill_conditioned"`), and decisively negative
      (`"indefinite"`).
- [ ] AC4 — Through `axes_reliability()`, three κ in [1.4e4, 1.4e7] at three
      different p: one strictly below the committed threshold returns numbers,
      one strictly above returns NA with its reason named, one within a factor
      of 2 of the threshold resolves as AC1's derivation implies. The upper
      cases reach the band through the `axes_fitted_cov` seam, since no
      converged fit is known to (`R/axes_corrected_se.R:444-449`).
- [ ] AC5 — `axes_reliability(cormat = ...)` on an item set carrying one pair at
      r = .9999 returns the outcome AC1's derivation implies, with
      `details$se_correction_failed` and `details$fit_scaling_failed` each
      asserted to the value the derivation states for this input; a second
      near-duplicate radius brackets the committed threshold from the other
      side.
- [ ] AC6 — Over the sites
      `grep -rniE 'tau|accuracy target|double\.eps|kappa' R/ tests/ NEWS.md`
      returns, plus the two comment blocks read whole
      (`R/axes_corrected_se.R:336-398`, `R/axes_scaled_fit.R:245-270`), every
      site stating the criterion's numeric claim carries the committed value;
      one stale value planted per spelling class — numeric literal, prose "tau
      floor", derived κ threshold — is caught by that sweep. `cairn/` is
      excluded deliberately: D-044 is superseded, never edited.
- [ ] AC7 — `devtools::document()` no diff, `devtools::test()` and
      `devtools::check()` clean (0/0/0), per `cairn/PROFILE.md`.

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [ ] T1 — Escalate the accuracy target via `/milestone-brief` (RB): what
      relative error may a reported corrected SE carry, priced against its own
      sampling variability? Ingest the RR. *(RB tripwire: no-oracle)*
- [ ] T2 — Write the derivation beside `axes_degeneracy_tau`; set the constant;
      restate the target at both roxygen sites.
- [ ] T3 — Threshold pins at p = 3/12/24 × three spectral forms; mutate
      `axes_degeneracy_tau` and record each pin's observed failure.
- [ ] T4 — Band cases at three κ / three p through `axes_reliability()`, upper
      cases via the `axes_fitted_cov` seam; committed `data-raw/` generator
      with a seed.
- [ ] T5 — The two `cormat` near-duplicate radii, same generator and seed.
- [ ] T6 — Run AC6's sweep, update every stale site and NEWS.md, then plant one
      stale value per spelling class and confirm the sweep catches each.
- [ ] T7 — If τ moved: superseding `DECISIONS.md` entry against D-044. Then
      `document()`, `test()`, `check()`.

## Work log

- 2026-08-22: created by /milestone-plan. Full criteria audit (user-facing tier) ran on a fresh-context [O] reader and returned 12 findings across 6 criteria; 11 fixed before writing, the AC4 seam question fixed on the code's own recorded ground. Audit also corrected a draft mis-citation of IP4 (the RNG contract, not the never-edit-history convention).
- 2026-08-22: plan gate chose loosening-is-live over assuming τ is already right because the two recorded exemplars leave a ~3-decade τ window in which the near-duplicate computes and RR18's exemplar still refuses; falsified by a derivation whose target lands at or below 6e-6.
- 2026-08-22: plan gate chose deriving τ from the SE's own sampling variability over keeping τ and rewording the refusal, because a refusal message cannot make a defensible analysis computable (GP2); falsified by evidence that every κ in the reopened band is a design defect rather than a usable fit.
- 2026-08-22: plan gate chose pre-build Fable escalation over letting implement offer it, at Jeff's choice, so the constant and its tests are built against a reviewed target; falsified by the RR returning no argument the session could not have reached.

## Decisions

## Review
