# M30: Circumplex coordinate-system design (Fable-reviewed) — DONE

- **Outcome:** ratified design for the coordinate-system rewrite. Docs-only (no
  code); M31 builds it. PR #54 (squash-merged 2026-07-17). Decided the API that
  owns `amax` + the polar transform, resolving the three DESIGN.md viz defects
  (silent `amax` misalignment, no configurable center, theme-frozen canvas).

**Verdict — GO (Option A)** (Fable RB08 → RR08; ratified as **D-019**):
`coord_circumplex()` subclasses ggplot2's `CoordRadial`, so `amax` and the
center become r-scale limits (`rlim`) — the shared-state defect becomes
structurally impossible — and the canvas grid themes natively. The arc geom
simplifies to a coord-bent `GeomRect`.

**Key decisions handed to M31** (full punch-list: `devel/m30-coord-spec.md` §11):
- ggplot2 floor re-pinned **≥ 4.0.0** (not 3.5.0 — the design's params are
  4.0.0-only; verified vs v3.5.2 source). User-approved dependency bump (D-019).
- Seam-straddle unwrap by **extension** (`xmax = xmin + span`) in the arc geom
  `setup_data()`; coord hard-pins `expand = FALSE` + `thetalim = c(0,360)` (pole
  guard); center via `rlim` (`inner.radius` decoupled/0); `amax`/`n` unconditional
  soft-deprecation, never error.
- R4 keep-working set adds `plot.circumplex_cpm` + `plot.circumplex_fit_structure`;
  `repel` branch needs redesign; ggforce likely removable via M31's own D-entry
  superseding the DESIGN.md V6 holding. 8-item boundary-test list (T-i1b…T-r2).

**Deps:** none. **Enables:** M31. RB08/RR08 archived under `cairn/reviews/archive/`.
