# M54: Axes-reliability (Strack 2013) build — `axes_reliability()`

**Status:** done (2026-07-23, PR #80 https://github.com/jmgirard/circumplex/pull/80)

**Goal:** Implement and oracle-validate the M53-designed `axes_reliability()` —
an item-level tau-equivalent CFA reading circumplex axes reliability off ξ1.

**Outcome:** Exported `axes_reliability(data, items, angles, instrument, sd)`
(dual input: instrument map or explicit angle+item map) with a
`circumplex_axes_reliability` S3 object + `print`/`summary`, all in
`R/axes_reliability.R`: fixed axis weights/item_n (snap_trig poles),
Spearman-Brown reliability + SEm, the flat fixed-links `axes_syntax`/`axes_fit`
(reusing `sem_fit_cfa`, `orthogonal=TRUE`), population/simulate helpers, the
Nunnally-Bernstein comparison, and an `axes_ols_shadow` cross-check + start
values. Refuse/boundary/listwise contract (ξ̂1≤0 → NA, never clipped). Bundled
`simulated_items` dataset; NEWS + `_pkgdown.yml`. T11 supplement dropped (SAGE 403).

**Decisions:** No new D-entry (D-025/D-026 govern; RR10 erratum ruling in
`reviews/archive/RR10-…`). Build API gate: both instrument + explicit map;
`sd = "std"|"raw"|numeric`.

**Review:** 3-lens fan-out + scorer (blame-history clean). Fixed: F1 (92,
unguarded lavaan example → `@examplesIf`), F2 (85, print note for identical X/Y
rows); F3 (60, blockwise-ζ2 doc note) sub-threshold → deferred candidate. All 16
ACs verified; check OK; suite 3170 pass.
