# M36: Visualization polish — certification legend key + non-finite guards — done

**Goal:** close the two shipped-code remainders of the M31–M33 visualization
track. PR #60 (squash 6d912a36), merged 2026-07-18.

**Outcome:** `ssm_plot_trajectory()`'s "Displacement interpretable" legend now
draws both keys (● TRUE, ○ FALSE) instead of rendering `FALSE` as a label with
no glyph whenever the data held no uncertified point — the common case on a
model-based `ssm_draws()` trajectory. One shared site, so the occasions and
table paths recover together. `coord_circumplex()`'s `amax`/`center` now reject
NA/NaN/±Inf at call time naming the argument, extending M32's `r_axis_angle`
treatment. New helper `legend_key_glyphs()` reads rendered key grobs.
Suite 2903 passing; check 0/0/0; 9/9 CI green.

**Key decisions:**
- Plan gate: the legend always shows both keys, documenting the encoding even
  where nothing is hollow (M33's intent in setting `drop = FALSE`).
- The fix is `show.legend = TRUE` on the existing layer. `override.aes` cannot
  reach a keyless break; `alpha = 0` and zero-row presence layers work but are
  inert. The zero-row version shipped to review and was removed there (F1).
- Two coord assertions pinning incidental message text were rewritten to assert
  argument name + "finite"; coverage strictly increased.

**Review:** 2 findings, both fixed (F1 inert presence layer + false comment, 92;
F2 stale superseded comment, 85). None below 80; other two lenses clean.
