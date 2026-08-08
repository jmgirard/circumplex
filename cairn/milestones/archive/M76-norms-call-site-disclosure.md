# M76: Disclose the reference sample at the standardizing call site

**Status:** done (2026-08-08, PR #104 https://github.com/jmgirard/circumplex/pull/104)

**Goal:** Make `norm_standardize()` say which normative sample it standardized against.

**Outcome:** `norm_standardize()` gains `quiet` (default `FALSE`); every non-quiet call
emits a `message()` naming the sample number, its `Size` and its `Population`, plus a
count of other samples — that count from a new internal `norm_sample_usable()`, which
shares the anchor-range predicate with D-040's refusal so it never advertises a sample
the function refuses. Every return carries `attr(x, "norm_sample")` (Instrument, Sample,
Size, Population) on both `append` paths, message or not; `Size`/`Population` are keyed
on `Sample`, not row position. Two refusals fixed: out-of-anchor-range now reads `Scale`
or `Abbrev` (it named no scale on the seven `Abbrev`-labelled instruments), and an
unmatched `sample` — `NA` included, via `which()` — gets its own error naming the
argument and the valid numbers. A pin fails if `norm_standardize`, `norms`, or
`Population` is renamed; 4 NEWS entries; `man/norm_standardize.Rd` regenerated.

**Decisions:** D-041 (keep the reference-statistics vocabulary; RR16 R1/R5/R6). Local:
the message prints the stored label as a plain description (RR16 R2/BC2); RR16 BC3's
per-sample kind field routed to a ROADMAP candidate, not this scope.

**Review:** Three lenses + scorer, 22 candidates. Returned once (defect return 1) on F1
(90, the other-samples clause counted refused samples) and F4 (85, `NA_real_` bypassed
AC4's refusal); F2 (88), F10 (80), F16 (88), F9 (78) fixed too. 17 logged below 80.
