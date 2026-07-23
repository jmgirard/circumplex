# M53: Axes-reliability (Strack 2013) — design spec + GO/NO-GO

**Status:** done (2026-07-23, PR #79 https://github.com/jmgirard/circumplex/pull/79)

**Goal:** Turn Strack et al. (2013)'s tau-equivalent CFA axes-reliability model
into a build-ready spec with a validation strategy and a Fable-reviewed GO/NO-GO
on shipping the feature in v2.0.0.

**Outcome:** Docs-only design. Shipped `devel/m53-axes-reliability-spec.md` (the
item-level restricted tau-equivalent CFA: five variance components, fixed
cosine-weight loadings from scale `Angle`, Spearman–Brown reliability/SEm/N–B,
two-layer oracle strategy, standalone `axes_reliability()` API; plan-gate:
standalone fn, octant/type-a MVP, item+instrument input) and the
`cairn/references/strack2013.md` source note (Table 3 = published-value oracle
for the formula layer). Fable RB09→RR09 = **GO**; the axes-reliability **build**
is a GO'd candidate to plan (`Driving RR: RR09`, ingests BC1–BC13; M7 depends then).

**Decisions:** D-025 (plan-time: admits the feature to v2.0.0 as a design→build
path, narrow D-001 supersession). D-026 (GO, six holdings; BC1–BC13 bind the build).

**Review:** 3-lens fan-out + inline scoring. F1 (88) FIXED — `strack2013.md`
Extraction status wrapped across 3 lines, regressed the M47 one-line rule;
joined. F2 (82) NOTE-ONLY (IP4) — RR09 §8 miscites BC3/BC9 (are BC4/BC10),
confined to the archived report, no downstream inheritance. Blame-history clean.
