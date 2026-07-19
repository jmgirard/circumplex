# Zimmermann & Wright (2017) transcription record (M4/W1)

**Source:** Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
interpersonal construct validation: Methodological advances in the circumplex
Structural Summary Approach. *Assessment, 24*(1), 3–23.
doi:10.1177/1073191115621795. Transcribed from the publisher PDF
(`~/Zotero/storage/K8TJRGT4/zimmermann2017.pdf`, SAGE OnlineFirst pagination
1–21; page numbers below use the PDF's printed pages).

**Protocol (Brief A §6.1 two-session rule, B6 precedent):**
Channel 1 = visual page read of the rendered PDF (Fable, 2026-07-07).
Channel 2 = independent `pdftotext -layout` text-layer extraction, diffed
against channel 1 on every load-bearing numeral (same date). One
discrepancy found and resolved: the text layer renders Eq. A7's radicand as
`2(ρ1−ρ3)+(1−ρ4)` where the page image shows `√2(ρ1−ρ3)+(1−ρ4)`; the √2
form is confirmed correct because only it reproduces all five published
scaling factors (see cross-validation below). **That resolution is no longer
inferential — Jeff confirmed Eq. A7 on the page 2026-07-19, both the √2
radicand and the leading ½** (the ½ had never been checked by any pass).

**Second independent human re-read: COMPLETE (Jeff), 2026-07-19.** Every
value in this record was checked against the primary source, and **none was
found wrong.** Also confirmed: Eq. A7 including its leading ½ (above), and
the vignette's characterizations of Studies 1–2 as faithful rather than
overclaiming. Two things changed as a result, neither a transcribed value:
the Study 2 threshold-source question below was resolved from Figure 5, and
the vignette's accuracy-table header was rescoped to say *bootstrap* CI.
Worksheet: `devel/m7-transcription-reread-checklist.md` section B.

**Threshold double-statement, resolved by Figure 5 (2026-07-19).** The paper
states the amplitude and displacement thresholds twice with different numbers:
Study 2 *Results* (p. 10) gives amplitude n ≥ 75 / n ≥ 150 and displacement
n ≥ 100 / n > 200, while the Study 2 *Discussion* on the same page summarizes
both together as 100/200. **Figure 5 (p. 12) supports the Results.** Reading
each panel for the smallest n at which the worst-case A = .10 curve falls
below the 2.5% band: Panel B (amplitude) crosses at 150 without GF and 75 with
GF; Panel C (displacement) is still above the line at n = 200 without GF, and
sits on it at n = 100 with GF. So the Discussion's 100/200 is **Panel C's
displacement thresholds applied to both parameters** — a conservative
simplification, not a competing measurement. The figure also accounts for the
authors' "greater than 200" and their "close to or dropped below" hedge.
This record and the vignette follow the Results; keeping that is the
recommendation. Values read off the rendered figure, not published (no
per-condition deviances exist — see the no-supplement finding below), so the
near-threshold with-GF displacement point at n = 100 is not precise.

**Internal cross-validation (transcription self-checks, all reproduced):**

- f_e = √((2ρ1+2ρ2+2ρ3+ρ4+1)/8) (Eq. A6, p. 18): IIP-C parameters → .737,
  IAS parameters → .240; both match the ratios quoted on p. 9.
- f_a = ½·√(√2(ρ1−ρ3)+(1−ρ4)) (Eq. A7, p. 18): IIP-C → .545, IAS → .845
  (p. 9); IIP-SC (Study 5 parameters) → .625 (p. 14).
- Eq. 3 (p. 12): |AFF_min| = 2.95·f_a·n^(−0.587) reproduces the paper's own
  worked values: f_a=.545, n=100 → .108 ("as large as .11", p. 12); n=1000
  → .028 (".03", p. 12); f_a=.625, N=1166 → .029 ("|.029|", p. 14).

## Study designs (transcribed)

**Common generating process (Study 1, p. 5–6):** population of 100,000
cases, eight octant scales + one target measure, **distributions assumed
multivariate normal**; octant intercorrelations follow the circulant
pattern of Table 1 (p. 2; parameters ρ1 > ρ2 > ρ3 > ρ4); the target
measure's correlations with the octants have known elevation, amplitude,
angular displacement, and goodness of fit. Octant angles (Figure 1A, p. 3):
LM = 0°, NO = 45°, PA = 90°, BC = 135°, DE = 180°, FG = 225°, HI = 270°,
JK = 315°. 5,000 samples drawn per condition.

**The two population octant matrices (Note 3, p. 18):** model-based
("reproduced") correlations from SEM analyses of real instruments —
- without a substantial general factor: ρ1 = .430, ρ2 = .030, ρ3 = −.360,
  ρ4 = −.740 (IAS, 2,988 students; Gurtman & Pincus, 2000; Wiggins, 1995);
- with a substantial general factor: ρ1 = .683, ρ2 = .500, ρ3 = .345,
  ρ4 = .288 (IIP-C, 1,981 students; Gurtman & Balakrishnan, 1998).

**Study 1** (point-estimate bias/precision, pp. 5–9): n ∈ {25, 50, 75, 100,
150, 200, 500}; 2 octant matrices; 80 target measures varying E ∈
{.0,.1,.2}, AFF ∈ {.0,.1,.2}, DOM ∈ {.0,.1,.2}, R² ∈ {.6,.7,.8,.9,1.0};
1,120 simulations total. Note 4 (p. 18): varying AFF/DOM ≡ varying
amplitude A ∈ {.10,.14,.20,.22,.28}; A = 0 excluded (goodness of fit
undefined) but examined separately (Figure 3, p. 7). R 2.12.2 on a Linux
cluster (p. 6).

**Study 2** (bootstrap CI coverage, pp. 9–11): same conditions; per sample,
95% percentile bootstrap CIs from **2,000 bootstrap replicates** for e,
aff, dom, a, δ, R². Accuracy = empirical coverage within Bradley's (1978)
liberal criterion **[92.5%, 97.5%]**.

**Study 3** (accuracy frontier for a and δ, pp. 11–13): 22 sample sizes
from n = 50 to 1,000; 57 affiliation values from .02 to .30 in steps of
.005; both octant matrices; **elevation and dominance fixed to 0, goodness
of fit fixed to 1**; 2,508 simulations. Judged accurate only when BOTH
amplitude and displacement coverage were in [92.5, 97.5]. Note 6 (p. 19):
varying AFF vs DOM arbitrary; holding one at 0 is the most stringent test.

**Study 4** (differences between profiles, p. 13): three difference types
(two uncorrelated measures; two correlated measures, ρ = .5; one measure in
two independent samples); n ∈ {50, 100, 400}; AFF₁ ∈ {.05, .12, .18};
AFF₂ ∈ {AFF₁, 0, −AFF₁}; both profiles E = 0, DOM = 0, R² = 1; GF matrix
only (Note 4 case); 81 simulations.

**Study 5** (real data, pp. 13–16): N = 1,166 undergraduates; IIP-SC
octants as circumplex, PDQ-4+ PD scales as target measures.

## Headline results (transcribed)

**Study 1 (pp. 6–9):** bias in e/dom/aff very small (average −.0007; most
extreme −.013). Amplitude bias substantial and consistently positive:
.0002–.136, average .021; relative bias average 15.5%, range 0–135.8% —
sample amplitude overestimates population amplitude under most conditions;
at n = 50 without a general factor, E(â) = .153 when A = 0 (p. 6). Bias in
δ trivial (avg −.03°, range −2.38° to 3.03°). Goodness of fit
underestimated (avg −.081, range −.382 to .027; relative −9.5%). Precision
of e/aff/dom/a is a function of n and general-factor presence only
(Table 2, p. 8); SE of δ additionally depends on amplitude — e.g. SE_δ ≈
50° at n = 100, no GF, A = .1; ≈ 10° at n = 500 with GF and A = .14
(p. 8). A general factor reduces the precision of elevation but increases
the precision of dom/aff/amplitude (p. 9). "In many cases samples larger
than 500 persons will be needed to achieve sufficient precision in the
determination of the interpersonal theme of a target measure" (p. 9).

**Study 2 (pp. 9–11):** coverage for e/aff/dom is a function of n only:
accurate (Bradley band) from **n ≥ 50** (mean deviance ≈ 3% at n = 25,
≈ 1.7% at n = 50, ≈ 0.3% at n = 500). Amplitude: accurate from **n ≥ 75
with** a general factor, **n ≥ 150 without**. Angular displacement:
accurate from **n ≥ 100 with** a general factor, **n > 200 without**
(pp. 10). Goodness of fit: coverage 0 when R² = 1 (boundary); accurate
only when population R² < .9 — "the bootstrap method seems unsuited for
computing CIs for goodness of fit" (p. 11). All amplitude/displacement
recommendations presume A ≥ .1 (Study 2 Discussion, p. 10).

**Study 3 (pp. 11–13):** minimum required affiliation |AFF_min| =
2.95·f_a·n^(−0.587) (Eq. 3, p. 12; log-log r = −.994), f_a from Eq. A7.
With a GF (f_a = .545): n = 100 needs AFF ≥ ~.11; n = 1,000 needs ~.03.
Recommendation: report the "probability of accurate CIs" (share of
bootstrap resamples in which aff or dom exceeds the minimum), interpret
a/δ CIs only when it is ≥ .50, fully trustworthy ≥ .95 (pp. 12–13, 16).

**Study 4 (p. 13):** difference CIs for e/aff/dom accurate under all
conditions (mean deviances 0.9%, 1.0%, 0.8%). Amplitude/displacement
difference CIs consistently inaccurate when AFF₂ = 0 (mean deviance
15.5%). Under remaining conditions, δ-difference CIs accurate at n = 400 &
AFF₁ = .05 (1.1%), n = 100 & AFF₁ = .12 (0.8%), n = 50 & AFF₁ = .18
(1.0%); a-difference CIs accurate at n = 100 & AFF₁ = .12 (2.0%) and
n = 50 & AFF₁ = .18 (1.5%) but just failed at n = 400 & AFF₁ = .05 (2.9%,
CIs too wide → conservative, Type II direction).

**Study 5, Table 4 rows used by the vignette (p. 15):** structural summary
statistics with 95% CIs for PD scales at N = 1,166 —
- Paranoid: e = .250 [.218, .280], aff = −.094 [−.129, −.060],
  dom = .117 [.080, .152], a = .150 [.115, .189],
  δ = 128.9° [116.7°, 141.6°], R² = .802, Prob = 1;
- Obsessive–compulsive: e = .228 [.193, .261], aff = .011 [−.021, .041],
  dom = −.005 [−.038, .032], a = .012, δ = 337.4°, R² = .117, Prob = .130
  (no CI brackets printed for its a and δ in the table — **confirmed
  2026-07-19 to be a deliberate omission by the authors, not a transcription
  gap**: the trigger is its low **Prob = .130**, not its low R². Table 4's own
  note defines Prob as "probability of accurate confidence intervals for
  amplitude and angular displacement" — precisely the two parameters whose
  CIs are withheld — and the authors' own rule is not to interpret a/δ CIs
  when the probability estimate is < .50 (pp. 12–13, 16). R² = .117 is a
  separate quantity and does not govern which intervals are printed).
*Cross-check:* `ssm_analyze(jz2017, PANO(), measures = c("PARPD","OCPD"))`
on the bundled data (the same sample) reproduces every point estimate of
both rows to the table's printed rounding (e/x/y/a/δ/R²; verified
2026-07-07).

**Study 5 (pp. 13–16):** IIP-SC CircE fit: equal spacing + equal
communality rejected (CFI = .824, TLI = .795, RMSEA = .169); unequal
spacing acceptable (CFI = .958, TLI = .931, RMSEA = .098). Model-based
IIP-SC parameters ρ1 = .580, ρ2 = .323, ρ3 = .134, ρ4 = .070 → f_a = .625;
at N = 1,166, AFF or DOM > |.029| suffices (probability estimates > .95
for all PD scales except obsessive–compulsive, Table 4, p. 15). General
discussion (pp. 16–17): rule of thumb — don't interpret a/δ CIs when the
probability estimate is < .50; n ≈ 300 for an ~84% chance of meeting the
stringent criterion when AFF/DOM ≥ |.15| with an IIP-C-like instrument;
n = 120 for the same chance of a > .50 estimate.

**Ipsatization (p. 4):** ipsatizing IPC octant scores before correlating
them with a target "discards any information on elevation" — an example of
research missing information by not using the full SSM.

**Named future-work items (p. 17–18):** accuracy under binary/nonnormal
targets, non-circumplex populations; BCa bootstrap, jackknife, and Monte
Carlo methods as alternatives (the MC method "would work with information
on means, standard deviations, and intercorrelations"); measurement-error
extension.

**Coincidence flagged for the pending re-read:** the value 15.5% appears
twice above for two *different* quantities — Study 1's average relative
amplitude bias (p. 6) and Study 4's mean deviance when AFF₂ = 0 (p. 13).
Both were independently confirmed in the channel-2 text layer (distinct
passages); noted here so the duplicate value is not mistaken for a paste
error, and for explicit attention in Jeff's re-read.

## F8 grid-characterization re-confirmation (spec §2, logged here)

The spec's from-memory case against a lookup module described the Z&W grid
as "coarse and fixed — specific instruments, specific n values, specific
population structures". **Confirmed at transcription:** exactly two
population octant matrices (IAS-like, IIP-C-like; Note 3); n restricted to
7 values (Studies 1–2), a 50–1,000 grid (Study 3), or {50,100,400}
(Study 4); target profiles restricted to E, AFF, DOM ∈ {0,.1,.2} × R² ∈
{.6,...,1.0}; octant angles only; one engine (95% percentile bootstrap,
2,000 replicates); contrasts only in Study 4's three difference types.
All correlation-path (target-measure) profiles — no mean-based group
profiles, no Monte Carlo engine, no non-octant angle sets, no user-chosen
`boots`. The characterization stands.

## No supplement exists — O5 bridge re-scope (spec §10, F7 mechanism)

The spec assumed "(+ supplemental materials — the sole source for all
Study 1–5 values)". **Transcription finding: the article has no
supplemental materials.** The text cites only the second author's website
(http://www.personalityprocesses.com/ssm/) for the R package and Study 5
example data; Europe PMC (PMID 26685192) records `hasSuppl: no`; the SAGE
suppl endpoint returns no material. Consequently **per-condition coverage
values were never published** — the published record is: aggregate mean
absolute deviances, Bradley-band accurate/inaccurate classifications,
threshold statements, and the Eq. 3 frontier.

**Re-scope (documented, not loosened):** the generating process *is*
MVN-expressible (explicit in the paper), so the simulator-compatibility
gate passes and the bridge runs. The comparison target changes from
"published coverage value ± combined MC error" (impossible: no published
per-condition values) to the sharpest published anchors: (i) at a Study 3
condition safely inside the published accurate region, the diagnostic's
e/a/d coverage (Wilson 95% CI) must be consistent with the Bradley band
[92.5, 97.5]; (ii) at a condition safely below the Eq. 3 frontier, a
and/or δ coverage must fall outside the band, i.e. the diagnostic must
reproduce their accurate/inaccurate classification; (iii) elevation
coverage at n ≥ 50 must be band-consistent (Study 2 threshold). Executed
in `devel/m4-zw-bridge.R` (seeded, committed with results).

## Change log

- 2026-07-07 — Initial transcription (Fable, W1). Channels 1+2 diffed; one
  text-layer artifact resolved by cross-validation (Eq. A7 √2). F8 grid
  characterization confirmed. No-supplement finding recorded; O5 bridge
  re-scoped per the F7 mechanism. Jeff's independent re-read pending.
- 2026-07-19 — Correction to the entry below, same day: the OCPD a/δ CI
  omission is triggered by its low **Prob (.130)**, **not** by its low R²
  (.117), which that entry wrongly gave. Table 4's note defines Prob as the
  probability of accurate CIs for amplitude and angular displacement — the
  two parameters actually withheld — and the authors' < .50 rule (pp. 12–13,
  16) is what applies. Corrected in the Table 4 block above.
- 2026-07-19 — Second independent human re-read begun (Jeff, M7 T3).
  Eq. A7 confirmed on the page including its leading ½, closing the one
  channel discrepancy that had been settled by reasoning; Note 3 matrices,
  Table 4 rows, and Figure 1A angles confirmed; OCPD's absent a/δ CIs
  confirmed as the authors' deliberate omission (R² = .117), a reason this
  record did not previously carry. The twice-occurring 15.5% re-confirmed on
  its two pages (6 and 13). New open question logged in Protocol above: the
  Study 2 Results and Discussion state the amplitude/displacement thresholds
  with different numbers. Re-read not yet complete.
- 2026-07-07 — Review fix (W1 /code-review): Table 4 PARPD/OCPD rows added
  with provenance (the vignette cites OCPD's a = .012, which was previously
  not traceable to this record) plus the jz2017 reproduction cross-check;
  the twice-occurring 15.5% flagged as a verified coincidence for the
  pending re-read.
