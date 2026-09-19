# Forward-looking source prospects — shelved circumplex sources (M46; wester2026 added 2026-09-19)

**Provenance.** Ingested 2026-07-20 by M46 from a survey reading of four PDFs on
the gitignored shelf (`cairn/references/sources/{nagy2019,weide2021,rogoza2021,tracey2000}.pdf`),
read read-only. No repo `references/` page was an input; this is a supply-push
triage of sources the repo does not yet rely on.
Pagination: —.
Extraction: first-hand survey reading of the four shelf PDFs; no verbatim values banked, nothing to re-verify against — observed 2026-07-20.

**Scope.** This is **not** a source note — it owns no single citekey and extracts
no values. It is a supply-push exploration record (tracking-rules "Exploring
prospective sources"): a triage of four sources deliberately shelved as future
research material, characterizing what each is and what it could seed, so a
later planner starts warm. It builds nothing — no feature, no oracle, no
value extraction — and confers no reliance: a per-source `<citekey>.md` page is
owed only if and when a milestone comes to rely on one (the M45 bar; D-024).
This is a reference, not an authority — status lives in `ROADMAP.md`, decisions
in `DECISIONS.md`, architecture in `DESIGN.md`.

**Evidence snapshot.** Each source read at survey altitude (abstract, front
matter, section headings), not line-by-line:

- nagy2019 — `cairn/references/sources/nagy2019.pdf` (25 pp) — observed 2026-07-20.
- weide2021 — `cairn/references/sources/weide2021.pdf` (14 pp) — observed 2026-07-20.
- rogoza2021 — `cairn/references/sources/rogoza2021.pdf` (9 pp; the shelf copy is an accepted proof, printed volume/page shown as placeholders) — observed 2026-07-20.
- tracey2000 — `cairn/references/sources/tracey2000.pdf` (24 pp; an Acrobat Paper-Capture OCR scan — a single text channel, M42-D1) — observed 2026-07-20.
- wester2026 — `cairn/references/sources/wester2026.pdf` (15 pp + front matter; born-digital, `pdftotext` clean) — added to the shelf by Jeff 2026-09-19 after a SITAR reader sent it; read in full (not survey altitude) the same day.

## What these four sources are

Neutral characterization, before any prospect judgment.

**nagy2019** — Nagy, Etzel & Lüdtke (2019), *Integrating covariates into
circumplex structures: an extension procedure for Browne's circular stochastic
process model*, Multivariate Behavioral Research, 54(3), 404–428,
doi:10.1080/00273171.2018.1534678. An SEM extension of Browne's (1992) circular
stochastic process model that models covariates' relationships with a *latent*
circumplex simultaneously, without shifting the circumplex indicators' angular
positions; it extends Browne's Fourier-series correlation function to each
covariate's correlation profile and derives interval estimates for the
covariate-to-circumplex parameters. Evaluated with a simulation study and an
interpersonal-circumplex × narcissism (rivalry/admiration) application.

**weide2021** — Weide, Scheuble & Beauducel (2021), *Bayesian and
Maximum-Likelihood Modeling and Higher-Level Scores of Interpersonal Problems
With Circumplex Structure*, Frontiers in Psychology, 12, 761378,
doi:10.3389/fpsyg.2021.761378. Bayesian vs. ML confirmatory factor analysis of
the IIP's three-factor structure (two circumplex factors, Dominance and Love,
plus a general Distress factor) on a non-clinical sample (N = 822), using
circumplex loading priors, with higher-level factor scores and external
criteria (Big Five, subclinical grandiose narcissism).

**rogoza2021** — Rogoza, Cieciuch & Strus (2021), *A three-step procedure for
analysis of circumplex models: An example of narcissism located within the
circumplex of personality metatraits*, Personality and Individual Differences
(article 109775), doi:10.1016/j.paid.2019.109775. Proposes a three-step
workflow: (1) verify circumplex structure via SEM; (2) locate external
variables via **Structural Summary Method** profiles; (3) test congruence of
empirical vs. theoretical locations via Procrustes rotation. Worked on
narcissism within the Circumplex of Personality Metatraits; ships pedagogical
tutorials.

**tracey2000** — Tracey (2000), *Analysis of Circumplex Models* (ch. 22), in
*Handbook of Applied Multivariate Statistics and Mathematical Modeling*
(pp. 641–664), Academic Press, doi:10.1016/B978-012691360-6/50000-8. A survey
chapter on evaluating circumplex models: it distinguishes how the circumplex is
operationalized (circulant vs. quasi-circumplex) and demonstrates several
analytic tools for testing circular structure, citing Hubert & Arabie (1987)
order tests among them — adjacent to the `fit_structure()` family.

**wester2026** — Wester, Etzel, Zimmermann, Hanraths, Iovoli, Hall & Rubel
(2026), *A Latent Structural Comparison Between the Circumplex Scales of
Interpersonal Problems (CSIP) and the Inventory of Interpersonal Problems
(IIP-C)*, Journal of Personality Assessment, 108(5), 657–671,
doi:10.1080/00223891.2026.2614580. An applied use of nagy2019's SPMC-E: a
German CSIP translation and the German IIP-C in a community sample
(N_T1 = 677, N_T2 = 514, one month apart), each instrument fit separately to
Browne's quasi-circumplex (CSIP best at m = 2, IIP-C at m = 3), then each
instrument's scales and two clinical covariates (dysphoria, personality
dysfunction) projected onto the *other* instrument's latent circumplex. It
reports the SPMC-E triple per projected variable (γ0 level, γ1 content
sensitivity, δ angle with CI), the Etzel, Holland & Nagy (2021) RMSECA
alignment index (≤ .08 = good) as the latent analogue of the SSM's R², the
β0-based level/shape variance split, and Fisher et al.'s (1985) A* congruence.
Substantive findings: both instruments fit a latent quasi-circumplex with
mean absolute angular deviations from theory of 9–13°; IIP-C LM and JK sit
15–21° apart versus 35–37° in the CSIP; the CSIP's dominance-pole scales (BC,
PA, NO) span ~78° versus ~119° in the IIP-C; dysphoria and personality
dysfunction show a submissive / submissive-cold style on the CSIP but no
interpersonal specificity on the IIP-C. Mplus 8.10, robust FIML; data on OSF.

## Prospect ledger — seedable oracles / methods / references

Tag vocabulary (the *kind* of prospect each is, the axis a later planner sorts
on): `oracle` · `method` · `reference` · `feature`. Each row's disposition (its
`candidate` ROADMAP row) is in the next section; IDs are stable — cite E1–E4,
never renumber.

| # | Source | Kind | Could seed | A taking-up milestone must verify |
|---|---|---|---|---|
| E1 | nagy2019 | feature / oracle | A covariate-extension for the CPM engine (`cpm_fit()`), or an oracle/benchmark for covariate-to-circumplex interval estimates | that the paper's Fourier-series parameterization matches the shipped `cpm_fit()` convention; the covariate interval-estimate procedure against the paper's simulation; that indicator angles stay invariant under the extension |
| E2 | weide2021 | oracle / reference | An inference-comparison oracle/benchmark for Bayesian vs. ML circumplex estimation, or a reference for a higher-level-scores feature | that the IIP three-factor model matches the package's SEM circumplex convention (`ssm_sem()`); that the priors/estimator are reproducible; that any borrowed number is page-anchored |
| E3 | rogoza2021 | method / reference | Vignette/method material tying the package's SSM to a full circumplex-analysis workflow, or a Procrustes-congruence step | that the SSM step matches the package's `ssm_*` estimand; that the equal-spacing / equal-communality definitions align with the package's angle conventions; whether the Procrustes step is in or out of scope |
| E4 | tracey2000 | reference | A background/reference page for `fit_structure()` structure-evaluation methods, or vignette context | which specific tools/definitions the repo actually relies on before any extraction; and — because it is an OCR scan — a second channel for any extracted value (M42-D1) |
| E5 | wester2026 | oracle / reference | A published end-to-end SPMC-E application (Table 2 CPM angles and communalities for the CSIP and IIP-C at two occasions, m = 2 and m = 3; Tables 3–4 SPMC-E γ0/γ1/δ with CIs; RMSECA values) usable as a numeric oracle for the CPM covariate-extension candidate and for `cpm_fit()` at m > 1; a reference for the bundled `csip` instrument's German validation and retest reliability; RMSECA as a candidate alignment index | that the paper's rotational constraint (minimize discrepancy to theoretical positions) is reproduced before comparing angles to `cpm_fit()`'s reference-scale convention; that two CSIP uniquenesses were fixed at 0.001, which any oracle must mirror; that the OSF data are actually retrievable (view-only link in the paper); the RMSECA definition against Etzel et al. (2021), which is not on the shelf |

## Disposition

Every ledger row lands as a `candidate` ROADMAP row (search-first, D-042); none
graduates here, and no per-source `<citekey>.md` page is authored (D-024):

- E1 → candidate "nagy2019 — covariate-extension feature / covariate-CI oracle".
- E2 → candidate "weide2021 — Bayesian-vs-ML inference-comparison oracle / higher-level-scores reference".
- E3 → candidate "rogoza2021 — three-step SSM + Procrustes method / vignette".
- E4 → candidate "tracey2000 — circumplex-evaluation reference for `fit_structure()`".
- E5 → folded into the ROADMAP's "Covariate extension of the CPM" candidate (registered 2026-09-19) as its published-application oracle; no separate row.

## Open questions

Claims about the repo's own state, dated where they sit:

- tracey2000 is **uncited** by any shipped code — the CAIS `@source` at
  `R/instrument_data.R:5` is Sodano & Tracey (2006), a different work; the M46
  plan's "appears only as CAIS provenance" note was wrong — observed 2026-07-20.
- wester2026 was read in full on 2026-09-19; its numbers above are from that reading, not banked as verified values — any oracle use re-reads the tables. The other four sources have not been read below survey altitude; the "must
  verify" items above are the reading a graduating milestone still owes —
  observed 2026-07-20.
- rogoza2021's shelf copy is an accepted proof; its printed volume and page
  range are placeholders, to be pinned from the published version when it
  graduates — observed 2026-07-20.
