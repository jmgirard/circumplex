# RR02: Contrast certification-conditional object contract (M15)

- **Date:** 2026-07-12
- **Brief:** `cairn/reviews/RB02-contrast-cert-object-contract.md`
- **Reviewer:** independent Fable-tier review (Claude Fable 5)
- **Evidence base:** all brief-specified regions read (`R/ssm_oop.R:116-193`,
  `R/ssm_ci_oop.R:60-145`, `R/ssm_ci_accuracy.R:91-131, 546-566, 681-717`,
  `tests/testthat/test-ci_accuracy.R:221-250`, spec §4.1/§4.2); every
  programmatic consumer of `Coverage_conditional` / `N_conditional` /
  `Cert_rate` / `d_conditional` traced by grep across `R/`, `tests/`,
  `vignettes/`, `man/`; the brief's diagnostic snippet executed
  (`load_all()`, seeds 311/312, reps 12, `amplitude_factors = c(1, 0)`) and
  the resulting `coverage`, `guardrail`, and `verdict` tables inspected;
  release status verified (`DESCRIPTION` Version 1.3.0.9002; `NEWS.md`
  places `ssm_ci_accuracy()` in the unreleased development-version section).

## Answers

### Q1. The object contract

**Recommended rule: measurements stay in the object; judgments follow
print.** The three surfaces are not the same kind of thing, and the coherent
contract falls out of distinguishing them:

- `coverage` and `guardrail` are **measurement tables** — the diagnostic's
  Monte Carlo data record.
- `verdict` is the **interpretive layer**: `print()`/`summary()` render their
  displacement line and verdict paragraph *from* it
  (`ssm_ci_verdict_blocks()` reads `x$verdict` directly,
  `R/ssm_ci_oop.R:60-108`).

Direction A is a statement about *interpretation* ("the package applies no
certification gate to a contrast"), not about what was *measured*. So it
moves the interpretive surface and leaves the measurement record intact.
Per surface:

1. **`coverage$Coverage_conditional` (+ `N_conditional`) for the contrast's
   displacement rows: (a) — keep intact, suppress from print.** It is a
   well-defined Monte Carlo quantity — P(contrast Δd CI covers | both
   profile rows certified) — with a documented conditioning event and a
   legitimate analytic use (Q4). NA'ing it destroys information while
   preventing no user-facing inconsistency: once print stops rendering the
   contrast's displacement line from it, the object column makes no claim
   that the package certifies contrasts. It becomes documented provenance,
   like `Left_miss`/`Right_miss` or the `Structural` flag.

2. **`verdict` Class for the contrast's displacement: change — recompute on
   the unconditional coverage** (details in Q2) **and relabel that row's
   `Parameter` from `"d_conditional"` to `"d"`.** The verdict cannot be
   "left intact and suppressed from print" because print does not merely
   *cite* it — print is generated from it. Keeping a
   certification-conditional Class in the object while print reports an
   unconditional coverage number would either make the printed Class
   disagree with the printed coverage on the same line, or force print to
   maintain a shadow classification the object contradicts. The verdict is
   the one surface where option (a) is internally impossible under
   Direction A.

3. **`guardrail$Cert_rate` for the contrast: (a) — keep, with `Caution`
   remaining NA** (already correct). It is the denominator provenance for
   the retained `Coverage_conditional`: per rung,
   `N_conditional = Cert_rate × N_reps` (verified on the snippet object),
   so keeping one without the other would orphan the retained conditional
   coverage from its conditioning rate. It is also an interpretable joint
   operating characteristic in its own right: how often a print-reading
   user would see *both* profile displacements certified.

Why the asymmetry is principled rather than ad hoc: the single rule
"measured quantities are retained and documented; interpretive/presentation
surfaces (verdict, print, summary — and plot, see Beyond the brief) match
`print.circumplex_ssm()`'s profiles-only certification stance" covers all
three surfaces, plus the two the brief did not enumerate, with no
exceptions.

### Q2. The contrast's displacement Verdict/Class

**Recompute on the unconditional coverage; do not drop it or mark it
not-assessable.** Three grounds:

- **It is the operating characteristic of what the user actually
  consumes.** `print.circumplex_ssm()` reports the contrast's displacement
  CI ungated (`R/ssm_oop.R:172-190`). The verdict's question is "does the
  CI procedure the package shows the user cover at the nominal rate," and
  for the contrast that procedure is unconditional. Classifying a
  conditional coverage the package never conditions on answers a question
  about a workflow the package does not implement.
- **The fixed premise removes the statistical rationale for conditioning.**
  Δa is a signed, unconstrained difference (spec §4.1); the boundary
  pathology that makes a *profile's* uncertified displacement
  uninterpretable does not exist for the contrast, so there is no regime in
  which the unconditional contrast coverage is a malformed estimand. The
  genuinely dangerous contrast regime — near-uniform Δd from small *row*
  amplitudes — is precisely what the unconditional number must include to
  be honest, and it is separately surfaced by `Branch_pathology_rate`.
- **Assessability.** The unconditional Class always has n = `N_reps` at
  c = 1 (contrast Δd truth is defined whenever the row truths are; flat
  populations are refused up front), whereas the conditional Class can be
  "never certified → not assessable" — an artifact of conditioning on an
  event the package doesn't use for this row. Marking the contrast
  not-assessable would be affirmatively wrong: the diagnostic measures the
  quantity in every replicate.

Mechanically this is cheap: `ssm_ci_verdict()` (`R/ssm_ci_accuracy.R:1026`)
can classify the contrast row from the coverage table's unconditional
`d` row at Condition 1 (`k = round(Coverage * N_reps)`, `n = N_reps`),
exactly as it already does for `e` and `a` — the per-replicate
`dcond_at_1` matrix is only needed for profile rows.

**Print should show**, for the contrast:
`Displacement   coverage XX.X% -- CLASS` with **no** `" when certified"`
suffix (drop the suffix by keying the parameter to `"d"` for the contrast
in the `ssm_ci_verdict_blocks()` loop at `R/ssm_ci_oop.R:83`), and the
verdict paragraph must say "displacement", never "certified displacement",
for the contrast (wording keys in `ssm_ci_verdict_text()`,
`R/ssm_ci_oop.R:200-249`). The "never certified at the as-estimated
condition (not assessable)" fallback (`R/ssm_ci_oop.R:89-93`) must be
unreachable for the contrast.

### Q3. `guardrail$Cert_rate` for the contrast

**Retain it.** Under the Q1 rule it is not a figure with no consumer:

- It is the documented conditioning rate behind the retained
  `coverage$Coverage_conditional` and `N_conditional` — the auditability
  link that lets a user verify the conditional column's denominator per
  rung. NA'ing `Cert_rate` while keeping `Coverage_conditional` would be
  the worst combination: a conditional coverage whose conditioning rate is
  hidden.
- Removing the contrast's guardrail *row* is not on the table anyway: that
  row carries other live figures — `Branch_pathology_rate` (the very
  pathology the contrast ladder is designed to provoke, spec §4.1) and
  `N_reps` — plus its Wilson bounds; only the `Cert_rate` cell was in
  question.
- Reproducibility: the joint-certification rate is an operating
  characteristic of the shipped profile guardrail (P(both rows certified))
  that costs nothing to keep and cannot be reconstructed from the other
  columns off the c-rungs where `N_conditional` happens to be reported.

The stale code comments must move with the decision: the assembly comment
(`R/ssm_ci_accuracy.R:546-551`) and the Caution-NA comment
(`R/ssm_ci_accuracy.R:690-696`) currently justify `cert[1] && cert[2]` as
"the conditioning device for the contrast's certified-displacement
coverage" *line*; after M15 it conditions no displayed number, only the
retained object columns. Likewise the guardrail-block comment at
`R/ssm_ci_oop.R:110-114` ("its certified-displacement coverage line above
still uses the joint-certification conditioning") becomes false and must be
rewritten.

### Q4. What is lost by reversing Milestone-close review #3

**There is a legitimate use, and it is preserved by keeping the object
columns (Q1), so nothing of analytic value is lost.** The joint-certification
event is exactly "a print-reading user sees both profile displacements
certified" — the situation in which a user following the package's own
guardrail would proceed to interpret each row's displacement and would
therefore be most tempted to interpret their contrast. Conditional coverage
answers: *among the analyses where the guardrail green-lights both rows,
does the contrast CI still cover Δd at the nominal rate?* That is a real
selection-effect question: certification selects replicates with larger
estimated row amplitudes, and in the near-zero regime this selection can
move contrast-CI behavior in either direction. In the brief's snippet the
conditional and unconditional numbers coincide (0.9167) only because
`Cert_rate = 1` at both rungs; they diverge exactly where the diagnostic
matters.

Keeping the measurement in `coverage`/`guardrail` while printing only the
unconditional number resolves the RB02 inconsistency completely: the print
inconsistency was never "the object contains a conditional quantity" — it
was "the printed displacement line for the contrast is conditioned on an
event print.circumplex_ssm() never shows." Object-level retention with
clear roxygen (this is a joint-certification *descriptive*, not a rule the
package applies) creates no printed claim. This confirms, rather than
changes, the Q1 answer: (a) for the measurement surfaces.

### Q5. Backward compatibility / least surprise

**Release fact first:** `DESCRIPTION` is at 1.3.0.9002 and `ssm_ci_accuracy()`
appears only in the unreleased "development version" section of `NEWS.md` —
the function has never shipped to CRAN. There are no external programmatic
consumers to break; "least surprise" governs the *first released* (2.0.0)
contract plus repo-internal consumers (tests, the plot method). This
strongly favors getting the contract right now over minimizing diffs.

Classification per surface, under the Q1-Q3 recommendation:

| Surface | Change class | Downstream effect |
|---|---|---|
| `coverage$Coverage_conditional`, `N_conditional` (contrast rows) | **No change** (values, columns, NA-pattern all unchanged) | None |
| `guardrail$Cert_rate` (contrast rows) | **No change**; `Caution` already NA | None |
| `verdict` contrast displacement row | **Altering an existing column's contents — loudly**: `Parameter` relabels `"d_conditional"` → `"d"` and `Coverage`/`N_reps`/`Wilson_*`/`Class` recompute unconditionally | A consumer filtering `Parameter == "d_conditional"` gets no contrast row (visible absence) instead of silently different numbers under an unchanged label |
| `print()`/`summary()` text | Wording change (suffix dropped, paragraph rewording) | Text output is not a programmatic contract; snapshots update |

The rejected alternative for the verdict — keeping the `"d_conditional"`
label with unconditional contents — is the silent-alteration worst case: a
label asserting a conditioning that no longer holds. The chosen relabel
makes the semantic change greppable and self-describing (profiles keep
`"d_conditional"` because their Class *is* certification-conditional; the
contrast carries `"d"` because its Class is not). Document the per-row
labeling in the roxygen `@return`.

**NEWS:** no compatibility entry is owed (nothing released), but the
existing development-version bullet (`NEWS.md`, the `ssm_ci_accuracy()`
item: "...the certification rate of the printed 'amplitude CI excludes
zero' guardrail, and displacement coverage conditional on certification")
should gain one clause stating that for a contrast row the displacement
verdict and printed coverage are unconditional, matching
`print.circumplex_ssm()`'s profiles-only certification — that is
documentation of the shipped 2.0.0 contract, not a changelog for users.

## Beyond the brief

1. **A fourth surface exists: `plot.circumplex_ci_accuracy()`**
   (`R/ssm_ci_oop.R:501-527`). It builds a "Displacement (certified)" panel
   from `Coverage_conditional`/`N_conditional` for *all* rows, contrast
   included. Under Direction A plus object-level retention, the plot would
   become the only shipped display still presenting a certified-displacement
   series for the contrast — reintroducing exactly the print mismatch M15
   removes, in the most visible medium. The coherence rule of Q1
   (presentation surfaces follow print) says: exclude the contrast series
   from the `d_cert` panel; its unconditional line already appears in the
   "Displacement" panel, and the data remain in the object for anyone who
   wants to plot the conditional series themselves. The milestone should
   decide this explicitly rather than leave it implicit.
2. **Superseded test.** `tests/testthat/test-ci_accuracy.R:221-250`: the
   assertions that survive M15 are `Caution` all-NA for the contrast,
   `Caution` present for profiles at c = 0, finite contrast `Cert_rate`,
   and the wording bars (no "significan", no "contrast displacement
   would"). New assertions should pin: the contrast verdict row has
   `Parameter == "d"` with `N_reps == reps`; profile verdict rows still
   carry `"d_conditional"`; the printed contrast displacement line contains
   no "when certified"; contrast `Coverage_conditional` remains populated
   in `coverage`.
3. **Implementation wording keys.** `ssm_ci_verdict_text()`
   (`R/ssm_ci_oop.R:169-262`) keys phrases on `"d_conditional"` ("mis-cover
   even when certified", "certified displacement", "Displacement CIs are
   trustworthy when certified."). The contrast path needs plain
   "displacement" phrasing throughout — in particular the positive sentence
   "Displacement CIs are trustworthy when certified." must not be emitted
   for a contrast. The spec §5.2 wording bar (never describe an interval
   excluding zero as a significance test) is unaffected but should be
   re-checked by the existing grep-based test.
4. **Snapshot churn.** `tests/testthat/_snaps/ci_accuracy.md` embeds
   coverage/guardrail/verdict headers and print output; expected to change
   for contrast objects only. Profile-only snapshots must be byte-identical
   — a useful regression guard that M15 touched nothing profile-side
   (constraint: `ssm_certified()` and profile reporting unchanged).
5. **Roxygen `@return`** (`R/ssm_ci_accuracy.R:91-117`) must state the
   contrast rule on all three surfaces in one place: verdict/printed
   displacement unconditional for the contrast; `Coverage_conditional` and
   `Cert_rate` retained for the contrast as joint-certification
   descriptives that no display consumes.
6. **Direction A itself:** nothing found in the code, spec, or the live
   object contradicts it; the review endorses it. The only latent risk —
   losing the selection-effect diagnostic — is fully mitigated by Q1(a).

## Recommendations

1. **Apply** — Keep `coverage$Coverage_conditional` and `N_conditional` for
   contrast rows unchanged; document in roxygen that for the contrast the
   conditioning event is joint certification of both profile rows, a
   descriptive the package never displays.
2. **Apply** — Recompute the contrast's displacement verdict on the
   unconditional coverage (k = round(Coverage × N_reps), n = N_reps at
   Condition 1) and relabel that verdict row's `Parameter` to `"d"`;
   profiles keep `"d_conditional"`. Overall worst-of mechanics unchanged.
3. **Apply** — Print the contrast displacement line without
   `" when certified"`; rework `ssm_ci_verdict_text()` contrast wording;
   make the "never certified (not assessable)" fallback unreachable for the
   contrast.
4. **Apply** — Retain `guardrail$Cert_rate` for the contrast (`Caution`
   stays NA); rewrite the three stale comments
   (`R/ssm_ci_accuracy.R:546-551`, `R/ssm_ci_accuracy.R:690-696`,
   `R/ssm_ci_oop.R:110-114`) to say the joint-certification rate is
   provenance for the retained object columns, not a conditioning device
   for any displayed line.
5. **Apply** — Exclude the contrast series from the "Displacement
   (certified)" panel in `plot.circumplex_ci_accuracy()` (beyond the
   brief's three surfaces, but required by the same coherence rule; if the
   milestone keeps it instead, record that as an explicit documented
   exception).
6. **Apply** — Update the roxygen `@return` and add one clause to the
   existing `NEWS.md` development bullet; supersede
   `test-ci_accuracy.R:221-250` per Beyond-the-brief item 2.
7. **Reject** — NA'ing or removing the contrast's conditional fields
   (option (b)) on any of the three surfaces except the verdict: it
   destroys a well-defined measurement with a legitimate analytic use
   (Q4), introduces silent value-to-NA changes, and buys no print
   consistency that suppression doesn't already provide.
8. **Reject** — Keeping the verdict `Parameter` label `"d_conditional"` for
   the contrast while filling it with unconditional contents: silent
   semantic alteration under an unchanged label, the worst outcome for a
   programmatic consumer.
9. **Reject** — Marking the contrast's displacement Class not-assessable:
   the unconditional quantity is measured in every replicate and is the
   operating characteristic of the CI the package actually prints.
