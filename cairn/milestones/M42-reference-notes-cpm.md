# M42: Source notes for the CPM model and its communality CIs

- **Status:** review
- **Priority:** normal
- **Depends on:** M40
- **Principles touched:** —
- **Branch/PR:** `m42-reference-notes-cpm` / https://github.com/jmgirard/circumplex/pull/68

## Goal

Author the Browne (1992) source note carrying the full CPM model
specification, plus the Browne (1982) communality-CI derivation the CPM oracle
path relies on.

## Scope

**In:** Two source notes, following the page conventions M40 establishes.

- `browne1992.md` — Browne (1992), *Circumplex models for correlation
  matrices*, *Psychometrika* 57(4) 469–497, shelf `sources/browne1992.pdf`.
  This page carries the **full CPM model specification** — enough to
  re-derive the estimand in `R/cpm_fit.R` without reopening the paper — not
  only the values the repo currently cites (plan-gate decision, 2026-07-19,
  carried over from M41).
- `browne1982.md` — Browne (1982), scoped to **pp. 95–96 only**; the shelf
  holds exactly those two pages as `sources/browne1982_pp95-96.pdf` and the
  rest of the source is neither available nor required. The page records the
  **general transformation-based CI method** those pages state — a symmetric
  interval on θ = h(γᵢ) inverted through h⁻¹, and the logarithmic instance
  for a variance or standard deviation. The communality-specific chain
  (nonsymmetric CIs on ρ(xᵢ, cᵢ) from symmetric CIs on ln vᵢᵢ) is *Grassi's*
  assembly of that method with `browne1992.md` eq. 4; Browne does not state
  it on these pages (amended 2026-07-19, T4 gate).

**Correction owed to a committed page.** `cairn/references/grassi2010.md:134-140`
carries an open question pointing at four page images
(`sources/browne1982_p95a.png`, `p95b`, `p96a`, `p96b`) that no longer exist —
the shelf now holds the consolidated PDF instead. That is a current-knowledge
claim proven false, so it is corrected **in place** and marked, per the
tracking rule; the same bullet's open question ("has not been checked against
Browne himself") is what `browne1982.md` resolves.

The repo's two-channel transcription protocol applies to both pages, in the
form M42-D1 fixes for an OCR-scanned source: a read of the rendered page
images, authoritative, cross-checked against `pdftotext -layout`, diffed on
every load-bearing numeral, with between-channel discrepancies recorded
rather than silently resolved.

**Out:** The fit-index benchmark pair → M41. Acton & Revelle (2004) and Wendt
et al. (2019) → M43. **Package code changes of any kind** — this is a
documentation milestone. If T3's parameter map finds `R/cpm_fit.R` departing
from Browne as published, that is a finding to record and escalate, never to
reconcile inside this milestone; the fix would be its own milestone. Editing
the `devel/` transcriptions → after M7 archives (ROADMAP candidate row).
Cudeck & Browne (1983), shelved as `cudeck1983.pdf` — assessed and owes no
page (M41 work log, 2026-07-19).

## Acceptance criteria

- [x] `cairn/references/browne1992.md` exists carrying every template section,
      with each extracted value page-, equation-, or table-anchored.
- [x] The page carries the full CPM model specification: a reader can map each
      parameter `R/cpm_fit.R` estimates to its published counterpart **from
      the page alone**, without opening the paper.
- [x] `cairn/references/browne1982.md` exists carrying every template section,
      scoped to pp. 95–96, recording the **general transformation-based CI
      method** these pages actually state — the symmetric interval on
      θ = h(γᵢ) inverted through h⁻¹, and the logarithmic instance for a
      variance or standard deviation — anchored to the printed equation
      numbers, and stating explicitly that the communality-specific
      application is Grassi's assembly of that method with `browne1992.md`
      eq. 4, not something Browne states on these pages.
- [x] `grassi2010.md`'s Browne-1982 bullet is corrected in place and marked:
      the stale PNG pointer names the shelved PDF, and the open question it
      records is resolved by reference to `browne1982.md`.
- [x] Each page's `Extraction:` status is one physical line stating its real
      per-channel standing — never a verification a channel did not perform
      (M40) — and each page's `Traces to` names specific citing lines
      (`R/cpm_fit.R`, `R/ssm_ci_accuracy.R:169`,
      `tests/testthat/test-cpm_oracles.R:133`, `cairn/references/grassi2010.md`),
      verified against the files.
- [x] `INDEX.md` carries one line per new page with the **filename** as link
      text (M40); `cairn_validate` reports `references index<->disk` PASS with
      no `references staleness` WARN; `git diff --stat devel/` is empty, no
      file outside `cairn/` is modified, and each written page's tail bytes
      are checked for leaked tool-call scaffolding (M34).

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T1, T4
- AC4 → T5
- AC5 → T2, T4, T6
- AC6 → T6, T7

## Tasks

- [x] T1. Re-verify `browne1992.pdf` and `browne1982_pp95-96.pdf` on the shelf
      and stamp the observation — the shelf is live and moved twice during
      M40, so confirm rather than trust this plan's inventory.
- [x] T2. Author `browne1992.md` — the full CPM model specification, two
      channels. Expect this to fill a working session on its own.
- [x] T3. Build the parameter map: each quantity `R/cpm_fit.R` estimates
      against its published counterpart, walked against the code rather than
      asserted. A departure is recorded and escalated, never reconciled here.
- [x] T4. Author `browne1982.md` from pp. 95–96, two channels.
- [x] T5. Correct `grassi2010.md:134-140` in place — PNG pointer → shelved
      PDF, open question resolved, correction marked.
- [x] T6. `Traces to` sections written against the actual citing lines;
      `INDEX.md` entries; `cairn_validate` clean; tail-byte and untouched-tree
      checks.
- [x] T7. Discharge the two dated observations this milestone falsifies on
      already-committed files: `browne1992a.md`'s citekey-trap paragraph
      (which says `browne1992.md` "does not exist yet" and names M42 as owing
      the reciprocal warning) and `INDEX.md`'s comment-block ledger of which
      sources still owe a page. Both are corrected in place and marked.

## Work log

- 2026-07-19: created by /milestone-plan, splitting M41 at the re-size gate its own `Out:` clause called for. Carries the two Browne sources: the 1992 CPM paper (the split-out M41 predicted — "a spec document in a source note's clothes") and the 1982 pp. 95–96 communality-CI derivation, which belongs here rather than with the fit benchmarks because it serves the same CPM oracle path (`tests/testthat/test-cpm_oracles.R:133`, `grassi2010.md`). Both sources verified present on the shelf at plan time — `browne1992.pdf` confirmed by first-page read as *Psychometrika* 57(4) 469–497, distinct from `browne1992a.pdf` (Browne & Cudeck, M41) — observed 2026-07-19. T5 exists because that plan-time inventory found `grassi2010.md` still naming four page images the consolidated PDF replaced.

- 2026-07-19: T1 done — both sources confirmed on the shelf and identified from their own pages, not from filenames: `browne1992.pdf` is 29 pages, p. 1 printing "PSYCHOMETRIKA-VOL. 57, NO. 4, 469-497 / DECEMBER 1992" and "CIRCUMPLEX MODELS FOR CORRELATION MATRICES / MICHAEL W. BROWNE"; `browne1982_pp95-96.pdf` is 2 pages carrying printed page numbers 95 and 96 — observed 2026-07-19. Both are Acrobat "Paper Capture" OCR scans, which is what M42-D1 responds to.
- 2026-07-19: question gate — three answers, all the recommended option. (1) **Substantive amendment to AC3**, user-approved: the criterion presumed Browne (1982) pp. 95–96 state a communality-CI derivation; the pages state the general transformation-based CI method (eqs. 1.6.29–1.6.41) with the log-variance instance (1.6.38–1.6.40) and mention neither communality nor the circumplex, so the criterion now asks for what is there plus an explicit statement that the communality application is Grassi's assembly. (2) OCR channel standing → M42-D1. (3) `browne1992.md` carries the implemented model only (§2, §3, §5.2 Requirements, §6), with §4 simplex and §5 Anderson under "Not extracted" — `grep -rn Anderson R/` returns nothing.
- 2026-07-19: minor amendment — **T7 added** (discovered sub-task) for two dated observations this milestone falsifies on already-committed files: `browne1992a.md:6-8` asserts `browne1992.md` "does not exist yet", and `INDEX.md`'s comment block lists Browne 1992/1982 as still owing pages. Coverage AC6 → T6, T7.

- 2026-07-19: T2+T3 done — `cairn/references/browne1992.md` authored, carrying the implemented model in full (eqs. 1–8, the six §5.2 Requirements, eqs. 30–48) plus a parameter map walked against `R/cpm_fit.R`. Two results worth naming. (a) **No numeric departure**: the engine's `zeta` is Browne's ζ*ᵢᵢ = ρ(xᵢ,cᵢ) of eqs. (3b*)/(4), verified by substituting into eq. (3) rather than asserted, and both scaling families' df reduce to eq. (6) under equivalent moment counts (the cancellation D-011 measured empirically). (b) **Three departures in kind, recorded not reconciled** per Scope: Browne's §6.7 IFA start-value recipe is not implemented (the engine uses its own starts), `cpm_spec()` imposes an m cap the paper does not print, and variant C (equal communality, free angles) appears nowhere in the paper. Also banked: five paper-internal errata, of which the p. 485 "[m/2] … less than m/2" definition is load-bearing — read literally it drops the top even harmonic from eq. (33).

- 2026-07-19: T4 done — `cairn/references/browne1982.md` authored. It opens with a scope warning because the pages do not say what the citing chain implies: eqs. (1.6.29)–(1.6.41) are a general transform-and-invert CI method (symmetric interval on θ = h(γᵢ), inverted through h⁻¹ so the result cannot include inadmissible values), and the log instance (1.6.38)–(1.6.40) composes in closed form to γ̂·exp(±c_α σ̂/γ̂) — `tests/testthat/test-cpm_oracles.R:136` term for term. The communality chain has three links and only the first is Browne (1982); links 2 and 3 are `browne1992.md` eq. 4 and Grassi's reporting choice. Attribution corrected, arithmetic unaffected. Full citation transcribed verbatim from `grassi2010.pdf`'s reference list and flagged as unverified against the source (the shelf has two interior pages, no title page). Three open questions banked: the unverified citation, the twice-printed "(1.6.9)" that reads as though it means (1.6.29) and is undecidable from two pages, and the p. 97 continuation of (1.6.41).
- 2026-07-19: amendment (same approval as the AC3 gate) — the **Scope** block carried the same false premise AC3 did and named the pre-M42-D1 channel protocol; both sentences replaced, text previewed in chat before this commit.

- 2026-07-19: T5 done — `grassi2010.md`'s Browne-1982 bullet corrected in place and marked. The stale four-PNG pointer now names `sources/browne1982_pp95-96.pdf`, and the open question is resolved by reference to `browne1982.md` — with the substantive correction that Browne pp. 95–96 never mention communalities. The paper-report sentence at `grassi2010.md:104-106` is accurate as a report of Grassi and is left standing, with a cross-reference added so a reader meets the caveat where the claim sits.
- 2026-07-19: T7 done — `browne1992a.md`'s citekey-trap paragraph no longer says `browne1992.md` "does not exist yet", and `INDEX.md`'s comment-block ledger no longer lists the two Browne sources as owing pages; both corrected in place and marked, per the correcting-current-knowledge rule.
- 2026-07-19: T6 done — both pages' `Traces to` written against actual citing lines, each verified by reading the line (three cited anchors were wrong on first write and fixed: `cpm_rho_deriv` is at `R/cpm_fit.R:46` not :53, and the oracle block is `test-cpm_oracles.R:131-137` with the comment at 132-135). `INDEX.md` carries one line per new page with the filename as link text. `cairn_validate`: **all checks passed** — `weight caps` PASS, `references index<->disk` PASS, `references staleness` OK (no WARN), `record density` OK; the 47 `work-log format` advisories are all pre-existing M7 hard-wraps, none from M42. `git diff --stat devel/` empty; no file outside `cairn/` modified. Tail bytes of both new pages checked by `od -c` — clean, single trailing newline, no leaked scaffolding. `devtools::test()` 0 failures / 3082 passing (the milestone touches no package file; run to confirm the tree is green).

## Decisions

### M42-D1 (2026-07-19): on an OCR-scanned source the page image is the authoritative channel and `pdftotext` is not an independent witness

**Context:** M41-D1 recorded that the repo's two-channel transcription protocol
runs two *machine* channels — `pdftotext -layout` and the implementing session's
read of the `pdftoppm` page image — and that neither is a human attestation.
Both M42 sources go further: `browne1992.pdf` and `browne1982_pp95-96.pdf` are
Acrobat "Paper Capture" OCR of scanned typescript (`pdfinfo` Producer line), so
the text layer is *derived from the same page image* rather than from a
born-digital text stream. On `browne1982_pp95-96.pdf` the text layer drops every
display equation, preserving only the equation numbers.
**Decision:** Both channels still run and every load-bearing numeral is still
diffed, but on an OCR-scanned source the rendered page image is the
**authoritative** channel and `pdftotext` is a cross-check for reading slips
only — never an independent witness to a value. Each page's `Extraction:` status
says so on its own line, and says plainly that a defect in the scan itself is
uncatchable by either channel.
**Consequences:** refines M41-D1 for the scanned-source case; supersedes nothing.
Every value on both M42 pages rests on one authoritative channel, which the
status states rather than implies.

## Review

Reviewed 2026-07-19. PR https://github.com/jmgirard/circumplex/pull/68.

### Acceptance-criteria evidence

- **AC1 — both pages carry every template section, values anchored.** Checked by
  script against the eight template sections; both pages carry all eight.
  `browne1992.md` 409 lines, `browne1982.md` 207. Every value in `Extracted
  values` carries a page or equation anchor in the source's own numbering
  (Browne's `(n)`, the 1982 chapter's `(1.6.n)`).
- **AC2 — a reader can map each estimated parameter from the page alone.** The
  parameter map's two load-bearing claims were re-derived independently at
  review, not read: (a) the ζ identity — `cpm_implied_cor(theta, zeta, beta)`
  vs Browne eq. (3) `D*(P_c + D_v)D*` with ζ* = (1+v)^(−1/2), built from the
  Grassi Appendix A v vector, agree to **max abs diff 2.22e-16** with the eq. (3)
  form's diagonal exactly 1; (b) the df bookkeeping — for all 10 legal (p, m)
  combinations at p ∈ {6, 8, 12}, the free family's q equals Browne's
  q = 3p + m − 1 exactly, both families' df equal Browne's d = ½p(p+1) − q
  exactly, and unit df == free df. The "no numeric departure" claim is
  verified, not asserted. The independent Opus reviewer reached the same
  conclusion and additionally established the cancellation is unconditional
  across variants, not only variant A.
- **AC3 — `browne1982.md` records the general method, scoped.** Present, scoped
  to pp. 95–96, eqs. (1.6.29)–(1.6.41) anchored to the printed numbers, opening
  with an explicit scope warning that the pages state a general method and the
  communality application is Grassi's assembly. The closed-form composition
  γ̂·exp(±c_α σ̂/γ̂) was checked term-for-term against
  `tests/testthat/test-cpm_oracles.R:136`.
- **AC4 — `grassi2010.md` corrected in place and marked.** The stale four-PNG
  pointer now names `sources/browne1982_pp95-96.pdf` (the PNG names survive only
  inside the correction, naming what was replaced); the open question is marked
  `**Resolved (M42, 2026-07-19).**` and resolved by reference to
  `browne1982.md`. The M7 T3 finding A5 sentence at `:104-106` is left verbatim
  and gains a cross-reference. The blame-history reviewer independently
  confirmed findings A2/A3/A4/A6 are untouched.
- **AC5 — Extraction statuses honest, Traces to verified.** Each page's
  `Extraction:` is one physical line (512 and 594 chars). Both state the M42-D1
  standing: the page image is authoritative, `pdftotext` is a cross-check only,
  no value read by a human. All Traces-to anchors were opened and read at
  review — 38 on `browne1992.md`, 7 on `browne1982.md`, all in range and all
  landing on content matching the page's description (one anchor was wrong on
  first write and is fixed, F4 below).
- **AC6 — INDEX, validate, diff scope, tail bytes.** All 6 INDEX entries use the
  filename as link text (the M40 regex trap). `cairn_validate`: **all checks
  passed**, 15 PASS including `references index<->disk` and `coverage complete`,
  `references staleness` OK with no WARN, `record density` OK. `git diff
  --stat devel/` empty; `git diff --name-only master..HEAD` lists only `cairn/`
  paths. Tail bytes of both pages checked with `od -c` — single trailing
  newline, no leaked scaffolding.

### Consistency gate

Universal: `cairn_validate` exit 0, all checks passed (47 `work-log format`
advisories, all pre-existing M7 hard-wraps, none from M42). No DESIGN.md
principle changed → `cairn_impact` skipped.

Toolchain (`r-package` profile `consistency-gate` slot): `devtools::document()`
produces no diff in `NAMESPACE` or `man/`; `pkgdown::check_pkgdown()` "No
problems found"; README.Rmd/README.md untouched by this diff; no NEWS entry
owed (no user-visible change — the diff touches no package file); `.Rbuildignore`
already carries `^cairn$` and no top-level file was added.
**`devtools::check(args = "--no-manual")`: 0 errors / 0 warnings / 0 notes**
(5m 8s). `--no-manual` is justified here because no roxygen changed — the M7
lesson that `--no-manual` masks PDF-manual breakage applies to roxygen edits,
and this diff has none. `devtools::test()`: 0 failures / 3082 passing.
CI on PR #68: **7/7 green** (ubuntu devel/release/oldrel-1, macos, windows,
pkgdown, test-coverage).

### Independent review — three lenses + scorer

- **[O] diff-bug (Opus):** 6 findings.
- **[S] blame-history (Sonnet):** 0 findings. Traced each in-place edit to its
  origin commit and found all three discharge debts prior milestones explicitly
  left (`60fc20b2` M40, `0dc74f32` M41); confirmed M42-D1 narrows M41-D1 for the
  scanned-source case without touching the protocol governing `browne1992a.md`
  or `hu1999.md`, whose Extraction lines are unmodified.
- **[S] prior-PR-comments (Sonnet):** 0 findings. Checked the diff against every
  M40 and M41 review lesson (INDEX link text, Extraction overclaim, undated
  absence claims, the M41 F1 "nothing implements X" trap) and found none
  regressed.

**Actioned (score ≥ 80):**

- **F1 (95) — fixed.** `browne1982.md` claimed "the package's own ζ intervals
  are symmetric on the natural scale", false for the default path: `ci_method`
  defaults to `"bootstrap"` (`R/cpm_fit.R:1527`), whose ζ intervals are
  **percentile** intervals via `stats::quantile()` (`:1316-1323`) and are
  asymmetric; only the analytic Wald branch (`:1644-1645`) is symmetric. Found
  independently by the implementing session at review before the reviewer
  reported it. The page now names both routes and explains that neither
  transforms first — the load-bearing claim (neither is Browne's route) was
  always true; the characterization was not.
- **F4 (90) — fixed.** `browne1992.md` anchored the eq. (4) conversion at
  `test-cpm_oracles.R:129-130`, which are comment lines; the assertion is at
  `:131`. Anchor corrected, with the comment cited separately.

**Below threshold, logged (4):**

- **F2 (40) — rejected.** Claimed `browne1982.md`'s Extraction status overclaims
  because the citation was not read off the source. Rejected on the scorer's
  reasoning, which is right: tracking-rules keeps the Citation block outside the
  Provenance block's scope, the status names the equations channel explicitly,
  and the page's first Open question already flags the citation as unverified.
- **F3 (75) — fixed anyway.** The absence claim in the Role paragraph carried no
  `— observed` stamp though the same claim is dated in Traces to. One-line fix,
  and it is precisely the failure mode this milestone series exists to prevent,
  so it was not worth banking.
- **F5 (70) — fixed anyway.** Departure 1 said eqs. (41)–(48) have "no repo line
  depending on them" while the parameter map cites §6.7's θ_r = 0 convention as
  the warrant for the `reference` pin. Departure 1 now scopes itself to the
  *recipe* and states that the reference convention is live, warning against
  reading it as licence to drop the §6.7 transcription.
- **F6 (28) — superseded by a stronger finding of the reviewer's own.** The
  scorer was right that the provenance claim ("no repo value is read from §8")
  is accurate. But checking it against the source turned up something no agent
  found: **Browne's Table 11, p. 494 (§8.2) prints β₀ = .638, β₁ = .362,
  ρ₁₈₀° = .28 and angles 0, 55, 112, 123, 192, 210, 269 — digit for digit the
  values `grassi2010.md` records from Grassi's Table 2 (p. 60).** So §8
  independently corroborates the m = 1 oracle, and Grassi's "coincide precisely
  with CIRCUM" claim is checkable against Browne's own printed output. The
  "Not extracted" section now records this, so a future milestone wanting a
  second published anchor knows where it is instead of reading §8 as empty.

### Outcome

All six acceptance criteria verified against fresh evidence. Six findings
raised, two actioned, three more fixed voluntarily, one rejected with reason.
No criterion was reinterpreted and no plan-owned text was edited at review.
