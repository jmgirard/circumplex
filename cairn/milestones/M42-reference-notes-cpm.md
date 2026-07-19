# M42: Source notes for the CPM model and its communality CIs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M40
- **Principles touched:** —
- **Branch/PR:** `m42-reference-notes-cpm`

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

- [ ] `cairn/references/browne1992.md` exists carrying every template section,
      with each extracted value page-, equation-, or table-anchored.
- [ ] The page carries the full CPM model specification: a reader can map each
      parameter `R/cpm_fit.R` estimates to its published counterpart **from
      the page alone**, without opening the paper.
- [ ] `cairn/references/browne1982.md` exists carrying every template section,
      scoped to pp. 95–96, recording the **general transformation-based CI
      method** these pages actually state — the symmetric interval on
      θ = h(γᵢ) inverted through h⁻¹, and the logarithmic instance for a
      variance or standard deviation — anchored to the printed equation
      numbers, and stating explicitly that the communality-specific
      application is Grassi's assembly of that method with `browne1992.md`
      eq. 4, not something Browne states on these pages.
- [ ] `grassi2010.md`'s Browne-1982 bullet is corrected in place and marked:
      the stale PNG pointer names the shelved PDF, and the open question it
      records is resolved by reference to `browne1982.md`.
- [ ] Each page's `Extraction:` status is one physical line stating its real
      per-channel standing — never a verification a channel did not perform
      (M40) — and each page's `Traces to` names specific citing lines
      (`R/cpm_fit.R`, `R/ssm_ci_accuracy.R:169`,
      `tests/testthat/test-cpm_oracles.R:133`, `cairn/references/grassi2010.md`),
      verified against the files.
- [ ] `INDEX.md` carries one line per new page with the **filename** as link
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
