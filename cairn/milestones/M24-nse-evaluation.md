<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M24: Tidyverse NSE in the user API — evaluation + standing decision

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate -->
- **Principles touched:** —   <!-- no numbered IP/GP exist yet (deferred to /design-interview); governs DESIGN.md "Dependency policy" prose -->
- **Branch/PR:** m24-nse-evaluation · https://github.com/jmgirard/circumplex/pull/48   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Decide, on recorded evidence, whether circumplex's user-facing functions
should support tidyverse-style non-standard evaluation, and record the
outcome as a standing D-entry.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** an evaluation memo (`devel/m24-nse-evaluation.md`) built from four
evidence strata — prior-art survey of comparable CRAN statistics packages;
dependency-delta analysis (tidyselect/vctrs Imports closure, R-version
floor); ergonomics + ambiguity comparison on real vignette call sites with
runnable spike snippets; back-compat analysis against the v1.0.0 NSE removal
(NEWS.md:412–416, "streamline and reduce dependencies") — closing with a
verdict and its record: on NO, a full-rejection D-entry with re-trigger
clause plus a DESIGN.md Dependency-policy one-liner cross-referencing it; on
GO, a superseding D-entry and a registered build candidate (plan-gate
answers, 2026-07-16: rejection scope = full, incl. tidyselect helpers — no
parked carve-out; DESIGN.md gets the one-liner).

**Out:** any adoption build (on GO → its own planned milestone(s) after the
RB gate); Suggests-gated tidyselect carve-out as a deferral (plan gate chose
full-rejection framing — a GO outcome supersedes rather than parks); any
change to R/, src/, or tests/ (docs-only milestone — no test scope; the
memo's spike snippets are evidence, not shipped code).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [x] AC1: `devel/m24-nse-evaluation.md` exists and covers all four strata:
      (a) column-spec interfaces of ≥6 comparable CRAN statistics packages,
      each with a citation to its documentation; (b) the exact Imports
      closure and R-version floor tidyselect would add (computed from
      current CRAN metadata, method shown); (c) ≥3 real call sites from the
      shipped vignettes rewritten in hypothetical NSE form beside the
      current SE form, including one programming/wrapper case requiring
      embracing; (d) back-compat analysis vs the v1.0.0 removal.
- [x] AC2: the memo's ambiguity spike is runnable R (data-mask
      column/env-variable collision + the embracing case), with output
      shown — evidence, not vibes.
- [x] AC3: the decision is recorded per the plan-gate answers: NO → a
      full-rejection D-entry (re-trigger clause included) + the DESIGN.md
      Dependency-policy one-liner citing it; GO → a superseding D-entry
      (v1.0.0 removal explicitly superseded) + a build-candidate ROADMAP
      row, with the `irreversible-api` RB gate honored before the GO is
      final.
- [x] AC4: docs-only — `git diff` for the milestone touches no files under
      R/, src/, or tests/.

## Coverage
<!-- owner: plan · create/amend-via-gate; AC/Task counted top-to-bottom.
     Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T1, T2, T3, T4
- AC2 → T3
- AC3 → T4
- AC4 → T1, T2, T3, T4 (verified at review from the milestone diff)

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate -->

- [x] T1: Prior-art survey — how do ≥6 comparable CRAN statistics packages
      (candidates: lavaan, psych, lme4, survey, mirt, semTools, easystats
      family; pick for comparability, not convenience) accept variable/
      column specifications; memo §1 with doc citations. (Sonnet-suitable.)
- [x] T2: Dependency-delta — tidyselect's transitive Imports closure and
      minimum R version vs current DESCRIPTION (R >= 3.4, rlang already
      imported for `.data` only); note D-006's vctrs refusal; memo §2.
- [x] T3: Ergonomics + ambiguity spike — rewrite ≥3 vignette call sites
      (e.g., `ssm_analyze(jz2017, scales = PANO())`,
      `intermediate-ssm-analysis.Rmd` measures/grouping calls, a long
      `score()` items case) in NSE form; runnable snippets for the
      column/env collision and embracing cases; memo §3.
- [x] T4: Synthesis + decision — memo §4 verdict weighing all strata against
      the v1.0.0 rationale; append the D-entry and DESIGN.md line (NO path)
      or superseding D-entry + build candidate (GO path). (RB tripwire:
      irreversible-api — offer Fable escalation before finalizing a GO;
      a NO leaves the shipped API untouched and needs no escalation.)

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-16: created by /milestone-plan (plan-gate: deeper decision
  milestone; rejection-scope = full; DESIGN.md one-liner = yes).
- 2026-07-16: T1 done — 7-package survey (6 inspected locally, rstatix via
  CRAN), memo §1; peer modeling packages are SE/formula, tidy-eval NSE only
  in tidyverse-identity or in-house-reimplementation packages.
- 2026-07-16: T2 done — memo §2: 6 net-new Imports (incl. vctrs, refused by
  D-006) and an R-floor jump 3.4 → 4.1 (glue ≥ 4.1); closure computed from
  live CRAN metadata, method shown.
- 2026-07-16: T3 done — memo §3: 3 vignette sites rewritten (PANO() already
  beats NSE; score()'s ascending-order contract makes starts_with() a
  mis-scoring channel); both spikes run with verbatim output (collision
  silently selects the wrong column; naive wrappers error without {{ }}).
- 2026-07-16: T4 done — memo §4 verdict NO (all four strata against; RB
  tripwire not fired — rejection leaves the shipped API untouched); D-014
  appended (full rejection + re-trigger clause); DESIGN.md Dependency-policy
  one-liner added. Verify slot vacuously clean (docs-only diff: devel/ +
  cairn/ only; R/, src/, tests/ untouched). Status → review.
- 2026-07-16: review correction (supersedes the T2 line's floor claim): the
  "R-floor jump 3.4 → 4.1" was false — ggplot2/htmlTable already put the
  effective floor at 4.1 (diff-bug reviewer, scored 84); memo §2/§4 and
  D-014 corrected pre-merge, verdict unaffected.

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md -->

## Review
<!-- owner: review · exclusive; evidence per criterion, consistency-gate
     results, review findings + triage. EXEMPT from the 150-line cap (M55). -->

Review 2026-07-16 (PR #48). Evidence gathered fresh, by command.

- AC1: memo exists with all four strata as `## §1–§4` headers (grep); §1
  table has 7 packages each with a citation column entry (6 inspected
  locally with versions, rstatix via CRAN); §2 shows the
  `tools::package_dependencies()` method and CRAN Depends floors; §3 has 3
  vignette sites (grep "Site N" = 3) incl. the embracing wrapper case; §4
  carries the back-compat stratum. PASS.
- AC2: both spikes re-run fresh in a clean Rscript session; all five
  documented outcomes asserted via `stopifnot()` (collision selects column
  `sel`; `all_of()` corrects; direct NSE works; naive wrapper errors
  "object 'PA' not found"; embraced wrapper works) — "AC2 spikes: all five
  assertions hold". PASS.
- AC3: NO path taken — `D-014` present in DECISIONS.md (line 341) with an
  explicit **Re-trigger** clause; DESIGN.md Dependency policy cites D-014
  (line 361). RB gate n/a (rejection leaves the shipped API untouched, per
  the task's tripwire condition). PASS.
- AC4: `git diff --name-only master..HEAD` = 5 files, all under `cairn/` or
  `devel/`; 0 files under R/, src/, tests/. PASS.

Consistency gate 2026-07-16: `cairn_validate.py` all checks passed;
`cairn_impact` skipped (no IP/GP changed — none exist yet);
`devtools::document()` no diff; pkgdown `check_pkgdown()` no problems;
`^devel$`/`^cairn$` in .Rbuildignore; NEWS entry not owed (no user-visible
change; docs-only); README untouched (inherited state); full
`devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes.

Independent review (3 lenses + scorer):
- [S] prior-PR-comments: no prior-PR evidence (no review comments on any of
  the 20 milestone-pattern PRs). Zero findings.
- [S] blame-history: zero findings; NEWS citations, D-006 quote, DESIGN.md
  section history, and the rlang-only-for-.data claim all verified accurate;
  legacy "tidyverse-ectomy" records corroborate the memo narrative.
- [O] diff-bug: ONE finding — the memo/D-014 "R-floor jump 3.4 → 4.1" claim
  is false by the memo's own method (ggplot2 and htmlTable already Depend on
  R ≥ 4.1 on current CRAN, so the effective floor is already 4.1 and
  tidyselect adds no floor increase). Independently re-verified by the
  orchestrator against live CRAN metadata. All other memo claims verified
  correct by the reviewer (closure, spikes, citations, survey table, D-006
  consistency, ACs).
- [S] scorer: finding scored 84 (≥ 80 → actioned). Sub-80 findings: none.
- Triage: FIX NOW — floor-jump clause corrected in memo §2/§4 (marked as a
  review correction) and in D-014 ground 2 (pre-merge, entry unpublished);
  the NO verdict stands on the remaining independently verified grounds.
  Verdict unaffected; no other changes.
