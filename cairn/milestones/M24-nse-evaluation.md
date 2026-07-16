<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M24: Tidyverse NSE in the user API — evaluation + standing decision

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate -->
- **Principles touched:** —   <!-- no numbered IP/GP exist yet (deferred to /design-interview); governs DESIGN.md "Dependency policy" prose -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

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

- [ ] AC1: `devel/m24-nse-evaluation.md` exists and covers all four strata:
      (a) column-spec interfaces of ≥6 comparable CRAN statistics packages,
      each with a citation to its documentation; (b) the exact Imports
      closure and R-version floor tidyselect would add (computed from
      current CRAN metadata, method shown); (c) ≥3 real call sites from the
      shipped vignettes rewritten in hypothetical NSE form beside the
      current SE form, including one programming/wrapper case requiring
      embracing; (d) back-compat analysis vs the v1.0.0 removal.
- [ ] AC2: the memo's ambiguity spike is runnable R (data-mask
      column/env-variable collision + the embracing case), with output
      shown — evidence, not vibes.
- [ ] AC3: the decision is recorded per the plan-gate answers: NO → a
      full-rejection D-entry (re-trigger clause included) + the DESIGN.md
      Dependency-policy one-liner citing it; GO → a superseding D-entry
      (v1.0.0 removal explicitly superseded) + a build-candidate ROADMAP
      row, with the `irreversible-api` RB gate honored before the GO is
      final.
- [ ] AC4: docs-only — `git diff` for the milestone touches no files under
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

- [ ] T1: Prior-art survey — how do ≥6 comparable CRAN statistics packages
      (candidates: lavaan, psych, lme4, survey, mirt, semTools, easystats
      family; pick for comparability, not convenience) accept variable/
      column specifications; memo §1 with doc citations. (Sonnet-suitable.)
- [ ] T2: Dependency-delta — tidyselect's transitive Imports closure and
      minimum R version vs current DESCRIPTION (R >= 3.4, rlang already
      imported for `.data` only); note D-006's vctrs refusal; memo §2.
- [ ] T3: Ergonomics + ambiguity spike — rewrite ≥3 vignette call sites
      (e.g., `ssm_analyze(jz2017, scales = PANO())`,
      `intermediate-ssm-analysis.Rmd` measures/grouping calls, a long
      `score()` items case) in NSE form; runnable snippets for the
      column/env collision and embracing cases; memo §3.
- [ ] T4: Synthesis + decision — memo §4 verdict weighing all strata against
      the v1.0.0 rationale; append the D-entry and DESIGN.md line (NO path)
      or superseding D-entry + build candidate (GO path). (RB tripwire:
      irreversible-api — offer Fable escalation before finalizing a GO;
      a NO leaves the shipped API untouched and needs no escalation.)

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-16: created by /milestone-plan (plan-gate: deeper decision
  milestone; rejection-scope = full; DESIGN.md one-liner = yes).

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md -->

## Review
<!-- owner: review · exclusive; evidence per criterion, consistency-gate
     results, review findings + triage. EXEMPT from the 150-line cap (M55). -->
