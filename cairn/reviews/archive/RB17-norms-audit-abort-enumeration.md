# RB17: Completeness of the norms-audit abort-site enumeration (M81)

- **Date:** 2026-08-09
- **Output required:** write findings to `cairn/reviews/RR17-norms-audit-abort-enumeration.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package on CRAN for interpersonal-circumplex data
analysis. It ships normative reference statistics for 15 instruments in
`data/`, transcribed by hand from published sources. Because a transcription
error would silently corrupt every user's standardized scores, the repo
carries a provenance audit: `data-raw/audit-norms.R` (821 lines, a
development-only script, not installed) compares every shipped norm value
against the value recorded in a committed source note under
`cairn/references/`, and writes a ledger plus a coverage report.

The script is defensive: it aborts rather than reporting a clean run over
data it never read. Those aborts are the safety property, so the test suite
carries a **registry of abort sites** — every `stop()`/`stopifnot()` the
script can raise — and asserts two things: that the registry and the script
agree on the set of sites, and that each site has a fixture provoking it with
that site's own message.

The registry's *domain* — how the suite decides what the full set of abort
sites is — has now failed three times, each time by a different mechanism,
each time discovered only at a review gate:

1. **Mechanism 1 (M79, return 2).** The count was scoped to one function's
   body. An abort that landed in a helper function was outside the counted
   domain. Fixed by widening to every function in the sourced environment.
2. **Mechanism 2 (M79, return 3).** The count enumerated the functions left
   behind by a defs-only `sys.source()` of the script — which deliberately
   skips the script's trailing run block. An abort landing in that run block
   was invisible. Measured: the guard counted 12 while the file contained 13.
   This failure is what caused the criterion to be re-cut out of M79 into a
   new milestone, M81.
3. **Mechanism 3 (M81, this review).** M81 replaced the sourced-environment
   count with a walk over the script's **parse tree**, which does cover the
   run block. At the M81 review gate a fresh-context reviewer found, and the
   orchestrator independently verified, that the walk drops **named**
   arguments — so a condition written in R's documented
   `stopifnot("message" = condition)` form contributes no registry key at
   all. Measured on the shipped script: adding a real third guard,
   `"divisor must be numeric" = is.numeric(batch$divisor)`, to
   `validate_batch()` produces a guard that genuinely fires, while the walk
   returns **14 sites against a baseline of 14** and the whole suite stays
   green at FAIL 0 | PASS 76. The `stopifnot(exprs = { ... })` form is worse:
   rewriting the two existing conditions in that form returns **12 sites
   against a baseline of 14**, silently dropping both.

Each repair was locally correct and each looked complete. The pattern — three
mechanisms, one shape — is why this brief exists. The milestone's plan gate on
2026-08-09 explicitly declined escalating this question and recorded the
condition that would reverse the call: *"Falsified by AC1 or AC2 failing again
by a third mechanism of the same shape."* That condition has now fired.

The question is therefore **not** "how do I handle named `stopifnot`
arguments" — that repair is mechanical and the session can write it. The
question is whether a *syntactic enumeration of abort sites over open-ended R
source* can be made complete at all, and if not, what shape the promise should
take instead.

Two secondary findings from the same review gate are the same shape — a
promise wider than the procedure backing it — and are in scope here:

- The matcher that checks a fixture raised *this site's* error builds a regex
  from the site's message template. A site whose message is fully
  interpolated (e.g. a behaviour-preserving rewrite to
  `stop(sprintf("source note %s not found", x))`) yields the key `{}`, whose
  regex is `"."` — matching any error whatsoever, including a broken fixture.
  Verified by measurement.
- The `stopifnot` branch of the matcher compares R's truncated deparsed
  condition as a *prefix* of the registered key, with no length floor: a
  one-character error message is accepted as "aborted at this site". Verified
  by measurement.

## Materials

Read these, in this order. Paths are relative to the repo root. The M81 work
is on branch `m81-norms-audit-abort-registry`; read the files as they stand in
the working tree.

1. `cairn/milestones/M81-norms-audit-abort-registry.md` — the milestone's
   Goal, Scope (In/Out), acceptance criteria AC1–AC5, work log, and the
   `## Review` section recording this gate's findings and their scores.
2. `tests/testthat/helper-norms-audit-script.R` (~185 lines) — the parse walk
   and the keying/matching layer. The functions that matter:
   `norms_audit_calls()` (the tree walk), `call_positional_args()` (the
   named-argument drop that mechanism 3 turns on), `norms_audit_stop_key()`
   and `norms_audit_stopifnot_keys()` (keying), `norms_audit_key_regex()` and
   `norms_audit_stopifnot_stem()` (matching), `expect_abort_at_site()`.
3. `tests/testthat/test-norms-audit-markers.R` — the `SCRIPT_ABORTS` registry
   (14 entries, each `(kind, key, fixture)`), the per-site message test, and
   the set-equality test.
4. `data-raw/audit-norms.R` — the script under enumeration. The abort sites
   are at lines 83, 88, 95, 165, 193, 224, 235, 240, 248, 271, 276, 325, 452.
   The trailing run block, skipped by a defs-only source, begins at line 761.
5. `tests/testthat/test-norms-audit-roster.R` — carries a related assertion
   (`norms_audit_resolves_name()`) built on the same walk.

To reproduce the measurements, from the repo root:

```
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-norms-audit-markers.R")'
```

and, for the walk in isolation:

```
Rscript -e 'library(testthat); source("tests/testthat/helper-norms-audit-script.R"); norms_audit_script_path <- function() "data-raw/audit-norms.R"; str(norms_audit_abort_sites())'
```

If you mutate `data-raw/audit-norms.R` to test a hypothesis, restore it
afterwards (`git checkout -- data-raw/audit-norms.R`) and say in the RR what
you measured.

## Questions

1. **Can it be made complete?** Enumerate the ways an abort site in this
   script could escape a parse-tree walk of the four heads `stop`,
   `stopifnot`, `base::stop`, `base::stopifnot`. Be exhaustive and concrete:
   named `stopifnot` conditions and `stopifnot(exprs = {...})` are two known
   ones — what else? Consider at least: aliasing (`abort <- stop`),
   `do.call("stop", ...)`, `rlang::abort()` / `cli::cli_abort()` (the repo's
   own profile prefers these for user-facing conditions), `match.arg()`,
   `.subset2` failures, `Recall`, aborts raised inside a function passed to
   `lapply`, and conditions signalled with `stop(<condition object>)`. For
   each, say whether it is reachable in *this* script's idiom or only in
   principle.

2. **Is syntactic enumeration the right instrument at all?** Given your answer
   to (1), judge whether the promise "the registry's domain is the set of all
   abort sites in this file" is achievable by any static procedure over
   unconstrained R source. If it is not, say so plainly — that is a
   legitimate and useful answer.

3. **If not, what shape should the promise take?** Evaluate at least these
   alternatives and recommend one, with reasoning:
   (a) **Constrain the script.** Require every abort in `data-raw/audit-norms.R`
   to go through one declared helper (e.g. `audit_abort(key, ...)`), and have
   the guard enumerate calls to that helper plus assert that no bare
   `stop`/`stopifnot`/`rlang::abort` appears anywhere in the file. This makes
   the domain closed by construction, at the cost of touching the script —
   which M81's Scope currently forbids (see Constraints).
   (b) **Behavioural enumeration.** Discover abort sites by executing the
   script's functions under instrumentation (e.g. tracing `stop`) rather than
   by reading them, accepting that coverage is then bounded by the fixtures.
   (c) **Keep the syntactic walk** and widen it to a stated, closed list of
   recognised abort forms, with the promise explicitly narrowed to that list.
   (d) Anything better you see.

4. **Site identity.** Sites are currently keyed on a *message template* (the
   call's literal fragments, with `{}` for interpolated arguments). Two
   defects follow: a fully interpolated message yields the degenerate key
   `{}` whose matcher accepts any error, and two sites can collide on one key
   (the two `source note not found` sites do, intentionally). Is message
   template the right identity for a site? Evaluate keying on **source
   position** instead (`parse(keep.source = TRUE)` gives srcrefs), which is
   unique and complete by construction but churns whenever the file is
   edited. Recommend an identity, and say how a fixture should be bound to a
   site under it so that AC2's "each site has a fixture that provokes *that*
   site" is mechanically checkable rather than asserted in a comment.

5. **The matcher.** Given your answer to (4), specify how
   `expect_abort_at_site()` should assert that a fixture raised *this* site's
   error. Address the degenerate-regex case and the `stopifnot` prefix-stem
   case named in the Background. Should the guard refuse to build a
   non-discriminating matcher at registry-build time rather than silently
   producing a weak one?

6. **Binding criteria.** Emit a `## Binding criteria` section (see Output
   format) stating what M81 must satisfy for this question to be settled.
   These will be ingested verbatim into M81's acceptance criteria and
   mechanically diffed, so make each one measurable and make the *bound* of
   each promise explicit — a criterion claiming completeness over a domain no
   named procedure enumerates is the exact defect this brief exists to
   resolve, and must not be reintroduced in the criteria that repair it.

## Constraints

Fixed, and not to be relitigated — but flag disagreement explicitly rather
than working around any of these silently:

- **The audit's purpose is fail-closed.** The script must abort rather than
  report a clean run over data it never read. Any proposal that trades an
  abort for a warning or a tidy count is against the standing design of this
  audit (the M79 goal, and the reason the registry exists at all).
- **No coverage-percentage target and no test-count target.** Test scope is
  set by acceptance criteria (repo tracking rules).
- **A test that breaks under a behaviour-preserving refactor is a defect in
  the test** (repo tracking rules, "What gets a test"). This bears directly on
  question 4: the current `stopifnot` keying reddens on a pure formal rename.
- **Minimal dependencies.** The package is base R plus rlang, ggplot2, boot,
  Rcpp/RcppArmadillo. Any proposal requiring a new package — including a
  static-analysis package such as `lintr` or `codetools` — must say so
  explicitly, because adding a dependency requires its own user gate and a
  recorded decision, and would likely be refused for a test-only helper.
  Note that `codetools` and `utils` ship with R and are not new dependencies
  in that sense; say which category your proposal falls in.
- **M81's Scope currently declares OUT: "adding, moving, or widening any
  guard in `data-raw/audit-norms.R` — this milestone changes tests and their
  enumeration, not the script's abort sites."** Alternative (3a) would
  violate this. **You are explicitly invited to recommend violating it** if
  that is the right answer; the Scope is amendable through the milestone's
  own gated amendment protocol, and this brief exists precisely because the
  within-Scope repairs have failed three times. Say clearly whether your
  recommendation requires that amendment.
- Also out of scope and not to be reopened: the coverage report's column
  schema (a separate milestone, M80), and any change to `data/`.

## Output format

In `cairn/reviews/RR17-norms-audit-abort-enumeration.md`: answer each question
by number with your reasoning and evidence; list any additional findings
separately under "Beyond the brief"; end with concrete recommendations, each
marked apply / consider / reject-with-reason. Where findings bind
implementation, also emit a `## Binding criteria` section: numbered `BC1…`,
each a measurable assertion checkable against evidence, with any numeric
projection stating its tolerance. These are ingested VERBATIM into M81's
acceptance criteria and mechanically diffed against this file; departures are
legal only through that milestone's shown "Deviations from RR17" table.
