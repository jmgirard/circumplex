# RR17: Completeness of the norms-audit abort-site enumeration (M81)

- **Date:** 2026-08-09
- **Answers:** cairn/reviews/RB17-norms-audit-abort-enumeration.md
- **Reviewer:** independent Fable-context review, fresh session
- **Evidence base:** all five listed materials read in order; every escape
  claim below measured by planting the form at the end of
  `data-raw/audit-norms.R`, running `norms_audit_abort_sites()` over the
  mutated file, and restoring byte-clean (`git status` clean at the end;
  measurement script preserved in the session scratchpad). Baseline walk:
  **14 sites** (12 `stop`, 2 `stopifnot` conditions), matching the milestone's
  record.

## 1. Can it be made complete? The escape enumeration

Measured on the shipped script, "escapes" meaning the walk still returns 14
sites with the form planted; "collected" meaning the count moved to 15.

**Escapes — silent drops *within* a recognised head** (the mechanism-3
family; the enumerator knows it is looking at `stopifnot` and discards what
it does not classify):

1. **Named `stopifnot` condition** (`"msg" = cond`). Escapes (measured 14).
   Reachable in this script's idiom: it is R's documented form, the natural
   next edit for anyone adding a guard with a better message, and the exact
   form the gate's planted `divisor` guard used.
2. **`stopifnot(exprs = { ... })`**. Escapes; worse, rewriting the two
   existing conditions this way returns 12 against a baseline of 14 (the
   gate's measurement, consistent with mine). Documented R, so reachable in
   principle; not used today.
3. Also in this family though not an *escape*: a `stop()` whose arguments are
   all non-literal — `stop(sprintf(...))` or `stop(<condition object>)`
   (measured: collected, count 15) — enters the domain but with the
   degenerate key `{}`, whose matcher regex is `"."` (measured:
   `norms_audit_key_regex("{}")` returns `"."`). Collected-but-unmatchable is
   a distinct failure class: AC1 stays green while AC2's assertion becomes
   vacuous.

**Escapes — heads outside the recognised four** (invisible to any walk keyed
on those heads):

4. **Aliasing** (`fail <- stop; fail("boom")`). Escapes (measured 14). Not in
   this script's idiom; in-principle only.
5. **`do.call("stop", list(...))`**. Escapes (measured 14). The script's only
   `do.call`s are `do.call(rbind, ...)` (lines 427, 636, 637, 697, 731);
   in-principle only.
6. **`rlang::abort()` / `cli::cli_abort()`**. Escapes (measured 14 for
   `rlang::abort`). Not used in the script today — but this is the *most
   plausible* real-world entry route: the repo's own profile prefers rlang
   for user-facing conditions, so a well-meaning modernisation pass is the
   realistic way one of these lands in the file.
7. **`(stop)("boom")`** — parenthesised head deparses to `"(stop)"`.
   Escapes (measured 14). In-principle only; trivially normalisable.
8. **`eval(parse(text = "stop(...)"))`**. Escapes (measured 14) — the code is
   a string, not a call. In-principle only; no static procedure of any kind
   can see it.
9. **`get("stop")(...)`, `base:::stop`**, and any other runtime name
   resolution. Escapes by the same argument as 4 and 8. In-principle only.
10. **`quit(status = 1)` / `q()`**. Aborts the *process* without signalling
    an R condition; escapes (measured 14). In-principle only.
11. **`warning()` under `options(warn = 2)`**. An abort by promotion, with no
    `stop` head anywhere. In-principle only.
12. **`match.arg()`**, subscript failures (`b[[NA]]`), coercion errors,
    `readLines()` connection failures, `.subset2`. Not `stop`-headed calls at
    all — and two of these are *reachable in this script today*: the
    registry's own RELOCATES annotations document `readLines()` raising
    "cannot open the connection" and `b[[NA]]` raising "subscript out of
    bounds" when a guard is no-oped. M81's Scope already declines this domain
    explicitly ("no procedure available here enumerates that domain, and AC2
    states the bound instead of claiming it") — that disposition is correct
    and should stand. `Recall` is not itself an abort form; a `stop` inside a
    recursive body is in the tree.

**Covered, for the record** (measured, count 15): a `stop()` inside an
anonymous function passed to `lapply` (the function's body is in the parse
tree); the native pipe `"x" |> stop()` (transformed to `stop("x")` at parse
time); and — by accident — `eval(quote(stop(...)))`, because the quoted call
is itself a call node in the tree.

So: two escape families. Family A (items 1–3) is *misclassification inside a
recognised head* — the enumerator silently discards argument shapes it does
not understand. Family B (items 4–12) is *heads the grammar does not name*.
All three historical mechanisms (function-scope, defs-only-source, named
args) plus both known current defects are Family A or its domain-boundary
analogue. Nothing has ever escaped through Family B in this repo — yet.

## 2. Is syntactic enumeration the right instrument at all?

**Over unconstrained R source: no, and provably not.** "The set of source
positions whose evaluation signals an error" is a semantic property of a
language with first-class functions, rebindable names, and runtime code
construction (`eval`/`parse`/`do.call`); Rice's theorem territory. Even the
narrower target "the set of calls that invoke `base::stop` at runtime" is
defeated by items 4, 8, and 9 above, which no parse-tree walk — indeed no
static procedure — can see. Three failures by three mechanisms is not bad
luck; it is what chasing an undecidable property with a syntactic instrument
looks like. The promise "the registry's domain is the set of all abort sites
in this file" is not achievable by any static procedure, and the milestone
should stop implying it.

**But the diagnosis is narrower than the theorem.** Every actual failure has
been Family A: the enumerator recognised the head and then *silently dropped*
what it did not classify. Silence under non-recognition is a design choice,
not a necessity. An enumerator that is **fail-closed over its own grammar**
— one that reddens the suite the moment it meets an argument shape it cannot
classify, instead of discarding it — converts every future Family-A
mechanism from a silent hole into a loud test failure. That is the same
doctrine the audit script itself follows for markers ("refuses the ones it
cannot read unambiguously"), applied one level up. Family B is then handled
the only way an open world can be: a stated, closed denylist of known
alternative abort spellings, with everything beyond the denylist explicitly
outside the promise.

Conclusion: syntactic enumeration is the right instrument **for a stated
closed grammar of abort forms, enforced fail-closed**. It is the wrong
instrument for "all aborts", and the promise must say which one it is making.

## 3. What shape should the promise take?

**(a) Constrain the script (`audit_abort(key, ...)`).** Rejected as the
primary move. It closes the *positive* domain by construction, but the
*negative* assertion it depends on — "no bare `stop`/`stopifnot`/
`rlang::abort` appears anywhere" — is itself a syntactic sweep with exactly
the Family-B escape surface (aliasing, `do.call`, `eval(parse)`). So (a) does
not eliminate the residual risk; it relocates it into the hygiene lint, while
also rewriting all 13 abort sites of a script whose textual stability is part
of its audit value, losing `stopifnot`'s automatic condition-deparse, and
requiring the Scope amendment. Keep it as the named fallback: if a fourth
mechanism of this shape lands *after* the fail-closed repair, (a) is the next
move, and at that point the amendment is justified.

**(b) Behavioural enumeration.** Rejected as the primary move. Discovering
sites by tracing `stop` under execution bounds the domain by the fixtures —
an abort site no fixture reaches is invisible, so the procedure cannot
distinguish "no unregistered sites" from "no fixture happened to reach one".
That is precisely the clean-run-over-unread-data shape this audit exists to
refuse. However, behavioural *binding* — using execution to verify that a
fixture reached *this* site — is the right complement to syntactic
*discovery*, and question 4 adopts it for the duplicate-template pair.

**(c) Keep the syntactic walk, widened and narrowed — RECOMMENDED, in the
hardened form:**

1. **Fail-closed classification within the four heads.** For every collected
   call, classify every argument or fail the suite naming the site. For
   `stop`: positional args are template fragments; named args `call.` and
   `domain` are control; any other named arg is a classification failure; a
   template with under-floor literal content (see Q5) is a build failure.
   For `stopifnot`: positional args are conditions keyed on deparsed text;
   named args are conditions keyed on the *name* (which is the runtime
   message) — except `exprs`, `exprObject`, and `local`, which are
   classification failures (statically enumerable in principle, but refusing
   keeps the grammar closed; the script does not use them, and introducing
   one now reddens loudly instead of dropping conditions).
2. **A denylist sweep over every call in the tree** (the walk already
   supports `heads = NULL`): fail, naming the site, on any call whose head
   deparses to `rlang::abort`, `abort`, `cli::cli_abort`, or `cli_abort`; any
   `do.call` whose first argument is the string or symbol of a recognised
   head; and any assignment (`<-`, `=`, `assign`) whose right-hand side is
   the bare symbol of a recognised head. Normalise a parenthesised head by
   stripping `(...)` before matching. The list is closed and stated in the
   test.
3. **The promise text narrows to the grammar.** AC1 claims completeness over
   the stated recognised forms plus the stated denylist, and lists what is
   outside: dynamically resolved names, string-built code, process exits,
   warning promotion, and non-call failure mechanisms (the AC2 bound, which
   already exists and is correct).

This requires **no Scope amendment** (the script is untouched; transient
mutation-measurements restored byte-clean are established M81 practice, not
"adding a guard") and **no new dependency** (base R parse-tree walking; not
even `codetools`).

**(d) Anything better?** One addition worth making cheap: the denylist and
classification failures should report the offending call *deparsed*, so the
red test names the site a reader can find. Beyond that, no; the hardened (c)
is the honest maximum for a test-side-only repair.

## 4. Site identity

**Message template alone: wrong.** Two proven defects — the degenerate `{}`
key whose matcher accepts anything, and the intentional two-site collision
that leaves AC2's "provokes *that* site" asserted in a comment rather than
checked.

**Source position alone: also wrong, twice over.** First, a srcref key
churns under any edit above the site: a whitespace reflow or an added comment
reddens the registry, which the repo rule names as a defect in the test.
Second — decisive — it is not mechanically checkable against the raised
condition: every `stop()` in this script passes `call. = FALSE`, so
`conditionCall()` is `NULL` and the error object carries nothing a srcref can
be compared to. A srcref identity would have to be verified by stack
inspection for *every* site, importing fragility everywhere to solve a
collision that exists in one place.

**Recommended identity: the composite
`(kind, enclosing top-level binding, template)`**, where the enclosing
binding is the name of the top-level `name <- function(...)` the site sits
inside (`"<run>"` for a site in the trailing run block), recorded by the same
walk while descending, plus an ordinal only if a full composite still
collides (none does today). Properties: stable under reflow and under edits
to other functions (no line numbers anywhere); discriminates the
`source note not found` pair (`parse_source_note` vs
`source_note_block_tags`); reddens only when a function is renamed — which
changes the name a stack-based binding check must look for, so the rename is
visible to the checking machinery itself, not a purely formal churn.
Registry build refuses two entries with an identical full identity. Srcrefs
may ride along as *diagnostic metadata* (for failure messages), never as
identity.

**Fixture binding under it:** the fixture must raise a message matching the
site's template (hardened per Q5) — and, *for any identity whose template is
shared with another site's* (today: exactly the pair), the test additionally
captures the abort's frame stack via a calling handler and asserts some
frame's function is `identical()` to the sourced environment's binding of the
identity's named function. That makes AC2's per-site claim mechanically
checkable where the message cannot discriminate, at the cost of stack
inspection only where it is needed. The known limitation — `stopifnot`
condition keys redden under a formal rename such as `batch` → `b` — is not a
violation of the refactor rule: `stopifnot` deparses the actual condition
into the *runtime error message*, so the rename changes the script's
observable abort output, and a registry keyed on messages tracking that
change is tracking behaviour, not form. Record that reading in the milestone
so it is not re-litigated.

## 5. The matcher

Specification for `expect_abort_at_site()` and the registry build, given the
Q4 identity:

1. **Build-time refusal of non-discriminating matchers: yes.** The registry
   build (or the enumerator, whichever constructs matchers first) must
   *error* — not warn, not degrade — on any `stop`-kind key whose total
   literal content is below a stated floor. Measured today: the shortest
   shipped key carries 23 literal characters (`"source note not found: "`),
   the longest 127. **Floor: 15 characters, tolerance [10, 23]** — any value
   in that band refuses `{}` and every sprintf-degenerate rewrite while
   accepting all 12 shipped keys. A silently weak matcher is mechanism four
   waiting to be found at a gate; refusing at build time is the same
   fail-closed doctrine as Q3.
2. **`stop` kind:** regex from the template as now (`.*` at each `{}`),
   with the floor above guaranteeing the regex has real anchoring content.
3. **`stopifnot` unnamed-condition kind:** keep the stem-prefix match
   (pinning R's truncation width would pin R's internals — the helper's
   existing reasoning is right) but add the missing floor: require
   `nchar(stem) >= min(nchar(squish(key)), 40)`. Measured: R's truncation
   currently leaves 66 characters of this script's longest condition before
   the `....` marker, so 40 passes both shipped conditions with margin while
   rejecting the one-character stems the current `nzchar()` check accepts
   (measured: stem `"i"` currently passes against
   `"is.data.frame(batch)"`). Tolerance on the constant: [20, 60].
4. **`stopifnot` named-condition kind (new under Q3):** the runtime message
   *is* the name, exactly — match by full string equality of
   `conditionMessage()` (after `stopifnot`'s prefix/suffix stripping if R
   adds any; measured today the message is the bare name). Equality is the
   strongest matcher available anywhere in this suite; use it.
5. **Cross-discrimination as a test, not a comment (F9):** capture each
   fixture's actual message once, evaluate every site's matcher against
   every captured message, and assert the matrix is diagonal-only at the
   *identity* level — the shared-template pair discriminates via the Q4
   stack assertion, so no intended off-diagonal cell remains.

## 6. Binding criteria

See `## Binding criteria` below.

## Beyond the brief

- **The run block contains zero abort sites today** (measured: no
  `stop`/`stopifnot` in lines 761–821), so F10's collectable-but-unfixturable
  tension is currently vacuous. State the disposition now, in the milestone,
  so it is policy rather than surprise: an abort site added to the run block
  must be introduced through a named function to be fixturable, and until
  then the set-equality test reddening on it is the intended behaviour.
- **`eval(quote(stop(...)))` is collected by accident** (the quoted call is a
  call node in the tree; measured, count 15). Harmless over-collection —
  fail-closed in the right direction — but worth a one-line comment in the
  helper so a future reader does not "fix" it.
- **`marker_defs()` and `roster_defs()` are the same function twice**
  (test-norms-audit-markers.R:30, test-norms-audit-roster.R:13); the shared
  helper file is the natural home. Cosmetic; no criterion.
- **`norms_audit_resolves_name()` (F6)** shares the bounded-promise shape —
  it checks neither `:::`'s package operand nor `get()`'s `envir` — but is
  outside this brief's questions; it is logged at the gate below threshold
  and I do not promote it. If BC1's promise-text pattern is adopted, the same
  "stated bound" sentence style fits its comment too.

## Recommendations

1. **Apply** — hardened (c): fail-closed argument classification within the
   four heads, plus the closed denylist sweep, plus the narrowed promise
   text (Q3; BC1–BC4). No Scope amendment required.
2. **Apply** — composite site identity `(kind, enclosing binding, template)`
   with build-time duplicate refusal and stack-assertion binding for
   shared-template identities (Q4; BC5–BC6).
3. **Apply** — matcher floors and build-time refusal of non-discriminating
   matchers; full-equality matching for named-form conditions;
   cross-discrimination matrix as a test (Q5; BC7–BC8).
4. **Consider** — parenthesised-head normalisation in the walk (strip
   `(...)` around the head before deparse); one line, closes escape 7.
5. **Consider** — folding `marker_defs()`/`roster_defs()` into the shared
   helper.
6. **Reject** — alternative (a), `audit_abort()` constraint, *as the current
   move*: it relocates rather than removes the Family-B residual, rewrites
   13 sites of a stability-valued script, and needs a Scope amendment — all
   for no net gain over hardened (c) today. Named fallback: if AC1 or AC2
   fails again by a *new* mechanism after BC1–BC8 land, adopt (a) then, with
   the amendment.
7. **Reject** — alternative (b), behavioural enumeration, as the discovery
   procedure: coverage bounded by fixtures is the exact false-negative shape
   the registry exists to refuse. Its execution machinery is adopted only
   for per-site *binding* (BC6).
8. **Reject** — srcref as site identity: churns under behaviour-preserving
   edits (repo rule) and is unverifiable against conditions raised with
   `call. = FALSE`.

## Binding criteria

- BC1 **Bounded promise.** AC1's completeness claim names its domain inside
  the criterion text: calls whose (paren-normalised) head deparses to `stop`,
  `stopifnot`, `base::stop`, or `base::stopifnot`, where a `stopifnot` site's
  conditions are its positional arguments plus every named argument except
  `exprs`, `exprObject`, and `local` (a named condition's key is its name).
  The criterion also names what is outside the promise: dynamically resolved
  or constructed calls (`get`, `eval(parse(text=))`, aliasing beyond BC4's
  denylist), process exits, `warning` promotion, and non-call failure
  mechanisms per the existing AC2 bound. No acceptance criterion of M81
  claims enumeration of "all aborts" or any domain no named procedure
  produces.
- BC2 **Fail-closed classification.** The enumerator raises an error —
  reddening the suite and naming the offending call deparsed — on any
  instance of a recognised head it cannot fully classify: a `stop()` named
  argument other than `call.`/`domain`; a `stopifnot()` argument named
  `exprs`, `exprObject`, or `local`; a `stop()` whose message template
  carries fewer than 15 literal characters (tolerance: any floor in
  [10, 23]; shipped minimum measured 23). Mutation-verified, script restored
  byte-clean after each: planting `stopifnot(exprs = {TRUE})` reddens the
  enumeration (pre-repair baseline: silently returns 12 sites of 14);
  planting `stop(sprintf("boom %s", 1))` reddens at matcher build
  (pre-repair baseline: builds regex `"."`).
- BC3 **Named-form coverage.** Planting
  `"divisor must be numeric" = is.numeric(batch$divisor)` in
  `validate_batch()`'s `stopifnot()` moves the collected count 14 → 15 and
  reddens the set-equality test as an unregistered site (pre-repair
  baseline, measured at the gate and reproduced: 14 → 14, suite green at
  FAIL 0 | PASS 76). Script restored byte-clean.
- BC4 **Denylist sweep.** A test walks every call in the script's parse tree
  and fails, naming the site, on: any call whose paren-normalised head
  deparses to `rlang::abort`, `abort`, `cli::cli_abort`, or `cli_abort`; any
  `do.call` whose first argument is the string or symbol of a BC1 head; any
  `<-`, `=`, or `assign` call whose right-hand side is the bare symbol of a
  BC1 head. The list is closed and stated in the test source.
  Mutation-verified for at least `do.call("stop", list("x"))` and
  `fail <- stop` (both measured invisible to the current walk: count stays
  14); script restored byte-clean. The criterion states that names resolved
  at runtime are outside the sweep, per BC1.
- BC5 **Site identity.** Every registry entry and every collected site
  carries the composite identity `(kind, enclosing top-level binding name —
  "<run>" for run-block sites, key)`; registry construction errors on two
  entries with an identical full identity; the two `source note not found`
  sites carry distinct identities (`parse_source_note`,
  `source_note_block_tags`). Identity contains no line or column numbers:
  inserting a comment line above every function in a scratch copy of the
  script and re-running the enumeration yields an identical identity set.
- BC6 **Mechanical fixture binding.** For every site whose key is shared by
  a site in a different function (today exactly the `source note not found`
  pair), the per-site test additionally captures the abort's frame stack via
  a calling handler and asserts a frame's function is `identical()` to the
  sourced environment's binding named by the site's identity — so AC2's
  "provokes *that* site" is checked by the suite, not asserted in a comment.
  Mutation-verified: pointing one of the pair's fixtures at the other
  function's trigger reddens its binding assertion.
- BC7 **Matcher floors.** `expect_abort_at_site()` (or the registry build)
  rejects a `stopifnot` stem shorter than `min(nchar(squish(key)), 40)`
  (tolerance on the constant: [20, 60]; measured: R's truncation leaves 66
  characters of the script's longest condition, and the current check
  accepts a 1-character stem). A named-form `stopifnot` site is matched by
  full string equality of the condition message with the key. No matcher in
  the suite reduces to a regex matching every string.
- BC8 **Cross-discrimination as a test.** One test captures each fixture's
  actual `conditionMessage()` once, evaluates every site's matcher against
  every captured message, and asserts the acceptance matrix is
  diagonal-only at the identity level (the shared-key pair discriminated by
  BC6's stack assertion, not exempted by comment).
- BC9 **Regression floor.** All previously verified properties hold on the
  finished branch: the AC3 mutations (both `validate_batch` conditions), the
  AC4 mutations, and the T4 run-block mutant redden as recorded in the work
  log; `devtools::test()` and `devtools::check(args = "--no-manual")` clean;
  re-running the audit leaves `data-raw/norms-audit-ledger.csv` and
  `data-raw/norms-audit-coverage.csv` unchanged but for the three stamp
  columns, compared column by column (AC5 unchanged).

## Binding criteria (revision 2, 2026-08-09)

This section supersedes `## Binding criteria` above, which stays as the
record of what was first proposed. Reissued after the ingest audit ([O],
fresh context). What changed and why:

- **BC8 (rev 1) was unsatisfiable as written**: under the composite identity
  the two `source note not found` sites share one key and therefore one
  message matcher, so a `conditionMessage()`-only matrix has a structurally
  guaranteed off-diagonal cell. Rev 2's BC10 asserts the off-diagonal
  accepting set *equals the declared shared-key pair set* instead of
  asserting diagonality, and delegates discrimination of those cells to the
  stack assertion (BC8).
- **BC7's (rev 1) closing sentence** — "no matcher in the suite" — was an
  unbounded universal over a domain no named procedure enumerates, the exact
  defect RB17 §6 forbids, and false today (audit re-measure: 79 bare
  `expect_error()` calls across 19 files, all outside M81's Scope In).
  Rev 2 bounds every matcher claim to registry-built matchers; the
  suite-wide `expect_error` hygiene is explicitly not bound here.
- **BC2's (rev 1) mutation evidence conflated two mutants**: appending
  `stopifnot(exprs = {TRUE})` leaves the shipped walk at 14, unchanged
  (re-verified 2026-08-09); the 12-of-14 figure belongs to rewriting
  `validate_batch()`'s two existing conditions into `exprs` form (gate
  measurement). Rev 2 states both correctly, gives the discrimination floor
  one home (registry-build matcher construction, BC9), and narrows the
  literal-floor tolerance to [10, 20].
- **BC5 (rev 1) dropped the ordinal escape hatch** its own §4 specified,
  making AC1 and the duplicate-identity refusal jointly unsatisfiable for
  two same-key sites in one function; carried no mutation clause; and its
  comment-insertion check passes against the shipped enumerator
  (`keep.source = FALSE`), so it is a guard, not a verification. All three
  fixed in BC7.
- Smaller audit items, all accepted: BC4 (rev 2) now binds the test name at
  `test-norms-audit-markers.R:464` and the helper header at
  `helper-norms-audit-script.R:11-14`, not only AC text. The denylist's
  alias/`assign`/`<<-`/higher-order gaps are closed by one cleaner rule — a
  bare BC1-head symbol in any non-head position — plus a named-`what`
  clause for `do.call`. BC8 (rev 2) pins "innermost sourced-binding frame"
  and requires non-vacuous capture. The stem-floor tolerance narrows to
  [20, 45]. Locale flakiness is bound: measured 2026-08-09, under
  `LANGUAGE=fr` `stopifnot(is.data.frame(batch))` raises
  `"is.data.frame(batch) n'est pas TRUE"`, `norms_audit_stopifnot_stem()`
  strips nothing, and the shipped match fails — so message assertions pin
  the C locale. The rev-1 BC9 universal ("all previously verified
  properties") is replaced by an enumerated list, and its "`test()` clean"
  is restated to match the T6 record (FAIL 0 suite-wide; the milestone's
  files WARN 0 in isolation).
- **Partition**: `[M81]` keeps what is built plus the minimal repair for
  the demonstrated hole (the named-`stopifnot` miss) and the promise-text
  bounding; `[successor]` takes the denylist, composite identity, stack
  binding, matcher floors, and the matrix. Dependency direction: the
  successor depends on M81 (its identity and matrix quantify over the
  classification-complete enumeration M81 ships); M81 depends on nothing
  new. Inseparable groups are flagged on the criteria.

- BC1 `[M81]` **Named-`stopifnot` conditions collected.** The enumerator
  treats every named argument of a collected `stopifnot()` call — except
  `exprs`, `exprObject`, and `local` — as one condition, keyed on its name
  (which is the runtime message). Mutation-verified, script restored
  byte-clean: planting `"divisor must be numeric" = is.numeric(batch$divisor)`
  in `validate_batch()`'s `stopifnot()` moves the collected count 14 → 15
  and reddens the set-equality test as an unregistered site. Pre-repair
  baseline (gate measurement, reproduced 2026-08-09): count stays 14 → 14
  and the suite stays green at FAIL 0 | PASS 76. Inseparable from BC2:
  treating named arguments as conditions forces a decision on `exprs` in
  the same change, or `stopifnot(exprs = {...})` would register a bogus
  site keyed `"exprs"`.
- BC2 `[M81]` **Fail-closed `stopifnot` classification.** The enumerator
  raises an error — reddening the suite and naming the offending call
  deparsed — on any `stopifnot()` argument named `exprs`, `exprObject`, or
  `local`. Mutation-verified, script restored byte-clean after each:
  (i) appending `stopifnot(exprs = { TRUE })` to the script reddens the
  enumeration (pre-repair baseline, measured 2026-08-09: the walk returns
  14, unchanged — the form is silently invisible); (ii) rewriting
  `validate_batch()`'s two existing conditions into `exprs` form reddens
  (pre-repair baseline, gate measurement: 12 sites against 14, silent).
- BC3 `[M81]` **Named-form matcher.** A named-form condition site is
  matched by full string equality of the abort's `conditionMessage()` with
  the site's key — no stem, no regex. Verified by a unit test driving
  `expect_abort_at_site()` with a synthetic named-form site: the exact
  message passes; any strict superstring or substring of it fails.
- BC4 `[M81]` **Bounded promise, in all three texts.** AC1's criterion
  text, the test name at `tests/testthat/test-norms-audit-markers.R:464`
  (today "no abort anywhere in the audit script is left unregistered"), and
  the helper header at `tests/testthat/helper-norms-audit-script.R:11-14`
  (today "The parse tree has neither hole") each state the enumerated
  domain — calls whose deparsed head is `stop`, `stopifnot`, `base::stop`,
  or `base::stopifnot`, with `stopifnot` conditions per BC1/BC2 — and name
  what is outside it: alternative abort spellings (until the successor's
  BC6 denylist, and beyond it for runtime-resolved names), dynamically
  constructed calls, process exits, `warning` promotion, and non-call
  failure mechanisms per the existing AC2 bound. No M81 text claims
  enumeration of "all aborts" or any domain no named procedure produces.
- BC5 `[M81]` **Regression floor, enumerated.** On the finished branch,
  each of the following reproduces with the same FAIL count as recorded in
  the work log (tolerance 0 on FAIL counts; PASS counts may grow by the
  number of tests this milestone adds): the two AC3 mutations (FAIL 2
  each), the two AC4 mutations (FAIL 1; control FAIL 0), and the T4
  run-block mutant (FAIL 1), each restored byte-clean. `devtools::test()`
  FAIL 0 suite-wide, and the milestone's two test files WARN 0 run in
  isolation (the T6 record's 4 warnings lie outside them);
  `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes;
  re-running the audit leaves `data-raw/norms-audit-ledger.csv` and
  `data-raw/norms-audit-coverage.csv` unchanged but for the three stamp
  columns, compared column by column.
- BC6 `[successor]` **Denylist sweep.** One test walks every call in the
  script's parse tree and fails, naming the deparsed call, on: (i) any call
  whose paren-normalised head deparses to `rlang::abort`, `abort`,
  `cli::cli_abort`, or `cli_abort`; (ii) any `do.call` or `base::do.call`
  whose first positional argument, or argument named `what`, is the string
  or symbol of a BC4 head; (iii) any appearance of a bare BC4-head symbol
  in a non-head position of any call — one rule covering `fail <- stop`,
  `fail <<- stop`, `assign("fail", stop)`, and `lapply(msgs, stop)` alike.
  The list is closed and stated in the test source; runtime-resolved names
  stay outside per BC4. Mutation-verified for at least
  `do.call("stop", list("x"))` and `fail <- stop` (both measured
  2026-08-09: invisible to the shipped walk, count stays 14), restored
  byte-clean.
- BC7 `[successor]` **Composite site identity with ordinal.** Every
  registry entry and collected site carries
  `(kind, enclosing top-level binding name — "<run>" for run-block sites,
  key, ordinal)`, the ordinal assigned in source order and distinguishing
  only entries otherwise identical. AC1's set equality compares this full
  identity in both directions, superseding "by key and count". Registry
  construction errors on two entries with identical full identity;
  mutation-verified: registering one entry twice reddens the build. Two
  same-key sites inside one function remain jointly satisfiable: verified
  on a scratch copy with two identical planted `stop()` calls in one
  function — the walk yields two identities differing in ordinal. The
  `source note not found` pair carries distinct identities
  (`parse_source_note`, `source_note_block_tags`). Identity contains no
  line or column numbers; the comment-insertion invariance check is
  recorded as a standing guard against future srcref keying, not as
  verification — it passes against the shipped enumerator too, which
  parses with `keep.source = FALSE`. Inseparable from BC8 and BC10: the
  matrix criterion is only satisfiable given identity-level structure plus
  stack discrimination, and cutting them apart across milestones would
  reproduce rev 1's BC8 failure at a milestone boundary.
- BC8 `[successor]` **Stack-bound fixtures for shared keys.** For every
  site whose `(kind, key)` is shared with a site under a different
  enclosing binding (today: exactly the `source note not found` pair), the
  per-site test captures the abort's frame stack via a calling handler
  established around the thunk with no exiting handler between it and the
  abort, asserts the capture is non-empty — a vacuous capture is a
  failure, never a silent pass — and asserts the *innermost* captured
  frame whose function is a sourced-environment binding is `identical()`
  to the binding the site's identity names ("innermost", so the assertion
  does not degrade if one of the pair ever calls the other).
  Mutation-verified: pointing one pair fixture at the other function's
  trigger reddens its binding assertion. Inseparable from BC7 and BC10.
- BC9 `[successor]` **Matcher floors, one home, locale pinned.** All
  discriminating-power checks live in one procedure: matcher construction
  at registry-build time. `expect_abort_at_site()` consumes prebuilt
  matchers and adds no floor of its own. The constructor errors on: a
  `stop`-kind key with fewer than 15 literal characters (tolerance: any
  floor in [10, 20]; shipped minimum measured 23, so the band keeps ≥ 3
  characters of headroom before a behaviour-preserving message edit could
  redden the build); a `stopifnot` stem shorter than
  `min(nchar(squish(key)), 40)` (tolerance on the constant: [20, 45];
  measured: R's truncation leaves 66 characters of the script's longest
  shipped condition, so the band keeps ≥ 21 characters of headroom, and
  the shipped check accepts a 1-character stem — measured). Every test
  asserting a `stopifnot` message runs with messages pinned to the C
  locale (`LANGUAGE=C` and `LC_MESSAGES=C` for the expression): measured
  2026-08-09, under `LANGUAGE=fr` the verdict is "n'est pas TRUE", the
  English-only strip removes nothing, and the shipped match fails. Must
  land no later than BC10, which shares the locale pin and whose
  off-diagonal set a degenerate matcher would explode.
- BC10 `[successor]` **Cross-discrimination matrix over declared pairs.**
  One test captures each fixture's `conditionMessage()` once (locale
  pinned per BC9), evaluates every registry-built matcher against every
  captured message, and asserts the set of accepting off-diagonal cells
  *equals* the declared shared-key pair set — today the two cells of the
  `source note not found` pair, in both directions, and nothing else.
  The declared cells are discriminated by BC8's stack assertion, not
  exempted by comment. The claim is bounded to registry-built matchers;
  suite-wide `expect_error` hygiene (audit re-measure: 79 bare calls
  across 19 files, all outside Scope In) is explicitly not bound by this
  criterion. Inseparable from BC7 and BC8.
- BC11 `[successor]` **Successor gate floor.** At the successor's review
  gate, every `[M81]` criterion above reproduces as stated (FAIL-count
  invariants, tolerance 0; PASS counts may grow with added tests),
  `devtools::test()` FAIL 0 with the touched test files WARN 0 in
  isolation, `devtools::check(args = "--no-manual")` 0/0/0, and the AC5
  stamp-only CSV comparison holds column by column.
