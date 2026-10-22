# M168: Close the degeneracy criterion remainders

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP3, GP4
- **Resolves:** —
- **Surface tier:** internal — test fixtures, a test-suite detector and a devel-only oracle script; no exported behavior changes
- **Branch/PR:** m168-degeneracy-criterion-remainders

## Goal

The test fixtures name their origin, a lavaan rename can no longer silence the CFI corroboration unnoticed, and the exact oracle's parser refuses malformed hex output.

## Scope

**In:** a `provenance` element on the two fixtures in `tests/testthat/fixtures/` that lack one (`rb18-counterexample-b.rds`, `mc-seeded-pins.rds`), under D-070, and the profile rule's pointer to D-070; a test that fails when `lav_cfi_ref()` (`tests/testthat/helper-lavaan-cfi.R`) gets no CFI from the installed lavaan; a value check in `exact_parse()` (`devel/degeneracy-oracle/exact_oracle.R`) for every `HEX_*` key.

**Out:** showing the certificate's estimate on computed fits (row part iii), declined by D-069 at this plan gate. Regenerating any fixture's values: D-070 keeps the bytes. A generator script for either fixture: rejected in D-070. Running the corroboration detector on CRAN: it skips there, so a lavaan release cannot fail the package's CRAN check.

## Acceptance criteria

- [ ] AC1: Every `.rds` file that `list.files("tests/testthat/fixtures", pattern = "\\.rds$")` returns carries a `provenance` element naming its source, its generator, and its seed, or stating which of these was not kept; in `rb18-counterexample-b.rds` and `mc-seeded-pins.rds`, every element present at the plan commit stays `identical()` to its plan-commit value with `num.eq = FALSE`.
- [ ] AC2: When lavaan is installed and `skip_on_cran()` does not skip, the test suite fails if `lav_cfi_ref()` gets no CFI from the installed lavaan's `lav_fit_cfi()` under both argument spellings (neither yields a finite number). It passes on the lavaan installed at review, and fails when `lav_fit_cfi_fn()` is replaced by one returning a function that accepts neither spelling, and by one returning `NULL`.
- [ ] AC3: `exact_parse()` in `devel/degeneracy-oracle/exact_oracle.R` stops, naming the key, when a `HEX_*` value does not split into the word count its key carries (`n_comp` for `HEX_V*` and `HEX_VNAIVE*`, 1 for `HEX_U*`), or when a word does not match `^-?0x[0-9a-f](\.[0-9a-f]+)?p[+-][0-9]+$`. The script's load-time guard asserts this on an empty value, a wrong word count for an `n_comp = 2` stub, and a non-hex word, and parses a valid stub written in Python's `float.hex()` form; every hex word in `cert_frozen` in `tests/testthat/test-axes-certificate.R` matches the same pattern.
- [ ] AC4: The profile's `verify` slot is clean: `devtools::test()` passes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4

## Tasks

- [x] T1: Add the `provenance` element to `rb18-counterexample-b.rds` (source: the RB18 random search over near-collinear 3×3 correlation matrices, `cairn/reviews/archive/RB18-axes-degeneracy-criterion-metric.md`; generator and seed not kept) and to `mc-seeded-pins.rds` (source: the M4 refactor's pre-refactor capture, legacy archive; seed 2026, calls as in `tests/testthat/test-ssm_montecarlo.R`; read its capture commit from `git log` before writing). Rewrite each file once from its plan-commit object plus the element, then confirm every old element `identical(num.eq = FALSE)` against `git show <plan-commit>:<path>` read into R. Point `cairn/PROFILE.md`'s fixture-provenance bullet at D-070. Grep the tests and `devel/` for any read of these fixtures' element names or length that the new element could change.
- [x] T2: In `tests/testthat/test-lavaan-cfi-helper.R`, add a test gated by `skip_on_cran()` and `skip_if_not_installed("lavaan")` only (not by `lav_fit_cfi_fn()` being non-NULL), asserting `lav_cfi_ref()` returns the closed-form CFI at one input. Plant both AC2 defects through `local_mocked_bindings()` on `lav_fit_cfi_fn` and see each fail before trusting the green; record the plants in the work log.
- [x] T3: In `exact_parse()`, check each `HEX_*` value's word count against its key and each word against the AC3 pattern, stopping with the key named. Extend the `local()` guard with the AC3 stubs. Check the pattern against every hex string in `cert_frozen` with a one-off read of the test file, and record the count in the work log.
- [x] T4: Run `devtools::test()`; review's consistency gate runs the check.

## Work log

- 2026-10-22: created by /milestone-plan.
- 2026-10-22: tier check: milestone — the row holds a design decision (fixture provenance) and an API question (part iii).
- 2026-10-22: idea vet: (iii) concern that the certificate runs only on selector-routed fits (D-062), so a shown estimate would be empty on almost every fit and binds GP4 — answered at the question set: do not build (D-069); (iv) the row's premise "two hex words" is wrong (`HEX_V*` carry `n_comp` words, `HEX_U*` one, in Python's `float.hex()` form) — reframed in AC3 by the agent; (i) and (ii) none.
- 2026-10-22: collision check: the source candidate row is absorbed whole ((i), (ii), (iv) here, (iii) to D-069); M107 and M148 archives are the origin of (i), (ii) and (iv); D-062 and D-067 bear on (iii) and are cited in D-069; no planned or in-progress milestone overlaps (M167 is blocked on CPM intervals).
- 2026-10-22: inbox sweep: 0 open issues, 0 open PRs on jmgirard/circumplex.
- 2026-10-22: lessons harvested: M65 family (a failing probe skips invisibly; lavaan renames) shapes T2; M108 family and M116 (plant in the code under test) shape T2's plants; M147 (`identical()` needs `num.eq = FALSE`) shapes AC1.
- 2026-10-22: scope widened from the row's (i) to the fixtures directory: a sweep of the 8 `.rds` files found `mc-seeded-pins.rds` also without provenance, so AC1 quantifies over the directory listing.
- 2026-10-22: criteria audit (reduced mode, internal tier, fresh Opus reader): AC1 note that "or capture commit" was weaker than the profile rule — fixed by "stating which of these was not kept" plus D-070's exception; AC2 "under either spelling" read as always failing and "not on CRAN" undefined — fixed to "under both" and `skip_on_cran()`; AC3 unsatisfiable (per-key word counts, Python hex form, platform subnormals) — fixed to per-key counts and a fixed pattern.
- 2026-10-22: question set: show the certificate's estimate on computed fits — not built, recorded as D-069; fixture provenance — record origin and keep bytes, recorded as D-070.
- 2026-10-22: plan gate chose recording provenance over a literal-copy generator because the generator would copy known bytes without rerunning the search; falsified by a fixture whose missing generator hides a stale value a test trusts.
- 2026-10-22: plan chose a CRAN-skipped detector over one that runs everywhere because a lavaan internal rename would otherwise fail the package's CRAN check; falsified by a rename that reaches a release without CI having run on the new lavaan.
- 2026-10-22: no changelog entry: internal tier, no user-visible change.
- 2026-10-22: status in-progress; branch m168-degeneracy-criterion-remainders cut from master at bfdc649f. Untracked `devel/m167-interval-smoke.rds` (M167's) left unstaged.
- 2026-10-22: T1 done: both fixtures gained a `provenance` list (source, generator, seed) via a one-off scratch script; every old element `identical(num.eq = FALSE)` to the bfdc649f copy read through `git show` (5 of 5 and 3 of 3); all 8 fixtures now carry provenance (`growth-fixef.rds` as one string naming all three); mc-seeded-pins' capture read from 6c7e5fc6 and its test comment; PROFILE.md bullet points at D-070; no test reads either fixture's element names or length; certificate, scaled-fit and Monte Carlo test files green. The simple-english hook flagged PROFILE.md's pre-existing text, left as is.
- 2026-10-22: T2 done: "M168: the corroboration reaches the installed lavaan" added, gated by `skip_on_cran()` and `skip_if_not_installed("lavaan")`; green on lavaan 0.7.2 (CFI 0.6). Plants were made in `lav_fit_cfi_fn()` in helper-lavaan-cfi.R, not by mocking, and each was reverted to zero diff. The first plant returned a function accepting neither spelling (it would return 0.6 if reached), and the second returned `NULL`. Each failed the M168 test at line 77 with `actual` NULL. Under `NOT_CRAN=false` the test skips "On CRAN". Implementation choice: planting the code under test over `local_mocked_bindings()` (lesson M116).
- 2026-10-22: T3 done: `exact_parse()` checks each `HEX_*` word count (`exact_hex_words()`: `n_comp`, 1 for `HEX_U_*`) and form (`exact_hex_pattern`), stopping with the keys named. The load-time guard stub now uses float.hex() words at per-key counts and adds empty, wrong-count and non-hex stubs. The guard, run alone, passes, and 383 of 383 hex strings in `cert_frozen` (test-axes-certificate.R lines 237-393) match the pattern. Plants in the parser each fail the guard (count check removed; form check removed), and the unplanted parser passes. A full `Rscript devel/degeneracy-oracle/exact_oracle.R` exits 0 with ANCHORS, SWEEP, REACHABLE and CERTIFICATE all PASS. The header's "carries only `S` and `ia`" claim was reworded, because the fixture also holds `a`, `b`, `kappa` and now `provenance`.
- 2026-10-22: T4 done: `devtools::test()` gave 15274 passed, 0 failed, 0 errors, and 1 skip (test-axes-scaled-fit.R:930, gated on the R and lavaan version, untouched). The 11 warnings all come from untouched files (ci_accuracy, pole-values, print-width, ssm_sem).
- 2026-10-22: claim audit: not owed — internal tier.
- 2026-10-22: status review; no deviations from the plan; no amendments.
