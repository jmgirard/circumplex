<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M12: Result-label DRY + statistical-core coverage tracking

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m12-label-dry-coverage   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Single-source the duplicated Group/Measure/Label construction in
`ssm_analyze*()` and add statistical-core coverage tracking, both landing on
master before the v2.0.0 freeze.

## Scope

**In:**
- Extract the Group/Measure/Label construction currently copy-pasted four times
  in `R/ssm_analysis.R` — mean-path pre-CI (`:333`–`:340`) and post-CI
  (`:370`–`:377`), correlation-path pre-CI (`:458`–`:472`) and post-CI
  (`:504`–`:518`) — into a single helper. **Byte-identical** output: the
  `Label`/`Group`/`Measure` columns of `ssm_analyze()` results and scores must
  not change for any branch (mean/corr × grouping/no-grouping × contrast).
- Add a codecov **component** (or flag) in `codecov.yml` scoping coverage to
  the statistical-core files (the `ssm_*` / `cpm_*` estimation sources), so the
  core's coverage is trackable separately from the whole package.

**Out:**
- degree/radian/contrast → vctrs/S7 class migration → IP-touching; routes to
  `/milestone-brief` first, then its own ~v2.1.0 milestone (candidate row).
- Analytic-CI Hessian recomputation (minor perf) → candidate row
  (oracle-validate when done).
- 0-vs-360 pole-snap cosmetic alignment → candidate row (D-003 parked it).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] **AC1** — The four inline Group/Measure/Label blocks in
      `R/ssm_analysis.R` are replaced by calls to one helper, and
      `ssm_analyze()` `results`/`scores` are byte-identical to pre-refactor.
      Evidence: existing `ssm_analyze` snapshot/print tests stay green **under
      `devtools::check()`** (not just `test()`/`load_all()` — see M11 lesson on
      scope-leak masking), plus a new targeted test asserting the helper's
      `Label`/`Group`/`Measure` across the branch matrix (mean+contrast+group;
      corr+group; corr+no-group+contrast; corr+no-group+no-contrast).
- [ ] **AC2** — `codecov.yml` defines a statistical-core component/flag whose
      path globs all resolve to existing `R/` files (no dead globs), and the
      config is valid. Evidence: every listed path matches ≥1 real file, and
      the `test-coverage.yaml` workflow still succeeds (config parses).
- [ ] **AC3** — `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1** — Test-first: add a targeted test pinning the current
      `Label`/`Group`/`Measure` output across the branch matrix (mean+contrast
      with grouping; corr with grouping; corr no-grouping with contrast; corr
      no-grouping no-contrast), asserted against present behavior before any
      refactor.
- [x] **T2** — Extract one helper (e.g. `build_result_labels()`) covering both
      score paths, parametrized by `score_type`/`grouping`/`contrast`; replace
      the four inline blocks at `R/ssm_analysis.R:333`, `:370`, `:458`, `:504`.
      Verify T1 + existing snapshot/print tests green.
- [ ] **T3** — Add the statistical-core component/flag to `codecov.yml`; confirm
      each path glob resolves to a real `R/` file and the config validates.
- [ ] **T4** — Full `devtools::check()` (clean-env, not `load_all()`); confirm
      0/0/0.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: T1+T2 — extracted `build_result_labels()` in `R/ssm_analysis.R`,
  replaced all four inline Label/Group/Measure blocks; added a direct helper
  unit test covering all branches (incl. corr no-contrast+grouping and corr
  multi-measure no-contrast, which weren't asserted end-to-end). Full
  `devtools::test()` byte-identical green (1823 pass, 0 fail); verified result/
  score row names + column types unchanged. The 3 suite WARNs are pre-existing.
- 2026-07-12: created by /milestone-plan. Promoted from the "Continuous /
  infrastructure refactors" candidate row (items: Group/Measure/Label dedup +
  statistical-core coverage tracking). Decided to **land pre-freeze** (v2.0.0
  freeze ~2026-07-26): dedup is byte-identical and coverage config is
  tooling-only, so neither expands v2.0.0's statistical/user-facing scope
  (D-001 "scope is the variable"). No RB tripwire — behavior-preserving refactor
  fenced by byte-pinned snapshots (M8 lesson: verify by suite-green + identical
  output; the pre-refactor output IS the oracle, so the estimation-code
  oracle-validation rule is satisfied without a separate oracle). Sibling items
  routed: vctrs/S7 → /milestone-brief; Hessian perf + pole-snap → candidate
  rows; "add R-devel to CI" retired (already in R-CMD-check.yaml:25).

## Decisions
<!-- owner: implement / review · append-only -->

## Review
<!-- owner: review · exclusive -->
