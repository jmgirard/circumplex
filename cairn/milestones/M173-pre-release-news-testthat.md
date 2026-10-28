<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M173: Shorten NEWS and fix two pre-release details

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP3, GP5   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — NEWS.md, a vignette and DESCRIPTION ship to CRAN users   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m173-pre-release-news-testthat   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Ready the development version for its CRAN release walk: a testthat floor the tests actually need, a correct vignette sentence on the analytic caution, and a development NEWS section cut to about 150 lines that loses no change a v2.0.1 user would notice.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** DESCRIPTION's testthat floor with a D-entry; the one "very large $N$" sentence in `evaluating-circumplex-structure` (`.Rmd.orig` and shipped `.Rmd`); a rewrite of NEWS.md lines 1-506 (51 bullets) that merges related bullets, drops steps between states that never shipped, and points to help pages or vignettes for detail.

**Out:** the version bump, `cran-comments.md`, `devtools::check(manual = TRUE)` and the arm64 harness, which belong to `/cairn-release`, so this milestone claims no release readiness. The other parts of the "Vignette prose follow-ons" row stay on that row. NEWS sections for released versions (2.0.1 and earlier) are not touched.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: DESCRIPTION's Suggests entry reads `testthat (>= V)`, where V is 3.1.7, or, if the 3.1.7 run fails, the lowest version under which the run passes, the versions tried including every CRAN release from 3.1.7 up to V. The run is the package's test suite under `testthat::test_local()` with `NOT_CRAN=true` set and testthat V loaded from a temporary library placed ahead of the user library; it passes when it reports 0 failures and 0 errors and no more skipped tests than the same run under the installed testthat 3.3.2.
- [ ] AC2: In `vignettes/evaluating-circumplex-structure.Rmd.orig` and the shipped `vignettes/evaluating-circumplex-structure.Rmd`, the sentence that said the analytic caution prints "up to very large $N$" when a boundary marker is present instead states that, when a boundary marker is present, the analytic caution prints for every $N$ below $N = 50000$, matching `cpm_analytic_ci_n_boundary_caution` in `R/cpm_fit.R`. `grep -n 'very large' vignettes/*.Rmd vignettes/*.Rmd.orig` prints nothing, and `git diff` of each of the two files against the plan commit shows only that sentence changed.
- [ ] AC3: Each row of the change ledger in this file's Decisions section (one row per user-visible change stated by the 51 development-section bullets of NEWS.md at the plan commit, each row citing its bullet) has one disposition that holds after the edit: (a) stated, where a post-edit development-section bullet states the change; (b) pointed, where a post-edit bullet names the change's subject and a help page (`?topic`) or vignette that, at the post-edit commit, states the change's after-state; or (c) dropped, where the row's before-state is absent from v2.0.1, shown by `git show v2.0.1:<path>` on the path the row names. Every row from a `## Breaking changes` bullet is (a), and its post-edit bullet sits under `## Breaking changes`. Every row naming a function, argument, dataset or vignette that v2.0.1 shipped, whose change alters returned values, refusals, warnings or printed output, is (a) or (b).
- [ ] AC4: Every function and argument named in backticks in a `## Breaking changes` bullet at the plan commit is named in backticks in a post-edit bullet under that heading (a script compares the two token sets).
- [ ] AC5: The post-edit development section of NEWS.md, from the `# circumplex (development version)` line to the line before `# circumplex 2.0.1`, is at most 150 lines (`awk` line count), down from 506. Where AC3 and AC5 cannot both hold, AC3 holds and AC5's figure is amended at a gate.
- [ ] AC6: Every backticked token and every numeric literal in the post-edit development section also appears in the development section at the plan commit, except a backticked `?topic` whose topic is an `\alias{}` in `man/*.Rd`, or a backticked `vignette("<name>")` whose `<name>.Rmd` is in `vignettes/` (a script checks both).
- [ ] AC7: `Rscript -e 'devtools::test()'` reports 0 failures with the installed testthat, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors and 0 warnings.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1
- AC2 → T2
- AC3 → T3, T4
- AC4 → T4, T5
- AC5 → T4, T5
- AC6 → T4, T5
- AC7 → T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits). -->

- [x] T1: Install testthat 3.1.7 (and any older dependency it needs) from the CRAN archive into a scratchpad library; run `testthat::test_local()` with `NOT_CRAN=true` under it and under 3.3.2, comparing failures, errors and skips. If 3.1.7 fails, try each later CRAN release in order and name in the work log the feature that raised the floor. Set the DESCRIPTION floor and append a D-entry for the re-pin.
- [x] T2: Rewrite the sentence at `evaluating-circumplex-structure.Rmd.orig:167-168` and the shipped `.Rmd:228-229` identically, from `R/cpm_oop.R:383-394` and `R/cpm_fit.R:1566-1567` read in the same session.
- [x] T3: Write the change ledger into this file's Decisions section and commit it before any commit that changes NEWS.md: one row per user-visible change, citing its bullet, naming the path whose v2.0.1 copy shows the before-state, and its planned disposition (a, b or c) with the post-edit bullet or the page it points to.
- [x] T4: Rewrite NEWS.md lines 1-506 from the ledger, merging related bullets (for example the print-wrapping entries and the vignette splits), keeping the four headings, with Breaking changes first.
- [x] T6: Review return 1: state in NEWS, or correct in the ledger to a page the bullet names, the detail of rows L28, L40, L56, L60, L67, L70 and L74; rerun the token checks and line count.
- [x] T5: Run the checks: the AC4 and AC6 token scripts, the AC5 line count, a read of each (b) row's page, `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-28: created by /milestone-plan.
- 2026-10-28: tier check: milestone — a dependency re-pin needs a D-entry, and the NEWS rewrite is more than one sitting's quick edit.
- 2026-10-28: idea vet: no concern; the floor matches `local_mocked_bindings()` (testthat 3.1.7, 72 uses in tests), the vignette fix matches `R/cpm_oop.R:383-394`, and the user chose the aggressive NEWS cut knowing fine detail moves to help pages.
- 2026-10-28: lessons used: 2026-07-19 (M7 family; a step that doesn't run reports success), which shaped AC1's `NOT_CRAN=true` and skip-count terms.
- 2026-10-28: question set: NEWS depth — aggressive, 150 lines or fewer, detail pointed to help pages and vignettes.
- 2026-10-28: question set: testthat — may download 3.1.7 and older dependencies from the CRAN archive into the scratchpad; raise the floor to the lowest passing version if 3.1.7 fails.
- 2026-10-28: question set: Breaking changes wording may be shortened; every refusal and affected function stays listed under that heading.
- 2026-10-28: criteria audit (full mode, fresh Opus reader, two passes): fixed AC1 self-contradiction, moved the work-log naming to T1, added NOT_CRAN and skip count, reworded AC2 to "every N below 50000" (cpm_oop.R:384, 394), made AC3 quantify over a ledger, exempted Breaking rows from (c) per D-065, used "after-state" in (b), added AC4's Breaking token check, exempted resolvable pointers in AC6, and made AC3 win over AC5's figure.
- 2026-10-28: collision check: absorbs candidate "Raise the testthat floor to 3.1.7" in full and item (iv) of "Vignette prose follow-ons" in part (items i-iii stay on that row); no D-entry or archive covers a NEWS cut since M7's consolidation; inbox sweep found 0 open issues and 0 external PRs.
- 2026-10-28: sizing: three small parts kept in one milestone, one pre-release PR, 7 criteria and 5 tasks, under the tripwires.
- 2026-10-28: plan gate chose the aggressive NEWS cut over the about-half cut at the user's choice; falsified by a v2.0.1 user change that a pointed page does not state.
- 2026-10-28: plan gate chose a scripted token check (AC6) over a reviewer read alone for new claims; falsified by a new behavior claim in plain prose that the script cannot see, which review must still catch.
- 2026-10-28: implement started on branch m173-pre-release-news-testthat.
- 2026-10-28: T2 done: the sentence now reads "the analytic caution prints for every $N$ below $N = 50000$", in `.Rmd.orig:168` and `.Rmd:229`, read against `R/cpm_oop.R:383-394` (unconditional caution below 2000, marker caution below `cpm_analytic_ci_n_boundary_caution` = 50000).
- 2026-10-28: T3 done: ledger of 77 rows (L1-L77) committed at 359eea1d, before any NEWS commit; one row is (c) (L46, the accuracy heading new since v2.0.1, whose v2.0.1 heading is `R/ssm_ci_oop.R:472`), 15 are (b), the rest (a).
- 2026-10-28: T4 done: development section rewritten to 21 bullets (N1-N21), 150 lines from 506; drafted in the scratchpad and copied over NEWS.md lines 1-506 by shell, since an Edit of a 506-line span was impractical (diff reviewable at the PR); the simple-english linter reports 0 hits on the new section.
- 2026-10-28: T4 token checks (scratchpad `news-tokens.R`, plan-commit NEWS vs new): AC4 24 Breaking tokens, 0 missing; AC6 4 new backticked tokens, all `?topic` with an `\alias{}`, 0 new numeric literals over a 141-token, 38-number old domain; plants (a dropped `ipsatize()`, `0.02`, `?not_a_topic`, `new_token`) each fired.
- 2026-10-28: T5 ledger corrections appended to Decisions (L34 to a, L32 pointer named in N16); the T3 line's "15 are (b)" was wrong, the corrected count is 8 (b), 1 (c), 68 (a).
- 2026-10-28: T1: testthat 3.1.7 from the CRAN archive does not build on R 4.6.1 (`reassign.c` calls `SET_BODY` and its siblings, gone from R's API; vendored Catch needs `std::uncaught_exception`). Scratch build with `-Wno-implicit-function-declaration`, `-D_LIBCPP_ENABLE_CXX20_REMOVED_UNCAUGHT_EXCEPTION`, and `reassign_function` stubbed to `Rf_error()`; only `with_mock()`/`local_mock()` reach it, and `grep -rnE '\b(with_mock|local_mock)\(' tests/ R/` finds none.
- 2026-10-28: T1 done: `test_local()` with `NOT_CRAN=true` gave 81 files, 1353 tests, 0 failed, 0 errors, 1 skipped under both 3.1.7 and 3.3.2, the same skip (`test-axes-scaled-fit.R`, "AC9: the committed rates reproduce exactly in the same environment") and the same test set; V = 3.1.7, so no later release was tried. DESCRIPTION set to `testthat (>= 3.1.7)`; D-073 appended.
- 2026-10-28: claim audit: 85 claims read, 5 corrected — NEWS.md (single-number acceptance vs `ssm_analyze_long()`, `vcov` warning only with the default contrast, amplitude and displacement match only, `NA` hollow only beside a verdict, SEM tier sentence); the same reader re-read the five and one reflowed sentence, all hold; vignette sentence and DESCRIPTION held.
- 2026-10-28: T5 done: token checks pass on the final NEWS (150 lines; AC4 0 missing; AC6 4 allowed pointers, 0 new numbers); each (b) row's page read and holds after the L34 correction; `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes, run on 8980d4f1, before the DESCRIPTION floor and the claim-audit NEWS fixes, which change no code; review reruns it on the final head.
- 2026-10-28: user override: the user asked to pause when implement is done, so implement stops at `review` without invoking /milestone-review.
- 2026-10-28: review return 1: AC3 fails as written; rows marked (a) whose detail the final NEWS does not state: L28 (margin widening for `angle_labels`), L40 (`instruments()` entries lose their leading space), L56 (reference scale now shows its communality interval), L60 (`?cpm_fit` lists the markers), L67 (menu built from the articles index), L70 ("Axes Reliability" now in the menu), L74 (plain-English vignettes link the introduction). Fix: state each in NEWS, or append a ledger correction to (b) where a page named in the bullet states it (`?ggcircumplex` states the margin; `?cpm_fit` lists the markers), keeping AC4-AC6 and the 150-line figure.
- 2026-10-28: T6 added for review return 1 (minor amendment); done: N6, N11, N16, N17 and N18 reworded to state the seven rows' detail, ledger correction appended; token checks pass (150 lines, AC4 0 missing, AC6 5 allowed pointers incl. `?ggcircumplex`, 0 new numbers), linter 0 hits, released sections identical.
- 2026-10-28: review return 2: AC3 fails on L56 (N16 states the reference scale's interval wrongly), L27 (`?coord_circumplex` does not state the plotmath minus rule), L24 and L65 (pointers missing); plus fix-now NEWS findings diff-bug #3-#11 and blame-history #1, #2, #7 listed in the Review section. Follow-up row "Tie documented caution limits to code" filed.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

- 2026-10-28 (T3) **Change ledger.** Bullets B1-B51 are the development-section bullets of NEWS.md at `8c42b975` in file order (Breaking B1-B3, New features B4-B10, Minor B11-B33, Documentation B34-B51). Targets N1-N21 are the post-edit bullets (Breaking N1-N2, New features N3-N7, Minor N8-N16, Documentation N17-N21). Disposition a = stated in the target; b = the target names the subject and points to the page; c = before-state absent from v2.0.1 (path checked). Before-state paths are v2.0.1 paths unless "new" (absent there: exports per `git show v2.0.1:NAMESPACE`, vignettes per `git ls-tree v2.0.1 vignettes/`).
  - L1 B1 repeated column in `scales` refused (8 functions listed) — R/ (v2.0.1 accepted) — a N1
  - L2 B1 number below 1 in `scales` refused — a N1
  - L3 B1 name occurring twice in `data` refused — a N1
  - L4 B1 character `occasions` in `ssm_analyze()` naming a repeated name refused — a N1
  - L5 B1 `ssm_sem_parameters()`, `ssm_sem_syntax()`, `cpm_fit()` `cormat` refuse a repeated label — a N1
  - L6 B2 `items` of `score()`, `ipsatize()`, `axes_reliability()` and `scales` of `self_standardize()` refuse the same three — a N1
  - L7 B2 why: `score()` key read wrong columns silently; `ipsatize()` counted twice — a N1 (one clause)
  - L8 B2 `axes_reliability()` refuses one column under two scales, and a number reaching a duplicated name — a N1
  - L9 B3 `ssm_analyze_long()` refusal message names the duplicated name — a N2
  - L10 B3 `NA` label listed twice refused (`ssm_sem_syntax()`, `ssm_sem_parameters()`, `cpm_fit()` `cormat`) — a N2
  - L11 B3 `ssm_sem()`/`ssm_sem_syntax()` refuse a `measures` entry named like a scale — a N2
  - L12 B4 `cpm_fit(check_lower = TRUE)`, note in print/summary, `details$lower_check`, order note, estimates unchanged — a N3
  - L13 B4 scope: quasi-circumplex unit scaling, no RNG, not exhaustive — b N3 → `?cpm_fit`
  - L14 B5 `ssm_growth_data()` long table of e, x, y — a N4
  - L15 B5 `ssm_growth_data()` refusal list — b N4 → `?ssm_growth_data`
  - L16 B5 `ssm_growth_formula()` formulas for glmmTMB, nlme, brms; print shows fit call; fits nothing — a N4
  - L17 B5 nlme added to Suggests — a N4
  - L18 B6 `ssm_trajectory()` from coef+vcov or draws, rows per time, e/x/y/a/d intervals and certification — a N4
  - L19 B6 contrast, zero-covariance refusal, print cautions, `input` attribute, `rbind()` — b N4 → `?ssm_trajectory`
  - L20 B6 `ssm_plot_trajectory()` plots the object — a N4
  - L21 B6 its amplitude and displacement match the growth vignette's former per-wave loop (shipped in v2.0.1 vignettes/growth-ssm-analysis.Rmd) — a N4
  - L22 B7 `ssm_trajectory()` balance warning — a N4 (one clause); detail b → `?ssm_trajectory`
  - L23 B8 `geom_ssm_ellipse()` layer and `ssm_ellipse_data()` generic — a N5
  - L24 B8 seam and origin handling, vignette use — b N5 → `?geom_ssm_ellipse`
  - L25 B9 datasets `simulated_occasions`, `simulated_growth`, `simulated_growth_origin` — a N7
  - L26 B10 `grid = "cartesian"` on `coord_circumplex()`/`ggcircumplex()` — a N6
  - L27 B10 cartesian grid detail (labels, ticks, plotmath minus) — b N6 → `?coord_circumplex`
  - L28 B10 `ggcircumplex(angle_labels = )` and margin widening — a N6
  - L29 B11 `cpm_fit()` bootstrap refit error excluded and counted; all-error gives NA with warning; non-finite counted — a N8
  - L30 B12 `ssm_plot_trajectory()` dashed segments by uninterpretable points, legend line type — R/ssm_trajectory.R (solid in v2.0.1) — a N16
  - L31 B12 `certified = NA` row drawn hollow — a N16
  - L32 B12 two vignettes describe the dashed segments — b N16 → "Growth Models on SSM Parameters"
  - L33 B13 `axes_reliability()` no longer refuses "unidentified" on conditioning; such fits are certified or refused "uncertified" — a N10
  - L34 B13 three exact grounds for "unidentified"; unchanged numbers; uncorrected arm keeps tolerance — b N10 → `?axes_reliability`
  - L35 B14 `scales(items = TRUE)` fixed on `iip32`/`iip64`, prints licence notice — a N9
  - L36 B15 `items()` and `scales(items = TRUE)` wrap item text — a N9
  - L37 B16 `cpm_fit()` print/summary drops Communality column; `results$Communality` kept — a N12
  - L38 B17 Heywood note, bootstrap note, axes summary sentence, ladder `Verdict:` wrap — a N11 (folded into "every note wraps")
  - L39 B18 `cpm_fit()` `Fit:` line breaks after `;` — a N11
  - L40 B18 `instruments()` wraps, no leading space — a N11
  - L41 B18 `norm_standardize()` message wraps; code matching it must allow a line break — a N11
  - L42 B19 growth vignette prints variance components and explains the vcov check — a N19
  - L43 B20 advanced visualization vignette section "The latent circumplex from a CPM fit" — a N20
  - L44 B20 figure-by-figure detail — b N20 → "Advanced Circumplex Visualization"
  - L45 B21 every prose caution/note wraps to `options(width = )`; tables and headings do not — a N11
  - L46 B22 accuracy summary heading two characters shorter — c (that heading is new since v2.0.1: R/ssm_ci_oop.R:472 had another)
  - L47 B23 `cpm_fit()` table headers `Theory`, `lci`, `uci`; `results` names kept — a N12
  - L48 B24 `print()` heading of `ssm_ci_accuracy()` wraps — a N11
  - L49 B25 structure vignette shows `print()` before `summary()`, warns before chunks — a N21
  - L50 B26 vignettes use `options(width = 77)` — a N18
  - L51 B27 `summary()` of `ssm_ci_accuracy()` shorter, one table, columns kept in `coverage`/`guardrail` — a N13
  - L52 B27 punctuation of verdicts changes (see dashes) — a N15
  - L53 B28 multi-group `ssm_sem()` print: `Verdict:` block with labeled lines; stored text unchanged — a N14
  - L54 B29 `dcfi`/`cr` columns and note only for two-group ML plain CFI; `invariance$dcfi_scope` — a N14
  - L55 B30 no `--` dash in `ssm_ci_accuracy()`, `axes_reliability()`, `ssm_sem()` output and the collinearity warning — a N15
  - L56 B31 `plot()` of `cpm_fit()` draws zero-width intervals (line, arc, cap); reference scale shows communality interval — a N16
  - L57 B32 `plot()` of `cpm_fit()` warns on zero communality interval; warnings name reasons — a N16
  - L58 B33 `ssm_plot_circle()` and `cpm_fit()` plot place the axis in the widest empty gap — a N16
  - L59 B34 `?cpm_fit` section "Which solution is reported" — a N17
  - L60 B35 `?cpm_fit` states both caution conditions (below 2000; 2000 to 50000 with a marker) and lists markers — a N17
  - L61 B36 boundary vignette: which weights the bootstrap misses and why; two old claims removed — R/vignettes evaluating-circumplex-structure.Rmd.orig:231,269 — a N21 (short); detail b → "CPM Fits at a Boundary"
  - L62 B37 "Not yet peer reviewed" notice on four vignettes — a N18
  - L63 B38 growth vignette rewritten on the helpers — a N19
  - L64 B38 Section 10 nlme and brms calls; `growth_brms_draws.rds`; `?simulated_growth` See also — b N19 → "Growth Models on SSM Parameters"
  - L65 B39 growth vignette Section 7 states measured REML coverage, no small-sample correction; lme4 remedies removed — a N19 (short); figures b → `?ssm_trajectory`
  - L66 B40 `?axes_reliability`: uncertified error estimate depends on the machine — a N17
  - L67 B41 website menu built from articles index, titled "Articles" — a N18
  - L68 B42 "SEM-Based SSM Analysis" and "Axes Reliability" split; new "Latent Group Contrasts", "Axes Reliability Caveats" — a N18
  - L69 B43 "Evaluating Circumplex Structure" split; new "CPM Fits at a Boundary", "Confidence Interval Accuracy", "Structure Tests and Ipsatization" — a N18
  - L70 B44 Level line, Overview, Wrap-up; menu by level; "Axes Reliability" in menu — a N18
  - L71 B45 shown code keeps to package calls; data simulation hidden; `data()` loads — a N18
  - L72 B46 `?ssm_plot_trajectory` bound placement description corrected; plot unchanged — a N17
  - L73 B47 advanced vignette occasions figures: panel shape, labels, octant spoke labels — a N20
  - L74 B48 six vignettes in plain English, link the introduction — a N18
  - L75 B49 four corrected statements (SEM tiers, uncertified display, `fit_structure()` labels, `cpm_fit()` free angles) — a N21
  - L76 B50 SEM latent profile table drops X and Y — a N21
  - L77 B51 structure vignette names the shorter reports; axes vignette explains `--` — a N21
- 2026-10-28 (T5) **Ledger corrections.** L34 is a N10, stated as "`"unidentified"` now names only a design defect": `?axes_reliability` does not list the three grounds, which appear only in comments (`R/axes_corrected_se.R:168`), so a pointer there could not hold; L34's other two parts state behavior that did not change. L32's pointer is the growth vignette, named in N16 ("as the legend and the growth vignette show"), which describes the dashed segments. Count after correction: 77 rows, 8 (b), 1 (c), 68 (a); L22, L61 and L65 are (a) with extra detail left to a page.
- 2026-10-28 (T6, review return 1) **Ledger corrections.** N6 now states the margin ("widens the margin") and points to `?ggcircumplex` (L28 a). N11 states that `instruments()` entries lose their leading space (L40 a). N16 names the reference scale's communality interval (L56 a). L60 is (b): N17 names `?cpm_fit`, whose page lists the five markers (`man/cpm_fit.Rd:215`). N18 states the menu is built from the articles index and lists "Axes Reliability" (L67 a, L70 a), and that the plain-English vignettes link the introduction (L74 a). Count: 77 rows, 9 (b), 1 (c), 67 (a).

## Review
<!-- owner: review · exclusive -->

- Pass 1 (2026-10-28, head a1b75f2b): AC3 failed on seven rows missing detail; returned (review return 1), fixed in T6.
- Pass 2 (2026-10-28, head d285bc26). spawned: diff-bug, blame-history, prior-review, ripple.
- Gate (head d285bc26): `cairn_validate` all pass; `devtools::document()` no diff, 0 `resolve link` lines; `pkgdown::check_pkgdown()` no problems; README untouched; `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R`, `tools/check-branch-protection.R` clean; master push verdicts R-CMD-check and test-coverage success at d08ecfd2 (later master commits docs-only).
- diff-bug #1: N16 calls the reference scale's zero-width interval its communality interval; it is the angle interval, drawn as a radial line (R/cpm_oop.R:458-460) — fix now (AC3 L56)
- diff-bug #2: L27 (b) pointer fails; `?coord_circumplex` does not state the plotmath minus rule, and rim ticks are in `?ggcircumplex` — fix now (AC3)
- diff-bug #3: N8 says a non-finite refit used to stop the call; at v2.0.1 an infinite one was accepted — fix now
- diff-bug #4: N12 drops "for scale names of up to 16 characters" — fix now
- diff-bug #5: N18 "Output fits 80 columns" overclaims; the `cat()` lavaan syntax stays wider — fix now
- diff-bug #6: N10 says the tolerance differed across platforms; whether it fired differed — fix now
- diff-bug #7: "no words change" now reads over the accuracy verdicts, whose joining words changed (L52) — fix now
- diff-bug #8: N21 "explains its printed `--`" reads against N15; `--` marks a missing Nunnally-Bernstein comparison — fix now
- diff-bug #9: N18 generalizes shown-code and `data()` to every vignette; it was six and two — fix now
- diff-bug #10: N21 "frees the factor covariances" omits variances; vignette says the full factor covariance matrix — fix now
- diff-bug #11: N14 drops "a rung with a `dcfi` value" and the default MLR exclusion — fix now
- diff-bug #12: L24 (b) pointer omits the vignette use — fix now (point to the vignette)
- diff-bug #13: D-073 Consequences omits the compiler flags — reject (planned change): the D-entry rule keeps build detail in the milestone file it cites, whose T1 work-log line has the flags
- diff-bug #14: no test ties "50000" in the vignette and Rd to `cpm_analytic_ci_n_boundary_caution` — follow-up, candidate row "Tie documented caution thresholds to code"
- prior-review #1: N19 lost M163's coverage figures and the `?ssm_trajectory` pointer that L65 relies on — fix now (pointer)
- prior-review #2: cartesian bullet lost its argument name — reject (false): N6 names `grid`
- ripple #1: ROADMAP candidate (iv) still lists the vignette defect — reject (planned change): post-merge hygiene narrows the row
- blame-history #1: `check_lower` no longer says it does not find every lower discrepancy (D-072) — fix now
- blame-history #2: `ssm_trajectory()` small-sample caution gone from NEWS — fix now (pointer names cautions)
- blame-history #3: D-065 and compressed Breaking text — reject (false): every refusal of a v2.0.1 function is under Breaking changes; `ssm_growth_data()` is new, so its refusals change no prior behavior
- blame-history #4: peer-review notice lost its meaning — reject (false): N18 says the four pages teach the package's own unpublished methods
- blame-history #5: machine-dependence note gone — reject (false): N17 states it; `details$naive_reason` is unchanged behavior (ledger L34)
- blame-history #6: glmmTMB 1e-6 agreement figure gone — reject (planned change): a validation figure, not a user-visible change
- blame-history #7: "The estimates do not change" narrower than estimates, fit indices and intervals — fix now
