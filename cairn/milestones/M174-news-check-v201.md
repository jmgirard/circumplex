<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M174: Check NEWS against v2.0.1 before release

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP3, GP5   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — NEWS.md ships to CRAN users   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Before the CRAN release walk, NEWS.md names every interface change since v2.0.1 and states each user-visible change that a read of the code commits finds.

## Scope
<!-- owner: plan · create/amend-via-gate -->

These terms apply below. The development section is the lines of NEWS.md before the `# circumplex 2.0.1` line. A bullet is a line of that section that starts with `* `, plus its next lines up to the next bullet or heading. The plan commit is the commit that adds this file. The merge head is the last commit of the milestone branch.

**In:** a script, `tools/news-interface-check.R`, that lists the interface differences between a git ref and the current code and matches each one in NEWS.md. A read of the 44 code commits since v2.0.1. Edits to the development section for each gap that the script or the read finds, inside the current 170 lines.

**Out:** the truth of sentences that this milestone does not change (M173 audited 85 claims in four review passes). The version bump, `cran-comments.md` and the release checks, which belong to `/cairn-release`. The NEWS sections of released versions.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: Each difference between v2.0.1 and the merge head in the five sets below is matched in the development section. The one exception is an export documented in `man/circumplex-ggproto.Rd` whose `geom_*()` constructor is matched. The sets are:
  1. the `export()` and `S3method()` lines of NAMESPACE,
  2. the argument names and default expressions of each function exported at both refs and each S3 method registered at both refs, read with `parse()` from top-level `name <- function(...)` assignments in `R/*.R`,
  3. the file names in `data/`, without `.rda`,
  4. the `VignetteIndexEntry` titles of `vignettes/*.Rmd`,
  5. the package names and version floors in the Depends, Imports and Suggests fields of DESCRIPTION.

  A backtick span names X if, after a leading `?` is dropped, its text is X, or begins with `X(`, or begins with X, optional spaces and `=`. These are the match rules:
  - Export, dataset, or added or removed package: one bullet holds a span that names it.
  - Argument difference: one bullet holds spans that name the function and the argument. For an S3 method, the generic stands for the function.
  - Added or removed S3 method: one bullet holds spans that name the generic and an export whose `man/*.Rd` file has the `\alias{}` of the method. The `man/` files come from the merge head for an added method and from v2.0.1 for a removed one. This rule checks that the two names appear together. It does not check that the bullet describes the method.
  - Version floor change: one bullet holds a span that names the package, and the same bullet holds the new floor as a whole token, with no digit or `.` on either side.
  - Vignette: the section holds the title in straight double quotes, after line breaks and runs of spaces become one space.

  The evidence is the output of `Rscript tools/news-interface-check.R v2.0.1`, run at the merge head with a clean working tree.
- [ ] AC2: Each sentence of the development section at the merge head that is not in it at the plan commit is true of the package at the merge head. A sentence about earlier behavior is true of v2.0.1 instead. Sentences are compared after line breaks and runs of spaces become one space.
- [ ] AC3: Each backtick span and each double-quoted title in the development section at the plan commit is also in it at the merge head (a script compares the two sets).
- [ ] AC4: `sed -n '/^# circumplex 2.0.1/,$p' NEWS.md` gives the same bytes at the plan commit and at the merge head.
- [ ] AC5: At the merge head, `awk '/^# circumplex 2.0.1/{exit} {n++} END{print n}' NEWS.md` prints 170 or less.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2, T4, T5
- AC2 → T3, T4, T5
- AC3 → T4, T5
- AC4 → T5
- AC5 → T4, T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits). -->

- [ ] T1: Write `tools/news-interface-check.R`. The `^tools$` line of `.Rbuildignore` already keeps it out of the build. The script takes a base git ref and reads that ref's files with `git show` and `git ls-tree`. It applies the AC1 sets and match rules to the current code and prints each difference as matched, exempt or missing. If a difference is missing, it exits 1. It finds names only by the AC1 span rule, so `ssm_trajectory` is not found inside `ssm_plot_trajectory`.
- [ ] T2: Show that the script reads a domain that is not empty and that it can fail. Against v2.0.1, it lists at least one difference in each set. Then run it on scratch copies of NEWS.md. Each plant below must be reported missing, with exit 1:
  - a name that is present only without backticks,
  - a name that is present only inside a longer name,
  - a floor change whose package name is present without the new floor,
  - an S3 method whose generic is `print`, with the generic removed from its bullet,
  - a vignette title in curly quotes,
  - a function span and its argument span in different bullets,
  - a package span and its floor in different bullets,
  - a name present only in the 2.0.1 section, or only in a heading,
  - a floor present only inside a longer number (`3.1.70`),
  - an added package present only without backticks,
  - a bare argument beside a backticked function.

  Each form below must still match: `f()`, `?f`, `arg = value`, a title split across two lines, and an S3 method whose generic is also the export.
- [ ] T3: List the commits with `git log --first-parent --oneline v2.0.1..<plan commit> -- R src data inst vignettes DESCRIPTION NAMESPACE`. Leave out the version bump `76f253fb` and the `plan` and `review` tracking commits (44 commits on 2026-10-28). Read the diff of each commit against the development section. An owed change is a change that users can see, that the section does not state, and that no page named in its bullet states. For each owed change, add or edit a bullet. Write each new sentence from a read of the code in the same session. Write one work-log line with the counts.
- [ ] T4: For each difference that the script reports missing, and each owed change from T3, edit NEWS.md. To stay at 170 lines, shorten other bullets, and restate each condition of a shortened bullet from the code. When you rewrap, keep each backtick span and each quoted title whole. Keep every span and title of the plan-commit section.
- [ ] T5: Run the checks at the merge head with a clean tree. Run the script, and check its set 1 and set 2 lists against a separate enumeration. Compare the spans and titles with the plan commit, compare the released sections, and count the lines. For each new sentence, record in the Review section a file and line that make it true.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-28: created by /milestone-plan.
- 2026-10-28: tier check: milestone — 44 code commits since v2.0.1 to read and a new script, more than one sitting.
- 2026-10-28: idea vet: one concern, that no procedure lists every change a user would notice and M173's hand ledger failed review three times; the user chose a scripted interface check plus a commit read, with no criterion that promises the read is complete.
- 2026-10-28: lessons used: 2026-10-28 (M173) NEWS compression lesson, which shaped T4's restate-from-code rule, the span-whole rewrap rule, and binding AC1 to a script; 2026-07-19 (M7 family), which shaped T2's non-empty-domain check.
- 2026-10-28: question set: check scope — script plus commit read (recommended option).
- 2026-10-28: question set: NEWS length — hold the development section at 170 lines (user chose this over the recommended 190).
- 2026-10-28: question set: the script is committed to `tools/` for reuse at later releases.
- 2026-10-28: criteria audit (full mode, fresh Opus reader): 12 findings, all fixed. AC1 now binds NEWS, not the script's exit, with one closed exemption (a ggproto object beside its matched constructor). AC1 gained span forms (`f()`, `?f`, `arg =`), whole-token matching, the set-2 domain (functions at both refs, generic for a method), a floor that must appear with the package, whitespace-collapsed straight-quote titles, and a co-mention scope for S3 methods. AC2 now covers sentences new since the plan commit, allows v2.0.1 truth for before-statements, and needs a cited file and line. AC3 and AC4 name their commands. T2 gained form and positive probes, and T5 runs on a clean tree.
- 2026-10-28: collision check: absorbs candidate "Check NEWS against v2.0.1 before release" in full (split from M173); M173 (done) shortened NEWS and dropped its ledger criterion; no D-entry covers NEWS checks. Release tripwire not fired, since no version ships here.
- 2026-10-28: inbox sweep: 0 open issues, 0 open PRs.
- 2026-10-28: plan gate chose a scripted interface check plus an unbound commit read over a per-commit ledger criterion, because the ledger failed review three times in M173; falsified by a review finding an owed change that the commit read missed.
- 2026-10-28: plan gate held 170 lines over 190 at the user's choice; falsified by a needed bullet that cannot fit without dropping a condition.
- 2026-10-28: plan checkpoint (half-done): committed while a fresh Opus re-audit of the repaired AC1-AC3 and of AC4 at 170 runs; its findings land in a follow-up plan commit before implement starts.
- 2026-10-28: re-audit (full mode, fresh Opus reader): 9 findings with repairs, 5 clean. Fixed: set 2 limited to exports and registered methods (it reached 9 internal helpers), span rule reworded to "is X, or begins with `X(` or X and `=`" with T1 using the same rule, exemption tied to `man/circumplex-ggproto.Rd`, floor as a whole token, AC2's citation clause moved to T5, new AC3 keeping every plan-commit span and title (guards M173's lost-condition failure under the 170-line hold; old AC3-AC4 now AC4-AC5), T2 gained 7 location and form plants, T5 checks the script's lists against a separate enumeration. It found 6 AC1 gaps today: 3 S3 methods (`print`/`rbind` on the trajectory, `print` on the growth formula), bare `nlme`, and the testthat floor.
- 2026-10-28: release-window advisory overridden: `cairn_validate` flags M174 by its title, but it ships no version and the user asked for it by name; the release itself stays with `/cairn-release`.
