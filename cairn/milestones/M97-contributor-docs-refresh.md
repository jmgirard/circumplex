# M97: Make the contributor docs describe this repo

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m97-contributor-docs-refresh`

## Goal

Rewrite the four `.github/` contributor-facing documents so they describe this
repo's actual practice rather than the usethis tidyverse template they were
generated from.

## Scope

Surface tier: **user-facing** — external contributors read these files as
GitHub renders them, and act on what they say.

**In:**
- `.github/CONTRIBUTING.md`, `SUPPORT.md`, `ISSUE_TEMPLATE.md`,
  `CODE_OF_CONDUCT.md`.
- The two dead `.Rbuildignore` entries (L8 `^\.travis\.yml$`, L13
  `^CONDUCT\.md$`) — neither file exists; the last Travis residue outside
  `NEWS.md` history.

**Out:**
- A committed checker fencing these docs against retired literals → declined at
  the plan gate; verification is hand-run and logged (see work log).
- `README.md`, vignettes, package documentation → untouched; a claim there
  falsified by this change lands as a candidate row.
- Enforcement policy beyond adopting Contributor Covenant 2.1 with the
  maintainer contact → not this milestone.

## Acceptance criteria

- [ ] **AC1** — `.github/CONTRIBUTING.md`'s pull-request section names
      `R-CMD-check.yaml` (the workflow that gates PRs) and states CLAUDE.md's
      "match existing code style" rule in place of an external style guide.
      Verified by `grep -F` over the PR-process section for `R-CMD-check.yaml`
      and for `match existing code style` (each ≥1 match), and by confirming
      `R-CMD-check.yaml` is present in `ls .github/workflows/`.
- [ ] **AC2** — `.github/CONTRIBUTING.md` names all four generated-file paths
      CLAUDE.md forbids hand-editing (`R/RcppExports.R`, `src/RcppExports.cpp`,
      `man/*.Rd`, `NAMESPACE`) and both regeneration commands
      (`devtools::document()`, `Rcpp::compileAttributes()`). Verified by
      `grep -F` for each of the six strings over that file, each ≥1 match.
- [ ] **AC3** — `.github/SUPPORT.md` carries no `github.com/<owner>/circumplex`
      URL whose owner is not `jmgirard`, and its issue-search link's
      origin-plus-path equals `DESCRIPTION`'s `BugReports:` value (a query
      string is permitted). Verified by
      `grep -Eo 'https://github\.com/[^/]+/circumplex[^ )>]*'` over the file and
      comparing each match against that field.
- [ ] **AC4** — A case-insensitive `grep -Fin -e` sweep for seven retired
      literals (`Travis`, `AppVeyor`, `styler`, `style.tidyverse.org`,
      `tidy-contrib`, `tidyverse/circumplex`, `community.rstudio.com`) over the
      file list `git ls-files` produces — excluding `NEWS.md` (release history)
      and `cairn/**` (tracking records quoting these literals; the T8 log lives
      there) — returns **no match**. Recorded per literal both before and after
      the change: each literal must show ≥1 pre-change match, and a literal
      showing none invalidates the sweep rather than passing it.
- [ ] **AC5** — No URL in the four `.github/` files has a host in
      {`tidyverse.org` and its subdomains, `rstudio.com` and its subdomains,
      `posit.co`, `rstd.io`}, the sole exception being `reprex.tidyverse.org`
      (the reprex package's own documentation), and no bare hostname from that
      same list appears as link text. Verified by enumerating with
      `grep -Eo '(https?://)?[a-z0-9.-]+\.(org|com|io)[^ )>]*'` over exactly
      those four files and extracting each host.
- [ ] **AC6** — Every http(s) URL enumerated by AC5 (trailing `>`, `)`, `.`, `,`
      stripped) resolved 2xx/3xx on its recorded probe:
      `curl -sIL -o /dev/null -w '%{http_code}'`, and on a 403/405/429 a
      documented GET retry (`curl -sL -o /dev/null -w '%{http_code}' -A <browser
      UA>`) whose own code must be 2xx/3xx. Any URL not reaching 2xx/3xx on
      either probe is repaired or removed. Statuses recorded in the work log.
- [x] **AC7** — `.github/CODE_OF_CONDUCT.md` is Contributor Covenant 2.1 as
      bundled by usethis 3.2.1 (template SHA-256
      `e45d34f51a88827f03e9bc868aabb872ccae96434a8c14c469a44147825a499a`), with
      template line 62's `{{{ contact }}}` placeholder replaced by the `cre`
      email in `DESCRIPTION`'s `Authors@R` (`me@jmgirard.com`). Verified by
      `shasum -a 256 .github/CODE_OF_CONDUCT.md` equalling
      `4828e2a242cad5e7d7f0f0c47eefe57ff0e66273ba6db3f90f227bb6ab1b87f4`, and,
      where usethis 3.2.1 is installed, by `diff` against the template showing
      exactly `62c62` with no additions or deletions. The substitution is
      length-preserving (15 characters either way) so the template's hard wrap
      is unchanged, and template line 62's trailing space is preserved
      byte-for-byte.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T1, T2, T4, T5, T7, T8
- AC5 → T1, T2, T4, T5, T6, T8
- AC6 → T6, T8
- AC7 → T6

## Tasks

- [x] **T1** — CONTRIBUTING.md: rewrite the framing (L3-5, L47-48) and the
      "someone from the team agrees" prerequisite (L17-18) for a
      solo-maintainer, non-tidyverse package.
- [x] **T2** — CONTRIBUTING.md: rewrite the PR-process section — replace the
      Travis/AppVeyor bullet (L25-27) with the actual workflows, and the
      styler/style-guide bullet (L28-31) with CLAUDE.md's rule.
- [x] **T3** — CONTRIBUTING.md: add the generated-files section (four paths, two
      regeneration commands), extending the `man/` warning at L12-13.
- [x] **T4** — SUPPORT.md: fix the issue-search link (L23) to DESCRIPTION's
      `BugReports:` value; replace the tidyverse help venues (L11, L14, L16,
      L32) and framing; keep reprex.
- [x] **T5** — ISSUE_TEMPLATE.md: replace the stale question venues (L1) and
      help links (L3).
- [x] **T6** — CODE_OF_CONDUCT.md: replace with Contributor Covenant 2.1;
      enforcement contact = DESCRIPTION's `cre` email.
- [x] **T7** — Delete the two dead `.Rbuildignore` entries (L8, L13); confirm
      `devtools::check()` is unaffected.
- [ ] **T8** — Run and log the AC4 pre/post sweep, the AC5 host enumeration, and
      the AC6 status probes; check tail bytes of every rewritten file
      (`tail -6 f | od -c`, M34 lesson); confirm `pkgdown::check_pkgdown()`
      passes.

## Work log

- 2026-08-19: created by /milestone-plan. Absorbs the M95-review candidate row (`[O]` diff-bug F13, rescoped) and widens it: the row named `.github/CONTRIBUTING.md` L25 and L28-30 only; investigation found the tidyverse framing (L3-5, L47-48), the "team" prerequisite (L17-18), a wrong-repo issue link at `SUPPORT.md:23` (`github.com/tidyverse/circumplex/issues` vs `DESCRIPTION`'s `jmgirard/circumplex`), stale help venues in `ISSUE_TEMPLATE.md:1`, a Contributor Covenant at 1.0.0 with no enforcement contact, and two dead `.Rbuildignore` entries.
- 2026-08-19: criteria audit ran in **full** mode (surface tier user-facing), twice, both times a fresh-context `[O]` reader that authored nothing. Pass 1 returned 10 findings, pass 2 returned 12; every one was a wording defect with a single right answer, all fixed before this file was written, none escalated to the question gate. Load-bearing catches: `.Rbuildignore:8` made the original whole-tree `Travis` sweep unsatisfiable; the original AC2 and AC5 bound instrument properties (the criterion's own text; the work log) rather than the docs; the original AC1 verified only absences, so deleting the PR section outright would have passed it; `https://stackoverflow.com/` returns 403 to a headless HEAD, so the first URL criterion would have forced removal of a live link; "no link directs a contributor to a tidyverse venue *as the authority*" was unfalsifiable and let `www.tidyverse.org/contribute` survive, and was replaced by a host list.
- 2026-08-19: plan gate chose hand-run, logged sweeps over a committed testthat fence because a checker over these docs can only fence recalled literals, not stale advice (the D-090 shape; the M96 lesson that a guard proving a negative over an open-ended grammar "loses to the next construct"); falsified by a post-merge reintroduction of any retired literal that a reviewer misses.
- 2026-08-19: plan gate chose a full rewrite over a minimal edit of the false claims alone, because the audit showed a minimal edit leaves the stale claim standing in paraphrase (a "check the CI services listed in the README" rewrite passes every literal grep); falsified by the rewrite introducing a new inaccuracy the greps cannot see.
- 2026-08-19: plan chose deleting the dead `.Rbuildignore` entries over exempting `.Rbuildignore` from AC4's sweep, because the entries name files that do not exist and cannot return; falsified by either pattern turning out to still match something `R CMD build` needs excluded.
- 2026-08-19: implement session started; branch `m97-contributor-docs-refresh` cut from pushed master (908b65e0), tree clean.
- 2026-08-19: T1 — CONTRIBUTING.md framing, prerequisites, and closing section rewritten: the tidy-contrib pointers (L3-5, L47-48) removed, the "someone from the team" prerequisite replaced by an issue-for-bugs / discussion-for-features split (Discussions is enabled and in live use, 2 threads), and the reprex link repointed from `www.tidyverse.org/help/#reprex` to `reprex.tidyverse.org` (AC5's sole permitted exception).
- 2026-08-19: T2 — PR-process section rewritten; AC1 probes green (`R-CMD-check.yaml` 1 match, `match existing code style` 1 match, workflow file present). The CI sentence was first drafted as "every pull request ... across several platforms and R versions" and corrected against `tools/ci-matrix.R:23-38`: only a PR touching the escalation set gets the three release platforms, any other PR gets one Ubuntu job, and the five-config matrix with R-devel/oldrel is the push-to-master matrix.
- 2026-08-19: T3 — "Generated files" section added after "Fixing typos"; AC2 probes green (all six strings >=1 match: `R/RcppExports.R` 1, `src/RcppExports.cpp` 1, `man/*.Rd` 1, `NAMESPACE` 2, `devtools::document()` 1, `Rcpp::compileAttributes()` 1). README.md/README.Rmd added as a third generated pair from PROFILE.md's consistency-gate slot.
- 2026-08-19: T4 — SUPPORT.md rewritten around this repo's own venues (Discussions for questions, Issues for bugs). AC3 probe green: the only `github.com/<owner>/circumplex` URLs are `jmgirard/...discussions` and `jmgirard/...issues`, and the issue-search link's origin-plus-path string-equals DESCRIPTION's `BugReports:` value with no query. The wrong-repo link (`github.com/tidyverse/circumplex/issues`) and the four tidyverse help venues are gone; the stale `help.github.com` qualifier link was repointed to `docs.github.com`.
- 2026-08-19: T5 — ISSUE_TEMPLATE.md's question venues (StackOverflow, community.rstudio.com) replaced by this repo's Discussions; the tidyverse help pointer dropped and the reprex link repointed to `reprex.tidyverse.org`.
- 2026-08-19: **amendment (substantive), AC7** — the pinned source `https://www.contributor-covenant.org/version/2/1/code_of_conduct.md` returns HTTP 404 (a 3449-byte "Page not found" page), making AC7 unsatisfiable as planned. Amended at a mini gate to the usethis 3.2.1 bundled template, which Jeff confirms is where these files originally came from; the upstream raw markdown was rejected as the pin because it carries a Hugo TOML front-matter block and unwrapped lines, so a shipped copy would differ from it on nearly every line. Amended wording was audited first by a fresh-context [O] reader that did not author it, returning four findings, all applied: the normative reference was machine-local (deliverable SHA now the primary check, template SHA inline as provenance); "the placeholder carries the email" was backwards; the `grep -F` probe was location-free and was dropped; and "exactly one changed line" silently rode two coincidences now named in the criterion (both strings are 15 characters, and template line 62 is the file's only line ending in a trailing space).
- 2026-08-19: T6 — CODE_OF_CONDUCT.md replaced (Contributor Covenant 1.0.0, 1396 bytes, no enforcement contact → 2.1 with `me@jmgirard.com`). AC7 verified independently in-session: template SHA-256 `e45d34f5…a499a`, deliverable SHA-256 `4828e2a2…1b87f4`, `diff` output exactly `62c62`, line 62's trailing space present in `od -c`, and the text's own 2.1 attribution at line 118.
- 2026-08-19: T7 — the two dead `.Rbuildignore` entries deleted. Both verified dead first: no top-level file matches either pattern, and `.travis.yml`/`CONDUCT.md` were last touched in the pre-cairn Travis era (a95585ab, 34592d96, d685b51a). `R CMD build` succeeds afterwards and the tarball still excludes `.github`, `cairn`, `tools` and `devel`.

## Decisions

## Review
