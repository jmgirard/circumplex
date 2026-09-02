#!/usr/bin/env Rscript

# Guard: every CI workflow's `extra-packages` allowlist must stay a mirror of
# DESCRIPTION Suggests.
#
# Why this exists. The check job sets `_R_CHECK_FORCE_SUGGESTS_: false`, so a
# package present in Suggests but missing from a workflow allowlist is simply
# never installed — its tests and vignette chunks skip, and `R CMD check` still
# reports success. That is a silent wrong-answer channel, not an error (cairn
# M52 recorded it in LESSONS after introducing the allowlists). This script
# turns it into a loud failure.
#
# Run by `.github/workflows/R-CMD-check.yaml` after setup-r, before the
# dependencies are installed. Not part of the built package (`^tools$` is in
# .Rbuildignore). Base R only — it must run before any dependency exists.
#
# When you add or remove a DESCRIPTION Suggest, update all three allowlists.
# When a workflow legitimately needs a package that is NOT a Suggest, add it to
# that file's `extra` below. When a workflow legitimately omits a Suggest, add
# it to `exclude` below with a comment saying why.
#
# The allowlist is only authoritative while the step also says
# `dependencies: '"hard"'` and does NOT say `needs:` — either of those two keys
# reverts the job to whole-Suggests resolution, which drags brms and the Stan
# toolchain back in while leaving the allowlist text untouched and this script
# green. So both keys are asserted too, scoped to the same `with:` block (a
# job-level `jobs.<id>.needs` is a different key and must not be confused with
# it). Found in the M58 review; without it the guard could report "in sync"
# over a job that had silently reverted M52 and M58 both.

# ---- policy ---------------------------------------------------------------

# `exclude`: Suggests deliberately not installed on that job.
# `extra`:   packages installed there that are not Suggests.
policy <- list(
  ".github/workflows/R-CMD-check.yaml" = list(
    # brms is never loaded by package code, tests, or the vignette build (its
    # chunk is eval = FALSE with a committed .rds fixture, cairn D-015) and
    # drags in the whole Stan toolchain. cairn M52.
    exclude = c("brms"),
    extra = c("rcmdcheck")
  ),
  ".github/workflows/test-coverage.yaml" = list(
    exclude = c("brms"),
    extra = c("xml2")
  ),
  ".github/workflows/pkgdown.yaml" = list(
    exclude = c("brms"),
    extra = c("pkgdown")
  ),
  # Re-renders vignettes/<name>.Rmd from its .Rmd.orig source and fails on a
  # stale committed copy. A missing Suggest here would let a fitting chunk
  # skip, so the re-render would match a copy carrying less output than the
  # source asks for -- the same silent wrong-answer channel this guard exists
  # for. brms is excluded for the same reason as everywhere else, and the
  # Bayesian vignette is not pre-computed.
  ".github/workflows/vignette-precompute.yaml" = list(
    exclude = c("brms"),
    extra = character(0L)
  )
)

# OpenMx and glmmTMB are deliberately NOT excluded anywhere: dropping them was
# considered and declined on measured grounds (cairn D-029). OpenMx's BC7
# oracle in tests/testthat/test-axes-reliability.R carries no skip_on_cran(),
# so it really does run under R CMD check; glmmTMB evaluates the growth
# vignette's fitting chunks. Re-excluding either needs a superseding D-entry.

# ---- helpers --------------------------------------------------------------

# DESCRIPTION Suggests, version constraints stripped.
read_suggests <- function(path = "DESCRIPTION") {
  field <- read.dcf(path, fields = "Suggests")[[1L]]
  if (is.na(field)) {
    return(character(0L))
  }
  parts <- trimws(strsplit(field, ",", fixed = TRUE)[[1L]])
  parts <- sub("\\s*\\(.*\\)$", "", parts)
  sort(parts[nzchar(parts)])
}

# Package names from a workflow's `extra-packages` value. Handles both the
# block form (`extra-packages: |` + indented lines) and the inline flow form
# (`extra-packages: any::pkgdown, local::.`), because either is valid YAML and
# the guard must not go blind if a file is rewritten in the other style.
read_allowlist <- function(path) {
  lines <- readLines(path, warn = FALSE)
  key <- grep("^\\s*extra-packages\\s*:", lines)
  if (length(key) != 1L) {
    stop(sprintf(
      "%s: expected exactly one `extra-packages:` key, found %d.",
      path, length(key)
    ), call. = FALSE)
  }

  key_indent <- nchar(sub("^(\\s*).*", "\\1", lines[[key]]))
  rest <- trimws(sub("^\\s*extra-packages\\s*:", "", lines[[key]]))

  if (nzchar(rest) && !grepl("^[|>]", rest)) {
    tokens <- strsplit(rest, ",", fixed = TRUE)[[1L]]
  } else {
    tokens <- character(0L)
    idx <- key + 1L
    while (idx <= length(lines)) {
      line <- lines[[idx]]
      if (nzchar(trimws(line))) {
        indent <- nchar(sub("^(\\s*).*", "\\1", line))
        if (indent <= key_indent) break
        tokens <- c(tokens, line)
      }
      idx <- idx + 1L
    }
  }

  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens) & !startsWith(tokens, "#")]
  # `any::pkg` / `github::user/repo` -> pkg; a bare `pkg` is left alone.
  pkgs <- sub("^.*::", "", tokens)
  pkgs <- sub("^.*/", "", pkgs)
  # `local::.` names the package under test, not a dependency.
  sort(unique(pkgs[pkgs != "."]))
}

# The sibling keys of `extra-packages:` within its own `with:` mapping. Walks
# out from that key in both directions, keeping only lines at exactly its
# indentation and stopping at the first line indented less (which ends the
# mapping). Block-scalar content is indented deeper and is skipped, so the
# allowlist entries themselves are never mistaken for keys.
read_with_block_keys <- function(path) {
  lines <- readLines(path, warn = FALSE)
  key <- grep("^\\s*extra-packages\\s*:", lines)
  if (length(key) != 1L) {
    return(character(0L))
  }
  indent_of <- function(l) nchar(sub("^(\\s*).*", "\\1", l))
  key_indent <- indent_of(lines[[key]])

  collect <- function(idxs) {
    out <- character(0L)
    for (i in idxs) {
      line <- lines[[i]]
      if (!nzchar(trimws(line))) next
      ind <- indent_of(line)
      if (ind < key_indent) break
      if (ind > key_indent) next
      if (startsWith(trimws(line), "#")) next
      out <- c(out, line)
    }
    out
  }
  block <- c(collect(rev(seq_len(key - 1L))), lines[[key]],
             collect(seq.int(key + 1L, length(lines))))
  trimws(block)
}

# ---- check ----------------------------------------------------------------

suggests <- read_suggests()
if (length(suggests) == 0L) {
  stop("DESCRIPTION declares no Suggests — refusing to pass vacuously.",
       call. = FALSE)
}

problems <- character(0L)

for (path in names(policy)) {
  if (!file.exists(path)) {
    problems <- c(problems, sprintf("%s: file not found.", path))
    next
  }
  rule <- policy[[path]]

  # The two keys that decide whether the allowlist governs at all.
  with_keys <- read_with_block_keys(path)
  dep_line <- grep("^dependencies\\s*:", with_keys, value = TRUE)
  if (length(dep_line) != 1L || !grepl("\"hard\"", dep_line, fixed = TRUE)) {
    problems <- c(problems, sprintf(
      "%s: the setup-r-dependencies step must carry `dependencies: '\"hard\"'` for its allowlist to govern; found %s. Anything else resolves ALL Suggests and reinstates brms.",
      path,
      if (length(dep_line) == 0L) "no `dependencies:` key" else paste0("`", dep_line, "`")
    ))
  }
  if (any(grepl("^needs\\s*:", with_keys))) {
    problems <- c(problems, sprintf(
      "%s: the setup-r-dependencies step must NOT carry a `needs:` key — it resolves dependency roles wholesale (e.g. `needs: website` pulls every Suggest, brms included) and silently overrides the allowlist.",
      path
    ))
  }

  unknown_exclude <- setdiff(rule$exclude, suggests)
  if (length(unknown_exclude)) {
    problems <- c(problems, sprintf(
      "%s: excludes %s, which is not in DESCRIPTION Suggests — drop the stale exclusion.",
      path, paste(unknown_exclude, collapse = ", ")
    ))
  }

  expected <- sort(union(setdiff(suggests, rule$exclude), rule$extra))
  actual <- read_allowlist(path)

  missing <- setdiff(expected, actual)
  if (length(missing)) {
    problems <- c(problems, sprintf(
      "%s: allowlist is MISSING %s — DESCRIPTION Suggests it, so it must be installed here (or excluded on purpose in tools/check-ci-deps.R).",
      path, paste(missing, collapse = ", ")
    ))
  }

  unexpected <- setdiff(actual, expected)
  if (length(unexpected)) {
    problems <- c(problems, sprintf(
      "%s: allowlist has UNEXPECTED %s — not a DESCRIPTION Suggest and not declared in this file's `extra` in tools/check-ci-deps.R.",
      path, paste(unexpected, collapse = ", ")
    ))
  }
}

if (length(problems)) {
  stop(
    "CI dependency allowlists are out of sync with DESCRIPTION Suggests:\n  - ",
    paste(problems, collapse = "\n  - "),
    "\n\nEvery allowlist must mirror DESCRIPTION Suggests, minus that file's\n",
    "documented exclusions, plus its documented extras. See tools/check-ci-deps.R.",
    call. = FALSE
  )
}

cat(sprintf(
  "CI dependency allowlists in sync with DESCRIPTION Suggests (%d): %s\n",
  length(suggests), paste(suggests, collapse = ", ")
))
