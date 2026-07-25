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
