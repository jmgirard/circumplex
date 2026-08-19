#!/usr/bin/env Rscript

# Static audit of `.github/workflows/master-red-alert.yaml` (cairn M96).
#
# The alert workflow has no test suite of its own — it runs only when a push
# run of a watched workflow fails on the default branch, which is precisely
# the moment nobody wants to be debugging it. This script reads the file and
# asserts the properties M96's acceptance criteria name, so they can be
# re-checked on demand instead of by inspection.
#
# The behavioural half — open-once, comment-thereafter, create-the-label-first
# — is exercised separately by tools/master-red-alert-dryrun.R, which runs the
# workflow's own shell body against stubbed `gh` calls.
#
# Not part of the built package (`^tools$` is in .Rbuildignore).

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("this audit parses YAML and needs the `yaml` package installed.",
       call. = FALSE)
}

PATH <- ".github/workflows/master-red-alert.yaml"
WATCHED <- c("R-CMD-check.yaml", "test-coverage.yaml")

if (!file.exists(PATH)) {
  stop(sprintf("%s: file not found.", PATH), call. = FALSE)
}

doc <- yaml::read_yaml(PATH)
raw <- readLines(PATH, warn = FALSE)

# YAML 1.1 reads a bare `on` key as the boolean true, which the yaml package
# renders as the name "TRUE". Accept either spelling rather than quoting the
# key in the workflow (GitHub's own examples leave it bare).
trigger_name <- intersect(c("on", "TRUE", "true"), names(doc))
problems <- character(0L)

# ---- AC1: trigger and gate condition --------------------------------------

if (length(trigger_name) != 1L) {
  problems <- c(problems, sprintf("%s: no `on:` block found.", PATH))
} else {
  trigger <- doc[[trigger_name]]
  if (!identical(names(trigger), "workflow_run")) {
    problems <- c(problems, sprintf(
      "%s: `on:` must name workflow_run and nothing else; found %s.",
      PATH, paste(names(trigger), collapse = ", ")
    ))
  } else {
    watched <- as.character(trigger$workflow_run$workflows)
    if (!setequal(watched, WATCHED) || length(watched) != length(WATCHED)) {
      problems <- c(problems, sprintf(
        "%s: `on.workflow_run.workflows` must be exactly %s; found %s.",
        PATH, paste(WATCHED, collapse = ", "),
        if (length(watched)) paste(watched, collapse = ", ") else "nothing"
      ))
    }
  }
}

jobs <- doc$jobs
if (length(jobs) != 1L) {
  problems <- c(problems, sprintf(
    "%s: expected exactly one job; found %d.", PATH, length(jobs)
  ))
}
job <- jobs[[1L]]

# The gate is one `if:` expression. Each condition is asserted on the parsed
# value with whitespace collapsed, so re-wrapping the expression cannot break
# the audit and dropping a condition cannot pass it.
cond <- gsub("[[:space:]]+", " ", paste(as.character(job[["if"]]), collapse = " "))
required <- c(
  "failure conclusion" =
    "github.event.workflow_run.conclusion == 'failure'",
  "push event" =
    "github.event.workflow_run.event == 'push'",
  "default branch" =
    "github.event.workflow_run.head_branch == github.event.repository.default_branch"
)
missing_cond <- required[!vapply(
  required, function(x) grepl(x, cond, fixed = TRUE), logical(1L)
)]
if (length(missing_cond)) {
  problems <- c(problems, sprintf(
    "%s: the job's `if:` does not require %s (found `%s`).",
    PATH, paste(names(missing_cond), collapse = ", "), cond
  ))
}

# ---- AC2: permissions ------------------------------------------------------

perms <- doc$permissions
if (is.null(perms)) perms <- job$permissions
if (!is.list(perms) || !length(perms)) {
  problems <- c(problems, sprintf(
    "%s: no `permissions:` mapping at the workflow or job level. Without one the job inherits the repository default, which may be write-all.",
    PATH
  ))
} else {
  values <- vapply(perms, as.character, character(1L))
  writes <- names(values)[values == "write"]
  if (!identical(writes, "issues")) {
    problems <- c(problems, sprintf(
      "%s: `permissions:` must grant write to `issues` and nothing else; write scopes found: %s.",
      PATH, if (length(writes)) paste(writes, collapse = ", ") else "none"
    ))
  }
}

# ---- report ----------------------------------------------------------------

if (length(problems)) {
  stop(
    "master-red-alert.yaml audit failed:\n  - ",
    paste(problems, collapse = "\n  - "),
    call. = FALSE
  )
}

cat(sprintf(
  "%s: watches %s; fires only on a failed push run of the default branch; grants issues: write and no other write scope.\n",
  PATH, paste(WATCHED, collapse = " + ")
))
