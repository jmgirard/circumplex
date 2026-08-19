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
  stop(sprintf("%s: expected exactly one job; found %d.", PATH, length(jobs)),
       call. = FALSE)
}
job <- jobs[[1L]]

# The gate is one `if:` expression, compared WHOLE. Testing for the three
# conditions as three independent substrings (the earlier shape) passed a
# `&&` -> `||` mutation, which would alert on every push run of either
# workflow, green ones and pull-request runs included. Whitespace is collapsed
# so re-wrapping the YAML cannot break the audit, and nothing else about the
# expression is left to interpretation.
EXPECTED_IF <- paste(
  "github.event.workflow_run.conclusion == 'failure' &&",
  "github.event.workflow_run.event == 'push' &&",
  "github.event.workflow_run.head_branch == github.event.repository.default_branch"
)
cond <- trimws(gsub("[[:space:]]+", " ",
                    paste(as.character(job[["if"]]), collapse = " ")))
if (!identical(cond, EXPECTED_IF)) {
  problems <- c(problems, sprintf(
    "%s: the job's `if:` must be exactly `%s`; found `%s`.",
    PATH, EXPECTED_IF, cond
  ))
}

# `on.workflow_run.workflows` matches each watched workflow's `name:` value,
# not its filename. The two coincide here only because both siblings set
# `name:` to their own filename; give one a human-readable name and this alert
# silently stops firing forever, with nothing else to notice it. So the names
# are read out of the watched files themselves.
for (w in WATCHED) {
  wf <- file.path(".github/workflows", w)
  if (!file.exists(wf)) {
    problems <- c(problems, sprintf(
      "%s: watched workflow %s does not exist.", PATH, wf
    ))
    next
  }
  declared <- yaml::read_yaml(wf)$name
  if (!identical(as.character(declared), w)) {
    problems <- c(problems, sprintf(
      "%s: `on.workflow_run.workflows` matches a workflow's `name:`, not its filename, and %s declares `name: %s`. Either restore `name: %s` there or list the declared name here.",
      PATH, wf, if (is.null(declared)) "nothing" else as.character(declared), w
    ))
  }
}

# ---- AC2: permissions ------------------------------------------------------

# GitHub's precedence: a job-level `permissions:` block REPLACES the
# workflow-level one rather than merging with it, so the job-level mapping is
# the effective one wherever it exists. Reading workflow-level first (the
# earlier shape) let a job-level `contents: write` pass unseen.
perms <- if (!is.null(job$permissions)) job$permissions else doc$permissions
if (!is.null(job$permissions) && !is.null(doc$permissions)) {
  problems <- c(problems, sprintf(
    "%s: `permissions:` is declared at both the workflow and the job level. The job-level block replaces the workflow-level one, so the two disagreeing is a trap; declare it once.",
    PATH
  ))
}
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

# --- serialization (not a criterion; a guard on T7's fix) ------------------
# Two pushes reddening the same workflow in quick succession would otherwise
# race: both search, neither finds, both create.
conc <- doc$concurrency
if (!is.list(conc) || !grepl("workflow_run", paste(conc$group, collapse = ""))) {
  problems <- c(problems, sprintf(
    "%s: needs a `concurrency:` block keyed on the watched workflow, so two alerts for the same workflow serialize instead of racing to create.",
    PATH
  ))
} else if (!identical(conc[["cancel-in-progress"]], FALSE)) {
  problems <- c(problems, sprintf(
    "%s: `concurrency.cancel-in-progress` must be false — a queued alert still needs to post; cancelling it loses the alert.",
    PATH
  ))
}

# ---- AC3(a): the values are tied to the workflow_run payload ---------------

# This is the ONLY place the issue's values are tied to the payload, and it is
# the whole of what this script promises about them. The dry run
# (tools/master-red-alert-dryrun.R) decides AC3(b) — that what actually
# reaches `gh` is boilerplate plus those values — by comparing captured
# output; it supplies the values itself, so it cannot see this binding.
#
# An earlier form of this section SCANNED the shell body for composed values.
# It was falsified twice at review, each time by a shell construct it did not
# know (command substitution; then composition at the `gh` call site, and
# re-assignment of the body after its heredoc). Proving a negative over an
# open-ended grammar is not a check, and the guarantee was deliberately
# descoped rather than widened a third time. Do not reinstate it.

runs <- vapply(job$steps, function(s) is.character(s$run), logical(1L))
if (sum(runs) != 1L) {
  stop(sprintf("%s: expected exactly one step with a `run:` body; found %d.",
               PATH, sum(runs)), call. = FALSE)
}
step <- job$steps[[which(runs)]]

# A step-level `if:` would silence the alert entirely while every check below
# still passed — the job-level gate AC1 pins says nothing about the step.
if (!is.null(step[["if"]])) {
  problems <- c(problems, sprintf(
    "%s: the alert step carries its own `if:` (`%s`). The gate belongs on the job; a step-level condition can disable the alert with nothing else noticing.",
    PATH, paste(as.character(step[["if"]]), collapse = " ")
  ))
}

EXPECTED_ENV <- c(
  ALERT_WORKFLOW   = "${{ github.event.workflow_run.name }}",
  ALERT_RUN_URL    = "${{ github.event.workflow_run.html_url }}",
  ALERT_HEAD_SHA   = "${{ github.event.workflow_run.head_sha }}",
  ALERT_CONCLUSION = "${{ github.event.workflow_run.conclusion }}",
  GH_TOKEN         = "${{ secrets.GITHUB_TOKEN }}",
  GH_REPO          = "${{ github.repository }}"
)
script <- strsplit(step$run, "\n", fixed = TRUE)[[1L]]
env <- vapply(step$env, as.character, character(1L))
if (!identical(env[order(names(env))], EXPECTED_ENV[order(names(EXPECTED_ENV))])) {
  problems <- c(problems, sprintf(
    "%s: the step's `env:` mapping must be exactly %s. Found: %s. This mapping is the only thing tying the issue's values to the workflow_run payload — a changed expression here reports the wrong run, and nothing downstream can tell.",
    PATH,
    paste(sprintf("%s=%s", names(EXPECTED_ENV), EXPECTED_ENV), collapse = "; "),
    if (length(env)) paste(sprintf("%s=%s", names(env), env), collapse = "; ") else "nothing"
  ))
}

# ---- AC5: the label is created before it is searched on --------------------

# `gh issue list --label X` on a label that does not exist returns an empty
# list rather than an error, so an uncreated label reads as "no open issue"
# and every failed push opens a new one.
label_create <- grep("gh label create", script, fixed = TRUE)
label_probe <- grep("gh label list", script, fixed = TRUE)
search <- grep("gh issue list", script, fixed = TRUE)

if (!length(label_create) || !length(label_probe)) {
  problems <- c(problems, sprintf(
    "%s: the shell body must probe for the marker label (`gh label list`) and create it when absent (`gh label create`).",
    PATH
  ))
} else if (!length(search)) {
  problems <- c(problems, sprintf("%s: no dedupe search (`gh issue list`).", PATH))
} else if (max(label_create) > min(search)) {
  problems <- c(problems, sprintf(
    "%s: the label is created at line %d of the shell body, after the dedupe search at line %d. A search on a nonexistent label returns empty and would defeat the dedupe.",
    PATH, max(label_create), min(search)
  ))
}

# ---- AC6: the alert installs nothing -------------------------------------

# tools/check-ci-deps.R keeps the R workflows' `extra-packages` allowlists in
# sync with DESCRIPTION Suggests. This workflow needs no allowlist entry there
# because it installs nothing at all: it runs `gh` and `jq`, both preinstalled
# on GitHub-hosted runners. That is asserted here rather than assumed, since
# check-ci-deps.R iterates a hand-written list of three workflow paths and
# cannot see this fourth file.
INSTALL_KEYS <- c("setup-r", "setup-r-dependencies", "extra-packages",
                  "install.packages")
found_keys <- INSTALL_KEYS[vapply(
  INSTALL_KEYS, function(k) any(grepl(k, raw, fixed = TRUE)), logical(1L)
)]
if (length(found_keys)) {
  problems <- c(problems, sprintf(
    "%s: dependency-install key(s) present: %s. This workflow must install nothing — otherwise it needs an allowlist entry in tools/check-ci-deps.R, which does not watch this file.",
    PATH, paste(found_keys, collapse = ", ")
  ))
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
  "%s: watches %s; fires only on a failed push run of the default branch; grants issues: write and no other write scope; carries the four workflow_run payload fields through `env:` and installs nothing.\n",
  PATH, paste(WATCHED, collapse = " + ")
))
