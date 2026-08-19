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

# ---- AC3: every value reaching the issue body comes from the payload -------

# The point of this section is that the issue text is REPORTED, never
# composed: a reader acting on the issue must be able to trust that the run
# URL and SHA name the run that actually failed. So it enumerates every
# interpolation site in the workflow and in its shell body, resolves each one,
# and requires that the sites reaching the body resolve to workflow_run
# payload fields and nothing else.

# The alert is the one step carrying a shell body; locating it by content
# rather than by position keeps this audit pointed at the right step if a
# setup step is ever added ahead of it.
runs <- vapply(job$steps, function(s) is.character(s$run), logical(1L))
if (sum(runs) != 1L) {
  stop(sprintf("%s: expected exactly one step with a `run:` body; found %d.",
               PATH, sum(runs)), call. = FALSE)
}
step <- job$steps[[which(runs)]]
script <- strsplit(step$run, "\n", fixed = TRUE)[[1L]]
env <- vapply(step$env, as.character, character(1L))

PAYLOAD_RE <- "^(github\\.event|context\\.payload)\\.workflow_run\\."

# --- interpolation sites of the form ${{ }} -------------------------------
# These are checked by SITE, not by value: comparing the set of expressions
# against the set in `env:` (the earlier shape of this check) let the same
# expression pass anywhere else in the file, including inside the shell body.
# The site that matters is the `run:` block — an expression there is
# substituted into the script before bash ever sees it, which is the standard
# Actions script-injection shape. Elsewhere in the file (`env:`,
# `concurrency:`, `if:`) an expression is evaluated by Actions itself and
# reaches no shell.
indent_of <- function(lines) nchar(sub("[^ ].*$", "", lines))
block_after <- function(key_line) {
  if (is.na(key_line)) return(integer(0L))
  after <- seq.int(key_line + 1L, length(raw))
  deeper <- indent_of(raw[after]) > indent_of(raw[key_line]) | !nzchar(raw[after])
  after[seq_len(match(FALSE, deeper, nomatch = length(after) + 1L) - 1L)]
}

run_key <- grep("^\\s*run:\\s*\\|", raw)[1L]
run_block <- block_after(run_key)
in_run <- intersect(grep("\\$\\{\\{", raw), run_block)
if (length(in_run)) {
  problems <- c(problems, sprintf(
    "%s: `${{ }}` expression(s) inside the `run:` body, at line(s) %s. Every value the shell reads must arrive through `env:`, where this audit can resolve it; interpolating one straight into the script is the Actions script-injection shape.",
    PATH, paste(in_run, collapse = ", ")
  ))
}

# Any `context.payload.*` read (a github-script residue) must be a
# workflow_run path too.
ctx <- unlist(regmatches(raw, gregexpr("context\\.payload\\.[A-Za-z0-9_.]+", raw)))
bad_ctx <- ctx[!grepl(PAYLOAD_RE, ctx)]
if (length(bad_ctx)) {
  problems <- c(problems, sprintf(
    "%s: `context.payload` reads outside the workflow_run payload: %s.",
    PATH, paste(unique(bad_ctx), collapse = ", ")
  ))
}

# --- the regions that reach the issue -------------------------------------
# Two of them: the `BODY=` heredoc, and the `TITLE=` assignment. The title is
# not decoration — it is the dedupe key, and it is what a create call carries.
# Scanning only the heredoc (the earlier shape) left a composed title
# unaudited.
heredoc_region <- function(var) {
  open <- grep(sprintf("^\\s*%s=.*<<'?([A-Za-z_][A-Za-z0-9_]*)'?", var), script)
  if (length(open) != 1L) {
    problems <<- c(problems, sprintf(
      "%s: expected exactly one `%s=` heredoc in the shell body; found %d.",
      PATH, var, length(open)
    ))
    return(character(0L))
  }
  delim <- sub(sprintf("^\\s*%s=.*<<'?([A-Za-z_][A-Za-z0-9_]*)'?.*$", var), "\\1",
               script[open])
  close <- open + which(script[-seq_len(open)] == delim)[1L]
  if (is.na(close)) {
    problems <<- c(problems, sprintf(
      "%s: the `%s=` heredoc is never closed by a bare `%s` line.",
      PATH, var, delim
    ))
    return(character(0L))
  }
  script[seq.int(open + 1L, close - 1L)]
}

body_region <- heredoc_region("BODY")
title_region <- grep("^\\s*TITLE=", script, value = TRUE)
if (length(title_region) != 1L) {
  problems <- c(problems, sprintf(
    "%s: expected exactly one `TITLE=` assignment in the shell body; found %d.",
    PATH, length(title_region)
  ))
}
reported <- c(body_region, title_region)

# Nothing reaching the issue may be produced rather than interpolated. A
# command substitution or a parameter expansion carrying a default is a value
# this scan cannot resolve and the payload did not supply, so it is refused
# outright rather than enumerated.
composed <- grep("\\$\\(|`|\\$\\{[A-Za-z_][A-Za-z0-9_]*[^A-Za-z0-9_}]",
                 reported, value = TRUE)
composed <- composed[!grepl("^\\s*\\\\`", composed)]
composed <- composed[grepl("\\$\\(|\\$\\{[A-Za-z_][A-Za-z0-9_]*[^A-Za-z0-9_}]", composed) |
                       grepl("[^\\\\]`", composed)]
if (length(composed)) {
  problems <- c(problems, sprintf(
    "%s: the issue title or body composes value(s) rather than reporting them, at: %s. Command substitution, backticks and defaulted expansions cannot resolve to a workflow_run field.",
    PATH, paste(trimws(composed), collapse = " | ")
  ))
}

# Assignments may be nested inside an `if`, so leading whitespace is part of
# the shape rather than a reason to miss one.
assigned <- sub("^\\s*([A-Za-z_][A-Za-z0-9_]*)=.*$", "\\1",
                grep("^\\s*[A-Za-z_][A-Za-z0-9_]*=", script, value = TRUE))

vars_in <- function(lines) {
  hits <- unlist(regmatches(
    lines, gregexpr("\\$\\{?[A-Za-z_][A-Za-z0-9_]*\\}?", lines)
  ))
  unique(gsub("^\\$\\{?|\\}$", "", hits))
}

# Resolve a shell variable to the set of payload expressions behind it,
# following local assignments (TITLE and BODY are built from env vars, so a
# value smuggled in through a local would still be caught here). Returns NA
# for a name that resolves to neither.
resolve <- function(name, seen = character(0L)) {
  if (name %in% seen) return(character(0L))
  if (name %in% names(env)) {
    expr <- trimws(gsub("^\\$\\{\\{|\\}\\}$", "", env[[name]]))
    return(if (grepl(PAYLOAD_RE, expr)) expr else NA_character_)
  }
  if (name %in% assigned) {
    rhs <- grep(sprintf("^\\s*%s=", name), script, value = TRUE)
    inner <- setdiff(vars_in(rhs), name)
    if (!length(inner)) return(character(0L))
    return(unlist(lapply(inner, resolve, seen = c(seen, name))))
  }
  NA_character_
}

# Every site in the reported regions must resolve to the payload.
reported_vars <- vars_in(reported)
resolved <- lapply(reported_vars, resolve)
names(resolved) <- reported_vars
unresolved <- reported_vars[vapply(resolved, anyNA, logical(1L))]
if (length(unresolved)) {
  problems <- c(problems, sprintf(
    "%s: value(s) substituted into the issue title or body that do not come from the workflow_run payload: %s.",
    PATH, paste(unresolved, collapse = ", ")
  ))
}

# Every site in the REST of the shell body is enumerated too, and each must be
# classifiable — an env value, a variable the script assigns, or one of the
# shell's own. An unknown name means the scan has stopped seeing the body it
# claims to cover.
# `jq --arg <name>` declares a name inside the filter, not in the shell.
jq_args <- unlist(regmatches(
  script, gregexpr("(?<=--arg )[A-Za-z_][A-Za-z0-9_]*", script, perl = TRUE)
))
other_vars <- setdiff(vars_in(script), c(reported_vars, jq_args))
unknown <- other_vars[!(other_vars %in% names(env) | other_vars %in% assigned)]
if (length(unknown)) {
  problems <- c(problems, sprintf(
    "%s: unclassifiable substitution(s) in the shell body: %s. Every site must be an `env:` value, a variable the body assigns, or a declared local.",
    PATH, paste(unknown, collapse = ", ")
  ))
}

paths <- unique(stats::na.omit(unlist(resolved)))
fields <- sub(PAYLOAD_RE, "", paths)
REQUIRED_FIELDS <- c("name", "html_url", "head_sha", "conclusion")
if (!all(REQUIRED_FIELDS %in% fields)) {
  problems <- c(problems, sprintf(
    "%s: the issue must name the failing workflow, run URL, head SHA and conclusion; payload fields it interpolates: %s. An empty or short enumeration fails rather than passing vacuously.",
    PATH, if (length(fields)) paste(fields, collapse = ", ") else "none"
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
  "%s: watches %s; fires only on a failed push run of the default branch; grants issues: write and no other write scope.\nIssue body interpolates %d workflow_run payload field(s) and nothing else: %s.\n",
  PATH, paste(WATCHED, collapse = " + "),
  length(fields), paste(sort(fields), collapse = ", ")
))
