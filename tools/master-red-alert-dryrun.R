#!/usr/bin/env Rscript

# Dry run of the alert's shell body against synthetic workflow_run payloads
# (cairn M96).
#
# The body in `.github/workflows/master-red-alert.yaml` runs only when a push
# run of a watched workflow fails on the default branch. Rather than wait for
# that, this script lifts the body straight out of the workflow file — so
# there is no second copy to drift — puts a recording stub for `gh` on PATH,
# and runs it against three fixtures:
#
#   1. marker label present, no open issue  -> exactly one issue created
#   2. marker label present, matching issue -> exactly one comment, no issue
#   3. marker label absent, no open issue   -> label created BEFORE the search
#   4. marker label absent, the create FAILS -> the alert still posts, unlabeled
#
# `jq` is NOT stubbed: the body pipes the issue list through it, so the real
# filter is exercised. It is preinstalled on GitHub-hosted runners.
#
# Not part of the built package (`^tools$` is in .Rbuildignore).

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("this dry run parses YAML and needs the `yaml` package installed.",
       call. = FALSE)
}
if (nzchar(Sys.which("jq")) == FALSE) {
  stop("this dry run needs `jq` on PATH — the alert body filters with it.",
       call. = FALSE)
}

PATH_YAML <- ".github/workflows/master-red-alert.yaml"
doc <- yaml::read_yaml(PATH_YAML)
steps <- doc$jobs[[1L]]$steps
runs <- vapply(steps, function(s) is.character(s$run), logical(1L))
if (sum(runs) != 1L) {
  stop("expected exactly one step with a `run:` body in ", PATH_YAML,
       call. = FALSE)
}
body <- steps[[which(runs)]]$run

# The synthetic failure payload. These stand in for the workflow_run fields
# the step's `env:` block carries in.
payload <- c(
  ALERT_WORKFLOW = "R-CMD-check.yaml",
  ALERT_RUN_URL = "https://github.com/jmgirard/circumplex/actions/runs/99999999999",
  ALERT_HEAD_SHA = "0f1e2d3c4b5a69788796a5b4c3d2e1f0abcdef01",
  ALERT_CONCLUSION = "failure",
  GH_TOKEN = "stub-token",
  GH_REPO = "jmgirard/circumplex"
)
EXPECTED_TITLE <- sprintf("master is red: %s", payload[["ALERT_WORKFLOW"]])

# The `gh` stub. It records the subcommand of every call in order, dumps each
# call's full argument vector for inspection, and answers the two read calls
# from fixture files. Anything it is not taught to answer is a hard failure,
# so a body that starts calling something new cannot pass unnoticed.
# $STUB_FAIL names a subcommand the stub should fail, so a fixture can ask
# what happens when GitHub refuses a call. Every failure mode of the label
# path was invisible while the stub could only succeed.
STUB <- '#!/bin/sh
printf "%s\\n" "$1 $2" >> "$STUB_LOG"
{ printf "== %s\\n" "$1 $2"; printf "%s\\n" "$@"; } >> "$STUB_ARGS"
if [ "$1 $2" = "${STUB_FAIL:-}" ]; then
  echo "stubbed failure: $1 $2" >&2
  exit 1
fi
case "$1 $2" in
  "label list")   cat "$STUB_LABELS" ;;
  "label create") : ;;
  "issue list")   cat "$STUB_ISSUES" ;;
  "issue comment") : ;;
  "issue create")  : ;;
  *) echo "unexpected gh call: $*" >&2; exit 1 ;;
esac
'

fixtures <- list(
  list(
    name = "label present, no open issue",
    labels = c("bug", "master-red"),
    issues = "[]",
    expect_calls = c("label list", "issue list", "issue create")
  ),
  list(
    name = "label present, matching open issue",
    labels = c("bug", "master-red"),
    issues = sprintf('[{"number": 42, "title": %s}]',
                     shQuote(EXPECTED_TITLE, type = "cmd")),
    expect_calls = c("label list", "issue list", "issue comment")
  ),
  list(
    name = "marker label absent",
    labels = c("bug"),
    issues = "[]",
    expect_calls = c("label list", "label create", "issue list", "issue create")
  ),
  # The alert exists for the moment master is broken, so a refused label call
  # must not cost the alert. `gh label create` is the realistic refusal: it
  # errors on an existing label without `--force`, which two racing alert runs
  # can produce on the repo\'s first-ever failure.
  list(
    name = "label create refused",
    labels = c("bug"),
    issues = "[]",
    fail = "label create",
    expect_calls = c("label list", "label create", "issue create")
  )
)

run_fixture <- function(fx) {
  dir <- tempfile("m96-dryrun-")
  bin <- file.path(dir, "bin")
  dir.create(bin, recursive = TRUE)

  writeLines(body, file.path(dir, "body.sh"))
  writeLines(STUB, file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), "0755")
  writeLines(fx$labels, file.path(dir, "labels.txt"))
  writeLines(fx$issues, file.path(dir, "issues.json"))

  log <- file.path(dir, "calls.log")
  args <- file.path(dir, "calls.args")
  file.create(log, args)

  # `env` is invoked directly rather than through system2()'s own `env=`
  # argument, which builds an unquoted command line.
  vars <- c(
    paste0(names(payload), "=", unname(payload)),
    paste0("PATH=", bin, ":", Sys.getenv("PATH")),
    paste0("STUB_LOG=", log),
    paste0("STUB_ARGS=", args),
    paste0("STUB_LABELS=", file.path(dir, "labels.txt")),
    paste0("STUB_ISSUES=", file.path(dir, "issues.json")),
    paste0("STUB_FAIL=", if (is.null(fx$fail)) "" else fx$fail)
  )
  status <- system2(
    "/usr/bin/env",
    c(shQuote(vars), "bash", shQuote(file.path(dir, "body.sh"))),
    stdout = TRUE, stderr = TRUE
  )
  list(
    status = attr(status, "status"),
    output = status,
    calls = readLines(log, warn = FALSE),
    args = readLines(args, warn = FALSE)
  )
}

problems <- character(0L)
report <- character(0L)

for (fx in fixtures) {
  res <- run_fixture(fx)

  if (!is.null(res$status)) {
    problems <- c(problems, sprintf(
      "fixture '%s': the body exited %d. Output:\n      %s",
      fx$name, res$status, paste(res$output, collapse = "\n      ")
    ))
    next
  }

  if (!identical(res$calls, fx$expect_calls)) {
    problems <- c(problems, sprintf(
      "fixture '%s': expected the call sequence [%s]; recorded [%s].",
      fx$name, paste(fx$expect_calls, collapse = " -> "),
      paste(res$calls, collapse = " -> ")
    ))
    next
  }

  # The counts the criterion names, asserted on the recorded log rather than
  # inferred from the sequence comparison above.
  n_create <- sum(res$calls == "issue create")
  n_comment <- sum(res$calls == "issue comment")
  expect_create <- sum(fx$expect_calls == "issue create")
  expect_comment <- sum(fx$expect_calls == "issue comment")
  if (n_create != expect_create || n_comment != expect_comment) {
    problems <- c(problems, sprintf(
      "fixture '%s': expected %d issue create / %d issue comment; recorded %d / %d.",
      fx$name, expect_create, expect_comment, n_create, n_comment
    ))
    next
  }

  # On the label-absent path the create must precede the search, not merely
  # appear somewhere in the run.
  if (all(c("label create", "issue list") %in% fx$expect_calls)) {
    if (which(res$calls == "label create")[1L] >
        which(res$calls == "issue list")[1L]) {
      problems <- c(problems, sprintf(
        "fixture '%s': the label was created after the dedupe search.", fx$name
      ))
      next
    }
  }

  # The issue text the run actually produced: the four payload values must
  # reach the body gh was handed, and the title must carry the workflow name.
  # A comment carries a body and no title, so the title is required only
  # where an issue was created.
  wanted <- payload[c("ALERT_WORKFLOW", "ALERT_RUN_URL", "ALERT_HEAD_SHA",
                      "ALERT_CONCLUSION")]
  if (n_create == 1L) wanted <- c(wanted, EXPECTED_TITLE)
  absent <- wanted[!vapply(
    wanted, function(v) any(grepl(v, res$args, fixed = TRUE)), logical(1L)
  )]
  if (length(absent)) {
    problems <- c(problems, sprintf(
      "fixture '%s': the arguments passed to gh never contain %s.",
      fx$name, paste(absent, collapse = ", ")
    ))
    next
  }
  if (n_comment == 1L && !any(grepl("^42$", res$args))) {
    problems <- c(problems, sprintf(
      "fixture '%s': the comment did not target the matching issue 42.", fx$name
    ))
    next
  }

  report <- c(report, sprintf("  ok  %-36s %s", fx$name,
                              paste(res$calls, collapse = " -> ")))
}

if (length(problems)) {
  stop(
    "master-red-alert dry run failed:\n  - ",
    paste(problems, collapse = "\n  - "),
    call. = FALSE
  )
}

cat(sprintf("%s: shell body dry-run against %d synthetic failure payload(s):\n%s\n",
            PATH_YAML, length(fixtures), paste(report, collapse = "\n")))
