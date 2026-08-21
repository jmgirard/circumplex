#!/usr/bin/env Rscript

# Dry run of the alert's shell body against synthetic workflow_run payloads
# (cairn M96).
#
# The body in `.github/workflows/master-red-alert.yaml` runs only when a push
# run of a watched workflow ends badly on the default branch. Rather than wait
# for that, this script lifts the body straight out of the workflow file — so
# there is no second copy to drift — puts a recording stub for `gh` on PATH,
# and runs it against the fixtures listed below:
#
#   1. marker label present, no open issue  -> exactly one issue created
#   2. marker label present, matching issue -> exactly one comment, no issue
#   3. marker label absent, no open issue   -> label created BEFORE the search
#   4. marker label absent, the create FAILS -> the alert still posts, unlabeled
#   5. the label LIST fails                 -> the alert still posts, unlabeled
#
# It also decides AC3(b): for every `gh issue create` and `gh issue comment`
# the stub records, in every fixture, the `--title` and `--body` it was handed
# are captured, each synthetic payload value is replaced by its field name,
# and the result must equal the committed template below. That is a
# comparison over what the alert PRODUCES; the binding of those values to the
# workflow_run payload is decided separately, by the `env:` check in
# tools/check-master-red-alert.R.
#
# `jq` is NOT stubbed: the body pipes the issue list through it, so the real
# filter is exercised. It is preinstalled on GitHub-hosted runners.
#
# What this harness does NOT decide: the stub dispatches on the gh subcommand
# alone and ignores flags, so it cannot see whether a call's `--label` or
# `--state` filter is right — a dropped `--label` on the search or the create
# would break deduping and pass here. It also decides the text produced under
# THIS environment, so a construct expanding to nothing locally and to text on
# a runner is out of reach. Both are accepted at internal tier (cairn M96).
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
# Each call has its argument vector written to its OWN file, NUL-separated. An earlier
# form appended every call to one file behind a `== <subcommand>` header line
# — in-band framing a multi-line --body could forge, truncating its own capture
# and hiding whatever followed. Nothing the alert writes is a delimiter here.
STUB_SEQ=$(( $(cat "$STUB_N" 2>/dev/null || echo 0) + 1 ))
printf "%s" "$STUB_SEQ" > "$STUB_N"
printf "%s\\0" "$1 $2" "$@" > "$STUB_ARGS_DIR/call-$STUB_SEQ"
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
  ),
  # The read call can be refused too — a token scope, a transient 5xx. Before
  # this fixture the body aborted here under `set -e` and posted nothing.
  list(
    name = "label list refused",
    labels = c("bug"),
    issues = "[]",
    fail = "label list",
    expect_calls = c("label list", "issue create")
  )
)

# The expected issue text, with each payload value standing as its field name.
# Committed once as a reviewed expectation — NEVER regenerate it from a
# capture that failed, which would make this check decide nothing.
EXPECTED_TITLE_TEMPLATE <- "master is red: <name>"
EXPECTED_BODY_TEMPLATE <- paste(
  "A push run of `<name>` on the default branch concluded `<conclusion>`.",
  "",
  "| field | value |",
  "|---|---|",
  "| workflow | `<name>` |",
  "| run | <html_url> |",
  "| head SHA | `<head_sha>` |",
  "| conclusion | `<conclusion>` |",
  "",
  "Close this issue once that workflow is green on the default branch",
  "again; nothing closes it automatically.",
  sep = "\n"
)

# Field names for the four payload values, longest value first so that
# replacing one can never leave a fragment of another behind.
FIELD_OF <- c(
  ALERT_WORKFLOW = "<name>", ALERT_RUN_URL = "<html_url>",
  ALERT_HEAD_SHA = "<head_sha>", ALERT_CONCLUSION = "<conclusion>"
)

# The four synthetic values must be non-empty and substrings neither of one
# another nor of the boilerplate, or replacing them by field name would be
# ill-defined — and an ill-defined normalization can hide exactly what this
# check is for.
vals <- payload[names(FIELD_OF)]
if (any(!nzchar(vals))) stop("synthetic payload values must be non-empty.", call. = FALSE)
for (i in seq_along(vals)) {
  others <- c(vals[-i], EXPECTED_BODY_TEMPLATE, EXPECTED_TITLE_TEMPLATE)
  if (any(grepl(vals[[i]], others, fixed = TRUE))) {
    stop(sprintf("synthetic value %s occurs inside another value or the template; pick a distinct one.",
                 names(vals)[[i]]), call. = FALSE)
  }
}

# Read the recorded calls back: one file per call, NUL-separated fields, the
# first being the subcommand. No text the alert produces can be mistaken for
# framing.
calls_from <- function(dir) {
  files <- list.files(dir, pattern = "^call-", full.names = TRUE)
  files <- files[order(as.integer(sub("^.*call-", "", files)))]
  lapply(files, function(f) {
    bytes <- readBin(f, "raw", file.size(f))
    fields <- vapply(split(bytes, cumsum(c(0L, head(bytes, -1L) == as.raw(0)))),
                     function(b) rawToChar(b[b != as.raw(0)]), character(1L),
                     USE.NAMES = FALSE)
    list(fields[1L], argv = fields[-1L])
  })
}

# Each argument is its own recorded field, so a flag's value is simply the
# next one — independent of argument order and of what the value contains.
flag_value <- function(argv, flag) {
  at <- which(argv == flag)
  if (!length(at) || at[1L] >= length(argv)) return(NULL)
  argv[at[1L] + 1L]
}

to_template <- function(text) {
  for (nm in names(FIELD_OF)) {
    text <- gsub(payload[[nm]], FIELD_OF[[nm]], text, fixed = TRUE)
  }
  text
}

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
  args_dir <- file.path(dir, "calls")
  dir.create(args_dir)
  file.create(log)

  # `env` is invoked directly rather than through system2()'s own `env=`
  # argument, which builds an unquoted command line.
  vars <- c(
    paste0(names(payload), "=", unname(payload)),
    paste0("PATH=", bin, ":", Sys.getenv("PATH")),
    paste0("STUB_LOG=", log),
    paste0("STUB_ARGS_DIR=", args_dir),
    paste0("STUB_N=", file.path(dir, "calls.n")),
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
    recorded = calls_from(args_dir)
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
  all_args <- unlist(lapply(res$recorded, function(c) c$argv))
  absent <- wanted[!vapply(
    wanted, function(v) any(grepl(v, all_args, fixed = TRUE)), logical(1L)
  )]
  if (length(absent)) {
    problems <- c(problems, sprintf(
      "fixture '%s': the arguments passed to gh never contain %s.",
      fx$name, paste(absent, collapse = ", ")
    ))
    next
  }
  if (n_comment == 1L && !any(all_args == "42")) {
    problems <- c(problems, sprintf(
      "fixture '%s': the comment did not target the matching issue 42.", fx$name
    ))
    next
  }

  # AC3(b): every call that produces issue text, in this fixture, must carry a
  # title and body that reduce to the committed templates once the payload
  # values are replaced by their field names.
  produced <- Filter(function(c) c[[1L]] %in% c("issue create", "issue comment"),
                     res$recorded)
  if (!length(produced)) {
    problems <- c(problems, sprintf(
      "fixture '%s': no issue create or comment was recorded, so there is no issue text to check.",
      fx$name
    ))
    next
  }
  bad <- character(0L)
  for (call in produced) {
    body_txt <- flag_value(call$argv, "--body")
    title_txt <- flag_value(call$argv, "--title")
    if (is.null(body_txt)) {
      bad <- c(bad, sprintf("`%s` carries no --body", call[[1L]]))
      next
    }
    missing_vals <- names(FIELD_OF)[!vapply(
      names(FIELD_OF),
      function(nm) grepl(payload[[nm]], body_txt, fixed = TRUE),
      logical(1L)
    )]
    if (length(missing_vals)) {
      bad <- c(bad, sprintf("`%s`'s body is missing payload value(s) %s",
                            call[[1L]], paste(missing_vals, collapse = ", ")))
      next
    }
    # The field-name markers must not already occur in the raw capture, or
    # the substitution is not injective and a hardcoded `<name>` in the body
    # would reduce to the template while the issue printed the placeholder.
    planted <- FIELD_OF[vapply(
      FIELD_OF, function(m) grepl(m, paste(title_txt, body_txt), fixed = TRUE),
      logical(1L)
    )]
    if (length(planted)) {
      bad <- c(bad, sprintf("`%s` already contains the field marker(s) %s, so the capture cannot be normalized",
                            call[[1L]], paste(planted, collapse = ", ")))
      next
    }
    if (!identical(to_template(body_txt), EXPECTED_BODY_TEMPLATE)) {
      bad <- c(bad, sprintf(
        "`%s`'s body does not reduce to the committed template. Reduced to:\n      %s",
        call[[1L]], gsub("\n", "\n      ", to_template(body_txt))
      ))
      next
    }
    if (!is.null(title_txt) &&
        !identical(to_template(title_txt), EXPECTED_TITLE_TEMPLATE)) {
      bad <- c(bad, sprintf("`%s`'s title reduces to \"%s\", not the committed \"%s\"",
                            call[[1L]], to_template(title_txt),
                            EXPECTED_TITLE_TEMPLATE))
    }
  }
  if (length(bad)) {
    problems <- c(problems, sprintf("fixture '%s': %s.", fx$name,
                                    paste(bad, collapse = "; ")))
    next
  }

  report <- c(report, sprintf("  ok  %-36s %-52s issue text: %d call(s) reduce to the template",
                              fx$name, paste(res$calls, collapse = " -> "),
                              length(produced)))
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
