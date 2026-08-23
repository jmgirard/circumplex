#!/usr/bin/env Rscript

# Guard: this repository's LIVE branch rulesets must match the intent committed
# in tools/branch-protection.json (cairn M105).
#
# Why this exists. Branch protection lives in GitHub's settings, not in the
# tree, so it can be changed in the web UI — weakened, disabled, or deleted —
# without leaving a trace any commit, review, or `R CMD check` would show. The
# committed JSON is the intent; this script is the only thing that notices when
# GitHub stops agreeing with it.
#
# It asserts `enforcement` as one of the compared fields deliberately. A ruleset
# whose rules are all still correct but whose enforcement has been flipped to
# "disabled" or "evaluate" is the failure mode that looks most like health: the
# rules are right there in the UI, and they bind nothing. Validating the rules
# without validating the switch that makes them authoritative leaves the hole
# exactly where regression is likeliest (the lesson cairn M58 recorded).
#
# Scope: rulesets with `target: "branch"` whose source is this repository.
# Tag and push rulesets, and any inherited from an owning organization, are
# outside what the committed file describes and are not compared.
#
# Run by /milestone-review via cairn/PROFILE.md's consistency-gate. Needs `gh`
# authenticated (the rulesets API is not public) and the `jsonlite` package;
# it says so and fails rather than passing when either is missing. Not part of
# the built package (`^tools$` is in .Rbuildignore).

INTENT <- "tools/branch-protection.json"

# ---- the compared domain --------------------------------------------------

# The fields compared between the committed intent and the live API. The
# comparison iterates this constant, so this vector — not any prose elsewhere —
# is the domain over which "the live rulesets match the committed intent" is
# claimed. Each name must have an extractor in EXTRACT below.
COMPARED_FIELDS <- c(
  "enforcement",
  "target",
  "ref_name_include",
  "ref_name_exclude",
  "rule_types",
  "required_status_check_contexts",
  "bypass_actors"
)

# One extractor per compared field, applied to BOTH sides — a projection bug
# therefore cannot make the two sides falsely agree in only one direction.
# Each returns a character vector; order-insensitive fields are sorted so a
# reordering in the API response is not reported as drift.
EXTRACT <- list(
  enforcement = function(r) as.character(r$enforcement %||% NA_character_),
  target = function(r) as.character(r$target %||% NA_character_),
  ref_name_include = function(r) {
    sort(vapply(r$conditions$ref_name$include %||% list(), as.character, ""),
         na.last = TRUE)
  },
  # The exclude list is a switch beside the rules it can disable: one pattern
  # matching the default branch turns the ruleset off for it while enforcement
  # stays "active" and include stays untouched (review finding, M105).
  ref_name_exclude = function(r) {
    sort(vapply(r$conditions$ref_name$exclude %||% list(), as.character, ""),
         na.last = TRUE)
  },
  rule_types = function(r) {
    sort(vapply(r$rules %||% list(), function(x) as.character(x$type), ""),
         na.last = TRUE)
  },
  required_status_check_contexts = function(r) {
    rules <- r$rules %||% list()
    checks <- rules[vapply(rules, function(x) identical(x$type, "required_status_checks"), NA)]
    # na.last = TRUE throughout: sort()'s default na.last = NA silently DROPS
    # an NA element, which would erase a malformed live entry instead of
    # flagging it (the M98 lesson's shape).
    sort(unlist(lapply(checks, function(x) {
      vapply(x$parameters$required_status_checks %||% list(),
             function(cc) as.character(cc$context), "")
    }), use.names = FALSE), na.last = TRUE)
  },
  bypass_actors = function(r) {
    sort(vapply(r$bypass_actors %||% list(), function(a) {
      sprintf("%s/%s/%s",
              as.character(a$actor_type %||% NA_character_),
              as.character(a$actor_id %||% NA_character_),
              as.character(a$bypass_mode %||% NA_character_))
    }, ""))
  }
)

`%||%` <- function(x, y) if (is.null(x)) y else x

stopifnot(setequal(COMPARED_FIELDS, names(EXTRACT)))

# ---- fail-closed preconditions --------------------------------------------

die <- function(...) stop(sprintf(...), call. = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  die("this guard parses JSON and needs the `jsonlite` package installed.")
}
if (!nzchar(Sys.which("gh"))) {
  die("this guard reads the live rulesets and needs the GitHub CLI (`gh`) on PATH.")
}
if (!identical(suppressWarnings(system2("gh", c("auth", "status"),
                                        stdout = FALSE, stderr = FALSE)), 0L)) {
  die("`gh` is not authenticated (`gh auth status` failed) — the rulesets API needs auth.")
}
if (!file.exists(INTENT)) {
  die("%s: file not found (run from the repository root).", INTENT)
}

# Every `gh` call is fail-closed: a non-zero exit, or output that is not the
# JSON we expect, stops the script. There is no arm that treats an unanswered
# question as agreement.
gh <- function(...) {
  args <- c(...)
  out <- suppressWarnings(system2("gh", args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    die("`gh %s` failed (exit %s):\n%s",
        paste(args, collapse = " "), status, paste(out, collapse = "\n"))
  }
  paste(out, collapse = "\n")
}

parse_json <- function(txt, what) {
  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) die("%s: could not be parsed as JSON: %s", what, conditionMessage(e))
  )
  parsed
}

# ---- read both sides ------------------------------------------------------

intent <- parse_json(paste(readLines(INTENT, warn = FALSE), collapse = "\n"), INTENT)
committed <- intent$rulesets %||% list()
if (!length(committed)) {
  die("%s: no `rulesets` array, or it is empty — nothing to enforce.", INTENT)
}

# The slug comes from the checkout's own remote, so this guard is not pinned to
# one repository name.
slug <- trimws(gh("repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"))
if (!grepl("^[^/]+/[^/]+$", slug)) {
  die("could not derive OWNER/REPO from the checkout (got %s).", dQuote(slug))
}

summaries <- parse_json(gh("api", "--paginate", sprintf("repos/%s/rulesets", slug)),
                        sprintf("the ruleset list for %s", slug))

# Only this repository's own branch rulesets; see the scope note in the header.
summaries <- Filter(function(s) {
  identical(s$target, "branch") && identical(s$source_type %||% "", "Repository")
}, summaries)

live <- lapply(summaries, function(s) {
  parse_json(gh("api", sprintf("repos/%s/rulesets/%s", slug, s$id)),
             sprintf("ruleset %s", s$id))
})
names(live) <- vapply(live, function(r) as.character(r$name), "")
names(committed) <- vapply(committed, function(r) as.character(r$name), "")

# GitHub permits two rulesets with one name; a by-name lookup would compare
# only the first and silently skip its twin. Refuse rather than guess.
if (anyDuplicated(names(live))) {
  die("two live branch rulesets share a name (%s) — refusing to compare by name.",
      paste(unique(names(live)[duplicated(names(live))]), collapse = ", "))
}

# ---- compare --------------------------------------------------------------

problems <- character(0L)

missing <- setdiff(names(committed), names(live))
for (nm in missing) {
  problems <- c(problems, sprintf(
    "ruleset %s is committed in %s but does not exist on %s.", dQuote(nm), INTENT, slug))
}
unexpected <- setdiff(names(live), names(committed))
for (nm in unexpected) {
  problems <- c(problems, sprintf(
    "branch ruleset %s exists on %s but is not committed in %s.", dQuote(nm), slug, INTENT))
}

show <- function(x) if (!length(x)) "<none>" else paste(x, collapse = ", ")

for (nm in intersect(names(committed), names(live))) {
  for (field in COMPARED_FIELDS) {
    want <- EXTRACT[[field]](committed[[nm]])
    got <- EXTRACT[[field]](live[[nm]])
    if (!identical(want, got)) {
      problems <- c(problems, sprintf(
        "ruleset %s, field %s:\n    committed: %s\n    live:      %s",
        dQuote(nm), field, show(want), show(got)))
    }
  }
}

# ---- report ---------------------------------------------------------------

if (length(problems)) {
  message(sprintf("Branch protection on %s does NOT match %s:\n", slug, INTENT))
  for (p in problems) message("  - ", p)
  message(sprintf(
    "\nEither the settings drifted and should be restored, or the change was\nintended and %s should be updated to match in the same commit.",
    INTENT))
  quit(status = 1L)
}

message(sprintf("Branch protection on %s matches %s:", slug, INTENT))
for (nm in names(committed)) {
  message(sprintf("  - %s: %s (%s), bypass %s",
                  nm,
                  show(EXTRACT$rule_types(live[[nm]])),
                  EXTRACT$enforcement(live[[nm]]),
                  show(EXTRACT$bypass_actors(live[[nm]]))))
}
