# Word-for-word parity of the printed cautions, before and after M131.
#
# M131 moves every prose caution and note onto one wrapping helper, so the
# line breaks move. The words must not. This script prints each caution twice,
# once from a build of the package at a named base commit and once from the
# working tree, collapses the line breaks in both, and compares the word
# sequences.
#
# Usage, from the repo root:
#   Rscript tools/m131-caution-word-parity.R [<base-commit>]
#
# The base commit defaults to 26bd64ac, the commit M131 branched from.
#
# Both runs evaluate the SAME fixtures, the ones in
# tests/testthat/helper-caution-fixtures.R in the working tree, so a fixture
# cannot be one thing before and another after. The base build has no such
# file of its own; the working tree's copy is sourced into it.
#
# A package can only be loaded once per R session, so each build is printed in
# a separate Rscript child process, and this parent compares the two results.

args <- commandArgs(trailingOnly = TRUE)

# ---- the child: print every fixture from whatever package is at `pkg` -------

capture_fixtures <- function(pkg, helper, out, width) {
  suppressMessages(pkgload::load_all(pkg, quiet = TRUE))
  source(helper, local = FALSE)
  fixtures <- caution_fixtures()
  captured <- lapply(names(fixtures), function(nm) {
    fx <- fixtures[[nm]]
    object <- tryCatch(fx$build(), error = function(e) e)
    if (inherits(object, "error")) {
      return(list(name = nm, error = conditionMessage(object)))
    }
    old <- options(width = width)
    on.exit(options(old), add = TRUE)
    lines <- tryCatch(
      utils::capture.output(fx$print(object)),
      error = function(e) e
    )
    if (inherits(lines, "error")) {
      return(list(name = nm, error = conditionMessage(lines)))
    }
    list(name = nm, marker = fx$marker, lines = lines)
  })
  names(captured) <- names(fixtures)
  saveRDS(captured, out)
}

if (length(args) >= 1 && args[[1]] == "--capture") {
  capture_fixtures(
    pkg = args[[2]],
    helper = args[[3]],
    out = args[[4]],
    width = as.integer(args[[5]])
  )
  quit(save = "no")
}

# ---- the parent -------------------------------------------------------------

base_commit <- if (length(args) >= 1) args[[1]] else "26bd64ac"
repo <- normalizePath(".")
helper <- file.path(repo, "tests", "testthat", "helper-caution-fixtures.R")
stopifnot(file.exists(helper))

work <- tempfile("m131-parity-")
dir.create(work)
base_tree <- file.path(work, "base")
dir.create(base_tree)

message("Extracting ", base_commit, " ...")
status <- system2(
  "git",
  c("archive", base_commit),
  stdout = file.path(work, "base.tar")
)
stopifnot(status == 0)
utils::untar(file.path(work, "base.tar"), exdir = base_tree)

# The width is the one the words are compared at. It is deliberately NOT the
# width either build wraps to: the comparison collapses line breaks, so the
# words must match at any width, and a width unlike both defaults keeps the
# test from passing by accident on identical breaks.
compare_width <- 100L

run_capture <- function(pkg, out) {
  status <- system2(
    "Rscript",
    c(
      shQuote(file.path(repo, "tools", "m131-caution-word-parity.R")),
      "--capture", shQuote(pkg), shQuote(helper), shQuote(out), compare_width
    )
  )
  stopifnot(status == 0)
  readRDS(out)
}

message("Printing the cautions from ", base_commit, " ...")
before <- run_capture(base_tree, file.path(work, "before.rds"))
message("Printing the cautions from the working tree ...")
after <- run_capture(repo, file.path(work, "after.rds"))

# ---- compare ----------------------------------------------------------------

words_of <- function(lines) {
  flat <- paste(lines, collapse = " ")
  parts <- unlist(strsplit(flat, "[ \t\r\n]+"))
  parts[nzchar(parts)]
}

rows <- union(names(before), names(after))
verdicts <- lapply(rows, function(nm) {
  b <- before[[nm]]
  a <- after[[nm]]
  if (is.null(b)) return(list(row = nm, verdict = "ADDED"))
  if (is.null(a)) return(list(row = nm, verdict = "LOST"))
  if (!is.null(b$error) || !is.null(a$error)) {
    return(list(
      row = nm,
      verdict = "ERROR",
      detail = paste(c(b$error, a$error), collapse = " | ")
    ))
  }
  wb <- words_of(b$lines)
  wa <- words_of(a$lines)
  # Whitespace is collapsed on both sides: a marker phrase can fall across a
  # line break at the comparison width, putting the continuation indent inside
  # the phrase. Matching raw text would report a caution as gone when it is
  # merely wrapped differently.
  squash <- function(x) paste(unlist(strsplit(x, "[ \t\r\n]+")), collapse = " ")
  fired_b <- grepl(squash(b$marker), squash(paste(b$lines, collapse = " ")),
                   fixed = TRUE)
  fired_a <- grepl(squash(a$marker), squash(paste(a$lines, collapse = " ")),
                   fixed = TRUE)
  if (!fired_b) {
    # The caution did not print at the base commit, so this fixture is not
    # evidence about what M131 preserved. Reported, never silently passed.
    return(list(row = nm, verdict = "NOT-IN-BASE"))
  }
  if (!fired_a) return(list(row = nm, verdict = "STOPPED PRINTING"))
  if (identical(wb, wa)) return(list(row = nm, verdict = "SAME WORDS"))
  first <- which(c(wb, rep(NA, max(0, length(wa) - length(wb)))) !=
                   c(wa, rep(NA, max(0, length(wb) - length(wa)))))
  at <- if (length(first) > 0) first[[1]] else min(length(wb), length(wa)) + 1
  list(
    row = nm,
    verdict = "WORDS DIFFER",
    detail = sprintf(
      "first difference at word %d: base %s, branch %s",
      at,
      if (at <= length(wb)) dQuote(wb[[at]], FALSE) else "<end>",
      if (at <= length(wa)) dQuote(wa[[at]], FALSE) else "<end>"
    )
  )
})

for (v in verdicts) {
  cat(sprintf("%-40s %s\n", v$row, v$verdict))
  if (!is.null(v$detail)) cat("    ", v$detail, "\n", sep = "")
}

bad <- Filter(function(v) !v$verdict %in% c("SAME WORDS", "NOT-IN-BASE"), verdicts)
same <- Filter(function(v) v$verdict == "SAME WORDS", verdicts)
cat("\n", length(same), " of ", length(verdicts),
    " fixtures print the same words; ", length(bad), " do not.\n", sep = "")
if (length(same) == 0) {
  stop("No fixture was compared. The comparison proved nothing.")
}
if (length(bad) > 0) quit(save = "no", status = 1)
