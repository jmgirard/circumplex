# Line-identity check for M131 (AC4).
#
# M131 re-wraps prose cautions and notes. Nothing else in the printed output
# may move. This script compares the branch against a base commit line by
# line and requires every changed group of lines to be attributable to a
# caution: a group whose text carries no known caution marker is reported as
# unattributed, and the script exits non-zero.
#
# It is the check that a word-level comparison cannot make. Collapsing line
# breaks to single spaces, which is how the word-parity script compares text,
# deletes exactly the column and blank-line information that makes a table a
# table. A branch that re-aligned a table's columns, or dropped a blank line
# before a note, would pass there and fail here.
#
# Two surfaces are compared:
#   1. the snapshot files the T1 census names as recording re-wrappable
#      output, base image versus working tree;
#   2. the full print() and summary() output of the axes-reliability
#      fixtures at width 80, which no snapshot file records at all.
#
# What it cannot decide, and what covers that instead (M131 review, O3). A
# destroyed PARAGRAPH boundary -- two sentences the emitter passed as separate
# elements, flowed onto one line -- carries the same words in a different line
# partition, which is the definition of a legitimate re-wrap. Nothing in the
# printed text tells the two apart, because the paragraph structure lives in
# the emitter, not in its output. That defect is caught instead by the
# committed snapshot in tests/testthat/_snaps/ci_accuracy.md, which pins the
# three settings sentences on three lines, and by the element-boundary tests
# in tests/testthat/test-wrap-prose.R.
#
# Usage, from the repo root:
#   Rscript tools/m131-line-identity.R [<base-commit>]

args <- commandArgs(trailingOnly = TRUE)
base_commit <- if (length(args) >= 1) args[[1]] else "26bd64ac"

snapshot_files <- c(
  "tests/testthat/_snaps/ci_accuracy.md",
  "tests/testthat/_snaps/cpm_api.md",
  "tests/testthat/_snaps/cpm_summary_markers.md",
  "tests/testthat/_snaps/fit_structure_api.md"
)

# ---- the markers that make a changed group attributable ---------------------
#
# One per census row, read from the same fixture registry the width tests use,
# so a row cannot be attributable here and absent there.

source("tests/testthat/helper-caution-fixtures.R")
markers <- vapply(caution_fixtures(), function(fx) fx$marker, character(1))
stopifnot(length(markers) > 0)

# A marker can straddle a line break, so a group's lines are joined with a
# space and the whitespace collapsed before the marker is looked for.
attributable <- function(group_lines) {
  flat <- paste(group_lines, collapse = " ")
  flat <- paste(unlist(strsplit(flat, "[ \t\r\n]+")), collapse = " ")
  hit <- markers[vapply(markers, function(m) {
    grepl(paste(unlist(strsplit(m, "[ \t\r\n]+")), collapse = " "), flat,
          fixed = TRUE)
  }, logical(1))]
  names(hit)
}

# ---- changed groups, by longest common subsequence --------------------------
#
# Base R has no line diff, and the shell's diff differs between platforms, so
# the comparison is done here. The inputs are printed output of a few hundred
# lines, where the quadratic table costs nothing.

changed_groups <- function(before, after) {
  n <- length(before)
  m <- length(after)
  lcs <- matrix(0L, n + 1L, m + 1L)
  if (n > 0 && m > 0) {
    for (i in seq(n, 1L)) {
      for (j in seq(m, 1L)) {
        lcs[i, j] <- if (identical(before[[i]], after[[j]])) {
          lcs[i + 1L, j + 1L] + 1L
        } else {
          max(lcs[i + 1L, j], lcs[i, j + 1L])
        }
      }
    }
  }
  groups <- list()
  pending_before <- character(0)
  pending_after <- character(0)
  flush <- function(at) {
    if (length(pending_before) == 0 && length(pending_after) == 0) return()
    groups[[length(groups) + 1L]] <<- list(
      at = at, before = pending_before, after = pending_after
    )
    pending_before <<- character(0)
    pending_after <<- character(0)
  }
  i <- 1L
  j <- 1L
  while (i <= n && j <= m) {
    if (identical(before[[i]], after[[j]])) {
      flush(i)
      i <- i + 1L
      j <- j + 1L
    } else if (lcs[i + 1L, j] >= lcs[i, j + 1L]) {
      pending_before <- c(pending_before, before[[i]])
      i <- i + 1L
    } else {
      pending_after <- c(pending_after, after[[j]])
      j <- j + 1L
    }
  }
  if (i <= n) pending_before <- c(pending_before, before[seq(i, n)])
  if (j <= m) pending_after <- c(pending_after, after[seq(j, m)])
  flush(max(i, 1L))
  groups
}

# A changed group is a re-wrap when it carries exactly the same words in the
# same order and those words sit on the lines differently. Both halves matter.
#
# Same words alone is not enough: re-aligning a table's columns leaves the
# words untouched and only the spacing inside each line changes, so the line
# partition is unchanged and the group is NOT a re-wrap.
#
# Non-empty matters too: adding or dropping a blank line leaves no words on
# either side, which would otherwise compare equal and be excused.
group_words <- function(x) {
  w <- unlist(strsplit(paste(x, collapse = " "), "[ \t\r\n]+"))
  w[nzchar(w)]
}

group_partition <- function(x) {
  lapply(x, function(l) {
    w <- unlist(strsplit(l, "[ \t\r\n]+"))
    w[nzchar(w)]
  })
}

group_blanks <- function(x) sum(!nzchar(trimws(x)))

is_rewrap <- function(before, after) {
  wb <- group_words(before)
  wa <- group_words(after)
  length(wb) > 0 &&
    identical(wb, wa) &&
    !identical(group_partition(before), group_partition(after))
}

# What a group carrying a caution marker is still required to prove.
#
# Carrying a marker is not on its own a licence (M131 review, O4). This
# milestone moves line breaks inside a caution and nothing else, so a group
# the marker excuses must show exactly that: the same words in the same order,
# and the same number of blank lines. Either half alone lets a real defect
# through. Without the word check, a group that holds a caution plus an
# altered neighbouring line is excused wholesale. Without the blank-line
# check, dropping the blank line that precedes a note is excused, because the
# dropped line lands in the note's own group and carries its marker with it --
# the defect AC4's second instrument probe plants, and the one this check was
# written for.
marker_is_safe <- function(before, after) {
  identical(group_words(before), group_words(after)) &&
    identical(group_blanks(before), group_blanks(after))
}

report <- function(label, before, after) {
  groups <- changed_groups(before, after)
  if (length(groups) == 0) {
    cat(sprintf("%-46s identical\n", label))
    return(character(0))
  }
  unattributed <- list()
  rows <- character(0)
  rewraps <- 0L
  for (g in groups) {
    rows_hit <- attributable(c(g$before, g$after))
    if (length(rows_hit) > 0 && marker_is_safe(g$before, g$after)) {
      rows <- union(rows, rows_hit)
    } else if (length(rows_hit) > 0) {
      g$why <- "carries a caution marker, but its words or blank lines moved"
      unattributed[[length(unattributed) + 1L]] <- g
    } else if (is_rewrap(g$before, g$after)) {
      rewraps <- rewraps + 1L
    } else {
      unattributed[[length(unattributed) + 1L]] <- g
    }
  }
  cat(sprintf(
    "%-46s %d group(s): %d by marker, %d re-wrap, %d NOT attributed\n",
    label, length(groups),
    length(groups) - rewraps - length(unattributed), rewraps,
    length(unattributed)
  ))
  if (length(rows) > 0) {
    cat("    attributed to: ", paste(sort(rows), collapse = ", "), "\n",
        sep = "")
  }
  for (g in unattributed) {
    cat("    UNATTRIBUTED near line ", g$at,
        if (!is.null(g$why)) paste0(" (", g$why, ")") else "", ":\n", sep = "")
    for (l in g$before) cat("      base   |", l, "\n")
    for (l in g$after) cat("      branch |", l, "\n")
  }
  vapply(unattributed, function(g) paste(c(g$before, g$after), collapse = " "),
         character(1))
}

# ---- 1. the snapshot files --------------------------------------------------

cat("Snapshot files, ", base_commit, " against the working tree:\n", sep = "")
bad <- character(0)
for (path in snapshot_files) {
  before <- system2(
    "git", c("show", paste0(base_commit, ":", path)), stdout = TRUE
  )
  if (!is.null(attr(before, "status"))) {
    stop("could not read ", path, " at ", base_commit)
  }
  after <- readLines(path, warn = FALSE)
  bad <- c(bad, report(path, before, after))
}

# ---- 2. the axes-reliability printers, which no snapshot records ------------

cat("\nAxes-reliability printers at width 80, which no snapshot records:\n")

repo <- normalizePath(".")
helper <- file.path(repo, "tests", "testthat", "helper-caution-fixtures.R")
work <- tempfile("m131-lines-")
dir.create(work)
base_tree <- file.path(work, "base")
dir.create(base_tree)
status <- system2("git", c("archive", base_commit),
                  stdout = file.path(work, "base.tar"))
stopifnot(status == 0)
utils::untar(file.path(work, "base.tar"), exdir = base_tree)

capture_from <- function(pkg, out) {
  status <- system2("Rscript", c(
    shQuote(file.path(repo, "tools", "m131-caution-word-parity.R")),
    "--capture", shQuote(pkg), shQuote(helper), shQuote(out), "80"
  ))
  stopifnot(status == 0)
  readRDS(out)
}

before_all <- capture_from(base_tree, file.path(work, "before80.rds"))
after_all <- capture_from(repo, file.path(work, "after80.rds"))

axes_rows <- grep(
  paste0(
    "row1[6-9]_|row2[0-6]_"
  ),
  names(after_all), value = TRUE
)
stopifnot(length(axes_rows) > 0)
for (nm in axes_rows) {
  b <- before_all[[nm]]
  a <- after_all[[nm]]
  if (is.null(b) || is.null(a) || !is.null(b$error) || !is.null(a$error)) {
    cat(sprintf("%-46s COULD NOT COMPARE\n", nm))
    bad <- c(bad, nm)
    next
  }
  bad <- c(bad, report(nm, b$lines, a$lines))
}

# ---- verdict ----------------------------------------------------------------

cat("\n")
if (length(bad) > 0) {
  cat(length(bad), " changed group(s) are not attributable to a caution.\n",
      sep = "")
  quit(save = "no", status = 1)
}
cat("Every changed group is attributable to a caution.\n")
