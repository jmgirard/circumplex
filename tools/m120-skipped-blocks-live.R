# AC3 instrument for M120: every test block this branch newly skips on CRAN
# must still RUN off CRAN -- not skip for some other reason, and not fail.
#
#   Rscript tools/m120-skipped-blocks-live.R [<base-ref>]     # default: master
#
# The block set is derived from the branch diff rather than typed out, so it
# cannot drift from what was actually changed: each version of tests/testthat/
# is scanned for skip_on_cran() calls, each call is mapped to the test_that()
# block enclosing it, and the newly skipped set is HEAD's names minus the base
# ref's. The suite then runs with NOT_CRAN=true and every named block must
# report a result that is neither a skip nor a failure. An empty newly-skipped
# set is an error, not a pass -- there would be nothing to check.

base <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(base)) base <- "master"

# Names of the test_that() blocks in `text` that contain a skip_on_cran() call.
# A call outside any block (in a file-level helper) is an error: it would skip
# blocks it does not name, and this check would then miss them.
skipped_block_names <- function(text, where) {
  lines <- strsplit(text, "\r?\n")[[1]]
  calls <- grep("^\\s*skip_on_cran\\(\\)\\s*$", lines)
  if (!length(calls)) return(character(0))
  opens <- grep("test_that\\(", lines)
  vapply(calls, function(ln) {
    before <- opens[opens < ln]
    if (!length(before)) {
      stop("skip_on_cran() at ", where, ":", ln, " sits outside any test_that() ",
           "block, so it names no block for this check to verify.", call. = FALSE)
    }
    start <- max(before)
    txt <- paste(lines[seq.int(start, min(start + 4L, length(lines)))], collapse = " ")
    m <- regmatches(txt, regexpr('"(\\\\.|[^"\\\\])*"', txt, perl = TRUE))
    if (!length(m)) stop("cannot read a test name at ", where, ":", start, call. = FALSE)
    eval(parse(text = m))          # unescape exactly as the R parser would
  }, character(1))
}

test_files <- list.files("tests/testthat", "^test-.*\\.R$", full.names = TRUE)
if (!length(test_files)) stop("no test files found", call. = FALSE)

at_base <- function(path) {
  out <- suppressWarnings(system2("git", c("show", paste0(base, ":", path)),
                                  stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) return(NULL)
  paste(out, collapse = "\n")
}

wanted <- do.call(rbind, lapply(test_files, function(path) {
  head_names <- skipped_block_names(paste(readLines(path, warn = FALSE), collapse = "\n"), path)
  old <- at_base(path)
  base_names <- if (is.null(old)) character(0) else
    skipped_block_names(old, paste0(base, ":", path))
  new_names <- setdiff(head_names, base_names)
  if (!length(new_names)) return(NULL)
  data.frame(file = basename(path), test = new_names)
}))

if (is.null(wanted) || !nrow(wanted)) {
  stop("no block is newly skipped on CRAN against ", base,
       " -- refusing to pass vacuously.", call. = FALSE)
}
cat("blocks newly skipped on CRAN by this branch:", nrow(wanted), "\n")

library(testthat)
pkgload::load_all(".", quiet = TRUE)
Sys.setenv(NOT_CRAN = "true")
res <- as.data.frame(test_dir("tests/testthat", reporter = "silent",
                              stop_on_failure = FALSE, package = "circumplex",
                              load_package = "none"))

cat(sprintf("whole suite off CRAN: %d blocks, %d skipped, %d failed, %d errored\n",
            nrow(res), sum(res$skipped), sum(res$failed > 0), sum(res$error)))

key <- function(d) paste(d$file, d$test, sep = "\r")
got <- res[match(key(wanted), key(res)), ]

problems <- character(0)
for (i in seq_len(nrow(wanted))) {
  label <- paste0(wanted$file[[i]], " :: ", wanted$test[[i]])
  if (is.na(got$test[[i]])) {
    problems <- c(problems, paste(label, "-- did not run at all off CRAN"))
  } else if (got$skipped[[i]]) {
    problems <- c(problems, paste(label, "-- SKIPPED off CRAN"))
  } else if (got$failed[[i]] > 0 || got$error[[i]]) {
    problems <- c(problems, paste(label, "-- FAILED off CRAN"))
  }
}
writeLines(sprintf("  %-36s %s", wanted$file, wanted$test))

if (length(problems)) {
  stop(length(problems), " of ", nrow(wanted),
       " newly CRAN-skipped blocks do not run cleanly off CRAN:\n  - ",
       paste(problems, collapse = "\n  - "), call. = FALSE)
}
cat("all", nrow(wanted), "newly CRAN-skipped blocks ran off CRAN: none skipped, none failed\n")
