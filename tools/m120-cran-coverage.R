# AC4 instrument for M120: no exported function may lose ALL of its CRAN-mode
# test coverage to this milestone's skips.
#
#   Rscript tools/m120-cran-coverage.R [<base-ref>]      # default: master
#
# Attribution is textual, not by tracing: a test_that() block "exercises" an
# exported function when the block's source calls it by name. That is coarse in
# one direction only -- it can credit a block that merely mentions a call it
# does not reach -- so the block a function is credited to is REPORTED, and the
# milestone plants defects in those blocks to show they really do bite.
#
# For every exported function that some newly CRAN-skipped block called, the
# script requires at least one block that still runs on CRAN to call it too,
# and names that block. A function with no caller anywhere is not this
# milestone's business and is listed separately rather than silently dropped.

base <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(base)) base <- "master"

exports <- {
  ns <- readLines("NAMESPACE", warn = FALSE)
  e <- grep("^export\\(", ns, value = TRUE)
  sort(gsub('^export\\(|\\)$|"', "", e))
}
if (!length(exports)) stop("NAMESPACE lists no exports", call. = FALSE)

# --- split every test file into blocks, with their source and skip status ----

blocks_of <- function(text, file) {
  lines <- strsplit(text, "\r?\n")[[1]]
  opens <- grep("^test_that\\(", lines)
  if (!length(opens)) return(NULL)
  ends <- c(opens[-1] - 1L, length(lines))
  do.call(rbind, Map(function(a, b) {
    src <- lines[seq.int(a, b)]
    txt <- paste(src, collapse = "\n")
    m <- regmatches(txt, regexpr('"(\\\\.|[^"\\\\])*"', txt, perl = TRUE))
    data.frame(
      file = file,
      test = if (length(m)) eval(parse(text = m)) else NA_character_,
      skips_cran = any(grepl("^\\s*skip_on_cran\\(\\)\\s*$", src)),
      src = txt
    )
  }, opens, ends))
}

read_at <- function(path, ref) {
  if (is.null(ref)) return(paste(readLines(path, warn = FALSE), collapse = "\n"))
  out <- suppressWarnings(system2("git", c("show", paste0(ref, ":", path)),
                                  stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) return(NULL)
  paste(out, collapse = "\n")
}

collect <- function(ref) {
  files <- if (is.null(ref)) {
    list.files("tests/testthat", "^test-.*\\.R$", full.names = TRUE)
  } else {
    grep("^tests/testthat/test-.*\\.R$",
         system2("git", c("ls-tree", "-r", "--name-only", ref), stdout = TRUE),
         value = TRUE)
  }
  out <- lapply(files, function(f) {
    txt <- read_at(f, ref)
    if (is.null(txt)) NULL else blocks_of(txt, basename(f))
  })
  do.call(rbind, out)
}

now <- collect(NULL)
was <- collect(base)
if (is.null(now) || is.null(was)) stop("could not read one of the two versions", call. = FALSE)

# A block calls `fn` when its source names it as a call.
calls <- function(src, fn) grepl(paste0("(^|[^A-Za-z0-9._])", fn, "\\s*\\("), src)

# The domain is a UNION of two things, because either can cut what CRAN sees:
# a block this branch newly SKIPS on CRAN, and a block this branch CHANGED at
# all (a block made cheaper rather than skipped covers less than it did).
newly_skipped <- now[now$skips_cran, ]
was_key <- paste(was$file, was$test, sep = "\r")
was_skipped <- was_key[was$skips_cran]
newly_skipped <- newly_skipped[
  !paste(newly_skipped$file, newly_skipped$test, sep = "\r") %in% was_skipped, ]

was_src <- setNames(was$src, was_key)
now_key <- paste(now$file, now$test, sep = "\r")
changed <- now[!(now_key %in% names(was_src)) |
                 now$src != was_src[now_key], ]
touched <- unique(rbind(newly_skipped, changed))
if (!nrow(newly_skipped)) {
  stop("no block is newly CRAN-skipped against ", base,
       " -- refusing to pass vacuously.", call. = FALSE)
}
live_now <- now[!now$skips_cran, ]

lost <- character(0); kept <- list(); orphan <- character(0)
for (fn in exports) {
  if (!any(calls(touched$src, fn))) next                # coverage not reduced
  hit <- which(calls(live_now$src, fn))
  if (!length(hit)) {
    lost <- c(lost, fn)
  } else {
    kept[[fn]] <- live_now[hit[[1]], c("file", "test")]
  }
}
for (fn in exports) {
  if (!any(calls(now$src, fn))) orphan <- c(orphan, fn)
}

domain <- sort(unique(c(lost, names(kept))))
if (!length(domain)) {
  stop("the domain is EMPTY -- no exported function is named by any block this\n",
       "branch skipped or changed. That is not a pass; it means the domain\n",
       "computation found nothing to check.", call. = FALSE)
}
writeLines(domain, "tools/m120-domain.txt")
cat("newly CRAN-skipped blocks:", nrow(newly_skipped),
    "| blocks this branch touched:", nrow(touched),
    "| blocks still live on CRAN:", nrow(live_now), "\n")
cat("domain written to tools/m120-domain.txt (", length(domain), "functions )\n")
cat("exported functions whose CRAN coverage this branch reduces:",
    length(lost) + length(kept), "\n\n")
for (fn in names(kept)) {
  cat(sprintf("  %-22s still live in  %-34s %s\n", fn,
              kept[[fn]]$file, kept[[fn]]$test))
}
if (length(orphan)) {
  cat("\nexports no test block calls at all (not this milestone's doing):\n  ",
      paste(orphan, collapse = ", "), "\n")
}
if (length(lost)) {
  stop("\n", length(lost),
       " exported function(s) lose ALL CRAN-mode coverage to this branch:\n  - ",
       paste(lost, collapse = "\n  - "), call. = FALSE)
}
cat("\nno exported function loses all of its CRAN-mode coverage\n")
