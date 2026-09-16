#!/usr/bin/env Rscript

# Guard for a vignette split: every sentence and every chunk of the page that
# was split must be in exactly one of the pages that replace it.
#
#   Rscript tools/check-vignette-split.R [--pairs <md> [--max-pairs <n>]] <base> <page>...
#
# <base> is the source that was split, as a path or as <git-ref>:<path>
# (`master:vignettes/x.Rmd.orig`). Each <page> is a path. Each is read the way
# tools/prose-sweep.R reads it, so "sentence" and "chunk" mean what that
# script's `--prose` and `--chunks` modes print.
#
# Sentences. The Level line and the body of `## 1. Overview` of every page are
# left out on both sides, because each page composes its own and the frame
# test checks what they name. Heading lines are left out too. What remains,
# the Wrap-up included, is compared as a multiset: a sentence the base holds
# twice must be held twice across the pages. A `--pairs` file lists the
# sentences the split changed, one per line, anywhere in the file:
#
#   - Reworded: <old text> => <new text>
#   - Added: <new text>
#
# Each text is read as prose the same way, so a listed text may hold a code
# span or split into two sentences. A reworded pair moves its old sentences
# out of the base residual and its new sentences out of the pages' residual;
# an added line moves its sentences out of the pages' residual only. A pair
# whose old text is not in the base residual, or whose new text is not in the
# pages' residual, is reported. More listed lines than `--max-pairs` allows
# is a failure.
#
# Chunks. Every fenced block of the base must appear once across the pages,
# byte for byte, header line included. The two preamble chunks that every
# page carries, the unlabelled `include = FALSE` options chunk and the chunk
# labelled `setup`, are left out on both sides. A block only a page holds is
# listed as added, which is not a failure. Every `precompute:volatile-numbers`
# start marker of the base must be in the page that holds the labelled chunk
# after it, still directly before that chunk, and that page must close it.
#
# Word counts. The prose word count of each page, the number `--prose | wc -w`
# gives, is printed for the page-length check.
#
# Exit status: 0 when nothing is dropped, unmatched or moved; 1 otherwise;
# 3 on a usage error. Base R only.

PREAMBLE_LABELS <- c("", "setup")
MAX_WORDS_NOTE <- 2600L

usage <- function(msg) {
  message("check-vignette-split: ", msg)
  message("usage: Rscript tools/check-vignette-split.R [--pairs <md> [--max-pairs <n>]] <base> <page>...")
  quit(status = 3L)
}

# The sweep is the definition of "sentence" and "chunk"; sourcing it gives its
# functions without running its command line (it guards on sys.nframe()).
sweep <- new.env()
sys.source("tools/prose-sweep.R", envir = sweep)

read_lines_at <- function(spec) {
  if (file.exists(spec)) {
    return(readLines(spec, warn = FALSE, encoding = "UTF-8"))
  }
  if (grepl(":", spec, fixed = TRUE)) {
    txt <- suppressWarnings(system2("git", c("show", spec), stdout = TRUE, stderr = FALSE))
    st <- attr(txt, "status")
    if (is.null(st) || st == 0L) {
      Encoding(txt) <- "UTF-8"
      return(txt)
    }
  }
  usage(paste0("cannot read ", spec))
}

# Blanks the Level line, the Overview body and the headings, keeping line
# numbers.
strip_frame <- function(lines) {
  out <- lines
  is_h2 <- grepl("^## ", lines)
  blank_section <- function(start_re) {
    start <- which(is_h2 & grepl(start_re, lines))
    for (s in start) {
      later <- which(is_h2 & seq_along(lines) > s)
      end <- if (length(later)) later[[1]] - 1L else length(lines)
      out[seq(s, end)] <<- ""
    }
  }
  blank_section("^## 1\\. Overview")
  out[grepl("^\\*\\*Level:\\*\\*", lines)] <- ""
  out[grepl("^\\s{0,3}#{1,6}\\s", lines)] <- ""
  out
}

sentences_of <- function(lines) {
  page <- sweep$split_page(lines)
  units <- sweep$prose_units(page$prose)
  ss <- unlist(lapply(units, sweep$unit_sentences), recursive = FALSE)
  vapply(ss, function(s) paste(s$words, collapse = " "), character(1))
}

word_count <- function(lines) {
  s <- sentences_of(lines)
  sum(lengths(strsplit(s, "[[:space:]]+")))
}

chunks_of <- function(lines) {
  page <- sweep$split_page(lines)
  vapply(page$blocks, function(b) paste(b, collapse = "\n"), character(1))
}

chunk_label <- function(block) {
  header <- sub("\n.*$", "", block)
  if (!grepl("^\\s{0,3}```\\{r", header)) return(NA_character_)
  trimws(sub("^\\s{0,3}```\\{r[ ,]*", "", sub("[,}].*$", "", header)))
}

is_preamble <- function(block) {
  lab <- chunk_label(block)
  !is.na(lab) && lab %in% PREAMBLE_LABELS
}

# Removes each element of `what` once from `from`; returns what was not there.
take <- function(from, what) {
  missing <- character(0)
  for (w in what) {
    i <- match(w, from)
    if (is.na(i)) missing <- c(missing, w) else from <- from[-i]
  }
  list(rest = from, missing = missing)
}

read_pairs <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  rew <- sub("^\\s*- Reworded:\\s*", "", lines[grepl("^\\s*- Reworded:", lines)])
  add <- sub("^\\s*- Added:\\s*", "", lines[grepl("^\\s*- Added:", lines)])
  bad <- rew[!grepl(" => ", rew, fixed = TRUE)]
  if (length(bad)) usage(paste0("a Reworded line has no ` => `: ", bad[[1]]))
  list(
    old = vapply(rew, function(x) sub(" => .*$", "", x), character(1), USE.NAMES = FALSE),
    new = c(vapply(rew, function(x) sub("^.* => ", "", x), character(1), USE.NAMES = FALSE), add),
    n_reworded = length(rew), n_added = length(add)
  )
}

args <- commandArgs(trailingOnly = TRUE)
pairs_path <- NULL
max_pairs <- Inf
files <- character(0)
i <- 1L
while (i <= length(args)) {
  if (args[[i]] == "--pairs") {
    if (i == length(args)) usage("--pairs needs a file")
    pairs_path <- args[[i + 1L]]
    i <- i + 2L
  } else if (args[[i]] == "--max-pairs") {
    if (i == length(args)) usage("--max-pairs needs a number")
    max_pairs <- suppressWarnings(as.integer(args[[i + 1L]]))
    if (is.na(max_pairs)) usage("--max-pairs needs a number")
    i <- i + 2L
  } else if (grepl("^--", args[[i]])) {
    usage(paste0("unknown flag ", args[[i]]))
  } else {
    files <- c(files, args[[i]])
    i <- i + 1L
  }
}
if (length(files) < 2L) usage("give the base and at least one page")
base_spec <- files[[1]]
pages <- files[-1]

base <- read_lines_at(base_spec)
page_lines <- lapply(pages, read_lines_at)
names(page_lines) <- pages

failures <- character(0)
fail <- function(...) failures <<- c(failures, paste0(...))

# --- sentences ---------------------------------------------------------------

base_s <- sentences_of(strip_frame(base))
page_s <- lapply(page_lines, function(l) sentences_of(strip_frame(l)))
union_s <- unlist(page_s, use.names = FALSE)
if (!length(base_s)) usage("the base has no sentences outside its frame")
if (!length(union_s)) usage("the pages have no sentences outside their frames")

common <- take(base_s, union_s)
old_residual <- common$rest        # in the base, not in the pages
new_residual <- common$missing     # in the pages, not in the base

n_pairs <- 0L
if (!is.null(pairs_path)) {
  pr <- read_pairs(pairs_path)
  n_pairs <- pr$n_reworded + pr$n_added
  if (n_pairs > max_pairs) fail(n_pairs, " listed lines, more than the ", max_pairs, " allowed")
  for (o in pr$old) {
    s <- sentences_of(o)
    t <- take(old_residual, s)
    if (length(t$missing)) fail("Reworded old text is not in the base residual: ", t$missing[[1]])
    old_residual <- t$rest
  }
  for (n in pr$new) {
    s <- sentences_of(n)
    t <- take(new_residual, s)
    if (length(t$missing)) fail("listed new text is not in the pages' residual: ", t$missing[[1]])
    new_residual <- t$rest
  }
}
for (s in old_residual) fail("dropped sentence: ", s)
for (s in new_residual) fail("unlisted new sentence: ", s)

# --- chunks ------------------------------------------------------------------

base_c <- Filter(Negate(is_preamble), chunks_of(base))
page_c <- lapply(page_lines, function(l) Filter(Negate(is_preamble), chunks_of(l)))
union_c <- unlist(page_c, use.names = FALSE)
if (!length(base_c)) usage("the base has no chunks outside its preamble")
tc <- take(union_c, base_c)
for (b in tc$missing) fail("base chunk not in any page byte for byte: ", sub("\n.*$", "", b))
added_chunks <- tc$rest
for (b in base_c) {
  n <- sum(union_c == b)
  if (n > 1L) fail("base chunk in ", n, " pages: ", sub("\n.*$", "", b))
}
for (p in pages) {
  labs <- vapply(page_c[[p]], chunk_label, character(1))
  labs <- labs[!is.na(labs)]
  dup <- unique(labs[duplicated(labs)])
  if (length(dup)) fail(p, " repeats chunk label(s): ", paste(dup, collapse = ", "))
}

# --- volatile-number markers -------------------------------------------------

START <- "<!--\\s*precompute:volatile-numbers start\\b"
END <- "<!--\\s*precompute:volatile-numbers end\\s*-->"

# For each start marker: the labels of the chunks between it and its end
# marker. A region that another start marker or the end of the file
# interrupts is reported as unclosed.
marker_regions <- function(lines) {
  opens <- grep("^\\s{0,3}```\\{r", lines)
  labels <- vapply(lines[opens], function(h) chunk_label(h), character(1))
  starts <- grep(START, lines)
  ends <- grep(END, lines)
  lapply(starts, function(s) {
    e <- ends[ends > s]
    next_start <- starts[starts > s]
    closed <- length(e) > 0L && (!length(next_start) || e[[1]] < next_start[[1]])
    if (!closed) return(list(line = s, labels = character(0), closed = FALSE))
    inside <- opens[opens > s & opens < e[[1]]]
    list(line = s, labels = labels[match(inside, opens)], closed = TRUE)
  })
}

base_m <- marker_regions(base)
page_m <- lapply(page_lines, marker_regions)
for (m in base_m) {
  if (!m$closed || !length(m$labels)) {
    fail("base start marker at line ", m$line, " is unclosed or holds no chunk")
    next
  }
  first <- m$labels[[1]]
  holder <- names(page_c)[vapply(page_c, function(cs) first %in% vapply(cs, chunk_label, character(1)), logical(1))]
  if (length(holder) != 1L) {
    fail("chunk `", first, "` (marked in the base) is in ", length(holder), " pages")
    next
  }
  hit <- Filter(function(x) x$closed && identical(x$labels, m$labels), page_m[[holder]])
  if (!length(hit)) {
    fail(holder, " has no closed marked region holding exactly chunk(s) ",
         paste0("`", m$labels, "`", collapse = ", "))
  }
}

# --- report ------------------------------------------------------------------

for (p in pages) {
  n <- word_count(page_lines[[p]])
  cat(sprintf("%-48s %5d prose words%s\n", p, n,
              if (n > MAX_WORDS_NOTE) paste0(" (over ", MAX_WORDS_NOTE, ")") else ""))
}
cat(length(base_s), "base sentences outside the frame;", length(union_s), "across the pages;",
    n_pairs, "listed pairs\n")
cat(length(base_c), "base chunks outside the preamble;", length(added_chunks), "added in the pages",
    if (length(added_chunks)) paste0(" (", paste(vapply(added_chunks, function(b) sub("\n.*$", "", b), character(1)), collapse = "; "), ")") else "",
    "\n", sep = " ")
cat(length(base_m), "marked regions in the base\n")

if (length(failures)) {
  cat(paste0("FAIL: ", failures, "\n"), sep = "")
  quit(status = 1L)
}
cat("every sentence and chunk of the base is in exactly one page\n")
