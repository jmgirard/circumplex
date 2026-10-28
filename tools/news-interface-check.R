#!/usr/bin/env Rscript

# Check that the development section of NEWS.md names every interface change
# since a released git ref.
#
# Usage: Rscript tools/news-interface-check.R <base-ref> [news-file]
#   e.g. Rscript tools/news-interface-check.R v2.0.1
#
# Run from the repository root. The base ref is read with `git show` and
# `git ls-tree`; the current state is the working tree. `news-file` defaults to
# NEWS.md and exists so the check can be run against a scratch copy.
#
# Five sets are compared between the base ref and the working tree:
#   1. the export() and S3method() lines of NAMESPACE;
#   2. the argument names and default expressions of each function exported at
#      both refs and each S3 method registered at both refs, read with parse()
#      from top-level `name <- function(...)` or `name = function(...)`
#      assignments in R/*.R (a function with no such assignment at one ref or
#      both is skipped);
#   3. the file names in data/, without .rda;
#   4. the VignetteIndexEntry titles of vignettes/*.Rmd;
#   5. the package names and version floors in DESCRIPTION's Depends, Imports
#      and Suggests.
#
# The development section is the lines of the news file before its second
# top-level (`# `) heading. A bullet is a line of that section starting with
# `* `, plus its next lines up to the next bullet or heading. A backtick span
# names X if, after a leading `?` is dropped, its text is X, or begins with
# `X(`, or begins with X, optional spaces and `=`.
#
# Match rules:
#   - export, dataset, added or removed package: one bullet names it;
#   - argument difference: one bullet names the function (for an S3 method,
#     its generic) and the argument;
#   - added or removed S3 method: one bullet names the generic and an export
#     whose man/*.Rd file has the method's \alias{} (man/ read at the working
#     tree for an added method, at the base ref for a removed one). This checks
#     that the bullet names both, which may be one name when the generic is
#     itself that export, not that the bullet describes the method;
#   - version floor change: one bullet names the package and holds the new
#     floor as a whole token, with no digit or `.` on either side (a floor
#     removed outright is always reported missing);
#   - vignette: the section holds the title in straight double quotes, after
#     line breaks and runs of spaces become one space.
# One exemption: an export whose name starts with `Geom`, documented in
# man/circumplex-ggproto.Rd, whose geom_*() constructor is matched.
#
# Prints one line per difference (matched, exempt or MISSING) and exits 1 if
# any difference is missing. Not part of the built package (`^tools$` is in
# .Rbuildignore). Base R only.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("usage: Rscript tools/news-interface-check.R <base-ref> [news-file]",
       call. = FALSE)
}
base <- args[[1L]]
news_file <- if (length(args) == 2L) args[[2L]] else "NEWS.md"

# ---- reading the two refs ---------------------------------------------------

git_lines <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("git ", paste(c(...), collapse = " "), " failed", call. = FALSE)
  }
  Encoding(out) <- "UTF-8"
  sub("\r$", "", out) # some R/ files have CRLF line endings
}

base_ls <- function(dir) {
  sub(paste0("^", dir, "/"), "",
      git_lines("ls-tree", "--name-only", base, paste0(dir, "/")))
}
base_read <- function(path) git_lines("show", paste0(base, ":", path))

refs <- list(
  base = list(
    ls = base_ls,
    read = base_read
  ),
  head = list(
    ls = function(dir) list.files(dir),
    read = function(path) readLines(path, warn = FALSE, encoding = "UTF-8")
  )
)

# ---- set extractors ---------------------------------------------------------

read_namespace <- function(ref) {
  ns <- ref$read("NAMESPACE")
  # roxygen quotes non-syntactic names: export("%>%"), S3method("[",foo)
  unquote <- function(x) gsub("^\\s*\"|\"\\s*$", "", x)
  exports <- unquote(sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE)))
  s3 <- grep("^S3method\\(", ns, value = TRUE)
  generic <- unquote(sub("^S3method\\(([^,]+),([^)]+)\\)$", "\\1", s3))
  class <- unquote(sub("^S3method\\(([^,]+),([^)]+)\\)$", "\\2", s3))
  # paste0() of two empty vectors gives ".", so build `method` only when
  # there are methods
  methods <- data.frame(
    generic = generic, class = class,
    method = if (length(generic) > 0L) paste0(generic, ".", class) else character(),
    stringsAsFactors = FALSE
  )
  list(exports = exports, methods = methods)
}

read_formals <- function(ref) {
  files <- grep("\\.[Rr]$", ref$ls("R"), value = TRUE)
  out <- list()
  for (f in files) {
    exprs <- parse(text = ref$read(file.path("R", f)), keep.source = FALSE)
    for (e in exprs) {
      # is.name() first: a call such as utils::globalVariables() has a call
      # as its head, which as.character() turns into a length-3 vector
      if (is.call(e) && is.name(e[[1L]]) &&
          as.character(e[[1L]]) %in% c("<-", "=") &&
          is.name(e[[2L]]) && is.call(e[[3L]]) &&
          identical(e[[3L]][[1L]], as.name("function"))) {
        fm <- as.list(e[[3L]][[2L]])
        out[[as.character(e[[2L]])]] <- vapply(
          fm, function(d) paste(deparse(d), collapse = " "), character(1L)
        )
      }
    }
  }
  out
}

read_vignettes <- function(ref) {
  files <- grep("\\.Rmd$", ref$ls("vignettes"), value = TRUE)
  titles <- vapply(files, function(f) {
    entry <- grep("VignetteIndexEntry", ref$read(file.path("vignettes", f)),
                  value = TRUE)
    if (length(entry) == 0L) NA_character_
    else sub(".*VignetteIndexEntry\\{(.*)\\}.*", "\\1", entry[[1L]])
  }, character(1L))
  unname(titles[!is.na(titles)])
}

read_deps <- function(ref) {
  dcf <- read.dcf(textConnection(ref$read("DESCRIPTION")))
  fields <- intersect(c("Depends", "Imports", "Suggests"), colnames(dcf))
  # One entry per field and package, so a move between fields is a difference
  out <- character()
  for (field in fields) {
    entries <- trimws(unlist(strsplit(dcf[1L, field], ",")))
    entries <- entries[nzchar(entries)]
    pkg <- trimws(sub("\\(.*$", "", entries))
    floor <- ifelse(grepl("\\(", entries),
                    trimws(sub("^[^(]*\\(\\s*>=?\\s*([^)]*)\\).*$", "\\1", entries)),
                    NA_character_)
    out <- c(out, stats::setNames(floor, paste(field, pkg)))
  }
  out
}

aliases_in <- function(ref, rd) {
  lines <- ref$read(file.path("man", rd))
  hits <- regmatches(lines, gregexpr("\\\\alias\\{[^}]*\\}", lines))
  sub("^\\\\alias\\{(.*)\\}$", "\\1", unlist(hits))
}

rd_with_alias <- function(ref, alias) {
  rds <- grep("\\.Rd$", ref$ls("man"), value = TRUE)
  rds[vapply(rds, function(rd) alias %in% aliases_in(ref, rd), logical(1L))]
}

# ---- the news section -------------------------------------------------------

news <- readLines(news_file, warn = FALSE, encoding = "UTF-8")
tops <- grep("^# ", news)
if (length(tops) < 2L) stop(news_file, " has fewer than two `# ` headings", call. = FALSE)
section <- news[seq_len(tops[[2L]] - 1L)]

bullets <- character()
current <- NULL
for (line in section) {
  if (grepl("^\\* ", line)) {
    if (!is.null(current)) bullets <- c(bullets, current)
    current <- line
  } else if (grepl("^#", line)) {
    if (!is.null(current)) bullets <- c(bullets, current)
    current <- NULL
  } else if (!is.null(current)) {
    current <- paste(current, line)
  }
}
if (!is.null(current)) bullets <- c(bullets, current)
squash <- function(x) gsub("\\s+", " ", x)
bullets <- squash(bullets)
section_text <- squash(paste(section, collapse = " "))

spans <- lapply(bullets, function(b) {
  s <- regmatches(b, gregexpr("`[^`]+`", b))[[1L]]
  sub("^\\?", "", substr(s, 2L, nchar(s) - 1L))
})

span_names <- function(span, x) {
  span == x || startsWith(span, paste0(x, "(")) ||
    (startsWith(span, x) && grepl("^\\s*=", substr(span, nchar(x) + 1L, nchar(span))))
}
bullet_names <- function(i, x) any(vapply(spans[[i]], span_names, logical(1L), x = x))
named_together <- function(...) {
  wanted <- list(...)
  any(vapply(seq_along(bullets), function(i) {
    all(vapply(wanted, function(w) any(vapply(w, function(x) bullet_names(i, x), logical(1L))),
               logical(1L)))
  }, logical(1L)))
}
floor_in <- function(i, floor) {
  esc <- gsub("([.\\\\])", "\\\\\\1", floor)
  grepl(paste0("(?<![0-9.])", esc, "(?![0-9.])"), bullets[[i]], perl = TRUE)
}

# ---- compare ----------------------------------------------------------------

results <- data.frame(set = character(), item = character(), status = character(),
                      stringsAsFactors = FALSE)
report <- function(set, item, ok, exempt = FALSE) {
  status <- if (ok) "matched" else if (exempt) "exempt" else "MISSING"
  results[nrow(results) + 1L, ] <<- list(set, item, status)
}

ns <- lapply(refs, read_namespace)
fm <- lapply(refs, read_formals)

# Set 1: exports.
ggproto_aliases <- list(
  base = if ("circumplex-ggproto.Rd" %in% refs$base$ls("man")) aliases_in(refs$base, "circumplex-ggproto.Rd") else character(),
  head = if ("circumplex-ggproto.Rd" %in% refs$head$ls("man")) aliases_in(refs$head, "circumplex-ggproto.Rd") else character()
)
geom_constructor <- function(x) {
  paste0("geom_", tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", sub("^Geom", "", x))))
}
for (side in c("head", "base")) {
  other <- if (side == "head") "base" else "head"
  for (x in setdiff(ns[[side]]$exports, ns[[other]]$exports)) {
    label <- paste(if (side == "head") "added" else "removed", "export", x)
    ok <- named_together(x)
    exempt <- grepl("^Geom", x) && x %in% ggproto_aliases[[side]] &&
      named_together(geom_constructor(x))
    report(1L, label, ok, exempt)
  }
}

# Set 1: S3 methods.
for (side in c("head", "base")) {
  other <- if (side == "head") "base" else "head"
  m <- ns[[side]]$methods
  for (k in which(!m$method %in% ns[[other]]$methods$method)) {
    rds <- rd_with_alias(refs[[side]], m$method[[k]])
    exports <- intersect(unique(unlist(lapply(rds, function(rd) aliases_in(refs[[side]], rd)))),
                         ns[[side]]$exports)
    label <- paste(if (side == "head") "added" else "removed", "S3 method", m$method[[k]])
    report(1L, label, length(exports) > 0L && named_together(m$generic[[k]], exports))
  }
}

# Set 2: arguments of functions exported or registered at both refs.
both_exports <- intersect(ns$base$exports, ns$head$exports)
both_methods <- merge(ns$base$methods, ns$head$methods)
targets <- c(stats::setNames(both_exports, both_exports),
             stats::setNames(both_methods$generic, both_methods$method))
for (f in names(targets)) {
  a <- fm$base[[f]]
  b <- fm$head[[f]]
  if (is.null(a) || is.null(b)) next
  for (arg in union(names(a), names(b))) {
    change <- if (!arg %in% names(a)) {
      "added"
    } else if (!arg %in% names(b)) {
      "removed"
    } else if (!identical(a[[arg]], b[[arg]])) {
      "default changed"
    } else {
      next
    }
    label <- paste0("argument ", f, "(", arg, ") ", change)
    report(2L, label, named_together(targets[[f]], arg))
  }
}

# Set 3: datasets.
data_names <- lapply(refs, function(r) sub("\\.rda$", "", grep("\\.rda$", r$ls("data"), value = TRUE)))
for (x in setdiff(data_names$head, data_names$base)) report(3L, paste("added dataset", x), named_together(x))
for (x in setdiff(data_names$base, data_names$head)) report(3L, paste("removed dataset", x), named_together(x))

# Set 4: vignette titles.
titles <- lapply(refs, read_vignettes)
title_in <- function(x) grepl(paste0("\"", squash(x), "\""), section_text, fixed = TRUE)
for (x in setdiff(titles$head, titles$base)) report(4L, paste0("added vignette \"", x, "\""), title_in(x))
for (x in setdiff(titles$base, titles$head)) report(4L, paste0("removed vignette \"", x, "\""), title_in(x))

# Set 5: dependencies.
deps <- lapply(refs, read_deps)
pkg_of <- function(key) sub("^\\S+ ", "", key)
field_of <- function(key) sub(" .*$", "", key)
for (k in setdiff(names(deps$head), names(deps$base))) {
  report(5L, paste("added package", pkg_of(k), "to", field_of(k)), named_together(pkg_of(k)))
}
for (k in setdiff(names(deps$base), names(deps$head))) {
  report(5L, paste("removed package", pkg_of(k), "from", field_of(k)), named_together(pkg_of(k)))
}
for (k in intersect(names(deps$base), names(deps$head))) {
  old <- deps$base[[k]]
  new <- deps$head[[k]]
  if (identical(old, new)) next
  p <- pkg_of(k)
  label <- paste0("floor of ", p, " in ", field_of(k), ": ", old, " -> ", new)
  ok <- !is.na(new) && any(vapply(seq_along(bullets), function(i) {
    bullet_names(i, p) && floor_in(i, new)
  }, logical(1L)))
  report(5L, label, ok)
}

# ---- output -----------------------------------------------------------------

for (i in seq_len(nrow(results))) {
  cat(sprintf("set %s  %-8s %s\n", results$set[[i]], results$status[[i]], results$item[[i]]))
}
missing <- sum(results$status == "MISSING")
cat(sprintf("\n%d differences since %s: %d matched, %d exempt, %d missing\n",
            nrow(results), base, sum(results$status == "matched"),
            sum(results$status == "exempt"), missing))
if (missing > 0L) quit(status = 1L)
