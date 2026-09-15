#!/usr/bin/env Rscript

# Echo sweep for the vignettes. It reads the code blocks a vignette reader sees
# and reports code that simulates data, builds or formats a display table, or
# checks internals. Such code belongs in a hidden chunk, with prose saying what
# it does.
#
#   Rscript tools/vignette-echo-sweep.R [<file>...]                # search
#   Rscript tools/vignette-echo-sweep.R --taught [<file>...]       # taught calls
#   Rscript tools/vignette-echo-sweep.R --hidden-names [<file>...] # hidden names used later
#   Rscript tools/vignette-echo-sweep.R --other [<file>...]        # other shown calls
#   Rscript tools/vignette-echo-sweep.R --self-test
#
# With no file, the sources are every vignettes/*.Rmd.orig and every
# vignettes/*.Rmd with no .Rmd.orig of the same name.
#
# An echoed chunk is a fenced code block (backticks or tildes, any info string)
# whose header has none of `echo = FALSE`, `echo = F`, `include = FALSE`, and
# whose body has no `#| echo: false` line, in any spacing. `eval = FALSE`
# chunks and plain `r` fences are echoed.
#
# Search mode prints `<file>:<line>: <text>` for each echoed line that matches
# SEARCH, and exits 1 when it prints a line.
#
# --taught lists each echoed R statement whose call, or the call on the right
# of its assignment, is a function circumplex exports, a `glmmTMB::` function,
# `brm()` or `set_prior()`. A statement is a top-level expression or an
# expression inside a `{` block or a function body. Each line reads
# `<file>:<line>: <top|nested> <function>(<argument names>)`, where a nested
# statement sits inside another statement, and `_` stands for an unnamed
# argument. The package must be installed.
#
# --hidden-names prints `<file>:<line>: <name> (assigned in hidden chunk at
# line <n>)` for each name that a hidden chunk assigns and a later echoed chunk
# uses (`all.names()` of its parsed code), at that name's first later use.
#
# --other prints each echoed R statement that calls no function from circumplex,
# glmmTMB, brms or ggplot2 and does more than ROUTINE calls, so a reader can
# judge whether it is plumbing. Those packages must be installed.
#
# --self-test runs the search on the fixtures in tools/vignette-echo-sweep-fixtures/
# and compares the output with expected-search.txt there. It exits 0 on a match.
#
# Base R only, apart from reading the installed packages' exports.

SEARCH <- paste0(
  "lapply\\(|sapply\\(|vapply\\(|mapply\\(|Map\\(|do\\.call\\(|Filter\\(|",
  "Reduce\\(|t?apply\\(|stopifnot\\(|stop\\(|identical\\(|all\\.equal\\(|kable|",
  "(^|[^A-Za-z0-9_.])r(norm|unif|binom|pois|exp|gamma|beta|chisq|t)\\(|",
  "sample\\(|mvrnorm|function ?\\(|\\\\\\(|for ?\\(|rbind\\(|reshape\\(|",
  "subset\\(|data\\.frame\\(|rownames\\(|readRDS\\("
)
FENCE_OPEN <- "^[[:blank:]]*(`{3,}|~{3,})"
HIDDEN_HEADER <- "echo[[:blank:]]*=[[:blank:]]*(FALSE|F)([^A-Za-z0-9_.]|$)|include[[:blank:]]*=[[:blank:]]*FALSE([^A-Za-z0-9_.]|$)"
HIDDEN_PIPE <- "^[[:blank:]]*#\\|[[:blank:]]*echo[[:blank:]]*:[[:blank:]]*false([^A-Za-z0-9_.]|$)"
R_HEADER <- "^[[:blank:]]*(`{3,}|~{3,})[[:blank:]]*(\\{[[:blank:]]*r([[:blank:],}]|$)|r[[:blank:]]*$)"
FIXTURES <- "tools/vignette-echo-sweep-fixtures"
# Calls --other does not list: attaching or loading data, seeding, and the
# generics that dispatch to circumplex methods.
ROUTINE <- c("library", "require", "data", "set.seed", "summary", "print", "plot")

default_sources <- function(dir = "vignettes") {
  orig <- Sys.glob(file.path(dir, "*.Rmd.orig"))
  rmd <- Sys.glob(file.path(dir, "*.Rmd"))
  rmd <- rmd[!paste0(rmd, ".orig") %in% orig]
  sort(c(orig, rmd))
}

# Splits a page into fenced blocks. Each block records its header line, the
# line numbers of its body, whether it is hidden and whether it is an R chunk.
read_chunks <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  chunks <- list()
  i <- 1L
  while (i <= length(lines)) {
    m <- regmatches(lines[[i]], regexec(FENCE_OPEN, lines[[i]]))[[1]]
    if (length(m) == 0L) {
      i <- i + 1L
      next
    }
    fence <- m[[2]]
    close_re <- paste0("^[[:blank:]]*", substr(fence, 1, 1), "{", nchar(fence), ",}[[:blank:]]*$")
    j <- i + 1L
    while (j <= length(lines) && !grepl(close_re, lines[[j]])) j <- j + 1L
    body <- if (j > i + 1L) seq(i + 1L, j - 1L) else integer(0)
    header <- lines[[i]]
    hidden <- grepl(HIDDEN_HEADER, header) || any(grepl(HIDDEN_PIPE, lines[body]))
    chunks[[length(chunks) + 1L]] <- list(
      header = header, start = i, body = body, text = lines[body],
      hidden = hidden, is_r = grepl(R_HEADER, header)
    )
    i <- j + 1L
  }
  chunks
}

search_file <- function(path, label = path) {
  out <- character(0)
  for (ch in read_chunks(path)) {
    if (ch$hidden) next
    hit <- grepl(SEARCH, ch$text)
    out <- c(out, sprintf("%s:%d: %s", label, ch$body[hit], ch$text[hit]))
  }
  out
}

# Parses an R chunk; drops `#|` option lines, which are comments to R.
parse_chunk <- function(ch) {
  tryCatch(
    parse(text = ch$text, keep.source = TRUE),
    error = function(e) {
      warning("could not parse the chunk at line ", ch$start, ": ",
              conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

call_name <- function(e) {
  if (!is.call(e)) return(NA_character_)
  f <- e[[1]]
  if (is.symbol(f)) return(as.character(f))
  if (is.call(f) && as.character(f[[1]]) %in% c("::", ":::")) {
    return(paste0(as.character(f[[2]]), "::", as.character(f[[3]])))
  }
  NA_character_
}

strip_assign <- function(e) {
  while (is.call(e) && call_name(e) %in% c("<-", "=") && length(e) == 3L) {
    e <- e[[3]]
  }
  e
}

# Walks the statements of an expression: the expression itself, each
# expression in a `{` block, and each function body, at any depth. `line` is
# the statement's first line in the chunk text; a `{` block carries a srcref
# for each of its statements, and any other statement inherits its parent's.
walk_statements <- function(e, nested, visit, line) {
  visit(e, nested, line)
  inner <- function(x) {
    if (!is.call(x)) return(invisible())
    nm <- call_name(x)
    if (identical(nm, "{")) {
      refs <- attr(x, "srcref")
      for (k in seq_along(x)[-1]) {
        at <- if (length(refs) >= k) refs[[k]][[1]] else line
        walk_statements(x[[k]], TRUE, visit, at)
      }
      return(invisible())
    }
    if (identical(nm, "function")) {
      walk_statements(x[[3]], TRUE, visit, line)
      return(invisible())
    }
    for (k in seq_along(x)[-1]) {
      if (!missing_arg(x, k)) inner(x[[k]])
    }
  }
  inner(e)
}

missing_arg <- function(x, k) {
  identical(x[[k]], quote(expr = ))
}

is_taught <- function(nm, exports) {
  if (is.na(nm)) return(FALSE)
  if (startsWith(nm, "glmmTMB::")) return(TRUE)
  if (startsWith(nm, "circumplex::")) nm <- sub("^circumplex::", "", nm)
  nm %in% c(exports, "brm", "set_prior", "brms::brm", "brms::set_prior")
}

taught_file <- function(path, exports, label = path) {
  out <- character(0)
  for (ch in read_chunks(path)) {
    if (ch$hidden || !ch$is_r) next
    exprs <- parse_chunk(ch)
    if (is.null(exprs)) next
    srcrefs <- attr(exprs, "srcref")
    for (k in seq_along(exprs)) {
      walk_statements(exprs[[k]], FALSE, function(stmt, nested, at) {
        rhs <- strip_assign(stmt)
        nm <- call_name(rhs)
        if (!is_taught(nm, exports)) return(invisible())
        args <- names(as.list(rhs)[-1])
        if (is.null(args)) args <- rep("", length(rhs) - 1L)
        args[args == ""] <- "_"
        out <<- c(out, sprintf("%s:%d: %s %s(%s)", label, ch$body[[1]] + at - 1L,
                               if (nested) "nested" else "top", nm,
                               paste(args, collapse = ", ")))
      }, srcrefs[[k]][[1]])
    }
  }
  out
}

assigned_names <- function(exprs) {
  found <- character(0)
  visit <- function(e) {
    if (!is.call(e)) return(invisible())
    nm <- call_name(e)
    if (nm %in% c("<-", "=", "<<-") && length(e) == 3L) {
      target <- e[[2]]
      while (is.call(target)) target <- target[[2]]
      if (is.symbol(target)) found <<- c(found, as.character(target))
    }
    if (identical(nm, "function")) return(invisible())
    for (k in seq_along(e)[-1]) if (!missing_arg(e, k)) visit(e[[k]])
  }
  for (e in exprs) visit(e)
  unique(found)
}

hidden_names_file <- function(path, label = path) {
  out <- character(0)
  defined <- list()
  reported <- character(0)
  for (ch in read_chunks(path)) {
    if (!ch$is_r) next
    exprs <- parse_chunk(ch)
    if (is.null(exprs)) next
    if (ch$hidden) {
      for (nm in assigned_names(exprs)) if (is.null(defined[[nm]])) defined[[nm]] <- ch$start
      next
    }
    srcrefs <- attr(exprs, "srcref")
    for (k in seq_along(exprs)) {
      e <- exprs[[k]]
      # `x <- rhs` reads the names in rhs; the target itself is not a use.
      read <- if (is.call(e) && call_name(e) %in% c("<-", "=") && length(e) == 3L && is.symbol(e[[2]])) {
        all.names(e[[3]])
      } else {
        all.names(e)
      }
      used <- intersect(read, names(defined))
      used <- setdiff(used, reported)
      line <- ch$body[[1]] + srcrefs[[k]][[1]] - 1L
      for (nm in used) {
        out <- c(out, sprintf("%s:%d: %s (assigned in hidden chunk at line %d)",
                              label, line, nm, defined[[nm]]))
      }
      reported <- c(reported, used)
      # A name the reader sees assigned in echoed code is no longer hidden.
      for (nm in assigned_names(list(e))) defined[[nm]] <- NULL
    }
  }
  out
}

other_file <- function(path, known, label = path) {
  out <- character(0)
  for (ch in read_chunks(path)) {
    if (ch$hidden || !ch$is_r) next
    exprs <- parse_chunk(ch)
    if (is.null(exprs)) next
    srcrefs <- attr(exprs, "srcref")
    for (k in seq_along(exprs)) {
      calls <- unique(unlist(lapply(all_calls(exprs[[k]]), call_name)))
      calls <- calls[!is.na(calls)]
      pkgd <- sub("^(circumplex|glmmTMB|brms|ggplot2)::.*$", "PKG", calls)
      if (any(pkgd == "PKG") || any(calls %in% known)) next
      if (length(calls) == 0L || all(calls %in% ROUTINE)) next
      first <- ch$body[[1]] + srcrefs[[k]][[1]] - 1L
      last <- ch$body[[1]] + srcrefs[[k]][[3]] - 1L
      out <- c(out, sprintf("%s:%d: %s", label, first,
                            paste(trimws(readLines(path, warn = FALSE)[first:last]), collapse = " ")))
    }
  }
  out
}

all_calls <- function(e) {
  if (!is.call(e)) return(list())
  c(list(e), unlist(lapply(as.list(e)[-1], function(x) {
    if (identical(x, quote(expr = ))) list() else all_calls(x)
  }), recursive = FALSE))
}

# Reads a saved --taught list and checks it against the current sources. Each
# baseline statement needs an echoed statement in the same source that calls
# the same function with the same argument names, one current statement per
# baseline statement, unless the exceptions file lists its `<file>:<line>`
# with a reason. Only a nested statement may be an exception. Prints each
# unmatched statement and each bad exception.
compare_taught <- function(baseline, exceptions, exports) {
  parse_list <- function(lines) {
    m <- regmatches(lines, regexec("^(.*):([0-9]+): (top|nested) (.*)$", lines))
    do.call(rbind, lapply(m, function(x) data.frame(
      file = x[[2]], line = x[[3]], level = x[[4]], call = x[[5]]
    )))
  }
  base <- parse_list(readLines(baseline, warn = FALSE))
  ex_lines <- grep("^[^#[:space:]]", readLines(exceptions, warn = FALSE), value = TRUE)
  ex <- regmatches(ex_lines, regexec("^(.*:[0-9]+): (.+)$", ex_lines))
  ex_keys <- vapply(ex, function(x) if (length(x)) x[[2]] else NA_character_, character(1))
  files <- unique(base$file)
  head <- parse_list(unlist(lapply(files[file.exists(files)], taught_file, exports = exports)))
  if (is.null(head)) head <- data.frame(file = character(0), call = character(0))
  pool <- paste(head$file, head$call)
  out <- character(0)
  for (i in seq_len(nrow(base))) {
    key <- paste0(base$file[[i]], ":", base$line[[i]])
    if (key %in% ex_keys) {
      if (base$level[[i]] != "nested") out <- c(out, paste("exception on a top-level statement:", key))
      next
    }
    hit <- match(paste(base$file[[i]], base$call[[i]]), pool)
    if (is.na(hit)) {
      out <- c(out, sprintf("not echoed at head: %s %s", key, base$call[[i]]))
    } else {
      pool[[hit]] <- NA_character_
    }
  }
  stale <- setdiff(ex_keys[!is.na(ex_keys)], paste0(base$file, ":", base$line))
  c(out, if (length(stale)) paste("exception names no baseline statement:", stale))
}

self_test <- function() {
  sources <- default_sources(FIXTURES)
  if (length(sources) == 0L) {
    message("vignette-echo-sweep: no fixture sources in ", FIXTURES)
    quit(status = 2L)
  }
  got <- unlist(lapply(sources, function(p) search_file(p, basename(p))))
  want <- readLines(file.path(FIXTURES, "expected-search.txt"), encoding = "UTF-8")
  if (identical(got, want)) {
    cat("self-test passed:", length(got), "expected lines from", length(sources), "sources\n")
    quit(status = 0L)
  }
  cat("self-test FAILED\n--- got\n", paste(got, collapse = "\n"),
      "\n--- expected\n", paste(want, collapse = "\n"), "\n", sep = "")
  quit(status = 1L)
}

if (identical(environment(), globalenv()) && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- "search"
  if (length(args) && startsWith(args[[1]], "--")) {
    mode <- sub("^--", "", args[[1]])
    args <- args[-1]
  }
  if (mode == "self-test") self_test()
  if (mode == "compare-taught") {
    if (length(args) != 2L) {
      message("usage: vignette-echo-sweep.R --compare-taught <baseline> <exceptions>")
      quit(status = 3L)
    }
    out <- compare_taught(args[[1]], args[[2]], getNamespaceExports("circumplex"))
    if (length(out)) writeLines(out)
    quit(status = if (length(out)) 1L else 0L)
  }
  files <- if (length(args)) args else default_sources()
  if (length(files) == 0L) {
    message("vignette-echo-sweep: no sources found")
    quit(status = 2L)
  }
  out <- switch(
    mode,
    search = unlist(lapply(files, search_file)),
    taught = {
      ex <- getNamespaceExports("circumplex")
      unlist(lapply(files, taught_file, exports = ex))
    },
    `hidden-names` = unlist(lapply(files, hidden_names_file)),
    other = {
      known <- unique(unlist(lapply(c("circumplex", "glmmTMB", "brms", "ggplot2"), function(p) {
        if (requireNamespace(p, quietly = TRUE)) getNamespaceExports(p) else character(0)
      })))
      unlist(lapply(files, other_file, known = known))
    },
    {
      message("vignette-echo-sweep: unknown mode --", mode)
      quit(status = 3L)
    }
  )
  if (length(out)) writeLines(out)
  quit(status = if (mode == "search" && length(out)) 1L else 0L)
}
