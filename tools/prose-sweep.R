#!/usr/bin/env Rscript

# Prose sweep for the vignettes. It reads the prose of an R Markdown page and
# reports what the plain-English rules in cairn/references/plain-vignettes.md
# forbid. That page defines "swept prose", "sentence" and "dash"; this script is
# the definition in code, so change the two together.
#
#   LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R <file>...      # report
#   Rscript tools/prose-sweep.R --prose <file>...                 # one sentence per line
#   Rscript tools/prose-sweep.R --chunks <file>...                # fenced blocks, no #> lines
#   Rscript tools/prose-sweep.R --inventory [--terms <md>] <file>...
#
# A file argument of `-` reads standard input.
#
# Swept prose is the page minus its YAML header, its HTML comments (multi-line
# ones too), its References section and every fenced block (backtick or tilde,
# with or without an info string).
#
# Report mode prints each sentence over 25 words, each dash (U+2014, `---`,
# ` -- `, `&mdash;`), each semicolon outside code spans, math and HTML entities,
# and each milestone, decision or review id. A code span or a math span counts
# as one word, and neither is searched for a dash or a semicolon. Ids are
# searched everywhere in the prose, code spans, link text and URLs included.
#
# Exit status: 0 clean, 1 on a finding, 2 when a file has no sentences, 3 on a
# usage error. With several files the highest status wins. `--chunks` reads no
# prose, so it never exits 1 or 2.
#
# Base R only.

MAX_WORDS <- 25L
ID_PATTERN <- "\\b(M[0-9]{2,3}|D-[0-9]{3}|RR[0-9]{2})\\b"
EMDASH <- "—"
CODE_TOKEN <- "CODESPAN"
MATH_TOKEN <- "MATHSPAN"
DEFAULT_TERMS <- "cairn/references/plain-vignettes.md"

usage_error <- function(msg) {
  message("prose-sweep: ", msg)
  message("usage: prose-sweep.R [--prose | --chunks | --inventory [--terms <md>]] <file>...")
  quit(status = 3L)
}

read_input <- function(path) {
  if (identical(path, "-")) {
    con <- file("stdin", encoding = "UTF-8")
  } else {
    if (!file.exists(path)) usage_error(paste0("no such file: ", path))
    con <- file(path, encoding = "UTF-8")
  }
  on.exit(close(con))
  readLines(con, warn = FALSE)
}

# Splits a page into prose lines and fenced blocks. Returns the prose as a
# character vector the same length as the input, with NA for every dropped
# line, and the fenced blocks as a list of character vectors.
split_page <- function(lines) {
  n <- length(lines)
  prose <- lines
  blocks <- list()
  i <- 1L

  # YAML header: only when the page opens with it.
  if (n > 0L && grepl("^---\\s*$", lines[[1]])) {
    end <- which(grepl("^(---|\\.\\.\\.)\\s*$", lines))
    end <- end[end > 1L]
    if (length(end)) {
      prose[seq_len(end[[1]])] <- NA
      i <- end[[1]] + 1L
    }
  }

  in_comment <- FALSE
  fence <- NULL
  current <- character(0)
  while (i <= n) {
    ln <- lines[[i]]
    if (!is.null(fence)) {
      current <- c(current, ln)
      prose[[i]] <- NA
      close_re <- paste0("^\\s{0,3}", fence$char, "{", fence$len, ",}\\s*$")
      if (grepl(close_re, ln)) {
        blocks[[length(blocks) + 1L]] <- current
        fence <- NULL
        current <- character(0)
      }
      i <- i + 1L
      next
    }
    if (!in_comment) {
      open <- regmatches(ln, regexec("^\\s{0,3}(`{3,}|~{3,})", ln))[[1]]
      if (length(open)) {
        fence <- list(char = substr(open[[2]], 1L, 1L), len = nchar(open[[2]]))
        current <- ln
        prose[[i]] <- NA
        i <- i + 1L
        next
      }
    }
    # HTML comments, possibly several on one line and possibly multi-line.
    kept <- ""
    rest <- ln
    touched <- in_comment
    repeat {
      if (in_comment) {
        pos <- regexpr("-->", rest, fixed = TRUE)
        if (pos < 0L) {
          rest <- ""
          break
        }
        rest <- substr(rest, pos + 3L, nchar(rest))
        in_comment <- FALSE
      } else {
        pos <- regexpr("<!--", rest, fixed = TRUE)
        if (pos < 0L) {
          kept <- paste0(kept, rest)
          break
        }
        touched <- TRUE
        kept <- paste0(kept, substr(rest, 1L, pos - 1L))
        rest <- substr(rest, pos + 4L, nchar(rest))
        in_comment <- TRUE
      }
    }
    prose[[i]] <- if (touched && !nzchar(trimws(kept))) NA else kept
    i <- i + 1L
  }
  if (!is.null(fence)) blocks[[length(blocks) + 1L]] <- current

  # References section: from its heading to the next heading of the same or a
  # higher level.
  heading_level <- function(x) {
    m <- regmatches(x, regexec("^\\s{0,3}(#{1,6})\\s", x))[[1]]
    if (length(m)) nchar(m[[2]]) else NA_integer_
  }
  j <- 1L
  while (j <= n) {
    ln <- prose[[j]]
    if (!is.na(ln) && grepl("^\\s{0,3}#{1,6}\\s+references?\\s*#*\\s*$", ln, ignore.case = TRUE)) {
      level <- heading_level(ln)
      prose[[j]] <- NA
      j <- j + 1L
      while (j <= n) {
        lv <- if (is.na(prose[[j]])) NA_integer_ else heading_level(prose[[j]])
        if (!is.na(lv) && lv <= level) break
        prose[[j]] <- NA
        j <- j + 1L
      }
      next
    }
    j <- j + 1L
  }

  list(prose = prose, blocks = blocks)
}

# Groups prose lines into units. A unit ends at a blank line, and a heading,
# list item, blockquote line or table row starts a new one. Each table cell is
# its own unit. Returns a list of list(text, line), where text keeps "\n"
# between source lines so a finding can be traced to its line.
prose_units <- function(prose) {
  units <- list()
  buf <- character(0)
  start <- NA_integer_
  flush <- function() {
    if (length(buf)) units[[length(units) + 1L]] <<- list(text = paste(buf, collapse = "\n"), line = start)
    buf <<- character(0)
    start <<- NA_integer_
  }
  for (i in seq_along(prose)) {
    ln <- prose[[i]]
    if (is.na(ln)) next
    if (!nzchar(trimws(ln))) {
      flush()
      next
    }
    # Horizontal rules and table alignment rows carry no prose.
    if (grepl("^\\s{0,3}([-*_]\\s*){3,}$", ln) ||
        grepl("^\\s*\\|?\\s*:?-+:?\\s*(\\|\\s*:?-+:?\\s*)*\\|?\\s*$", ln) && grepl("|", ln, fixed = TRUE)) {
      flush()
      next
    }
    if (grepl("^\\s*\\|", ln)) {
      flush()
      for (cell in split_cells(ln)) {
        if (nzchar(trimws(cell))) units[[length(units) + 1L]] <- list(text = trimws(cell), line = i)
      }
      next
    }
    if (grepl("^\\s{0,3}#{1,6}\\s", ln)) {
      flush()
      units[[length(units) + 1L]] <- list(text = sub("^\\s{0,3}#{1,6}\\s+", "", ln), line = i)
      next
    }
    if (grepl("^\\s*>", ln)) {
      flush()
      units[[length(units) + 1L]] <- list(text = sub("^\\s*>\\s?", "", ln), line = i)
      next
    }
    if (grepl("^\\s*([-*+]|[0-9]+[.)])\\s+", ln)) {
      flush()
      buf <- sub("^\\s*([-*+]|[0-9]+[.)])\\s+", "", ln)
      start <- i
      next
    }
    if (!length(buf)) start <- i
    buf <- c(buf, ln)
  }
  flush()
  units
}

# Splits a table row on pipes that sit outside code spans.
split_cells <- function(ln) {
  chars <- strsplit(ln, "", fixed = TRUE)[[1]]
  cells <- character(0)
  cur <- ""
  in_code <- FALSE
  for (ch in chars) {
    if (ch == "`") in_code <- !in_code
    if (ch == "|" && !in_code) {
      cells <- c(cells, cur)
      cur <- ""
    } else {
      cur <- paste0(cur, ch)
    }
  }
  c(cells, cur)
}

# Replaces each span matched by `pattern` with `token`, followed by the
# newlines the span held so that later text keeps its line.
replace_spans <- function(text, pattern, token) {
  m <- gregexpr(pattern, text, perl = TRUE)
  regmatches(text, m) <- list(vapply(regmatches(text, m)[[1]], function(s) {
    paste0(" ", token, " ", strrep("\n", lengths(regmatches(s, gregexpr("\n", s, fixed = TRUE)))))
  }, character(1)))
  text
}

CODE_RE <- "(`+)(?!`)[\\s\\S]*?(?<!`)\\1(?!`)"
DISPLAY_MATH_RE <- "\\$\\$[\\s\\S]*?\\$\\$"
INLINE_MATH_RE <- "(?<!\\\\)\\$(?=[^\\s$])[^$]*?(?<=[^\\s$\\\\])\\$"
ENTITY_RE <- "&(#[0-9]+|#[xX][0-9a-fA-F]+|[A-Za-z][A-Za-z0-9]*);"

mask_spans <- function(text) {
  text <- replace_spans(text, CODE_RE, CODE_TOKEN)
  text <- replace_spans(text, DISPLAY_MATH_RE, MATH_TOKEN)
  replace_spans(text, INLINE_MATH_RE, MATH_TOKEN)
}

is_word <- function(tok) grepl("[[:alnum:]]", tok)

# Splits one unit into sentences. Returns a list of list(words, line).
unit_sentences <- function(unit) {
  masked <- mask_spans(unit$text)
  lines <- strsplit(masked, "\n", fixed = TRUE)[[1]]
  out <- list()
  words <- character(0)
  line <- NA_integer_
  for (k in seq_along(lines)) {
    toks <- strsplit(trimws(lines[[k]]), "[[:space:]]+")[[1]]
    toks <- toks[nzchar(toks)]
    for (tok in toks) {
      if (!is_word(tok) && !length(words)) next
      if (!length(words)) line <- unit$line + k - 1L
      words <- c(words, tok)
      if (ends_sentence(tok)) {
        out[[length(out) + 1L]] <- list(words = words, line = line)
        words <- character(0)
      }
    }
  }
  if (length(words)) out[[length(out) + 1L]] <- list(words = words, line = line)
  Filter(function(s) any(is_word(s$words)), out)
}

ends_sentence <- function(tok) {
  if (!grepl("[.!?][\"')\\]*_]*$", tok, perl = TRUE)) return(FALSE)
  bare <- gsub("^[\"'(\\[*_]+|[\"')\\]*_]+$", "", tok, perl = TRUE)
  # A period after a single capital letter (an initial) does not end one, nor
  # does a Latin abbreviation that usually runs on.
  if (grepl("^[A-Z]\\.$", bare)) return(FALSE)
  if (grepl("^(e\\.g|i\\.e|al|vs|cf)\\.$", bare)) return(FALSE)
  TRUE
}

unit_line_of <- function(unit, text, pos) {
  unit$line + lengths(regmatches(substr(text, 1L, pos), gregexpr("\n", substr(text, 1L, pos), fixed = TRUE)))
}

sweep_findings <- function(units, label) {
  found <- character(0)
  for (u in units) {
    for (s in unit_sentences(u)) {
      nw <- sum(is_word(s$words))
      if (nw > MAX_WORDS) {
        found <- c(found, sprintf("%s:%d: long sentence (%d words): %s", label, s$line, nw, paste(s$words, collapse = " ")))
      }
    }
    masked <- mask_spans(u$text)
    dash_re <- paste0(EMDASH, "|---|(?<=\\s)--(?=\\s)|&mdash;")
    for (pos in hits(masked, dash_re)) {
      found <- c(found, sprintf("%s:%d: dash: %s", label, unit_line_of(u, masked, pos), context(masked, pos)))
    }
    no_entities <- gsub(ENTITY_RE, "ENTITY", masked, perl = TRUE)
    for (pos in hits(no_entities, ";")) {
      found <- c(found, sprintf("%s:%d: semicolon: %s", label, unit_line_of(u, no_entities, pos), context(no_entities, pos)))
    }
    for (pos in hits(u$text, ID_PATTERN)) {
      found <- c(found, sprintf("%s:%d: id: %s", label, unit_line_of(u, u$text, pos), context(u$text, pos)))
    }
  }
  found
}

hits <- function(text, pattern) {
  m <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (m[[1]] < 0L) integer(0) else as.integer(m)
}

context <- function(text, pos) {
  flat <- gsub("\n", " ", text, fixed = TRUE)
  trimws(substr(flat, max(1L, pos - 30L), min(nchar(flat), pos + 30L)))
}

chunk_lines <- function(blocks) {
  as.character(unlist(lapply(blocks, function(b) b[!grepl("^\\s*#>", b)]), use.names = FALSE))
}

read_terms <- function(path) {
  if (!file.exists(path)) usage_error(paste0("no precision list at ", path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  start <- which(grepl("^#{2,3}\\s+Precision list\\s*$", lines))
  if (!length(start)) usage_error(paste0("no `## Precision list` heading in ", path))
  terms <- character(0)
  for (ln in lines[-seq_len(start[[1]])]) {
    if (grepl("^#", ln)) break
    m <- regmatches(ln, regexec("^\\s*[-*]\\s+`?([A-Za-z][A-Za-z -]*[A-Za-z])`?", ln))[[1]]
    if (length(m)) terms <- c(terms, tolower(m[[2]]))
  }
  if (!length(terms)) usage_error(paste0("the precision list in ", path, " is empty"))
  unique(terms)
}

inventory <- function(units, terms) {
  items <- character(0)
  for (u in units) {
    text <- u$text
    codes <- regmatches(text, gregexpr(CODE_RE, text, perl = TRUE))[[1]]
    items <- c(items, paste0("code: ", trimws(gsub("^`+|`+$", "", codes))))
    no_code <- gsub(CODE_RE, " ", text, perl = TRUE)
    flat <- gsub("\n", " ", no_code, fixed = TRUE)
    deg <- regmatches(flat, gregexpr("[0-9]+(\\.[0-9]+)?(°| degrees?\\b)", flat, perl = TRUE))[[1]]
    items <- c(items, paste0("degree: ", sub("(°| degrees?)$", "", deg, perl = TRUE)))
    nums <- regmatches(flat, gregexpr("(?<![A-Za-z0-9_.])[0-9]+(\\.[0-9]+)?(?![A-Za-z0-9_])", flat, perl = TRUE))[[1]]
    items <- c(items, paste0("number: ", nums))
    lower <- tolower(flat)
    for (t in terms) {
      if (grepl(paste0("\\b", t), lower, perl = TRUE)) items <- c(items, paste0("term: ", t))
    }
  }
  sort(unique(items[!grepl(": $", items)]))
}

main <- function(args) {
  mode <- "report"
  terms_path <- DEFAULT_TERMS
  files <- character(0)
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (a %in% c("--prose", "--chunks", "--inventory")) {
      if (mode != "report") usage_error("give at most one mode flag")
      mode <- sub("^--", "", a)
    } else if (a == "--terms") {
      if (i == length(args)) usage_error("--terms needs a file")
      i <- i + 1L
      terms_path <- args[[i]]
    } else if (grepl("^--", a)) {
      usage_error(paste0("unknown flag ", a))
    } else {
      files <- c(files, a)
    }
    i <- i + 1L
  }
  if (!length(files)) usage_error("no files given")
  if (mode == "inventory") terms <- read_terms(terms_path)

  status <- 0L
  for (f in files) {
    label <- if (identical(f, "-")) "<stdin>" else f
    page <- split_page(read_input(f))
    if (mode == "chunks") {
      writeLines(chunk_lines(page$blocks))
      next
    }
    units <- prose_units(page$prose)
    sentences <- unlist(lapply(units, unit_sentences), recursive = FALSE)
    if (!length(sentences)) {
      message(label, ": no sentences")
      status <- max(status, 2L)
      next
    }
    if (mode == "prose") {
      writeLines(vapply(sentences, function(s) paste(s$words, collapse = " "), character(1)))
    } else if (mode == "inventory") {
      writeLines(inventory(units, terms))
    } else {
      found <- sweep_findings(units, label)
      if (length(found)) {
        writeLines(found)
        status <- max(status, 1L)
      }
    }
  }
  status
}

if (sys.nframe() == 0L) quit(status = main(commandArgs(trailingOnly = TRUE)))
