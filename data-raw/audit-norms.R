# Norms provenance audit (M72)
#
# Compares every shipped audited-field value against the value recorded in the
# instrument's committed source note, and writes a ledger of mismatches.
#
# The source-side values are PARSED from the notes rather than retyped here on
# purpose: a hand-typed table in this script would be a second transcription by
# the same reader, so a green ledger would prove only that one reader copied
# consistently, not that the package matches the source. The note is the single
# source of truth; this script is the comparison.
#
# Usage (from the package root, with the package loaded):
#   Rscript -e 'devtools::load_all(); source("data-raw/audit-norms.R")'
# Writes: data-raw/norms-audit-ledger.csv   (or -prefix.csv, see LEDGER_PATH)
#         data-raw/norms-audit-coverage.csv (the coverage report, see AC3)

AUDIT_BATCH <- c(
  csie = "locke2007",
  csig = "locke2014",
  csip = "boudreaux2018",
  csiv = "locke2000",
  iitc = "bliton2019"
)

# Documented unit deviations: shipped value = source value / divisor.
# csip's source prints octant SUM scores (0-24); data-raw/csip.R divides by 8
# to express them as item means on the instrument's own 0-3 anchor range.
AUDIT_DIVISOR <- c(csip = 8)

NOT_PUBLISHED <- "not-published-in-source"

# A source note may also table a norm sample the source publishes and the
# package does not ship. Those rows carry field = "note-only" and are exempt
# from the coverage report: they have no shipped counterpart by construction.
NOTE_ONLY <- "note-only"

# A Reference row's source-side value is the author-year credit for the norm
# SAMPLE, and the audit is only as strong as where that credit came from. Where
# the source itself prints the credit, the anchor names the page and the two
# sides of the comparison have independent origins. Where no page prints it --
# locke2000's undated CSIV norms table is the known case -- the note author
# CONSTRUCTED the credit, so comparing it against the shipped string compares
# two descendants of one origin and can never fail (the M72 lesson). Those rows
# are not removed, because the shipped credit still has to be recorded
# somewhere; they are LABELLED with this token in the anchor cell and listed as
# exempt coverage rows, so a reader of the coverage report can tell a quoted
# credit from an authored one instead of finding them indistinguishable.
CONSTRUCTED_CREDIT <- "constructed-credit"

# --- source side -------------------------------------------------------------

# Parse the machine-readable block a source note carries between its
# audit-values markers. Returns a data.frame(field, scale, value, anchor).
parse_source_note <- function(citekey,
                              dir = file.path("cairn", "references")) {
  path <- file.path(dir, paste0(citekey, ".md"))
  if (!file.exists(path)) {
    stop("source note not found: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  b <- grep("<!-- audit-values-begin -->", lines, fixed = TRUE)
  e <- grep("<!-- audit-values-end -->", lines, fixed = TRUE)
  if (length(b) != 1L || length(e) != 1L || e <= b) {
    stop("source note ", citekey, " has no single audit-values block",
         call. = FALSE)
  }
  rows <- lines[(b + 1L):(e - 1L)]
  rows <- rows[grepl("^\\|", rows)]
  # drop the header and the |---|---| separator
  rows <- rows[-(1:2)]
  cells <- lapply(strsplit(sub("^\\|", "", sub("\\|$", "", rows)), "|",
                           fixed = TRUE), trimws)
  # A row that does not split into exactly four cells is a malformed note, not
  # a row to skip: silently dropping it would remove a value from the audit
  # while every count still read clean (an anchor containing a literal "|" is
  # the way this happens).
  bad <- which(vapply(cells, length, integer(1)) != 4L)
  if (length(bad)) {
    stop("source note ", citekey, " has ", length(bad),
         " malformed audit row(s); first: ", rows[[bad[[1]]]], call. = FALSE)
  }
  empty <- which(vapply(cells, function(x) !nzchar(x[[3]]), logical(1)))
  if (length(empty)) {
    stop("source note ", citekey, " has ", length(empty),
         " audit row(s) with an empty value; first: ", rows[[empty[[1]]]],
         call. = FALSE)
  }
  data.frame(
    field  = vapply(cells, `[`, character(1), 1L),
    scale  = vapply(cells, `[`, character(1), 2L),
    value  = vapply(cells, `[`, character(1), 3L),
    anchor = vapply(cells, `[`, character(1), 4L),
    stringsAsFactors = FALSE
  )
}

# --- shipped side ------------------------------------------------------------

# Enumerate shipped audited-field values from the package object itself.
# Norms[[1]]'s scale-name column is `Scale` for some instruments and `Abbrev`
# for others, so the key is normalised here rather than assumed.
shipped_values <- function(inst) {
  obj <- get(inst, envir = asNamespace("circumplex"))
  norms <- obj$Norms[[1]]
  src <- obj$Norms[[2]]
  scales <- obj$Scales
  key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"

  out <- rbind(
    data.frame(field = "M", scale = norms[[key]],
               value = as.character(norms$M), stringsAsFactors = FALSE),
    data.frame(field = "SD", scale = norms[[key]],
               value = as.character(norms$SD), stringsAsFactors = FALSE),
    data.frame(field = "Angle", scale = scales$Abbrev,
               value = as.character(scales$Angle), stringsAsFactors = FALSE),
    data.frame(field = "Items", scale = scales$Abbrev,
               value = normalise_items(scales$Items), stringsAsFactors = FALSE),
    data.frame(field = "Size", scale = "—",
               value = as.character(src$Size), stringsAsFactors = FALSE),
    data.frame(field = "Population", scale = "—",
               value = as.character(src$Population), stringsAsFactors = FALSE),
    data.frame(field = "Reference", scale = "—",
               value = as.character(src$Reference), stringsAsFactors = FALSE),
    data.frame(field = "URL", scale = "—",
               value = as.character(src$URL), stringsAsFactors = FALSE)
  )
  out$instrument <- inst
  out
}

# "1,  9, 17" and "1, 9, 17" are the same assignment; compare on the numbers.
normalise_items <- function(x) {
  vapply(strsplit(x, ",", fixed = TRUE), function(p) {
    paste(as.integer(trimws(p)), collapse = ", ")
  }, character(1))
}

# --- comparison --------------------------------------------------------------

# Angles are compared modulo 360 so the package's LM = 360 matches a source
# printing 0 (DESIGN.md IP2).
values_agree <- function(field, shipped, source, divisor) {
  if (identical(source, NOT_PUBLISHED)) return(NA)
  if (field %in% c("M", "SD")) {
    s <- suppressWarnings(as.numeric(source)) / divisor
    p <- suppressWarnings(as.numeric(shipped))
    if (is.na(s) || is.na(p)) return(FALSE)
    # compare at the precision the source prints, after the documented divisor
    return(isTRUE(all.equal(p, s, tolerance = 1e-8)))
  }
  if (identical(field, "Angle")) {
    s <- suppressWarnings(as.numeric(source)) %% 360
    p <- suppressWarnings(as.numeric(shipped)) %% 360
    if (is.na(s) || is.na(p)) return(FALSE)
    return(isTRUE(all.equal(p, s, tolerance = 1e-8)))
  }
  # The note records the author-year credit the source itself supports for the
  # norm sample; the shipped Reference must credit it, and may carry further
  # context around it (csiv cites the instrument's article alongside the
  # website that publishes the norms). Containment, not equality, is therefore
  # the comparison -- but a shipped string that drops the credit still fails,
  # which is exactly the pre-fix csiv defect.
  if (identical(field, "Reference")) {
    return(grepl(trimws(source), trimws(shipped), fixed = TRUE))
  }
  identical(trimws(shipped), trimws(source))
}

audit_norms <- function(batch = AUDIT_BATCH) {
  ledger <- list()
  coverage <- list()

  for (inst in names(batch)) {
    citekey <- batch[[inst]]
    divisor <- if (inst %in% names(AUDIT_DIVISOR)) AUDIT_DIVISOR[[inst]] else 1
    ship <- shipped_values(inst)
    note_all <- parse_source_note(citekey)
    note_only <- note_all[note_all$field == NOTE_ONLY, , drop = FALSE]
    note <- note_all[note_all$field != NOTE_ONLY, , drop = FALSE]

    if (nrow(note_only)) {
      coverage[[length(coverage) + 1L]] <- data.frame(
        instrument = inst, side = "note-only-sample",
        field = note_only$scale, scale = note_only$value,
        exempt = TRUE, stringsAsFactors = FALSE
      )
    }

    constructed <- note[note$field == "Reference" &
                          grepl(CONSTRUCTED_CREDIT, note$anchor, fixed = TRUE),
                        , drop = FALSE]
    if (nrow(constructed)) {
      coverage[[length(coverage) + 1L]] <- data.frame(
        instrument = inst, side = "constructed-credit-reference",
        field = "Reference", scale = constructed$value,
        exempt = TRUE, stringsAsFactors = FALSE
      )
    }

    # every shipped value must have a source-side entry, and vice versa
    ship_key <- paste(ship$field, ship$scale)
    note_key <- paste(note$field, note$scale)
    miss_source <- ship[!(ship_key %in% note_key), , drop = FALSE]
    miss_ship <- note[!(note_key %in% ship_key), , drop = FALSE]
    if (nrow(miss_source)) {
      coverage[[length(coverage) + 1L]] <- data.frame(
        instrument = inst, side = "shipped-value-not-in-note",
        field = miss_source$field, scale = miss_source$scale,
        exempt = FALSE, stringsAsFactors = FALSE
      )
    }
    if (nrow(miss_ship)) {
      coverage[[length(coverage) + 1L]] <- data.frame(
        instrument = inst, side = "note-value-not-shipped",
        field = miss_ship$field, scale = miss_ship$scale,
        exempt = FALSE, stringsAsFactors = FALSE
      )
    }

    for (i in seq_len(nrow(ship))) {
      j <- match(ship_key[[i]], note_key)
      if (is.na(j)) next
      agree <- values_agree(ship$field[[i]], ship$value[[i]],
                            note$value[[j]], divisor)
      if (isTRUE(agree)) next
      ledger[[length(ledger) + 1L]] <- data.frame(
        instrument = inst,
        field = ship$field[[i]],
        scale = ship$scale[[i]],
        shipped = ship$value[[i]],
        source = note$value[[j]],
        anchor = note$anchor[[j]],
        kind = if (is.na(agree)) "not-published" else "mismatch",
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    ledger = if (length(ledger)) do.call(rbind, ledger) else empty_ledger(),
    coverage = if (length(coverage)) do.call(rbind, coverage) else
      data.frame(instrument = character(0), side = character(0),
                 field = character(0), scale = character(0),
                 exempt = logical(0), stringsAsFactors = FALSE)
  )
}

empty_ledger <- function() {
  data.frame(instrument = character(0), field = character(0),
             scale = character(0), shipped = character(0),
             source = character(0), anchor = character(0),
             kind = character(0), stringsAsFactors = FALSE)
}

# --- cross-check the two shipped angle copies --------------------------------

# Scales$Angle and Norms[[1]]$Angle are two independent shipped copies of the
# same fact; nothing else in the package makes them agree.
angle_copies_agree <- function(batch = AUDIT_BATCH) {
  out <- list()
  add <- function(inst, scale, na, sa, why) {
    out[[length(out) + 1L]] <<- data.frame(
      instrument = inst, scale = scale, norms_angle = na, scales_angle = sa,
      problem = why, stringsAsFactors = FALSE
    )
  }
  for (inst in names(batch)) {
    obj <- get(inst, envir = asNamespace("circumplex"))
    norms <- obj$Norms[[1]]
    scales <- obj$Scales
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    j <- match(norms[[key]], scales$Abbrev)

    # An unmatched scale name is a split, not a row to skip: which(NA) drops
    # the row, so the unjoined case has to be caught before the comparison.
    unjoined <- which(is.na(j))
    if (length(unjoined)) {
      add(inst, norms[[key]][unjoined], norms$Angle[unjoined], NA_real_,
          "scale name not found in Scales$Abbrev")
    }
    # An NA in either copy compares FALSE against everything and would
    # otherwise be invisible to the != test below.
    nas <- which(is.na(norms$Angle) | (!is.na(j) & is.na(scales$Angle[j])))
    if (length(nas)) {
      add(inst, norms[[key]][nas], norms$Angle[nas], scales$Angle[j][nas],
          "NA angle in a shipped copy")
    }
    ok <- !is.na(j) & !is.na(norms$Angle) & !is.na(scales$Angle[j])
    bad <- which(ok & (norms$Angle %% 360) != (scales$Angle[j] %% 360))
    if (length(bad)) {
      add(inst, norms[[key]][bad], norms$Angle[bad], scales$Angle[j][bad],
          "copies disagree modulo 360")
    }
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(instrument = character(0), scale = character(0),
               norms_angle = numeric(0), scales_angle = numeric(0),
               problem = character(0), stringsAsFactors = FALSE)
}

# --- the IP2 convention, which the modulo comparison cannot see --------------

# Angles are degrees in [0, 360) in the user API with LM = 360, never 0
# (DESIGN.md IP2). The audit compares against the source modulo 360, so a
# shipped 0 where the convention wants 360 agrees with the source and passes
# silently. This check is on the shipped side only, where the convention lives.
ip2_convention_holds <- function(batch = AUDIT_BATCH) {
  out <- list()
  for (inst in names(batch)) {
    obj <- get(inst, envir = asNamespace("circumplex"))
    norms <- obj$Norms[[1]]
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    copies <- list(
      list(where = "Norms[[1]]$Angle", scale = as.character(norms[[key]]),
           angle = norms$Angle),
      list(where = "Scales$Angle", scale = as.character(obj$Scales$Abbrev),
           angle = obj$Scales$Angle)
    )
    for (cp in copies) {
      bad <- which(is.na(cp$angle) | cp$angle <= 0 | cp$angle > 360)
      if (length(bad)) {
        out[[length(out) + 1L]] <- data.frame(
          instrument = inst, copy = cp$where, scale = cp$scale[bad],
          angle = cp$angle[bad], stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(instrument = character(0), copy = character(0),
               scale = character(0), angle = numeric(0),
               stringsAsFactors = FALSE)
}

# --- run ---------------------------------------------------------------------

# Two stamps, not one. A run's own HEAD is necessarily the PARENT of the
# commit that lands the ledger, so a single "commit" column can never name the
# commit containing the file. Recording the script and the package data
# separately also lets a pre-fix snapshot be rebuilt honestly: today's script
# against an earlier tree's data/, each named.
#
# Every column is filled with `rep(x, nrow(ledger))` rather than the scalar x.
# `df$col <- x` recycles a length-1 value to any POSITIVE number of rows and
# errors on zero ("replacement has 1 row, data has 0"), so the scalar form
# worked on every ledger M72 happened to produce and would have crashed the run
# on the first fully-clean one -- the run this audit exists to reach.
stamp_ledger <- function(ledger,
                         generated = format(Sys.Date()),
                         script_commit = NA_character_,
                         data_commit = NA_character_) {
  n <- nrow(ledger)
  ledger$generated <- rep(as.character(generated), n)
  ledger$script_commit <- rep(as.character(script_commit), n)
  ledger$data_commit <- rep(as.character(data_commit), n)
  ledger
}

if (!isTRUE(getOption("norms_audit_defs_only", FALSE))) {
  LEDGER_PATH <- Sys.getenv("NORMS_AUDIT_LEDGER",
                            "data-raw/norms-audit-ledger.csv")
  COVERAGE_PATH <- Sys.getenv("NORMS_AUDIT_COVERAGE",
                              "data-raw/norms-audit-coverage.csv")
  res <- audit_norms()
  angle_check <- angle_copies_agree()
  ip2_check <- ip2_convention_holds()

  disp_path <- "data-raw/norms-audit-dispositions.csv"
  ledger <- res$ledger
  if (file.exists(disp_path) && nrow(ledger)) {
    disp <- utils::read.csv(disp_path, stringsAsFactors = FALSE)
    k <- paste(ledger$instrument, ledger$field, ledger$scale)
    dk <- paste(disp$instrument, disp$field, disp$scale)
    ledger$disposition <- disp$disposition[match(k, dk)]
    ledger$disposition[is.na(ledger$disposition)] <- "UNDISPOSITIONED"
  } else if (nrow(ledger)) {
    ledger$disposition <- "UNDISPOSITIONED"
  } else {
    ledger$disposition <- character(0)
  }

  git_head <- function() {
    tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
             error = function(e) NA_character_)
  }
  or_head <- function(v) if (nzchar(v)) v else git_head()

  ledger <- stamp_ledger(
    ledger,
    script_commit = or_head(Sys.getenv("NORMS_AUDIT_SCRIPT_COMMIT")),
    data_commit = or_head(Sys.getenv("NORMS_AUDIT_DATA_COMMIT"))
  )

  utils::write.csv(ledger, LEDGER_PATH, row.names = FALSE)
  utils::write.csv(res$coverage, COVERAGE_PATH, row.names = FALSE)

  gaps <- res$coverage[!res$coverage$exempt, , drop = FALSE]
  cat("norms audit\n")
  cat("  ledger rows:      ", nrow(ledger), " -> ", LEDGER_PATH, "\n", sep = "")
  cat("  coverage rows:    ", nrow(res$coverage), " -> ", COVERAGE_PATH, "\n",
      sep = "")
  cat("  coverage gaps:    ", nrow(gaps), "\n", sep = "")
  cat("  note-only rows:   ", sum(res$coverage$exempt), "\n", sep = "")
  cat("  angle-copy splits:", nrow(angle_check), "\n", sep = "")
  cat("  IP2 breaches:     ", nrow(ip2_check), "\n", sep = "")
  if (nrow(res$coverage)) print(res$coverage)
  if (nrow(angle_check)) print(angle_check)
  if (nrow(ip2_check)) print(ip2_check)
  if (nrow(ledger)) {
    print(ledger[, c("instrument", "field", "scale", "kind", "disposition")])
  }
}
