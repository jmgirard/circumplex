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
  keep <- vapply(cells, length, integer(1)) == 4L
  cells <- cells[keep]
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
  for (inst in names(batch)) {
    obj <- get(inst, envir = asNamespace("circumplex"))
    norms <- obj$Norms[[1]]
    scales <- obj$Scales
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    j <- match(norms[[key]], scales$Abbrev)
    bad <- which((norms$Angle %% 360) != (scales$Angle[j] %% 360))
    if (length(bad)) {
      out[[length(out) + 1L]] <- data.frame(
        instrument = inst, scale = norms[[key]][bad],
        norms_angle = norms$Angle[bad], scales_angle = scales$Angle[j][bad],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(instrument = character(0), scale = character(0),
               norms_angle = numeric(0), scales_angle = numeric(0),
               stringsAsFactors = FALSE)
}

# --- run ---------------------------------------------------------------------

if (!isTRUE(getOption("norms_audit_defs_only", FALSE))) {
  LEDGER_PATH <- Sys.getenv("NORMS_AUDIT_LEDGER",
                            "data-raw/norms-audit-ledger.csv")
  res <- audit_norms()
  angle_check <- angle_copies_agree()

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

  ledger$generated <- "2026-08-06"
  ledger$commit <- tryCatch(
    system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) NA_character_
  )

  utils::write.csv(ledger, LEDGER_PATH, row.names = FALSE)

  gaps <- res$coverage[!res$coverage$exempt, , drop = FALSE]
  cat("norms audit\n")
  cat("  ledger rows:      ", nrow(ledger), " -> ", LEDGER_PATH, "\n", sep = "")
  cat("  coverage gaps:    ", nrow(gaps), "\n", sep = "")
  cat("  note-only rows:   ", sum(res$coverage$exempt), "\n", sep = "")
  cat("  angle-copy splits:", nrow(angle_check), "\n", sep = "")
  if (nrow(res$coverage)) print(res$coverage)
  if (nrow(angle_check)) print(angle_check)
  if (nrow(ledger)) {
    print(ledger[, c("instrument", "field", "scale", "kind", "disposition")])
  }
}
