# Re-derive every shipped sample's reference kind from the audit record and
# compare it against the column the builders ship (M78).
#
# The kind is a package-authored inference over the provenance audit, not a
# value any source prints, so IP5's "published-source provenance recorded in
# data-raw/" is met by two records that must agree: the per-sample basis comment
# beside each builder's `Kind` value, and the Reference kind table in
# cairn/references/norms-audit.md. This script is the comparison between the
# audit table and the shipped data -- it reads the note, never the builders, so
# a builder edited without the note (or the reverse) shows up as a disagreement
# rather than as two copies of one mistake.
#
# Run from the repository root:
#   Rscript data-raw/derive-norms-kind.R
# Exits non-zero on any disagreement, any sample the table does not cover, and
# any table row naming a sample that is not shipped.

KIND_TOKENS <- c("standardization", "published", "unsourced")
NOTE <- "cairn/references/norms-audit.md"

if (!file.exists(NOTE)) {
  stop("run this from the repository root: ", NOTE, " not found", call. = FALSE)
}

# --- the audit record ------------------------------------------------------

lines <- readLines(NOTE, warn = FALSE)
start <- grep("^## Reference kind$", lines)
if (length(start) != 1L) {
  stop("expected exactly one '## Reference kind' section in ", NOTE,
       ", found ", length(start), call. = FALSE)
}
ends <- grep("^## ", lines)
ends <- ends[ends > start]
stop_at <- if (length(ends)) ends[[1]] - 1L else length(lines)
section <- lines[(start + 1L):stop_at]

# Table rows only: four pipe-delimited cells whose second is a bare integer.
# The header and the separator row both fail that test, so neither needs
# naming here -- a format change that broke the parse would empty `record` and
# fail the coverage check below rather than pass silently.
cells <- lapply(section[grepl("^\\|", section)], function(ln) {
  parts <- strsplit(sub("\\|\\s*$", "", sub("^\\s*\\|", "", ln)), "\\|")[[1]]
  trimws(parts)
})
cells <- Filter(function(p) length(p) >= 3L && grepl("^[0-9]+$", p[[2]]), cells)

if (!length(cells)) {
  stop("parsed no rows from the Reference kind table in ", NOTE, call. = FALSE)
}

record <- data.frame(
  instrument = vapply(cells, `[[`, character(1), 1),
  sample = as.numeric(vapply(cells, `[[`, character(1), 2)),
  kind = vapply(cells, `[[`, character(1), 3),
  stringsAsFactors = FALSE
)

bad_token <- setdiff(record$kind, KIND_TOKENS)
if (length(bad_token)) {
  stop("the audit table uses kinds outside the controlled vocabulary: ",
       paste(bad_token, collapse = ", "), call. = FALSE)
}

# --- the shipped data ------------------------------------------------------

# Read data/*.rda directly rather than through utils::data(), which resolves
# against the INSTALLED package and would compare the audit note to whatever
# version happens to be on .libPaths() -- reporting a freshly rebuilt column as
# absent. The directory listing is also the enumeration, so a newly added
# instrument is compared instead of silently skipped.
files <- sort(list.files("data", pattern = "\\.rda$", full.names = TRUE))
shipped <- do.call(rbind, lapply(files, function(f) {
  e <- new.env()
  nm <- load(f, envir = e)[[1]]
  obj <- get(nm, envir = e)
  if (!inherits(obj, "circumplex_instrument")) return(NULL)
  info <- obj$Norms[[2]]
  if (is.null(info$Kind)) {
    return(data.frame(
      instrument = nm, sample = info$Sample, kind = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    instrument = nm, sample = info$Sample, kind = as.character(info$Kind),
    stringsAsFactors = FALSE
  )
}))

key <- function(d) paste0(d$instrument, ":", d$sample)

# --- the comparison --------------------------------------------------------

problems <- character(0)

uncovered <- setdiff(key(shipped), key(record))
if (length(uncovered)) {
  problems <- c(problems, paste0(
    "shipped samples the audit table does not cover: ",
    paste(uncovered, collapse = ", ")
  ))
}

unshipped <- setdiff(key(record), key(shipped))
if (length(unshipped)) {
  problems <- c(problems, paste0(
    "audit table rows naming samples that are not shipped: ",
    paste(unshipped, collapse = ", ")
  ))
}

both <- intersect(key(shipped), key(record))
from_record <- record$kind[match(both, key(record))]
from_data <- shipped$kind[match(both, key(shipped))]
bad <- which(is.na(from_data) | from_record != from_data)
if (length(bad)) {
  problems <- c(problems, paste0(
    "kind disagreements (sample: audit vs shipped): ",
    paste0(
      both[bad], ": ", from_record[bad], " vs ",
      ifelse(is.na(from_data[bad]), "<absent>", from_data[bad]),
      collapse = "; "
    )
  ))
}

cat("Reference-kind derivation\n")
cat("  audit table rows: ", nrow(record), "\n", sep = "")
cat("  shipped samples:  ", nrow(shipped), "\n", sep = "")
for (k in KIND_TOKENS) {
  cat("  ", k, ": ", sum(from_data == k, na.rm = TRUE), "\n", sep = "")
}

if (length(problems)) {
  cat("\nDISAGREEMENTS\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
  quit(status = 1L)
}

cat("\nzero disagreements\n")
