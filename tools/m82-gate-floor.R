## M82 AC6 gate floor: re-run M81's five recorded mutations.
##
## Each must redden with the M81 test it was recorded against NAMED among the
## failures, at a FAIL count no lower than the one M81's work log records.
## Counts may exceed the recorded ones -- M82 adds tests over the same fixtures,
## which is why AC6 dropped RR17 BC11's tolerance 0.
##
## Restores are from a SCRATCH SNAPSHOT, never `git checkout --`: M81 logged that
## trap twice, on this very helper, because `git checkout --` restores from the
## INDEX and so silently reverts adjacent uncommitted work along with the
## mutation. Each file's blob hash is re-checked after every restore.
##
## Run: Rscript tools/m82-gate-floor.R

suppressMessages(devtools::load_all(quiet = TRUE))

snap_dir <- tempfile("m82-snapshots-")
dir.create(snap_dir)

blob <- function(path) {
  system2("git", c("hash-object", path), stdout = TRUE)
}
snapshot <- function(path) {
  file.copy(path, file.path(snap_dir, basename(path)), overwrite = TRUE)
  blob(path)
}
restore <- function(path, want) {
  file.copy(file.path(snap_dir, basename(path)), path, overwrite = TRUE)
  got <- blob(path)
  if (!identical(got, want)) {
    stop("restore left ", path, " at ", got, ", not ", want)
  }
  invisible(TRUE)
}

# Apply a literal, unambiguous edit; refuse anything else.
edit_file <- function(path, from, to) {
  txt <- readLines(path, warn = FALSE)
  joined <- paste(txt, collapse = "\n")
  n <- length(gregexpr(from, joined, fixed = TRUE)[[1]])
  if (!nzchar(from) || n != 1L) stop("pattern not unique in ", path, ": ", n)
  writeLines(strsplit(sub(from, to, joined, fixed = TRUE), "\n",
                      fixed = TRUE)[[1]], path)
}

# Run test files and report the FAIL count plus the names of failing tests.
run_files <- function(files) {
  res <- as.data.frame(testthat::test_dir(
    "tests/testthat", filter = paste(files, collapse = "|"),
    reporter = "silent", stop_on_failure = FALSE
  ))
  bad <- res$failed > 0L | res$error
  list(failed = sum(res$failed) + sum(res$error),
       tests = res$test[bad])
}

AUDIT <- "data-raw/audit-norms.R"
FILES <- "norms-audit"

MUTANTS <- list(
  list(
    id = "AC3-1 delete is.data.frame(batch)",
    path = AUDIT,
    from = "stopifnot(is.data.frame(batch),\n            all(c(",
    to   = "stopifnot(all(c(",
    recorded = 2L,
    named = "every registered abort site raises its own message"
  ),
  list(
    id = "AC3-2 delete the required-columns condition",
    path = AUDIT,
    from = paste0("stopifnot(is.data.frame(batch),\n            all(c(\"instrument\", ",
                  "\"sample\", \"citekey\", \"divisor\", \"scales\") %in%\n",
                  "                  names(batch)))"),
    to   = "stopifnot(is.data.frame(batch))",
    recorded = 2L,
    named = "every registered abort site raises its own message"
  ),
  list(
    id = "T4 unregistered stop() in the run block",
    path = AUDIT,
    from = "  res <- audit_norms()",
    to   = "  if (FALSE) stop(\"an unregistered run-block abort site\")\n  res <- audit_norms()",
    recorded = 1L,
    named = "no stop()/stopifnot() site the walk collects is unregistered"
  ),
  list(
    id = "AC4 hard-code the instrument list",
    path = AUDIT,
    from = "    nms <- get(\"instrument_names\", envir = ns)()",
    to   = "    nms <- c(\"cais\", \"csie\", \"csig\", \"csip\", \"csiv\", \"iei\", \"igicr\",\n             \"iip32\", \"iip64\", \"iipsc\", \"iis32\", \"iis64\", \"ipipipc\",\n             \"isc\", \"iitc\")",
    recorded = 1L,
    named = "the roster is the package's own enumeration, not a copy"
  ),
  list(
    id = "AC4 control: switch to the ::: shape",
    path = AUDIT,
    from = "    nms <- get(\"instrument_names\", envir = ns)()",
    to   = "    nms <- circumplex:::instrument_names()",
    recorded = 0L,
    named = NA_character_
  )
)

want <- snapshot(AUDIT)
cat("baseline blob:", want, "\n\n")

base <- run_files(FILES)
cat(sprintf("BASELINE            failed=%d\n\n", base$failed))
stopifnot(base$failed == 0L)

ok <- TRUE
for (m in MUTANTS) {
  edit_file(m$path, m$from, m$to)
  r <- run_files(FILES)
  hit <- if (is.na(m$named)) NA else any(grepl(m$named, r$tests, fixed = TRUE))
  pass <- r$failed >= m$recorded && (is.na(hit) || isTRUE(hit)) &&
    (m$recorded > 0L || r$failed == 0L)
  ok <- ok && pass
  cat(sprintf("%-44s failed=%-3d recorded=%-3d named-test-failed=%-5s %s\n",
              m$id, r$failed, m$recorded, hit, if (pass) "OK" else "**MISS**"))
  if (length(r$tests)) {
    cat("    failing tests:\n")
    for (nm in unique(r$tests)) cat("      -", nm, "\n")
  }
  restore(m$path, want)
}

cat("\nrestored blob:", blob(AUDIT), " matches baseline:",
    identical(blob(AUDIT), want), "\n")
cat("GATE FLOOR:", if (ok) "OK" else "MISS", "\n")
