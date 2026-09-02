# AC4 instrument for M120, part two: the blocks that keep a function's CRAN-mode
# coverage have to be able to CATCH a wrong answer, not merely to run.
#
#   Rscript tools/m120-planted-defects.R                  # verify the designations
#   Rscript tools/m120-planted-defects.R --probe f.csv    # try candidates, report
#
# Each entry in PATCHES is one deliberate defect: an exact string in R/ or src/
# replaced by a wrong one, of the form AC4 names -- a magnitude error in a
# returned estimate, or an angular wrap error at the 0/360 or +/-180 boundary.
#
# tools/m120-designations.csv then names, per exported function, the ONE test
# block that carries its CRAN-mode coverage and the defects that block must
# catch. For each pair the script:
#   1. runs the block unpatched in CRAN MODE (NOT_CRAN unset) and requires it to
#      PASS -- a block that skips there, as every vdiffr snapshot does, covers
#      nothing on CRAN and cannot be a designation;
#   2. rebuilds with the defect planted and requires the same block to go RED.
# Only the designated blocks are run, so this stays minutes rather than the hour
# a whole-suite sweep per defect costs.
#
# Restoration is `git checkout --`, not a copy held in this process: a run killed
# mid-patch would otherwise leave the defect in the tree, and the next run would
# read the patched file as its "original". For the same reason the script
# refuses to start on a dirty R/ or src/.

PATCHES <- list(
  list(id = "ssm-magnitude", form = "magnitude", file = "src/parameters.cpp",
       old = "double ampl = std::sqrt(std::pow(xval, 2) + std::pow(yval, 2));",
       new = "double ampl = 1.05 * std::sqrt(std::pow(xval, 2) + std::pow(yval, 2));",
       note = "SSM amplitude inflated 5% in the shared C++ estimator"),
  list(id = "ssm-wrap", form = "wrap", file = "src/parameters.cpp",
       old = "disp = modu(std::atan2(yval, xval), 2 * M_PI);",
       new = "disp = std::atan2(yval, xval);",
       note = "SSM displacement left on atan2's (-pi, pi] branch: 350 deg reports as -10"),
  list(id = "sem-magnitude", form = "magnitude", file = "R/ssm_sem.R",
       old = "a <- sqrt(x^2 + y^2)", new = "a <- 1.05 * sqrt(x^2 + y^2)", all = TRUE,
       note = "latent SSM amplitude inflated 5% in both SEM transforms"),
  list(id = "sem-wrap", form = "wrap", file = "R/ssm_sem.R",
       old = "d <- atan2(y, x) %% (2 * pi)", new = "d <- atan2(y, x)", all = TRUE,
       note = "latent displacement left on atan2's branch in both SEM transforms"),
  list(id = "cpm-magnitude", form = "magnitude", file = "R/cpm_fit.R",
       old = "    zeta = nat$zeta,", new = "    zeta = 1.05 * nat$zeta,",
       note = "CPM communality estimate inflated 5% on the reported path"),
  # The obvious cpm wrap defect -- dropping `%% (2 * pi)` from theta -- was tried
  # and reddened NOTHING even with every block live: the fitted radians already
  # land in [0, 2*pi) on the tested inputs, so it is not a defect there at all.
  # The pole label IS reachable, and is the repo's stated 0/360 invariant
  # (LM = 360, never 0; D-003, M20).
  list(id = "cpm-pole", form = "wrap", file = "R/cpm_fit.R",
       old = "  theta_deg[pole] <- 360", new = "  theta_deg[pole] <- 0",
       note = "a CPM angle on the 0/360 pole reported as 0 instead of 360"),
  # ssm_sem_syntax() returns model syntax, not an estimate; the numeric thing it
  # DOES return is the OLS projection matrix on its `weights` attribute, which is
  # what a magnitude error in its output means.
  list(id = "semweights-magnitude", form = "magnitude", file = "R/ssm_sem_syntax.R",
       old = "  w <- solve(crossprod(b), t(b))",
       new = "  w <- 1.05 * solve(crossprod(b), t(b))",
       note = "the SSM OLS projection weights inflated 5% at their single derivation"),
  # angle_unwrap owns its own degree arithmetic (R/convenience_functions.R) and
  # never reaches the C++ estimator, so its wrap defect is planted there: drop
  # the shortest-signed-rotation wrap and a pole crossing runs backwards.
  list(id = "unwrap-wrap", form = "wrap", file = "R/convenience_functions.R",
       old = "  d <- ((w[-1] - w[-n] + 180) %% 360) - 180",
       new = "  d <- w[-1] - w[-n]",
       note = "angle_unwrap taking the raw difference instead of the shortest signed rotation"),
  list(id = "axes-magnitude", form = "magnitude", file = "R/axes_reliability.R",
       old = "c(x = axis_reliability_sb(xi1, item_n[[\"x\"]]),",
       new = "c(x = 1.05 * axis_reliability_sb(xi1, item_n[[\"x\"]]),",
       note = "x-axis reliability inflated 5% (axes output carries no angle, so no wrap form)")
)
PATCH_BY_ID <- setNames(PATCHES, vapply(PATCHES, function(p) p$id, character(1)))

args <- commandArgs(trailingOnly = TRUE)
probe_file <- if ("--probe" %in% args) args[[which(args == "--probe") + 1L]] else NA_character_
spec_file <- if (!is.na(probe_file)) probe_file else "tools/m120-designations.csv"
if (!file.exists(spec_file)) stop("no spec file at ", spec_file, call. = FALSE)
spec <- read.csv(spec_file, stringsAsFactors = FALSE)
if (!nrow(spec)) stop(spec_file, " is empty -- refusing to pass vacuously.", call. = FALSE)
stopifnot(all(c("fn", "file", "test", "patches") %in% names(spec)))

patched_files <- unique(vapply(PATCHES, function(p) p$file, character(1)))
dirty <- system2("git", c("status", "--porcelain", "--", patched_files), stdout = TRUE)
if (length(dirty)) {
  stop("R/ or src/ has uncommitted changes, so this script cannot safely plant\n",
       "and revert defects there. Commit or stash first:\n  ",
       paste(dirty, collapse = "\n  "), call. = FALSE)
}
restore <- function(files) {
  if (system2("git", c("checkout", "--", files)) != 0L)
    stop("could not restore ", paste(files, collapse = ", "), call. = FALSE)
}
on.exit(restore(patched_files), add = TRUE)

apply_patch <- function(p) {
  txt <- paste(readLines(p$file, warn = FALSE), collapse = "\n")
  if (!grepl(p$old, txt, fixed = TRUE))
    stop("patch ", p$id, ": its target text is not in ", p$file,
         " -- the patch is stale and would prove nothing.", call. = FALSE)
  n <- length(gregexpr(p$old, txt, fixed = TRUE)[[1]])
  if (!isTRUE(p$all) && n != 1L)
    stop("patch ", p$id, ": target occurs ", n, " times in ", p$file,
         "; set all = TRUE if that is intended.", call. = FALSE)
  writeLines(gsub(p$old, p$new, txt, fixed = TRUE), p$file)
}

library(testthat)

# In probe mode --live runs blocks with every skip lifted, so a block that is
# currently skipped on CRAN can still be asked whether it WOULD catch a defect
# if it were brought back. Never used when verifying designations: there the
# question is what CRAN itself runs.
probe_live <- "--live" %in% args

# Run ONE block in CRAN mode. Returns "pass", "skip", "red", or "timeout".
run_block <- function(file, test) {
  Sys.setenv(NOT_CRAN = if (probe_live) "true" else "")
  out <- tryCatch({
    setTimeLimit(elapsed = 180, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)
    r <- as.data.frame(testthat::test_file(
      file.path("tests/testthat", file), desc = test,
      reporter = "silent", package = "circumplex"))
    if (!nrow(r)) return("absent")
    if (any(r$skipped)) return("skip")
    if (any(r$failed > 0) || any(r$error)) return("red")
    "pass"
  }, error = function(e) if (grepl("time limit", conditionMessage(e))) "timeout" else "red")
  out
}

# --- control: every designated block must PASS on CRAN, unpatched -------------
pkgload::load_all(".", quiet = TRUE, compile = TRUE)
cat("control run (no defect planted, CRAN mode):\n")
control <- character(nrow(spec))
for (i in seq_len(nrow(spec))) {
  control[i] <- run_block(spec$file[i], spec$test[i])
  cat(sprintf("  %-6s %-34s %s\n", control[i], spec$file[i], spec$test[i]))
}
bad_control <- which(control != "pass")

# --- each defect, against the blocks that must catch it ----------------------
result <- vector("list", nrow(spec))
for (pid in unique(unlist(strsplit(spec$patches, ";")))) {
  pid <- trimws(pid)
  p <- PATCH_BY_ID[[pid]]
  if (is.null(p)) stop("unknown patch id '", pid, "' in ", spec_file, call. = FALSE)
  rows <- which(vapply(strsplit(spec$patches, ";"),
                       function(x) pid %in% trimws(x), logical(1)))
  apply_patch(p)
  ok <- tryCatch({ pkgload::load_all(".", quiet = TRUE, compile = TRUE); TRUE },
                 error = function(e) { message("load_all failed under ", pid, ": ",
                                               conditionMessage(e)); FALSE })
  cat(sprintf("\n== %s (%s) -- %s\n", p$id, p$form, p$note))
  for (i in rows) {
    st <- if (ok) run_block(spec$file[i], spec$test[i]) else "build-failed"
    result[[i]] <- c(result[[i]], setNames(st, pid))
    cat(sprintf("  %-12s %-34s %s\n", st, spec$file[i], spec$test[i]))
  }
  restore(p$file)
}
pkgload::load_all(".", quiet = TRUE, compile = TRUE)   # leave a clean build behind

if (!is.na(probe_file)) {
  cat("\nprobe finished; nothing is asserted in probe mode\n")
  quit(save = "no")
}

problems <- character(0)
for (i in bad_control) {
  problems <- c(problems, sprintf(
    "%s: %s :: %s does not PASS on CRAN unpatched (it %s), so it covers nothing there",
    spec$fn[i], spec$file[i], spec$test[i], control[i]))
}
for (i in seq_len(nrow(spec))) {
  for (pid in names(result[[i]])) {
    if (!identical(unname(result[[i]][[pid]]), "red")) {
      problems <- c(problems, sprintf("%s: %s :: %s did not go red under %s (it %s)",
                                      spec$fn[i], spec$file[i], spec$test[i], pid,
                                      result[[i]][[pid]]))
    }
  }
}
if (length(problems)) {
  stop(length(problems), " designated block/defect pair(s) failed:\n  - ",
       paste(problems, collapse = "\n  - "), call. = FALSE)
}
cat("\nall", nrow(spec), "designated blocks pass on CRAN unpatched and go red under",
    "every defect they must catch\n")
