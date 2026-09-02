# AC4 instrument for M120, part two: the blocks that keep a function's CRAN-mode
# coverage have to be able to CATCH a wrong answer, not merely to run.
#
#   Rscript tools/m120-planted-defects.R [--report]
#
# Each entry in PATCHES is one deliberate defect: an exact string in R/ or src/
# replaced by a wrong one, of the form AC4 names -- a magnitude error in a
# returned estimate, or an angular wrap error at the 0/360 or +/-180 boundary.
# For each patch the package is rebuilt with the defect in place, the whole
# suite is run in CRAN MODE (NOT_CRAN unset, so the milestone's skips apply),
# and the blocks that go red are recorded. A block that stays green under a
# defect on the path it covers is not covering that path.
#
# DESIGNATIONS then states, per exported function, the block that carries its
# CRAN-mode coverage and the patches that must redden it. The script fails if
# any designated block survives a patch it is supposed to catch, if a patch
# reddens nothing at all (it would prove nothing), or if a designated block is
# not live on CRAN. --report skips the designation check and just prints, per
# patch, which blocks reddened -- how a designation is chosen in the first place.
#
# The working tree is restored after every patch, including on error.

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
  # The obvious cpm wrap defect -- dropping `%% (2 * pi)` from theta -- was
  # tried and reddened NOTHING even with every block live: the fitted radians
  # already land in [0, 2*pi) on the tested inputs, so it is not a defect there
  # at all. The pole label IS reachable, and is the repo's stated 0/360
  # invariant (LM = 360, never 0; D-003, M20).
  list(id = "cpm-pole", form = "wrap", file = "R/cpm_fit.R",
       old = "  theta_deg[pole] <- 360", new = "  theta_deg[pole] <- 0",
       note = "a CPM angle on the 0/360 pole reported as 0 instead of 360"),
  list(id = "semweights-magnitude", form = "magnitude", file = "R/ssm_sem.R",
       old = "  weights <- sem_ols_weights(th, names = scales)",
       new = "  weights <- 1.05 * sem_ols_weights(th, names = scales)",
       note = "the SEM projection weights inflated 5% where ssm_sem() derives them"),
  list(id = "axes-magnitude", form = "magnitude", file = "R/axes_reliability.R",
       old = "c(x = axis_reliability_sb(xi1, item_n[[\"x\"]]),",
       new = "c(x = 1.05 * axis_reliability_sb(xi1, item_n[[\"x\"]]),",
       note = "x-axis reliability inflated 5% (axes output carries no angle, so no wrap form)")
)

# function -> the block carrying its CRAN-mode coverage, and what must redden it
DESIGNATIONS <- read.csv(text = "fn,file,test,patches
", stringsAsFactors = FALSE)
if (file.exists("tools/m120-designations.csv")) {
  DESIGNATIONS <- read.csv("tools/m120-designations.csv", stringsAsFactors = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
report_only <- "--report" %in% args
propose <- "--propose" %in% args

# Which defects a function's designated block has to catch. A function is
# matched by the first pattern it satisfies, so the SEM entries precede the
# general ssm_ one.
REQUIRED <- list(
  list(pattern = "^ssm_sem_syntax$",  patches = c("semweights-magnitude")),
  list(pattern = "^ssm_sem",          patches = c("sem-magnitude", "sem-wrap")),
  list(pattern = "^ssm_",             patches = c("ssm-magnitude", "ssm-wrap")),
  list(pattern = "^cpm_",             patches = c("cpm-magnitude", "cpm-pole")),
  # axes_reliability returns reliabilities and standard errors of measurement:
  # no angular component, so AC4 asks only for the magnitude form.
  list(pattern = "^axes_",            patches = c("axes-magnitude"))
)
required_for <- function(fn) {
  for (r in REQUIRED) if (grepl(r$pattern, fn)) return(r$patches)
  character(0)
}

apply_patch <- function(p) {
  raw <- readLines(p$file, warn = FALSE)
  txt <- paste(raw, collapse = "\n")
  n <- length(gregexpr(p$old, txt, fixed = TRUE)[[1]])
  if (!grepl(p$old, txt, fixed = TRUE)) {
    stop("patch ", p$id, ": its target text is not in ", p$file,
         " -- the patch is stale and would prove nothing.", call. = FALSE)
  }
  if (!isTRUE(p$all) && n != 1L) {
    stop("patch ", p$id, ": target text occurs ", n, " times in ", p$file,
         "; set all = TRUE if that is intended.", call. = FALSE)
  }
  writeLines(gsub(p$old, p$new, txt, fixed = TRUE), p$file)
}

# Restoration is `git checkout --`, not a copy held in this process: a run killed
# mid-patch (a timeout, a Ctrl-C) would otherwise leave the defect in the tree,
# and the NEXT run would read the patched file as its "original" and restore to
# it. For the same reason the script refuses to start on a dirty R/ or src/ --
# it cannot tell a leftover defect from the maintainer's work in progress.
restore <- function(files) {
  st <- system2("git", c("checkout", "--", files))
  if (st != 0L) stop("could not restore ", paste(files, collapse = ", "), call. = FALSE)
}
patched_files <- unique(vapply(PATCHES, function(p) p$file, character(1)))
dirty <- system2("git", c("status", "--porcelain", "--", patched_files), stdout = TRUE)
if (length(dirty)) {
  stop("R/ or src/ has uncommitted changes, so this script cannot safely plant and\n",
       "revert defects there. Commit or stash first:\n  ",
       paste(dirty, collapse = "\n  "), call. = FALSE)
}
on.exit(restore(patched_files), add = TRUE)

library(testthat)
red_under <- list()

for (p in PATCHES) {
  apply_patch(p)
  ok <- tryCatch({
    # Always compile: a previous iteration's src/ patch must be built back OUT
    # before this one runs, or every later patch would inherit that defect and
    # every later result would be a lie about which block caught what.
    pkgload::load_all(".", quiet = TRUE, compile = TRUE)
    TRUE
  }, error = function(e) { message("load_all failed under ", p$id, ": ", conditionMessage(e)); FALSE })
  if (ok) {
    Sys.setenv(NOT_CRAN = "")
    res <- as.data.frame(test_dir("tests/testthat", reporter = "silent",
                                  stop_on_failure = FALSE, package = "circumplex",
                                  load_package = "none"))
    red <- res[!res$skipped & (res$failed > 0 | res$error), c("file", "test")]
  } else {
    red <- data.frame(file = character(0), test = character(0))
  }
  restore(p$file)
  red_under[[p$id]] <- red
  cat(sprintf("\n== %s (%s) -- %s\n   %d live CRAN block(s) went red\n",
              p$id, p$form, p$note, nrow(red)))
  if (report_only && nrow(red)) {
    print(head(red[order(red$file), ], 40), row.names = FALSE)
  }
}
pkgload::load_all(".", quiet = TRUE, compile = TRUE)   # leave a clean build behind

dead <- names(red_under)[vapply(red_under, nrow, 1L) == 0L]
if (length(dead)) {
  stop("these planted defects reddened NOTHING, so they demonstrate nothing: ",
       paste(dead, collapse = ", "), call. = FALSE)
}
if (propose) {
  # Propose one designation per statistical-family export: the block that is
  # live on CRAN, names the function, and reddens under every defect that
  # function must catch. Ties break on the block that reddens under the most
  # defects overall, then alphabetically, so the proposal is deterministic.
  blocks_of <- function(text, file) {
    lines <- strsplit(text, "\r?\n")[[1]]
    opens <- grep("^test_that\\(", lines); if (!length(opens)) return(NULL)
    ends <- c(opens[-1] - 1L, length(lines))
    do.call(rbind, Map(function(a, b) {
      src <- lines[seq.int(a, b)]; txt <- paste(src, collapse = "\n")
      m <- regmatches(txt, regexpr('"(\\\\.|[^"\\\\])*"', txt, perl = TRUE))
      data.frame(file = file, test = if (length(m)) eval(parse(text = m)) else NA,
                 skips = any(grepl("^\\s*skip_on_cran\\(\\)\\s*$", src)), src = txt)
    }, opens, ends))
  }
  all_blocks <- do.call(rbind, lapply(
    list.files("tests/testthat", "^test-.*\\.R$", full.names = TRUE),
    function(f) blocks_of(paste(readLines(f, warn = FALSE), collapse = "\n"), basename(f))))
  live <- all_blocks[!all_blocks$skips, ]
  exports <- gsub('^export\\(|\\)$|"', "", grep("^export\\(", readLines("NAMESPACE", warn = FALSE), value = TRUE))
  fams <- sort(grep("^(ssm_|cpm_|axes_)", exports, value = TRUE))
  out <- NULL
  for (fn in fams) {
    need <- required_for(fn)
    if (!length(need)) next
    cand <- live[grepl(paste0("(^|[^A-Za-z0-9._])", fn, "\\s*\\("), live$src), ]
    if (!nrow(cand)) { cat("NO LIVE CALLER  ", fn, "\n"); next }
    keykey <- paste(cand$file, cand$test)
    reddens <- vapply(need, function(pid) {
      r <- red_under[[pid]]; keykey %in% paste(r$file, r$test)
    }, logical(nrow(cand)))
    if (!is.matrix(reddens)) reddens <- matrix(reddens, nrow = nrow(cand))
    ok <- which(apply(reddens, 1, all))
    if (!length(ok)) { cat("NO BLOCK CATCHES ALL OF", paste(need, collapse = "+"), "for", fn, "\n"); next }
    breadth <- vapply(ok, function(i) sum(vapply(names(red_under), function(pid)
      keykey[i] %in% paste(red_under[[pid]]$file, red_under[[pid]]$test), logical(1))), numeric(1))
    pick <- ok[order(-breadth, keykey[ok])][1]
    out <- rbind(out, data.frame(fn = fn, file = cand$file[pick], test = cand$test[pick],
                                 patches = paste(need, collapse = ";")))
  }
  if (!is.null(out)) {
    write.csv(out, "tools/m120-designations.csv", row.names = FALSE)
    cat("\nwrote", nrow(out), "designations to tools/m120-designations.csv\n")
    print(out[, c("fn", "file", "patches")], row.names = FALSE)
  }
  quit(save = "no")
}
if (report_only) quit(save = "no")

if (!nrow(DESIGNATIONS)) {
  stop("no designations in tools/m120-designations.csv -- refusing to pass vacuously.",
       call. = FALSE)
}
problems <- character(0)
for (i in seq_len(nrow(DESIGNATIONS))) {
  d <- DESIGNATIONS[i, ]
  for (pid in trimws(strsplit(d$patches, ";")[[1]])) {
    if (!pid %in% names(red_under)) {
      problems <- c(problems, paste0(d$fn, ": unknown patch id '", pid, "'")); next
    }
    r <- red_under[[pid]]
    if (!any(r$file == d$file & r$test == d$test)) {
      problems <- c(problems, sprintf("%s: %s :: %s stayed GREEN under %s",
                                      d$fn, d$file, d$test, pid))
    } else {
      cat(sprintf("  red  %-20s %-32s %s\n", pid, d$file, d$test))
    }
  }
}
if (length(problems)) {
  stop(length(problems), " designated block/defect pair(s) failed:\n  - ",
       paste(problems, collapse = "\n  - "), call. = FALSE)
}
cat("\nevery designated block reddens under every defect it must catch\n")
