# M94: the bootstrap fired-marker line in summary(), and its fences.
#
# Byte-identity fences (AC3): the analytic-path summary() output and the
# bootstrap print() output must be byte-identical to the merge-base commit of
# the m94 branch. The snapshots under _snaps/cpm_summary_markers.md were
# captured at that commit; to regenerate the merge-base capture at review,
# check out `git merge-base master m94-bootstrap-marker-list`, copy this file
# and its helper fixtures in, run it, and diff the snapshot file — it must not
# differ from the committed one. Analytic fits use the deterministic cormat
# path (no RNG); expect_snapshot() does not run on CRAN by design.

m94_labels <- function() c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

# Analytic fixtures, mirroring the calibration tests in test-cpm_api.R.
m94_clean_P0 <- function() {
  tr <- cpm_clean_truth()
  cpm_implied_cor(as.numeric(as_radian(as_degree(tr$angles))),
                  tr$zeta, tr$beta)
}

# Bootstrap fixtures (memoized: each is reused across several tests). All
# consume the global RNG (bootstrap resampling), seeded per call; callers of
# these builders need no seed of their own.
m94_boot_jz <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      e <- new.env()
      data("jz2017", package = "circumplex", envir = e)
      on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
      set.seed(101)
      fit <<- suppressWarnings(
        cpm_fit(e$jz2017, scales = PANO(), angles = octants(), boots = 25)
      )
    }
    fit
  }
})

m94_boot_big <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
      set.seed(2026)
      sim <- cpm_simulate(m94_boot_jz(), n = 2500)
      fit <<- suppressWarnings(
        cpm_fit(as.data.frame(sim), scales = colnames(sim),
                angles = octants(), boots = 25)
      )
    }
    fit
  }
})

m94_boot_clean <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      theta <- c(0, 45, 90, 135, 180, 225, 270, 315) * pi / 180
      P <- cpm_implied_cor(theta, rep(0.75, 8), c(0.45, 0.35, 0.25, 0.15))
      on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
      set.seed(7)
      X <- as.data.frame(matrix(stats::rnorm(800 * 8), 800) %*% chol(P))
      colnames(X) <- m94_labels()
      fit <<- suppressWarnings(
        cpm_fit(X, scales = m94_labels(), angles = octants(), boots = 25)
      )
    }
    fit
  }
})

# ---- AC3: analytic path byte-identical to merge-base ------------------------

test_that("analytic summary() output is byte-identical to merge-base (four regimes)", {
  tr <- cpm_clean_truth()
  P0 <- m94_clean_P0()
  # (1) clean N >= 2000
  clean <- cpm_fit(cormat = P0, scales = paste0("V", 1:8), angles = tr$angles,
                   n = 5000, m = 3)
  expect_snapshot(summary(clean))
  # (2) marker-firing N >= 2000 (Heywood): warnings at fit time, not display time
  voc <- cpm_oracle_voc()
  hey <- suppressWarnings(cpm_fit(cormat = voc$R, scales = voc$names,
                                  angles = voc$th_start, n = 5000, m = 2))
  expect_true(cpm_boundary_proximity(hey))
  expect_snapshot(summary(hey))
  # (3) N < 2000
  small <- cpm_fit(cormat = P0, scales = paste0("V", 1:8), angles = tr$angles,
                   n = 300, m = 3)
  expect_snapshot(summary(small))
  # (4) free-scaling N >= 2000
  free <- cpm_fit(cormat = P0, scales = paste0("V", 1:8), angles = tr$angles,
                  n = 5000, m = 3, scaling = "free")
  expect_snapshot(summary(free))
})

test_that("an analytic marker-firing fit names each fired label exactly once", {
  voc <- cpm_oracle_voc()
  hey <- suppressWarnings(cpm_fit(cormat = voc$R, scales = voc$names,
                                  angles = voc$th_start, n = 5000, m = 2))
  fired <- cpm_boundary_markers(hey)
  expect_gt(length(fired), 0)
  out <- paste(capture.output(summary(hey)), collapse = "\n")
  for (lab in fired) {
    hits <- gregexpr(lab, out, fixed = TRUE)[[1]]
    n_hits <- if (identical(as.integer(hits[1]), -1L)) 0L else length(hits)
    expect_identical(n_hits, 1L)
  }
})

# ---- AC1/AC2: the bootstrap fired-marker note -------------------------------

# The note's variable sentence is wrapped at emission time, so label and
# prefix assertions run on the whitespace-normalized block; the fixed caveat
# lines are additionally pinned raw (they are literals in the code).
m94_marker_block <- function(out) {
  start_pat <- "Note: boundary/weak-identification markers fired:"
  end_pat <- "'When a fit sits at a boundary')."
  start <- regexpr(start_pat, out, fixed = TRUE)
  end <- regexpr(end_pat, out, fixed = TRUE)
  if (start < 0 || end < 0) return(NA_character_)
  substr(out, start, end + nchar(end_pat) - 1L)
}

m94_caveat_raw <- paste0(
  "  What has been measured about these markers covers analytic intervals\n",
  "  only, and not every marker was measured; they are not validated as\n",
  "  predictors of the bootstrap intervals shown here (see the vignette\n",
  "  section 'When a fit sits at a boundary')."
)

# The note must sit inside the `# Diagnostics` section — including on fits
# where no diagnostic line fires and the note is the section's only content
# (review round 1, F2/F3): the header must precede the note with no other
# section header between them.
m94_expect_note_in_diagnostics <- function(out) {
  hdr <- regexpr("# Diagnostics", out, fixed = TRUE)
  start <- regexpr("Note: boundary/weak-identification markers fired:",
                   out, fixed = TRUE)
  expect_gt(hdr, 0)
  expect_gt(start, hdr)
  between <- substr(out, hdr + nchar("# Diagnostics"), start - 1L)
  expect_false(grepl("\n# ", between, fixed = TRUE))
}

test_that("bootstrap summary() prints the fired-marker note: >= 2 markers, N < 2000", {
  jz <- m94_boot_jz()
  expect_identical(jz$details$ci_method, "bootstrap")
  expect_lt(jz$details$N, 2000)
  fired <- cpm_boundary_markers(jz)
  expect_identical(fired, c("Heywood communality",
                            "small correlation-function weight",
                            "ill-conditioned Hessian"))
  out <- paste(capture.output(summary(jz)), collapse = "\n")
  m94_expect_note_in_diagnostics(out)
  block <- m94_marker_block(out)
  expect_false(is.na(block))
  norm <- gsub("\\s+", " ", block)
  # The full variable sentence, labels joined "; " in cpm_boundary_markers()
  # order — asserting the "; " join and every fired label at once.
  expect_match(norm, paste0(
    "Note: boundary/weak-identification markers fired: ",
    paste(fired, collapse = "; "), "."), fixed = TRUE)
  for (lab in setdiff(unname(cpm_marker_labels()), fired)) {
    expect_false(grepl(lab, block, fixed = TRUE))
  }
  expect_match(out, m94_caveat_raw, fixed = TRUE)
})

test_that("bootstrap summary() prints the fired-marker note: exactly 1 marker, N >= 2000", {
  big <- m94_boot_big()
  expect_identical(big$details$ci_method, "bootstrap")
  expect_gte(big$details$N, 2000)
  fired <- cpm_boundary_markers(big)
  expect_identical(fired, "small correlation-function weight")
  out <- paste(capture.output(summary(big)), collapse = "\n")
  # This fixture fires no diagnostic *line*, so it is exactly the case where
  # the note must still open the Diagnostics section (round-1 F2).
  expect_identical(length(cpm_diagnostic_lines(big$details)), 0L)
  m94_expect_note_in_diagnostics(out)
  block <- m94_marker_block(out)
  expect_false(is.na(block))
  norm <- gsub("\\s+", " ", block)
  expect_match(norm, paste0(
    "Note: boundary/weak-identification markers fired: ", fired, "."),
    fixed = TRUE)
  for (lab in setdiff(unname(cpm_marker_labels()), fired)) {
    expect_false(grepl(lab, block, fixed = TRUE))
  }
  expect_match(out, m94_caveat_raw, fixed = TRUE)
})

test_that("the marker note claims no interval consequence (banned phrases absent)", {
  jz <- m94_boot_jz()
  out <- paste(capture.output(summary(jz)), collapse = "\n")
  block <- m94_marker_block(out)
  expect_false(is.na(block))
  for (p in c("mis-cover", "near a parameter boundary",
              "no effect", "does not affect")) {
    expect_false(grepl(p, block, fixed = TRUE))
  }
})

test_that("a bootstrap fit with no fired marker prints no marker note", {
  cl <- m94_boot_clean()
  expect_identical(cl$details$ci_method, "bootstrap")
  expect_identical(cpm_boundary_markers(cl), character(0))
  out <- paste(capture.output(summary(cl)), collapse = "\n")
  expect_false(grepl("boundary/weak-identification markers fired",
                     out, fixed = TRUE))
})

# ---- AC3: bootstrap print() byte-identical to merge-base --------------------

test_that("print() on a bootstrap marker-firing fit is byte-identical to merge-base", {
  # Bootstrap CI endpoints differ across platforms at the 3rd decimal (BLAS),
  # so this snapshot is a local-only regression pin, per the test-cpm_api.R
  # bootstrap-render precedent.
  skip_on_ci()
  skip_on_cran()
  jz <- m94_boot_jz()
  expect_gt(length(cpm_boundary_markers(jz)), 0)
  expect_snapshot(print(jz))
})
