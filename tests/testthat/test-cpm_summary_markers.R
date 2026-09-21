# M94: the bootstrap fired-marker line in summary(), and its fences.
#
# Snapshot fences (AC3 of M94): the analytic-path summary() output and the
# bootstrap print() output are pinned as snapshots. M94 captured them
# byte-identical to the merge-base of its branch. M133 re-captured them after
# shortening the results-table headers, so they now pin the M133 layout, and
# expect_cpm_table_one_block() checks that the table's values are unchanged.
# Analytic fits use the deterministic cormat path (no RNG); expect_snapshot()
# does not run on CRAN by design.

m94_labels <- function() c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

# Eight scale names of exactly 16 characters (M146 width fixture).
m146_long_names <- function() sprintf("LongScaleName_%s", m94_labels())

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

# ---- AC3: analytic path snapshot fence ---------------------------------------

test_that("analytic summary() output matches its snapshot (four regimes)", {
  # The snapshot is a same-machine claim: the committed baselines were
  # captured on the authoring machine, and PR #123's CI
  # measured the cross-platform deltas directly — a third-decimal communality
  # (0.562 vs 0.563 on ubuntu/windows) and the residual tie-break pair of the
  # saturated free fit — while macOS matched. covr perturbs optimizer results
  # the same way (M59). So this fence runs where its baseline was captured,
  # like the bootstrap print() fence below.
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  tr <- cpm_clean_truth()
  P0 <- m94_clean_P0()
  # (1) clean N >= 2000
  clean <- cpm_fit(cormat = P0, scales = paste0("V", 1:8), angles = tr$angles,
                   n = 5000, m = 3)
  expect_cpm_table_one_block(clean, summary)
  expect_snapshot(summary(clean))
  # (2) marker-firing N >= 2000 (Heywood): warnings at fit time, not display time
  voc <- cpm_oracle_voc()
  hey <- suppressWarnings(cpm_fit(cormat = voc$R, scales = voc$names,
                                  angles = voc$th_start, n = 5000, m = 2))
  expect_true(cpm_boundary_proximity(hey))
  expect_cpm_table_one_block(hey, summary)
  expect_snapshot(summary(hey))
  # (3) N < 2000
  small <- cpm_fit(cormat = P0, scales = paste0("V", 1:8), angles = tr$angles,
                   n = 300, m = 3)
  expect_cpm_table_one_block(small, summary)
  expect_snapshot(summary(small))
  # (4) free-scaling N >= 2000
  free <- cpm_fit(cormat = P0, scales = paste0("V", 1:8), angles = tr$angles,
                  n = 5000, m = 3, scaling = "free")
  expect_cpm_table_one_block(free, summary)
  expect_snapshot(summary(free))
})

test_that("an analytic marker-firing fit names each fired label exactly once", {
  skip_on_cran()
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

# The whole note now wraps to the reader's console width, so where its line
# breaks fall depends on that width and no assertion may depend on them. The
# block is located and returned with its whitespace collapsed, and every
# assertion below runs on that. This pins the words and their order, which is
# what the note promises, and it makes the negative assertions stronger: a
# banned phrase broken across two lines is now found, where a raw search
# would have missed it.
#
# Some assertions still read the raw output. Five of them check a property of
# the layout, which collapsing whitespace would erase: the note sits in the
# Diagnostics section, no blank line is doubled, the header and the note are
# separated by exactly one blank line, no fired marker label is split across a
# line break, and every line of the trailing caveat keeps its two-space
# continuation leader (m94_expect_caveat_indent(), added by M131). Two others read raw output for a different reason. The
# snapshot records the whole rendering, and the no-note check looks for the
# absence of the note's opening clause, which never splits because it is
# handed to wrap_prose() as one atomic unit.
m94_marker_block <- function(out) {
  flat <- gsub("\\s+", " ", out)
  start_pat <- "Note: boundary/weak-identification markers fired:"
  end_pat <- "'When a fit sits at a boundary')."
  start <- regexpr(start_pat, flat, fixed = TRUE)
  end <- regexpr(end_pat, flat, fixed = TRUE)
  if (start < 0 || end < 0) return(NA_character_)
  substr(flat, start, end + nchar(end_pat) - 1L)
}

# Raw-output check: every line of the caveat carries the two-space continuation
# leader. Collapsing whitespace erases indentation, so m94_caveat_words cannot
# see this and nothing outside the snapshots pinned it (M131 review, O9). The
# leader is what keeps the caveat inside the note it belongs to, and
# wrap_prose() counts it against the width rather than adding it on top, so a
# lost leader is a real layout change, not a cosmetic one.
m94_expect_caveat_indent <- function(out) {
  lines <- unlist(strsplit(out, "\n", fixed = TRUE))
  first <- grep("What has been measured about these markers", lines, fixed = TRUE)
  expect_length(first, 1L)
  # The caveat's end is found by consuming lines until they carry all of its
  # words, because its closing phrase straddles a line break at most widths
  # and matching it raw would find nothing.
  squash <- function(x) paste(unlist(strsplit(x, "[ \t]+")), collapse = " ")
  caveat <- character(0)
  for (i in seq(first[[1]], length(lines))) {
    caveat <- c(caveat, lines[[i]])
    if (grepl(m94_caveat_words, squash(paste(caveat, collapse = " ")),
              fixed = TRUE)) {
      break
    }
  }
  expect_match(squash(paste(caveat, collapse = " ")), m94_caveat_words,
               fixed = TRUE)
  expect_gte(length(caveat), 2L)
  for (line in caveat) {
    expect_match(
      line, "^  [^ ]",
      info = paste0("caveat line lost its leader: ", line)
    )
  }
}

m94_caveat_words <- paste(
  "What has been measured about these markers covers analytic intervals",
  "only, and not every marker was measured; they are not validated as",
  "predictors of the bootstrap intervals shown here (see the vignette",
  "section 'When a fit sits at a boundary')."
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
  # Byte-level spacing pin (round-2 finding 1/3): exactly one blank line
  # everywhere — never a doubled one — so the seam between the header (or a
  # preceding diagnostic line) and the note renders like every other section.
  expect_false(grepl("\n\n\n", out, fixed = TRUE))
}

test_that("bootstrap summary() prints the fired-marker note: >= 2 markers, N < 2000", {
  skip_on_cran()
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
  # Raw-output (un-normalized) check: no fired label is split by the wrap.
  for (lab in fired) {
    expect_match(out, lab, fixed = TRUE)
  }
  expect_match(block, m94_caveat_words, fixed = TRUE)
  m94_expect_caveat_indent(out)
})

test_that("bootstrap summary() prints the fired-marker note: exactly 1 marker, N >= 2000", {
  skip_on_cran()
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
  # Byte pin of the note-opens-the-section seam: header, one blank line, note.
  expect_match(out,
               "# Diagnostics\n\n  Note: boundary/weak-identification",
               fixed = TRUE)
  block <- m94_marker_block(out)
  expect_false(is.na(block))
  norm <- gsub("\\s+", " ", block)
  expect_match(norm, paste0(
    "Note: boundary/weak-identification markers fired: ", fired, "."),
    fixed = TRUE)
  for (lab in setdiff(unname(cpm_marker_labels()), fired)) {
    expect_false(grepl(lab, block, fixed = TRUE))
  }
  # Raw-output (un-normalized) check: no fired label is split by the wrap.
  for (lab in fired) {
    expect_match(out, lab, fixed = TRUE)
  }
  expect_match(block, m94_caveat_words, fixed = TRUE)
  m94_expect_caveat_indent(out)
})

test_that("the marker note claims no interval consequence (banned phrases absent)", {
  skip_on_cran()
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
  skip_on_cran()
  cl <- m94_boot_clean()
  expect_identical(cl$details$ci_method, "bootstrap")
  expect_identical(cpm_boundary_markers(cl), character(0))
  out <- paste(capture.output(summary(cl)), collapse = "\n")
  expect_false(grepl("boundary/weak-identification markers fired",
                     out, fixed = TRUE))
})

# ---- AC3: bootstrap print() snapshot fence ----------------------------------

test_that("print() on a bootstrap marker-firing fit matches its snapshot", {
  # Bootstrap CI endpoints differ across platforms at the 3rd decimal (BLAS),
  # so this snapshot is a local-only regression pin, per the test-cpm_api.R
  # bootstrap-render precedent.
  skip_on_ci()
  skip_on_cran()
  jz <- m94_boot_jz()
  expect_gt(length(cpm_boundary_markers(jz)), 0)
  expect_cpm_table_one_block(jz, print)
  expect_snapshot(print(jz))
})

# ---- M133: one-block results table for every fixture in this file -----------

test_that("print() and summary() show the results table as one block for every fixture", {
  # The check compares the printed table with the same object's rounded
  # results, so it does not depend on platform numerics and runs on CI.
  skip_on_cran()
  tr <- cpm_clean_truth()
  P0 <- m94_clean_P0()
  voc <- cpm_oracle_voc()
  fits <- list(
    clean = cpm_fit(cormat = P0, scales = paste0("V", 1:8),
                    angles = tr$angles, n = 5000, m = 3),
    hey = suppressWarnings(cpm_fit(cormat = voc$R, scales = voc$names,
                                   angles = voc$th_start, n = 5000, m = 2)),
    small = cpm_fit(cormat = P0, scales = paste0("V", 1:8),
                    angles = tr$angles, n = 300, m = 3),
    free = cpm_fit(cormat = P0, scales = paste0("V", 1:8),
                   angles = tr$angles, n = 5000, m = 3, scaling = "free"),
    # M146: eight 16-character names, free scaling, analytic intervals. With
    # Communality printed this table reached 84 columns and split at 77.
    free_long = cpm_fit(cormat = P0, scales = m146_long_names(),
                        angles = tr$angles, n = 5000, m = 3,
                        scaling = "free", ci_method = "analytic"),
    boot_jz = m94_boot_jz(),
    boot_big = m94_boot_big(),
    boot_clean = m94_boot_clean()
  )
  # The long-name fixture is the width case only if its names are 16
  # characters and its intervals are computed, so both are asserted here.
  long <- fits$free_long
  expect_true(all(nchar(long$results$Scale) == 16))
  free_rows <- seq_len(nrow(long$results)) != long$details$reference
  expect_true(all(is.finite(long$results$Angle_lci[free_rows])))
  expect_true(all(is.finite(long$results$Zeta_lci)))
  for (nm in names(fits)) {
    for (printer in list(print = print, summary = summary)) {
      expect_cpm_table_one_block(fits[[nm]], printer)
    }
  }
})

# ---- M146: the Heywood clause and the marker note at every width -------------
#
# m94_boot_jz() fires the Heywood diagnostic line and three markers, among them
# "small correlation-function weight", the longest label. Both notes are swept
# over widths 30 to 120.

test_that("the Heywood note never breaks inside its zeta clause (30 to 120)", {
  skip_on_cran()
  jz <- m94_boot_jz()
  expect_true(isTRUE(jz$details$heywood))
  unit <- "(ζ > 0.995,"
  old <- options(width = 80)
  on.exit(options(old), add = TRUE)
  for (w in 30:120) {
    options(width = w)
    for (printer in list(print, summary)) {
      out <- capture.output(printer(jz))
      expect_true(any(grepl(unit, out, fixed = TRUE)),
                  info = paste("width", w))
    }
  }
})

test_that("the marker note stays inside the width except for a lone label", {
  skip_on_cran()
  jz <- m94_boot_jz()
  fired <- cpm_boundary_markers(jz)
  expect_true("small correlation-function weight" %in% fired)
  labels <- c(paste0(fired, ";"), paste0(fired, "."))
  old <- options(width = 80)
  on.exit(options(old), add = TRUE)
  for (w in 30:120) {
    options(width = w)
    out <- capture.output(summary(jz))
    # "Note:" may end the line before this word at narrow widths.
    first <- grep("boundary/weak-identification", out, fixed = TRUE)
    if (length(first) == 1 && !grepl("Note:", out[first], fixed = TRUE)) {
      first <- first - 1L
    }
    last <- grep("What has been measured", out, fixed = TRUE)
    expect_length(first, 1)
    expect_length(last, 1)
    block <- out[first:(last - 1L)]
    wide <- block[nchar(block, type = "width") > w]
    # A line may pass the width only when it holds one whole marker label.
    expect_true(all(trimws(wide) %in% labels), info = paste("width", w))
    # The opening clause breaks between words once it cannot fit whole.
    if (w < 51) {
      expect_false(any(grepl(
        "Note: boundary/weak-identification markers fired:", out,
        fixed = TRUE
      )), info = paste("width", w))
    }
  }
})
