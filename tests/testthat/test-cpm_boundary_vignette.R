# Guards for the boundary-regime guidance in
# vignettes/evaluating-circumplex-structure.Rmd. That section makes factual
# claims about fits the reader runs on the page, so these tests run the
# vignette's own chunks rather than a re-typed copy of them: prose and example
# cannot drift apart without a failure here.

vignette_path <- function() {
  # devtools::test() reads the source tree; R CMD check reads the built
  # package, where R CMD build has copied the vignette source into inst/doc.
  candidates <- c(
    test_path("..", "..", "vignettes", "evaluating-circumplex-structure.Rmd"),
    system.file("doc", "evaluating-circumplex-structure.Rmd",
                package = "circumplex")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  # Some builds install the package without vignettes -- covr does, which is
  # why these guards skipped rather than errored there. They run under
  # devtools::test() (source tree) and under R CMD check (inst/doc), which is
  # where they are meant to bite.
  skip_if(
    length(hit) == 0L,
    "vignette source unavailable in this build (installed without vignettes)"
  )
  hit[[1]]
}

boundary_heading <- "### When a fit sits at a boundary"

# The boundary subsection's own text: from its heading to the next heading of
# the same level (or the end of the file). Scoping is the point -- a marker
# label named anywhere else in the vignette must not satisfy the sweep below.
boundary_section_text <- function(lines = readLines(vignette_path(), warn = FALSE)) {
  start <- which(trimws(lines) == boundary_heading)
  expect_length(start, 1L)
  rest <- lines[seq.int(start + 1L, length(lines))]
  # A chunk's R comments also begin a line with "# ", so only headings outside
  # fenced code end the section.
  fenced <- cumsum(grepl("^```", rest)) %% 2L == 1L | grepl("^```", rest)
  ends <- which(grepl("^#{1,3} ", rest) & !fenced)
  stop_at <- if (length(ends) > 0L) ends[[1]] - 1L else length(rest)
  paste(rest[seq_len(stop_at)], collapse = "\n")
}

# Source of one labelled chunk, so a test can evaluate exactly what the reader
# sees rather than a paraphrase of it.
vignette_chunk <- function(label,
                           lines = readLines(vignette_path(), warn = FALSE)) {
  opens <- grep("^```\\{r[ ,}]", lines)
  labels <- sub("^```\\{r[ ,]*", "", sub("[,}].*$", "", lines[opens]))
  hit <- opens[trimws(labels) == label]
  expect_length(hit, 1L)
  closes <- grep("^```\\s*$", lines)
  end <- closes[closes > hit][[1]]
  lines[seq.int(hit + 1L, end - 1L)]
}

# The paragraph introduced by a given opening phrase, so an assertion can be
# aimed at the sentences that read a particular chunk rather than at the whole
# section (where a label named in the glossary would satisfy it vacuously).
section_paragraph <- function(opening, section = boundary_section_text()) {
  paras <- strsplit(section, "\n[[:space:]]*\n")[[1]]
  hit <- paras[grepl(opening, paras, fixed = TRUE)]
  expect_length(hit, 1L)
  hit[[1]]
}

# Chunks share one environment when the vignette is knitted, so a chunk that
# builds on an earlier one is run after it here too. `data()` writes to the
# global environment whatever envir the surrounding call uses, so anything it
# creates is removed again on exit.
run_chunks <- function(labels) {
  pre <- ls(globalenv())
  on.exit(rm(list = setdiff(ls(globalenv()), pre), envir = globalenv()))
  env <- new.env(parent = globalenv())
  warned <- character(0)
  for (label in labels) {
    utils::capture.output(
      warned <- c(warned, capture_warnings(
        eval(parse(text = vignette_chunk(label)), envir = env)
      ))
    )
  }
  # The chunks are slow (the displayed fit bootstraps 500 times), so the
  # warnings they raise ride along with the environment rather than costing a
  # second evaluation.
  attr(env, "warnings") <- warned
  env
}

test_that("the boundary section names every marker label the package prints", {
  section <- boundary_section_text()
  labels <- cpm_marker_labels()
  expect_gt(length(labels), 0L)
  for (lab in labels) {
    expect_true(
      grepl(lab, section, fixed = TRUE),
      info = paste0("marker label absent from the boundary section: ", lab)
    )
  }
})

test_that("the demonstration fit fires exactly the markers the section names", {
  env <- run_chunks(c("cpm", "boundary_demo"))
  expect_true(exists("demo", envir = env, inherits = FALSE))
  fired <- cpm_boundary_markers(get("demo", envir = env))
  # The prose reads this fit; the set it names is pinned here so a change to
  # either side fails rather than silently disagreeing.
  expect_setequal(fired, "small correlation-function weight")
  # Aimed at the paragraph that reads the demo, not the whole section -- the
  # glossary names every label, so a section-wide search would pass vacuously.
  para <- section_paragraph("One marker fires here")
  for (lab in fired) {
    expect_true(grepl(lab, para, fixed = TRUE), info = lab)
  }
  # ...and the paragraph must not name a marker this fit did not fire.
  for (lab in setdiff(cpm_marker_labels(), fired)) {
    expect_false(grepl(lab, para, fixed = TRUE), info = lab)
  }
})

test_that("the marker-list locus paragraph states the shipped printing behavior", {
  # M94 moved the fired-marker list onto the bootstrap path; the paragraph
  # that says where the list prints must say so, and the pre-M94 claim (the
  # bootstrap path shows "the individual diagnostic notes rather than the
  # list") must be gone.
  para <- section_paragraph("`summary()` prints that list when")
  expect_false(grepl("rather than the list", para, fixed = TRUE))
  # Source-wrapped prose: assert on the whitespace-normalized paragraph so a
  # re-wrap cannot fail these for a non-reason.
  norm <- gsub("\\s+", " ", para)
  expect_match(norm, "descriptive note at every sample size", fixed = TRUE)
  expect_match(norm,
               "validated as interval predictors on the analytic path only",
               fixed = TRUE)
})

test_that("the displayed fit still shows what the section's opening reads", {
  # The section opens by reading this fit: a Heywood case at NO with a
  # zero-width interval, and an ill-conditioning warning from the same chunk.
  # Without these pins the whole premise could go stale silently.
  env <- run_chunks("cpm")
  fit <- get("cpm", envir = env)
  expect_true(isTRUE(fit$details$heywood))
  expect_true("Heywood communality" %in% cpm_boundary_markers(fit))

  no <- which(as.character(fit$results$Scale) == "NO")
  expect_length(no, 1L)
  expect_lt(abs(fit$results$Zeta[[no]] - 1), 1e-6)
  expect_identical(fit$results$Zeta_lci[[no]], fit$results$Zeta_uci[[no]])

  # The warning the prose says the chunk emits, asserted as the condition it
  # is rather than as a bare failure.
  expect_true(
    any(grepl("Hessian is ill-conditioned", attr(env, "warnings"), fixed = TRUE))
  )
})

test_that("the angle paragraph's pinned figures and ordering still hold", {
  # The displayed fit's point estimates do not depend on the bootstrap (the
  # analytic and bootstrap fits agree to 0 on Angle, checked 2026-08-16), but
  # the chunk is evaluated as written so a change to the example is caught.
  env <- run_chunks("cpm")
  res <- get("cpm", envir = env)$results

  # PA is the fixed reference, so every departure below is measured from it.
  # The prose names PA specifically, so the scale name is pinned, not just the
  # index.
  fit <- get("cpm", envir = env)
  expect_equal(fit$details$reference, 1)
  expect_identical(as.character(fit$details$scales[[fit$details$reference]]), "PA")
  expect_equal(res$Angle[[1]], 90, tolerance = 1e-8)

  # Absolute tolerances, in degrees: testthat's `tolerance` is relative, so
  # expect_equal(78.7, tolerance = 0.05) would pass at 82.0 -- four degrees of
  # silent drift in a figure the prose quotes. Pinned against the measured
  # values rather than the prose's rounded ones, so the margin is the whole
  # tolerance rather than whatever rounding left over.
  dev <- ((res$Angle - res$Angle_theory + 180) %% 360) - 180
  expect_lt(abs(max(abs(dev)) - 65.77), 0.05)
  expect_identical(as.character(res$Scale[[which.max(abs(dev))]]), "LM")

  # Eight circularly adjacent gaps, wrap-around included, against a
  # theoretical 45 degrees.
  sorted <- sort(res$Angle %% 360)
  gaps <- c(diff(sorted), 360 - (sorted[[length(sorted)]] - sorted[[1]]))
  expect_length(gaps, 8L)
  expect_lt(abs(min(gaps) - 18.77), 0.05)
  expect_lt(abs(max(gaps) - 78.70), 0.05)

  # The claim the old bullet got right: the circumplex ordering is preserved.
  # Order around a circle has no first element, so the estimated sequence need
  # only be a rotation of the theoretical one -- an identical-sequence test
  # would fail on nothing worse than where the wrap happens to fall.
  est_order <- as.character(res$Scale[order(res$Angle)])
  theory_order <- as.character(res$Scale[order(res$Angle_theory)])
  rotations <- vapply(
    seq_along(theory_order),
    function(k) {
      idx <- (seq_along(theory_order) + k - 2L) %% length(theory_order) + 1L
      identical(est_order, theory_order[idx])
    },
    logical(1)
  )
  expect_true(any(rotations))
})

test_that("the summary help page points at the boundary section", {
  # man/ exists in the source tree but not in an installed package, where the
  # help text lives in the Rd database instead. Reading whichever is present
  # keeps this guard live under R CMD check rather than skipping there.
  rd <- test_path("..", "..", "man", "summary.circumplex_cpm.Rd")
  txt <- if (file.exists(rd)) {
    paste(readLines(rd, warn = FALSE), collapse = " ")
  } else {
    db <- tools::Rd_db("circumplex")
    entry <- db[["summary.circumplex_cpm.Rd"]]
    expect_false(is.null(entry))
    paste(as.character(entry), collapse = " ")
  }
  heading <- sub("^#+ ", "", boundary_heading)
  expect_true(
    grepl(heading, txt, fixed = TRUE),
    info = "summary.circumplex_cpm.Rd does not name the boundary section"
  )
  # And the heading it names is really in the vignette.
  expect_true(
    any(trimws(readLines(vignette_path(), warn = FALSE)) == boundary_heading)
  )
})
