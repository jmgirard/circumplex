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
  if (length(hit) == 0L) {
    stop(
      "evaluating-circumplex-structure.Rmd found in neither the source tree ",
      "nor inst/doc; the boundary-guidance guards cannot run"
    )
  }
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
  ends <- which(grepl("^#{1,3} ", rest))
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

run_chunk <- function(label) {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    eval(parse(text = vignette_chunk(label)), envir = env)
  )
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
  env <- run_chunk("boundary_demo")
  expect_true(exists("demo", envir = env, inherits = FALSE))
  fired <- cpm_boundary_markers(get("demo", envir = env))
  # The prose reads this fit; the set it names is pinned here so a change to
  # either side fails rather than silently disagreeing.
  expect_setequal(
    fired,
    c("Heywood communality", "small correlation-function weight",
      "ill-conditioned Hessian")
  )
  section <- boundary_section_text()
  for (lab in fired) {
    expect_true(grepl(lab, section, fixed = TRUE), info = lab)
  }
})

test_that("the angle paragraph's pinned figures and ordering still hold", {
  # The displayed fit's point estimates do not depend on the bootstrap (the
  # analytic and bootstrap fits agree to 0 on Angle, checked 2026-08-16), but
  # the chunk is evaluated as written so a change to the example is caught.
  env <- run_chunk("cpm")
  res <- get("cpm", envir = env)$results

  # PA is the fixed reference, so every departure below is measured from it.
  expect_equal(get("cpm", envir = env)$details$reference, 1)
  expect_equal(res$Angle[[1]], 90, tolerance = 1e-8)

  dev <- ((res$Angle - res$Angle_theory + 180) %% 360) - 180
  expect_equal(max(abs(dev)), 65.8, tolerance = 0.05)
  expect_identical(as.character(res$Scale[[which.max(abs(dev))]]), "LM")

  # Eight circularly adjacent gaps, wrap-around included, against a
  # theoretical 45 degrees.
  sorted <- sort(res$Angle %% 360)
  gaps <- c(diff(sorted), 360 - (sorted[[length(sorted)]] - sorted[[1]]))
  expect_length(gaps, 8L)
  expect_equal(min(gaps), 18.8, tolerance = 0.05)
  expect_equal(max(gaps), 78.7, tolerance = 0.05)

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
  rd <- test_path("..", "..", "man", "summary.circumplex_cpm.Rd")
  skip_if_not(file.exists(rd), "man/ not present in this build")
  txt <- paste(readLines(rd, warn = FALSE), collapse = " ")
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
