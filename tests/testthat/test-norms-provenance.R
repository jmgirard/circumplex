# Norms provenance pins (M72).
#
# These lock every audited field of every instrument whose norms have been
# verified against a published source, so a later silent edit to a shipped
# norm value fails here rather than reaching users. The audited values and
# their source anchors live in cairn/references/<citekey>.md; the comparison
# that established them is data-raw/audit-norms.R.

# The shipped instruments are enumerated by the same procedure instruments()
# uses -- data() plus a class filter -- rather than a hand-list, so a newly
# added instrument is caught by the completeness test below instead of
# silently going unpinned. (instruments() itself prints and returns NULL, so
# its return value cannot be used here.)
shipped_instruments <- function() {
  nms <- utils::data(package = "circumplex")$results[, "Item"]
  sort(Filter(function(nm) {
    e <- new.env()
    utils::data(list = nm, package = "circumplex", envir = e)
    inherits(get(nm, envir = e), "circumplex_instrument")
  }, nms))
}

audited <- list(
  csie = list(
    scale      = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    M          = c(7.23, 6.44, 6.93, 7.24, 7.31, 8.51, 7.9, 7.3),
    SD         = c(1.68, 1.66, 1.82, 1.54, 1.53, 1.11, 1.2, 1.37),
    norm_angle = c(90, 135, 180, 225, 270, 315, 360, 45),
    abbrev     = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    angle      = c(90, 135, 180, 225, 270, 315, 360, 45),
    items      = c("4, 12, 20, 28", "7, 15, 23, 31", "2, 10, 18, 26", "5, 13, 21, 29", "8, 16, 24, 32", "3, 11, 19, 27", "6, 14, 22, 30", "1,  9, 17, 25"),
    size       = 367L,
    population = "American college students",
    reference  = "Locke & Sadler (2007)",
    url        = "https://kennethlocke.org/CSIE/CSIE_Norms.html"
  ),
  csig = list(
    scale      = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    M          = c(2.96, 2.53, 2.02, 1.88, 2.24, 2.89, 2.97, 2.96),
    SD         = c(0.68, 0.86, 0.88, 0.74, 0.9, 0.76, 0.71, 0.68),
    norm_angle = c(90, 135, 180, 225, 270, 315, 360, 45),
    abbrev     = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    angle      = c(90, 135, 180, 225, 270, 315, 360, 45),
    items      = c("8, 16, 24, 32", "5, 13, 21, 29", "2, 10, 18, 26", "7, 15, 23, 31", "4, 12, 20, 28", "1, 9, 17, 25", "6, 14, 22, 30", "3, 11, 19, 27"),
    size       = 665L,
    population = "MTurkers from US, Canada, and India about interactions between nations",
    reference  = "Locke (2014)",
    url        = "https://doi.org/10.1177/0146167213514280"
  ),
  csip = list(
    scale      = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    M          = c(0.375, 0.4, 0.7, 0.9, 0.8875, 0.8125, 0.925, 0.5875),
    SD         = c(0.4875, 0.475, 0.6375, 0.6875, 0.6375, 0.575, 0.5875, 0.5),
    norm_angle = c(90, 135, 180, 225, 270, 315, 360, 45),
    abbrev     = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    angle      = c(90, 135, 180, 225, 270, 315, 360, 45),
    items      = c("1, 9, 17, 25, 33, 41, 49, 57", "2, 10, 18, 26, 34, 42, 50, 58", "3, 11, 19, 27, 35, 43, 51, 59", "4, 12, 20, 28, 36, 44, 52, 60", "5, 13, 21, 29, 37, 45, 53, 61", "6, 14, 22, 30, 38, 46, 54, 62", "7, 15, 23, 31, 39, 47, 55, 63", "8, 16, 24, 32, 40, 48, 56, 64"),
    size       = 712L,
    population = "American college students",
    reference  = "Boudreaux, Ozer, Oltmanns, & Wright (2018)",
    url        = "https://doi.org/10.1037/pas0000505"
  ),
  csiv = list(
    scale      = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    M          = c(2.53, 1.38, 1.1, 1.66, 1.77, 2.67, 2.83, 2.93),
    SD         = c(0.63, 0.71, 0.7, 0.78, 0.75, 0.71, 0.69, 0.57),
    norm_angle = c(90, 135, 180, 225, 270, 315, 360, 45),
    abbrev     = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    angle      = c(90, 135, 180, 225, 270, 315, 360, 45),
    items      = c("1,  9, 17, 25, 33, 41, 49, 57", "4, 12, 20, 28, 36, 44, 52, 60", "7, 15, 23, 31, 39, 47, 55, 63", "2, 10, 18, 26, 34, 42, 50, 58", "5, 13, 21, 29, 37, 45, 53, 61", "8, 16, 24, 32, 40, 48, 56, 64", "3, 11, 19, 27, 35, 43, 51, 59", "6, 14, 22, 30, 38, 46, 54, 62"),
    size       = 1200L,
    population = "American college students",
    reference  = "Locke (n.d.); instrument published as Locke (2000)",
    url        = "https://kennethlocke.org/CSIV/CSIV_Norms.html"
  ),
  iitc = list(
    scale      = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    M          = c(1.18, 0.83, 0.76, 0.85, 1.24, 2.13, 2.66, 1.88),
    SD         = c(0.81, 0.77, 0.73, 0.74, 0.78, 0.86, 0.85, 1.02),
    norm_angle = c(90, 135, 180, 225, 270, 315, 360, 45),
    abbrev     = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    angle      = c(90, 135, 180, 225, 270, 315, 360, 45),
    items      = c(" 1,  9, 17, 25, 33, 41, 49, 57", " 2, 10, 18, 26, 34, 42, 50, 58", " 3, 11, 19, 27, 35, 43, 51, 59", " 4, 12, 20, 28, 36, 44, 52, 60", " 5, 13, 21, 29, 37, 45, 53, 61", " 6, 14, 22, 30, 38, 46, 54, 62", " 7, 15, 23, 31, 39, 47, 55, 63", " 8, 16, 24, 32, 40, 48, 56, 64"),
    size       = 862L,
    population = "American college students",
    reference  = "Bliton & Pincus (2019)",
    url        = "https://doi.org/10.1177/1073191119864661"
  )
)

test_that("audited instruments' norm values match their verified sources (M72)", {
  for (inst in names(audited)) {
    exp <- audited[[inst]]
    obj <- get(inst)
    norms <- obj$Norms[[1]]
    src <- obj$Norms[[2]]
    scales <- obj$Scales
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"

    expect_identical(as.character(norms[[key]]), exp$scale, info = inst)
    expect_equal(norms$M, exp$M, info = inst)
    expect_equal(norms$SD, exp$SD, info = inst)
    expect_equal(norms$Angle, exp$norm_angle, info = inst)

    expect_identical(as.character(scales$Abbrev), exp$abbrev, info = inst)
    expect_equal(scales$Angle, exp$angle, info = inst)
    expect_identical(as.character(scales$Items), exp$items, info = inst)

    # Size and Population print via norms(), so they are pinned too: an edit
    # to either is user-visible and must not pass silently.
    expect_identical(as.integer(src$Size), exp$size, info = inst)
    expect_identical(as.character(src$Population), exp$population, info = inst)

    # Provenance. csie/csiv were repointed at M72 because their cited articles
    # publish no octant statistics; these pins keep them from drifting back.
    expect_identical(as.character(src$Reference), exp$reference, info = inst)
    expect_identical(as.character(src$URL), exp$url, info = inst)
  }
})

test_that("the two shipped copies of each scale angle agree (M72)", {
  # Scales$Angle and Norms[[1]]$Angle are independent copies of one fact;
  # nothing in the package makes them agree, so an edit to one can desync them.
  for (inst in names(audited)) {
    obj <- get(inst)
    norms <- obj$Norms[[1]]
    scales <- obj$Scales
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    j <- match(norms[[key]], scales$Abbrev)
    expect_equal(norms$Angle %% 360, scales$Angle[j] %% 360, info = inst)
  }
})

test_that("every audited instrument is a shipped instrument (M72)", {
  # Runtime half: works against the installed package, so it runs on CRAN.
  expect_true(all(names(audited) %in% shipped_instruments()))
})

test_that("norms-audit.md lists every shipped instrument (M72)", {
  # DEVELOPMENT-ONLY half: cairn/ is repo tracking, not installed, so this
  # cannot run under R CMD check. It is split out from the runtime assertions
  # above deliberately -- a whole-test skip here would silently take the pins
  # with it (the M70 lesson).
  status <- testthat::test_path("..", "..", "cairn", "references", "norms-audit.md")
  skip_if_not(file.exists(status), "cairn/ not present (installed package)")

  lines <- readLines(status, warn = FALSE)
  for (inst in shipped_instruments()) {
    expect_true(
      any(grepl(paste0("^\\| ", inst, " \\|"), lines)),
      info = paste(inst, "missing from norms-audit.md status table")
    )
  }
})
