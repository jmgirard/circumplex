# Norms provenance pins (M72).
#
# These lock the FULL Norms and Scales objects of every instrument whose norms
# have been verified against a published source, so a later silent edit to any
# shipped value -- a mean, an angle, an item map, the sample size, the
# population label, the provenance reference or URL, or the Sample key that
# joins the two norms frames -- fails here rather than reaching users.
#
# Traceability: every value below is a row of data-raw/norms-audit-ledger.csv,
# which carries the source value and its page/table anchor for each audited
# field, or a value the ledger records as not-published-in-source or as an
# approved intended-deviation. The source anchors themselves live in
# cairn/references/<citekey>.md; the comparison that established them is
# data-raw/audit-norms.R. Pinning the whole object rather than a field list is
# deliberate: a field list pins what its author remembered to name.

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

audited_objects <- list(
  csie = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(7.23, 6.44, 6.93, 7.24, 7.31, 
      8.51, 7.9, 7.3), SD = c(1.68, 1.66, 1.82, 1.54, 1.53, 1.11, 1.2, 
      1.37)), class = "data.frame", row.names = c(NA, -8L)), structure(list(
          Sample = 1, Size = 367, Population = "American college students", 
          Reference = "Locke & Sadler (2007)", URL = "https://kennethlocke.org/CSIE/CSIE_Norms.html"), class = "data.frame", row.names = c(NA, 
      -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c("4, 12, 20, 28", "7, 15, 23, 31", "2, 10, 18, 26", 
          "5, 13, 21, 29", "8, 16, 24, 32", "3, 11, 19, 27", "6, 14, 22, 30", 
          "1,  9, 17, 25"), Label = c("+A", "+A-C", "-C", "-A-C", "-A", 
          "-A+C", "+C", "+A+C")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  csig = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(2.96, 2.53, 2.02, 1.88, 2.24, 
      2.89, 2.97, 2.96), SD = c(0.68, 0.86, 0.88, 0.74, 0.9, 0.76, 
      0.71, 0.68)), class = "data.frame", row.names = c(NA, -8L)), 
          structure(list(Sample = 1, Size = 665, Population = "MTurkers from US, Canada, and India about interactions between nations", 
              Reference = "Locke (2014)", URL = "https://doi.org/10.1177/0146167213514280"), class = "data.frame", row.names = c(NA, 
          -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c("8, 16, 24, 32", "5, 13, 21, 29", "2, 10, 18, 26", 
          "7, 15, 23, 31", "4, 12, 20, 28", "1, 9, 17, 25", "6, 14, 22, 30", 
          "3, 11, 19, 27"), Label = c("Be authoritative", "Be tough", 
          "Be self-protective", "Be wary", "Be conflict-avoidant", 
          "Be cooperative", "Be understanding", "Be respected")), class = "data.frame", row.names = c(NA, 
      -8L))
  ),
  csip = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Scale = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(0.375, 0.4, 0.7, 0.9, 0.8875, 
      0.8125, 0.925, 0.5875), SD = c(0.4875, 0.475, 0.6375, 0.6875, 
      0.6375, 0.575, 0.5875, 0.5)), class = "data.frame", row.names = c(NA, 
      -8L)), structure(list(Sample = 1, Size = 712, Population = "American college students", 
          Reference = "Boudreaux, Ozer, Oltmanns, & Wright (2018)", 
          URL = "https://doi.org/10.1037/pas0000505"), class = "data.frame", row.names = c(NA, 
      -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c("1, 9, 17, 25, 33, 41, 49, 57", "2, 10, 18, 26, 34, 42, 50, 58", 
          "3, 11, 19, 27, 35, 43, 51, 59", "4, 12, 20, 28, 36, 44, 52, 60", 
          "5, 13, 21, 29, 37, 45, 53, 61", "6, 14, 22, 30, 38, 46, 54, 62", 
          "7, 15, 23, 31, 39, 47, 55, 63", "8, 16, 24, 32, 40, 48, 56, 64"
          ), Label = c("Domineering", "Self-Centered", "Distant", "Socially Inhibited", 
          "Nonassertive", "Exploitable", "Self-Sacrificing", "Intrusive"
          )), class = "data.frame", row.names = c(NA, -8L))
  ),
  csiv = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(2.53, 1.38, 1.1, 1.66, 1.77, 
      2.67, 2.83, 2.93), SD = c(0.63, 0.71, 0.7, 0.78, 0.75, 0.71, 
      0.69, 0.57)), class = "data.frame", row.names = c(NA, -8L)), 
          structure(list(Sample = 1, Size = 1200, Population = "American college students", 
              Reference = "Locke (n.d.); instrument published as Locke (2000)", 
              URL = "https://kennethlocke.org/CSIV/CSIV_Norms.html"), class = "data.frame", row.names = c(NA, 
          -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c("1,  9, 17, 25, 33, 41, 49, 57", "4, 12, 20, 28, 36, 44, 52, 60", 
          "7, 15, 23, 31, 39, 47, 55, 63", "2, 10, 18, 26, 34, 42, 50, 58", 
          "5, 13, 21, 29, 37, 45, 53, 61", "8, 16, 24, 32, 40, 48, 56, 64", 
          "3, 11, 19, 27, 35, 43, 51, 59", "6, 14, 22, 30, 38, 46, 54, 62"
          ), Label = c("+A", "+A-C", "-C", "-A-C", "-A", "-A+C", "+C", 
          "+A+C")), class = "data.frame", row.names = c(NA, -8L))
  ),
  iitc = list(
    Norms = 
      list(structure(list(Sample = c(1, 1, 1, 1, 1, 1, 1, 1), Abbrev = c("PA", 
      "BC", "DE", "FG", "HI", "JK", "LM", "NO"), Angle = c(90, 135, 
      180, 225, 270, 315, 360, 45), M = c(1.18, 0.83, 0.76, 0.85, 1.24, 
      2.13, 2.66, 1.88), SD = c(0.81, 0.77, 0.73, 0.74, 0.78, 0.86, 
      0.85, 1.02)), class = "data.frame", row.names = c(NA, -8L)), 
          structure(list(Sample = 1, Size = 862, Population = "American college students", 
              Reference = "Bliton & Pincus (2019)", URL = "https://doi.org/10.1177/1073191119864661"), class = "data.frame", row.names = c(NA, 
          -1L)))
    ,
    Scales = 
      structure(list(Abbrev = c("PA", "BC", "DE", "FG", "HI", "JK", 
      "LM", "NO"), Angle = c(90, 135, 180, 225, 270, 315, 360, 45), 
          Items = c(" 1,  9, 17, 25, 33, 41, 49, 57", " 2, 10, 18, 26, 34, 42, 50, 58", 
          " 3, 11, 19, 27, 35, 43, 51, 59", " 4, 12, 20, 28, 36, 44, 52, 60", 
          " 5, 13, 21, 29, 37, 45, 53, 61", " 6, 14, 22, 30, 38, 46, 54, 62", 
          " 7, 15, 23, 31, 39, 47, 55, 63", " 8, 16, 24, 32, 40, 48, 56, 64"
          ), Label = c("Dominant", "Calculating", "Cold", "Self-Critical", 
          "Submissive", "Ingratiating", "Warm", "Gregarious")), class = "data.frame", row.names = c(NA, 
      -8L))
  )
)

test_that("audited instruments' norms and scales match their verified sources (M72)", {
  for (inst in names(audited_objects)) {
    exp <- audited_objects[[inst]]
    obj <- get(inst)
    expect_equal(obj$Norms, exp$Norms, info = inst)
    expect_equal(obj$Scales, exp$Scales, info = inst)
  }
})

test_that("the two shipped copies of each scale angle agree (M72)", {
  # Scales$Angle and Norms[[1]]$Angle are independent copies of one fact;
  # nothing in the package makes them agree, so an edit to one can desync them.
  for (inst in names(audited_objects)) {
    obj <- get(inst)
    norms <- obj$Norms[[1]]
    scales <- obj$Scales
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    j <- match(norms[[key]], scales$Abbrev)
    expect_false(anyNA(j), info = inst)
    expect_equal(norms$Angle %% 360, scales$Angle[j] %% 360, info = inst)
  }
})

test_that("shipped angles follow the LM = 360 convention (M72)", {
  # DESIGN.md IP2: degrees in the user API run in (0, 360] with LM = 360, never
  # 0. The audit compares against the source modulo 360, which cannot see this.
  for (inst in names(audited_objects)) {
    obj <- get(inst)
    for (a in list(obj$Norms[[1]]$Angle, obj$Scales$Angle)) {
      expect_false(anyNA(a), info = inst)
      expect_true(all(a > 0 & a <= 360), info = inst)
    }
  }
})

test_that("every audited instrument is a shipped instrument (M72)", {
  # Runtime half: works against the installed package, so it runs on CRAN.
  expect_true(all(names(audited_objects) %in% shipped_instruments()))
})

test_that("norms-audit.md lists every shipped instrument (M72)", {
  # DEVELOPMENT-ONLY half: cairn/ is repo tracking, not installed, so this
  # cannot run under R CMD check. It is split out from the runtime assertions
  # above deliberately -- a whole-test skip here would silently take the pins
  # with it (the M70 lesson).
  status <- testthat::test_path("..", "..", "cairn", "references", "norms-audit.md")
  skip_if_not(file.exists(status), "cairn/ not present (installed package)")

  lines <- readLines(status, warn = FALSE)
  # Scoped to the status table's own section: every audited instrument also
  # appears in the citekey map further down, so a file-wide search passes over
  # a deleted status row.
  starts <- grep("^## ", lines)
  from <- grep("^## Audit status", lines)
  expect_length(from, 1L)
  to <- c(starts[starts > from], length(lines) + 1L)[[1]]
  section <- lines[(from + 1L):(to - 1L)]

  for (inst in shipped_instruments()) {
    expect_true(
      any(grepl(paste0("^\\| ", inst, " \\|"), section)),
      info = paste(inst, "missing from norms-audit.md status table")
    )
  }
})
