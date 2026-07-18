test_that("Single-group mean-based SSM plot is correct", {
  data("aw2009")
  set.seed(12345)
  res <- ssm_analyze(aw2009, scales = 1:8)
  p <- ssm_plot_circle(res)

  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("single group mean ssm", p)

  p2 <- ssm_plot_circle(res, palette = NULL)
  vdiffr::expect_doppelganger("single group mean ssm no palette", p2)
  
  p3 <- ssm_plot_curve(res, angle_labels = PANO())
  vdiffr::expect_doppelganger("single group mean ssm with labels", p3)
})

test_that("Single-group correlation-based SSM plot is correct", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")
  p <- ssm_plot_circle(res)
  
  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("single group correlation ssm", p)
})

test_that("Measure-contrast SSM plot is correct", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(
    jz2017,
    scales = 2:9,
    measures = c("ASPD", "NARPD"),
    contrast = TRUE
  )
  p <- ssm_plot_contrast(res, drop_xy = TRUE)

  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("measure-contrast ssm", p)
  
  p2 <- ssm_plot_circle(res)
  
  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("measure-contrast-circle ssm", p2)
  
  p3 <- ssm_plot_curve(res, drop_lowfit = TRUE)
  vdiffr::expect_doppelganger("measure-contrast-curve ssm", p3)
})

test_that("Group-contrast correlation-based SSM plot is correct", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(
    jz2017, 
    scales = 2:9, 
    measures = "NARPD",
    grouping = "Gender",
    contrast = TRUE
  )
  p <- ssm_plot_contrast(res)

  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("group-constrast correlation ssm", p)
})

test_that("Removing plots with low fit works as expected", {
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "OCPD")
  expect_error(ssm_plot_circle(res, drop_lowfit = TRUE))
})

test_that("many plots works as expected", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = 10:13)
  p <- ssm_plot_circle(res)
  vdiffr::expect_doppelganger("many_circle-plots", p)
  p2 <- ssm_plot_curve(res)
  vdiffr::expect_doppelganger("many_curve-plots", p2)
  p3 <- ssm_plot_circle(res, repel = TRUE)
  vdiffr::expect_doppelganger("many_circle repel", p3)
})

test_that("things are working at 0/360", {
  data("jz2017")
  set.seed(12345)
  dat <- jz2017[sample(1:nrow(jz2017), size = 100), ]
  res <- ssm_analyze(dat, 2:9, measures = 19)
  p <- ssm_plot_circle(res)
  vdiffr::expect_doppelganger("cross-zero circle", p)
})

test_that("ggcircumplex() builds a public circular canvas", {
  p <- ggcircumplex(octants())
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("ggcircumplex octant canvas", p)
})

test_that("ggcircumplex() derives angles and labels from an instrument", {
  data("csip")

  # Instrument input must resolve to the same canvas as passing that
  # instrument's angles and abbreviations explicitly (proves instrument-aware
  # labeling, including the LM = 360 scale). Compare the built plot data rather
  # than rendering twice, so the equivalence is exact and device-independent.
  p_inst <- ggcircumplex(instrument = csip)
  p_expl <- ggcircumplex(
    angles = csip$Scales$Angle,
    labels = csip$Scales$Abbrev
  )
  expect_true(ggplot2::is_ggplot(p_inst))
  expect_equal(
    ggplot2::ggplot_build(p_inst)$data,
    ggplot2::ggplot_build(p_expl)$data
  )
  vdiffr::expect_doppelganger("ggcircumplex instrument canvas", p_inst)

  # An explicit labels argument still overrides the instrument's abbreviations
  p_override <- ggcircumplex(instrument = csip, labels = LETTERS[1:8])
  expect_true(ggplot2::is_ggplot(p_override))
})

test_that("ggcircumplex() validates its arguments", {
  data("csip")
  # labels must match the number of angles
  expect_error(ggcircumplex(octants(), labels = c("A", "B")))
  # instrument must be an actual instrument object
  expect_error(ggcircumplex(instrument = mtcars))
  # scalar numeric requirements
  expect_error(ggcircumplex(octants(), amax = c(0.5, 1)))
  expect_error(ggcircumplex(octants(), font_size = "big"))
})

test_that("ggcircumplex() no longer exposes amin, and rings are 0-centered (R3)", {
  # amin relabelled the rings on an amin..amax scale while the geoms always
  # map amplitude as a*5/amax (amin = 0), so any nonzero amin silently
  # mislabelled the amplitude axis. The argument is removed; the amplitude
  # scale is fixed at 0 (center) to amax (outer ring), matching the geoms.
  expect_error(ggcircumplex(octants(), amin = 0.25), "unused argument")

  # The amplitude (r) axis runs from 0 at the center to amax at the outer ring,
  # owned by coord_circumplex() -- rings are 0-centered, matching the geoms.
  pp <- ggplot2::ggplot_build(ggcircumplex(amax = 0.5))$layout$panel_params[[1]]
  expect_equal(pp$r.range, c(0, 0.5))
})

test_that("the canvas furniture responds to theme elements (R3, AC3)", {
  # The rings/spokes are the coord's themed panel grid, not frozen drawn geoms:
  # a theme() change must reach them (the old theme_void() canvas could not be
  # restyled). Assert at the grob level that panel.grid recolouring lands.
  collect_col <- function(gr) {
    out <- if (is.null(gr$gp$col)) character(0) else gr$gp$col
    if (!is.null(gr$children)) {
      out <- c(out, unlist(lapply(gr$children, collect_col)))
    }
    out
  }
  p <- ggcircumplex(octants(), amax = 0.5) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "red", linewidth = 2)
    )
  g <- ggplot2::ggplotGrob(p)
  panel <- g$grobs[[which(g$layout$name == "panel")]]
  cols <- collect_col(panel)
  expect_true(any(grepl("red|FF0000", cols, ignore.case = TRUE)))
})

test_that("plot functions warn about unrecognized arguments", {
  data("aw2009")
  set.seed(1)
  res <- ssm_analyze(aw2009, scales = 1:8, boots = 50)

  # A typo'd argument lands in ... and is flagged rather than silently ignored
  expect_warning(ssm_plot_circle(res, angle_lables = PANO()), "disregarded")
  expect_warning(ssm_plot_curve(res, angle_lables = PANO()), "disregarded")

  data("jz2017")
  set.seed(1)
  cres <- ssm_analyze(
    jz2017, scales = 2:9, grouping = "Gender", contrast = TRUE, boots = 50
  )
  expect_warning(ssm_plot_contrast(cres, nonsense_arg = 1), "disregarded")

  # A clean call emits no "disregarded" warning (partial matches are fine)
  w <- capture_warnings(ssm_plot_circle(res, angle_labels = PANO()))
  expect_false(any(grepl("disregarded", w)))
})

test_that("ssm_plot_circle warns by name and omits an undefined-displacement profile (R2)", {
  # A flat (zero-amplitude) group has d_est = NA; v1.2.0 drew it at the origin
  # with a ggplot 'Removed rows' warning, the new geoms dropped it silently.
  # Decision: drop it, but warn naming the profile so it never vanishes silently.
  set.seed(1)
  n <- 30
  g_normal <- matrix(rnorm(n * 8, mean = 3), nrow = n, ncol = 8)
  v <- rnorm(n, mean = 3)
  g_flat <- matrix(v, nrow = n, ncol = 8) # identical columns -> flat mean profile
  dat <- as.data.frame(rbind(g_normal, g_flat))
  names(dat) <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  dat$grp <- rep(c("normal", "flat"), each = n)

  set.seed(2)
  res <- suppressWarnings(
    ssm_analyze(dat, scales = 1:8, grouping = "grp", boots = 50)
  )
  expect_true(any(is.na(res$results$d_est))) # the flat group is undefined

  # ssm_plot_circle warns, names the omitted profile, and still builds
  expect_warning(
    p <- ssm_plot_circle(res),
    "undefined displacement"
  )
  expect_true(ggplot2::is_ggplot(p))
  expect_silent(invisible(ggplot2::ggplot_build(p)))
})
