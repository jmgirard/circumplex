test_that("Single-group mean-based SSM plot is correct", {
  skip_if_not_installed("vdiffr")
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
  skip_if_not_installed("vdiffr")
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")
  p <- ssm_plot_circle(res)
  
  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("single group correlation ssm", p)
})

test_that("Measure-contrast SSM plot is correct", {
  skip_if_not_installed("vdiffr")
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
  skip_if_not_installed("vdiffr")
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
  skip_on_cran()
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "OCPD")
  expect_error(ssm_plot_circle(res, drop_lowfit = TRUE))
})

test_that("many plots works as expected", {
  skip_if_not_installed("vdiffr")
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
  skip_if_not_installed("vdiffr")
  data("jz2017")
  set.seed(12345)
  dat <- jz2017[sample(1:nrow(jz2017), size = 100), ]
  res <- ssm_analyze(dat, 2:9, measures = 19)
  p <- ssm_plot_circle(res)
  vdiffr::expect_doppelganger("cross-zero circle", p)
})

test_that("ggcircumplex() builds a public circular canvas", {
  skip_if_not_installed("vdiffr")
  p <- ggcircumplex(octants())
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("ggcircumplex octant canvas", p)
})

test_that("ggcircumplex() derives angles and labels from an instrument", {
  skip_if_not_installed("vdiffr")
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
  skip_on_cran()
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
  skip_on_cran()
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

# --- T4: repel label ergonomics -----------------------------------------------

test_that("ssm_plot_circle(repel = TRUE) adds a coord-aware repel layer (T4)", {
  skip_on_cran()
  skip_if_not_installed("ggrepel")
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = c("NARPD", "ASPD"))
  p <- ssm_plot_circle(res, repel = TRUE)
  # A repel label layer is present and maps to amplitude/displacement, so the
  # coord (not hand-computed cartesian) places the labels.
  repel_idx <- which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomLabelRepel"), logical(1)
  ))
  expect_length(repel_idx, 1L)
  mp <- p$layers[[repel_idx]]$mapping
  expect_true(all(c("x", "y", "label") %in% names(mp)))
  expect_no_error(ggplot2::ggplot_build(p))
  # repel replaces the colour legend.
  expect_equal(p$theme$legend.position, "none")
})

test_that("ssm_plot_circle(repel = TRUE) errors clearly when ggrepel is absent (T4)", {
  skip_on_cran()
  testthat::local_mocked_bindings(has_ggrepel = function() FALSE)
  data("aw2009")
  set.seed(1)
  res <- ssm_analyze(aw2009, scales = 1:8, boots = 50)
  expect_error(ssm_plot_circle(res, repel = TRUE), "ggrepel")
})

# --- T5: exported circumplex canvas theme -------------------------------------

test_that("theme_circumplex() is exported and validates base_size (T5)", {
  expect_true("theme_circumplex" %in% getNamespaceExports("circumplex"))
  expect_s3_class(theme_circumplex(), "theme")
  expect_error(theme_circumplex(base_size = -1), "base_size")
  expect_error(theme_circumplex(base_size = c(1, 2)), "base_size")
})

test_that("theme_circumplex() default reproduces the canvas theme; base_size varies it (T5)", {
  skip_on_cran()
  # Default path is output-preserving: ggcircumplex() uses theme_circumplex()
  # internally, so the default theme equals the canvas theme (baselines unchanged).
  base <- ggcircumplex(octants(), font_size = 12)$theme
  expect_equal(theme_circumplex(12)$text$size, base$text$size)
  # Non-default path: a larger base_size changes the theme's base text size.
  expect_gt(theme_circumplex(20)$text$size, theme_circumplex(12)$text$size)
})

test_that("plot functions warn about unrecognized arguments", {
  skip_on_cran()
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
  skip_on_cran()
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

test_that("ssm_plot_circle() puts the amplitude axis in a gap with no point", {
  skip_on_cran()
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, measures = c("NARPD", "ASPD"),
                     boots = 20)
  axis_angle <- function(res) {
    p <- suppressWarnings(ssm_plot_circle(res))
    ggplot2::ggplot_build(p)$layout$coord$r_axis_inside
  }
  # A point in the 0-45 gap moves the axis off the default 22.5.
  res$results$d_est <- c(17.8, 200)
  res$results$d_lci <- res$results$d_est - 5
  res$results$d_uci <- res$results$d_est + 5
  expect_equal(axis_angle(res), 67.5)
  # Points away from the 0-45 gap keep the default placement.
  res$results$d_est <- c(100, 200)
  res$results$d_lci <- res$results$d_est - 5
  res$results$d_uci <- res$results$d_est + 5
  expect_equal(axis_angle(res), 22.5)
})

# --- M142: grid = "cartesian" and angle_labels on the canvas -----------------

# The built theta labels keyed by their break.
theta_labels <- function(p) {
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  br <- pp$theta$get_breaks()
  lab <- as.character(pp$theta$get_labels())
  keep <- is.finite(br)
  stats::setNames(lab[keep], as.character(br[keep]))
}
# The rendered theta label text grob: the one whose labels are the theta labels.
theta_text_grob <- function(p) {
  want <- unname(theta_labels(p))
  g <- ggplot2::ggplotGrob(p)
  panel <- g$grobs[[which(g$layout$name == "panel")]]
  found <- NULL
  walk <- function(gr) {
    if (inherits(gr, "text") && setequal(as.character(gr$label), want)) {
      found <<- gr
    }
    kids <- if (inherits(gr, "gtable")) gr$grobs else gr$children
    for (k in kids) walk(k)
  }
  walk(panel)
  found
}

test_that("ggcircumplex() validates grid and angle_labels (AC1, AC3)", {
  expect_error(ggcircumplex(octants(), grid = "square"), "`grid`", fixed = TRUE)
  # is_flag() names the argument in stopifnot()'s message (D-005 idiom).
  expect_error(ggcircumplex(octants(), angle_labels = "yes"), "is_flag(angle_labels)", fixed = TRUE)
  expect_error(ggcircumplex(octants(), angle_labels = NA), "is.na(angle_labels)", fixed = TRUE)
  expect_error(ggcircumplex(octants(), angle_labels = c(TRUE, FALSE)), "is_flag(angle_labels)", fixed = TRUE)
  expect_error(ggcircumplex(octants(), angle_labels = 1), "is_flag(angle_labels)", fixed = TRUE)
})

test_that("angle_labels = TRUE formats text labels as <label> (<angle>°) (AC3)", {
  # 0 and 360 both label as 360° (LM = 360); other angles round to the nearest
  # whole degree.
  p <- ggcircumplex(
    c(0, 360, 11.4, 90, 200.6), labels = c("A", "B", "C", "D", "E"),
    angle_labels = TRUE
  )
  lab <- theta_labels(p)
  expect_equal(
    unname(lab[c("0", "360", "11.4", "90", "200.6")]),
    c("A (360°)", "B (360°)", "C (11°)", "D (90°)", "E (201°)")
  )
  # Labels resolved from an instrument get the same format.
  q <- ggcircumplex(instrument = csip, angle_labels = TRUE)
  expect_equal(
    unname(theta_labels(q)[as.character(csip$Scales$Angle)]),
    paste0(csip$Scales$Abbrev, " (", csip$Scales$Angle, "°)")
  )
  expect_true("LM (360°)" %in% theta_labels(q))
  # With no text labels the break already reads as its angle and is left as it
  # is (implement gate, 2026-09-20).
  expect_equal(theta_labels(ggcircumplex(octants(), angle_labels = TRUE)),
               theta_labels(ggcircumplex(octants())))
  expect_true("90°" %in% theta_labels(ggcircumplex(octants(), angle_labels = TRUE)))
  # angle_labels = FALSE leaves text labels alone.
  expect_equal(unname(theta_labels(ggcircumplex(octants(), labels = PANO()))), PANO())
})

test_that("angle_labels = TRUE rotates each theta label along its radius (AC3)", {
  skip_on_cran()
  p <- ggcircumplex(octants(), labels = PANO(), angle_labels = TRUE)
  txt <- theta_text_grob(p)
  expect_false(is.null(txt))
  deg <- as.numeric(sub(".*\\((\\d+)°\\).*", "\\1", as.character(txt$label)))
  rot <- rep_len(txt$rot, length(deg))
  # Along the radius: the text angle equals the displacement, up to a half turn
  # (the guide flips labels that would read upside down).
  expect_equal((rot - deg) %% 180, rep(0, length(deg)), tolerance = 1e-8)
  # Without angle_labels the labels keep the theme's angle (0).
  txt0 <- theta_text_grob(ggcircumplex(octants(), labels = PANO()))
  expect_false(is.null(txt0))
  expect_equal(rep_len(txt0$rot, 8) %% 360, rep(0, 8))
  # With the default degree labels angle_labels = TRUE is a no-op in full:
  # no rotation and no widened margin, not only an unchanged format.
  deg_on <- ggcircumplex(octants(), angle_labels = TRUE)
  deg_off <- ggcircumplex(octants())
  expect_equal(rep_len(theta_text_grob(deg_on)$rot, 8) %% 360, rep(0, 8))
  expect_identical(deg_on$theme, deg_off$theme)
  expect_gt(
    as.numeric(ggplot2::calc_element("plot.margin", p$theme))[[1]],
    as.numeric(ggplot2::calc_element("plot.margin", deg_off$theme))[[1]]
  )
})

# --- M145: the angle_labels margin is sized per side ------------------------

# The four plot.margin sides in pt, named t/r/b/l (ggplot2's margin order).
margin_pt <- function(p) {
  m <- ggplot2::calc_element("plot.margin", ggplot2::complete_theme(p$theme))
  expect_identical(unique(grid::unitType(m)), "points")
  stats::setNames(as.numeric(m), c("t", "r", "b", "l"))
}
theme_margin_pt <- function(font_size) {
  m <- ggplot2::calc_element(
    "plot.margin", ggplot2::complete_theme(theme_circumplex(font_size))
  )
  stats::setNames(as.numeric(m), c("t", "r", "b", "l"))
}
# The M145 oracle restates the rule in the test; the hard-coded pt values in
# the test below are the independent check. A drawn label at displacement `a`
# reaches half the font size per character along its radius, and each side
# takes the longest projection onto its outward direction (0 = right,
# 90 = top), or the theme's margin.
margin_oracle <- function(drawn, a, font_size) {
  len <- 0.5 * font_size * nchar(drawn)
  u <- list(t = sin(a * pi / 180), r = cos(a * pi / 180),
            b = -sin(a * pi / 180), l = -cos(a * pi / 180))
  reach <- vapply(u, function(ui) max(pmax(0, ui) * len), numeric(1))
  pmax(theme_margin_pt(font_size), reach[c("t", "r", "b", "l")])
}

test_that("angle_labels = TRUE sizes each margin side from the labels pointing at it (M145 AC1)", {
  for (g in c("polar", "cartesian")) {
    # Octants with PANO, LM at 360. One fact stated independently of the
    # oracle: at 12 pt, "PA (90°)" (8 characters) sets the top to 48 pt and the
    # 9-character LM/DE/HI labels set the other sides to 54 pt.
    p <- ggcircumplex(octants(), labels = PANO(), grid = g, angle_labels = TRUE)
    expect_equal(margin_pt(p), c(t = 48, r = 54, b = 54, l = 54), tolerance = 1e-10)
    drawn <- circumplex_angle_labels(PANO(), octants())
    expect_equal(margin_pt(p), margin_oracle(drawn, octants(), 12), tolerance = 1e-10)

    # Left and right differ: a long label at 180, a short one at 0 (drawn as
    # 360). Top and bottom keep the theme's margin.
    lab2 <- c("Warm", "Cold-hearted")
    q <- ggcircumplex(c(0, 180), labels = lab2, grid = g, angle_labels = TRUE)
    expect_equal(margin_pt(q), c(t = 6, r = 66, b = 6, l = 114), tolerance = 1e-10)
    expect_equal(
      margin_pt(q),
      margin_oracle(c("Warm (360°)", "Cold-hearted (180°)"), c(0, 180), 12),
      tolerance = 1e-10
    )

    # Labels at 90 and 270 only: left and right keep the theme's margin.
    v <- ggcircumplex(c(90, 270), labels = c("Up", "Down"), grid = g, angle_labels = TRUE)
    expect_equal(margin_pt(v), c(t = 48, r = 6, b = 66, l = 6), tolerance = 1e-10)
    expect_equal(margin_pt(v)[c("r", "l")], theme_margin_pt(12)[c("r", "l")])

    # font_size scales both the reach and the theme's floor (10 pt at 20).
    f <- ggcircumplex(c(0, 180), labels = lab2, grid = g, angle_labels = TRUE,
                      font_size = 20)
    expect_equal(margin_pt(f), c(t = 10, r = 110, b = 10, l = 190), tolerance = 1e-10)
    expect_equal(
      margin_pt(f),
      margin_oracle(c("Warm (360°)", "Cold-hearted (180°)"), c(0, 180), 20),
      tolerance = 1e-10
    )
  }
})

test_that("without angle-labelled text the margin is the theme's (M145 AC2)", {
  for (g in c("polar", "cartesian")) {
    off <- ggcircumplex(octants(), labels = PANO(), grid = g, angle_labels = FALSE)
    deg <- ggcircumplex(octants(), grid = g, angle_labels = TRUE)
    expect_equal(margin_pt(off), theme_margin_pt(12))
    expect_equal(margin_pt(deg), theme_margin_pt(12))
  }
})

test_that("building an angle-labelled canvas opens no graphics device (M145 review)", {
  # Reading the theme margin in pt must not go through grid::convertUnit(),
  # which opens a device (Rplots.pdf in a script) just to build the plot. An
  # earlier test can leave a device open, which would hide a new one, so the
  # devices are closed first; that is skipped where a user's plots are open.
  skip_if(interactive())
  grDevices::graphics.off()
  ggcircumplex(octants(), labels = PANO(), angle_labels = TRUE)
  expect_null(grDevices::dev.list())
})

test_that("grid = \"cartesian\" turns the theta tick marks on; polar leaves the theme as today (AC3)", {
  cart <- ggcircumplex(octants(), grid = "cartesian")
  expect_true(ggplot2::is_theme_element(
    ggplot2::calc_element("axis.ticks.theta", cart$theme), "line"
  ))
  expect_gt(as.numeric(ggplot2::calc_element("axis.ticks.length.theta", cart$theme)), 0)
  expect_identical(cart$coordinates$grid, "cartesian")
  polar <- ggcircumplex(octants(), grid = "polar")
  expect_identical(polar$theme, theme_circumplex(base_size = 12))
  expect_identical(polar$theme, ggcircumplex(octants())$theme)
  expect_true(ggplot2::is_theme_element(
    ggplot2::calc_element("axis.ticks.theta", polar$theme), "blank"
  ))
  expect_identical(polar$coordinates$grid, "polar")
})

test_that("cartesian canvas snapshots (AC2, AC3)", {
  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger(
    "ggcircumplex cartesian canvas",
    ggcircumplex(octants(), labels = PANO(), grid = "cartesian")
  )
  vdiffr::expect_doppelganger(
    "ggcircumplex cartesian angle labels",
    ggcircumplex(octants(), labels = PANO(), grid = "cartesian", angle_labels = TRUE)
  )
})
