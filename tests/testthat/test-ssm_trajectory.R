# Trajectory plotting for occasions objects (M33).
#
# Fixture provenance: every fixture below is generated in-file by
# make_traj_data() from a named seed; no committed data files. Each occasion is
# a cosine profile with a KNOWN displacement, so the recovered trajectory
# crosses the 0/360 seam deterministically rather than by luck -- the M13 teeth
# rule (a seam guard whose fixture never straddles has no teeth).
#
# Invariants are asserted at the data level, never by eye: devtools::check()
# runs clean on a visually wrong figure.

make_traj_data <- function(d = c(350, 359, 8, 16), n = 60, e = 2, a = 1,
                           noise = 1, seed = 33,
                           scales = c(
                             "PA", "BC", "DE", "FG",
                             "HI", "JK", "LM", "NO"
                           )) {
  set.seed(seed)
  ang <- as.numeric(octants())
  a <- rep_len(a, length(d)) # per-occasion amplitude (a = 0 is a flat profile)
  blocks <- lapply(seq_along(d), function(j) {
    mu <- e + a[[j]] * cos((ang - d[[j]]) * pi / 180)
    block <- matrix(rnorm(n * length(ang), sd = noise), n, length(ang)) +
      matrix(mu, n, length(ang), byrow = TRUE)
    colnames(block) <- paste0(scales, "_", j)
    block
  })
  df <- as.data.frame(do.call(cbind, blocks))
  df$Gender <- factor(rep(c("F", "M"), length.out = n))
  df
}

occ_list <- function(labels, n_occ = length(labels),
                     scales = c(
                       "PA", "BC", "DE", "FG",
                       "HI", "JK", "LM", "NO"
                     )) {
  out <- lapply(seq_len(n_occ), function(j) paste0(scales, "_", j))
  names(out) <- labels
  out
}

traj_fit <- function(d = c(350, 359, 8, 16), labels = paste0("T", seq_along(d)),
                     boots = 200, seed = 33, ...) {
  data <- make_traj_data(d = d, seed = seed)
  set.seed(seed)
  ssm_analyze(data, occasions = occ_list(labels), boots = boots, ...)
}

# ssm_trajectory_frame(): reshape ---------------------------------------------

test_that("the reshape emits one row per group, occasion, and parameter", {
  skip_on_cran()
  res <- traj_fit()
  df <- ssm_trajectory_frame(res)

  expect_equal(nrow(df), 4 * 5)
  expect_setequal(unique(df$Parameter), c("e", "x", "y", "a", "d"))
  expect_setequal(levels(df$Panel), unname(ssm_trajectory_panels()))
  expect_true(all(c("Group", "Occasion", "est", "lci", "uci", "Certified")
  %in% names(df)))
})

test_that("drop_xy removes only the x and y panels", {
  skip_on_cran()
  res <- traj_fit()
  df <- ssm_trajectory_frame(res, drop_xy = TRUE)

  expect_setequal(unique(df$Parameter), c("e", "a", "d"))
  expect_equal(levels(df$Panel), c("Elevation", "Amplitude", "Displacement"))
})

test_that("a grouped object yields one series per group level", {
  skip_on_cran()
  res <- traj_fit(grouping = "Gender")
  df <- ssm_trajectory_frame(res)

  expect_setequal(levels(df$Group), c("F", "M"))
  expect_equal(nrow(df), 2 * 4 * 5)
})

# Occasion ordering is temporal, never alphabetical (AC2) ----------------------

test_that("occasions keep their list order when labels sort the other way", {
  skip_on_cran()
  # T10 is listed second but sorts FIRST alphabetically. An implementation that
  # lets the character Occasion column reach a discrete scale unfactored
  # silently reverses the time axis.
  res <- traj_fit(d = c(350, 10), labels = c("T2", "T10"))
  df <- ssm_trajectory_frame(res)

  expect_equal(levels(df$Occasion), c("T2", "T10"))
  expect_false(identical(levels(df$Occasion), sort(c("T2", "T10"))))
})

test_that("the long-format path preserves its occasion ordering too", {
  skip_on_cran()
  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  wide <- make_traj_data(d = c(350, 10))
  long <- rbind(
    within(
      setNames(wide[, paste0(scales, "_1")], scales),
      {
        id <- seq_len(nrow(wide))
        occasion <- "T2"
      }
    ),
    within(
      setNames(wide[, paste0(scales, "_2")], scales),
      {
        id <- seq_len(nrow(wide))
        occasion <- "T10"
      }
    )
  )
  long$occasion <- factor(long$occasion, levels = c("T2", "T10"))
  set.seed(33)
  res <- ssm_analyze_long(long,
    scales = scales, id = "id",
    occasion = "occasion", boots = 200
  )
  df <- ssm_trajectory_frame(res)

  expect_equal(levels(df$Occasion), c("T2", "T10"))
})

# Displacement seam continuity (AC3) ------------------------------------------

test_that("the fixture really does straddle the 0/360 seam", {
  skip_on_cran()
  # Guards the guard: if this stops holding, every seam assertion below goes
  # vacuous and would pass against a linear implementation.
  res <- traj_fit()
  d <- as.numeric(res$results$d_est)

  expect_true(any(d > 270) && any(d < 90))
  # and at least one interval is stored reversed (lower > upper)
  expect_true(any(as.numeric(res$results$d_lci) >
    as.numeric(res$results$d_uci)))
})

test_that("the displacement branch is continuous across the seam", {
  skip_on_cran()
  res <- traj_fit()
  df <- ssm_trajectory_frame(res)
  d <- df[df$Parameter == "d", ]
  d <- d[order(d$Occasion), ]

  # No occasion-to-occasion step approaches a full turn. A naive wrap leaves a
  # ~344 degree jump between the pre- and post-seam occasions.
  steps <- abs(diff(d$est))
  expect_true(all(steps < 90))
  expect_true(max(steps) < 180)
})

test_that("each CI bound lands on its own estimate's branch", {
  skip_on_cran()
  res <- traj_fit()
  df <- ssm_trajectory_frame(res)
  d <- df[df$Parameter == "d", ]

  # The ribbon is well formed: lower below the estimate, upper above it.
  # Placing a straddling bound by the estimate's branch offset instead inverts
  # the ribbon.
  expect_true(all(d$lci <= d$est))
  expect_true(all(d$est <= d$uci))

  # The plotted width is the interval's stored counterclockwise arc span. This
  # is the real invariant and holds at every width; asserting merely that the
  # width is under 180 degrees would be vacuous, since placing each bound by its
  # own signed distance from the estimate guarantees that arithmetically even
  # when the ribbon is inverted (review finding, M33).
  expect_equal(
    d$uci - d$lci,
    ssm_arc_span(
      as.numeric(res$results$d_lci), as.numeric(res$results$d_uci)
    ),
    tolerance = 1e-9
  )
})

test_that("an interval wider than a half-turn stays upright", {
  skip_on_cran()
  # The regime D-007 certification exists to flag: a zero-amplitude occasion
  # whose displacement is essentially unknown. Its stored interval spans most of
  # the circle, which an implementation that places each bound independently
  # cannot represent -- it clamps both into (-180, 180] of the estimate and
  # renders a near-total-uncertainty band as a narrow INVERTED one that reads as
  # the most precise occasion in the series.
  data <- make_traj_data(d = c(20, 0, 40), a = c(1.2, 0, 1.2), noise = 1.5,
                         n = 40, seed = 23)
  set.seed(23)
  res <- ssm_analyze(data, occasions = occ_list(c("T1", "T2", "T3")),
                     boots = 400)

  spans <- ssm_arc_span(
    as.numeric(res$results$d_lci), as.numeric(res$results$d_uci)
  )
  expect_true(any(spans > 180)) # the fixture reaches the regime at all

  df <- ssm_trajectory_frame(res)
  d <- df[df$Parameter == "d", ]

  expect_true(all(d$uci >= d$lci)) # never inverted
  expect_true(all(d$lci <= d$est & d$est <= d$uci)) # estimate inside its band
  expect_equal(d$uci - d$lci, spans, tolerance = 1e-9)
})

test_that("the seam guard has teeth against a linear implementation", {
  skip_on_cran()
  # Mutation check, not eyeballing: recompute the displacement panel the naive
  # way (bounds carried by the estimate's branch offset rather than their own
  # signed distance) and confirm the assertions above go red.
  res <- traj_fit()
  results <- res$results
  est <- as.numeric(results$d_est)
  branch <- angle_unwrap(est)
  offset <- branch - est
  naive_lci <- as.numeric(results$d_lci) + offset
  naive_uci <- as.numeric(results$d_uci) + offset

  # At least one interval is inverted or absurdly wide under the naive rule.
  expect_true(any(naive_lci > branch | branch > naive_uci |
    (naive_uci - naive_lci) >= 180))
})

# Certification marking (AC4) -------------------------------------------------

test_that("certification is carried per occasion from the amplitude CI pair", {
  skip_on_cran()
  res <- traj_fit()
  df <- ssm_trajectory_frame(res)

  expect_type(df$Certified, "logical")
  expect_equal(
    df$Certified[df$Parameter == "d"],
    unname(ssm_certified(res$results$a_lci, res$results$a_uci))
  )
  # the fixture has a genuine signal, so every occasion certifies
  expect_true(all(df$Certified))
})

test_that("a near-zero-amplitude occasion fails certification", {
  skip_on_cran()
  # a = 0 in one occasion: the amplitude CI lower bound collapses toward zero
  # relative to the interval width, so D-007 declines to certify its
  # displacement.
  res <- traj_fit()
  results <- res$results
  results$a_lci[[2]] <- 0.001
  results$a_uci[[2]] <- 1
  expect_false(ssm_certified(results$a_lci[[2]], results$a_uci[[2]]))
})

# Degenerate occasions and the contrast row (AC5) ------------------------------

test_that("an occasion with no location leaves a gap, not a broken tail", {
  skip_on_cran()
  res <- traj_fit()
  res$results$a_est[[2]] <- NA_real_ # flat occasion: no location
  df <- ssm_trajectory_frame(res)
  d <- df[df$Parameter == "d", ]
  d <- d[order(d$Occasion), ]

  expect_true(is.na(d$est[[2]]))
  # the occasions after the gap keep a defined, continuous branch -- the whole
  # point of bridging rather than letting angle_unwrap()'s NA policy run on
  expect_false(any(is.na(d$est[c(1, 3, 4)])))
  expect_true(all(abs(diff(d$est[c(1, 3, 4)])) < 90))
})

test_that("the contrast row is dropped, not plotted as a time point", {
  skip_on_cran()
  res <- traj_fit(d = c(350, 10), labels = c("T1", "T2"), contrast = TRUE)
  expect_true(res$details$contrast)
  expect_equal(nrow(res$results), 3) # two occasions + the contrast row

  df <- ssm_trajectory_frame(res)
  expect_equal(nrow(df), 2 * 5)
  expect_setequal(levels(droplevels(df$Occasion)), c("T1", "T2"))
  expect_false(any(grepl("-", as.character(df$Occasion), fixed = TRUE)))
})

# ssm_plot_trajectory(): the plot itself --------------------------------------

# Locate a layer's built data by geom class rather than by index: the layer
# order is an implementation detail and an added layer would silently shift a
# hardcoded index onto the wrong data (M31).
traj_layer <- function(p, geom_class, which = 1L) {
  built <- ggplot2::ggplot_build(p)
  idx <- which(vapply(
    p$layers, function(l) inherits(l$geom, geom_class), logical(1)
  ))
  built$data[[idx[[which]]]]
}

test_that("both occasions constructors yield a ggplot", {
  skip_on_cran()
  expect_true(ggplot2::is_ggplot(ssm_plot_trajectory(traj_fit())))

  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  wide <- make_traj_data(d = c(350, 10))
  long <- rbind(
    transform(setNames(wide[, paste0(scales, "_1")], scales),
      id = seq_len(nrow(wide)), occasion = "T1"
    ),
    transform(setNames(wide[, paste0(scales, "_2")], scales),
      id = seq_len(nrow(wide)), occasion = "T2"
    )
  )
  set.seed(33)
  res <- ssm_analyze_long(long,
    scales = scales, id = "id",
    occasion = "occasion", boots = 200
  )
  expect_true(ggplot2::is_ggplot(ssm_plot_trajectory(res)))
})

test_that("every requested parameter gets its own panel", {
  skip_on_cran()
  p <- ssm_plot_trajectory(traj_fit())
  built <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(built$layout$layout$PANEL)), 5)

  p2 <- ssm_plot_trajectory(traj_fit(), drop_xy = TRUE)
  built2 <- ggplot2::ggplot_build(p2)
  expect_equal(length(unique(built2$layout$layout$PANEL)), 3)
})

test_that("a grouped object draws one series per group", {
  skip_on_cran()
  p <- ssm_plot_trajectory(traj_fit(grouping = "Gender"))
  line <- traj_layer(p, "GeomLine")

  expect_equal(length(unique(line$group)), 2)
})

test_that("confidence bands are drawn per occasion", {
  skip_on_cran()
  p <- ssm_plot_trajectory(traj_fit())
  ribbon <- traj_layer(p, "GeomRibbon")

  expect_true(all(c("ymin", "ymax") %in% names(ribbon)))
  expect_equal(nrow(ribbon), 4 * 5)
  expect_true(all(ribbon$ymin <= ribbon$ymax))
})

test_that("the plotted displacement path is continuous across the seam", {
  skip_on_cran()
  # The load-bearing assertion, at the data level: check() runs clean on a
  # visually wrong figure, so the built layer data is the only honest witness.
  p <- ssm_plot_trajectory(traj_fit())
  built <- ggplot2::ggplot_build(p)
  panels <- built$layout$layout
  d_panel <- panels$PANEL[panels$Panel == "Displacement"]

  line <- traj_layer(p, "GeomLine")
  d_line <- line[line$PANEL == d_panel, ]
  d_line <- d_line[order(d_line$x), ]

  expect_true(all(abs(diff(d_line$y)) < 90))
})

test_that("uncertified occasions render hollow and certified ones filled", {
  skip_on_cran()
  res <- traj_fit()
  # Force occasion 2 below the D-007 certification ratio by widening its
  # amplitude interval down toward zero; every other occasion keeps its signal.
  res$results$a_lci[[2]] <- 0.001
  res$results$a_uci[[2]] <- 1
  expect_false(ssm_certified(res$results$a_lci[[2]], res$results$a_uci[[2]]))

  p <- ssm_plot_trajectory(res)
  pts <- traj_layer(p, "GeomPoint", which = 2L) # the displacement-panel layer

  expect_setequal(unique(pts$shape), c(16, 1))
  expect_equal(sum(pts$shape == 1), 1) # exactly the uncensored occasion
})

test_that("the certification legend draws both keys when nothing is uncertified", {
  skip_on_cran()
  res <- traj_fit()
  df <- ssm_trajectory_frame(res)
  expect_true(all(df$Certified)) # the regime the defect hides in

  # ggplot2 draws a key's glyph only for values present in layer data, so the
  # FALSE break kept alive by drop = FALSE used to render as a label with a
  # zeroGrob where its hollow symbol belongs -- a legend that names an encoding
  # it does not show. Read the rendered keys, not the scale.
  keys <- legend_key_glyphs(ssm_plot_trajectory(res), "Displacement interpretable")

  expect_length(keys, 2)
  expect_false(any(vapply(keys, function(k) all(is.na(k)), logical(1))))
  # Exactly one glyph per key: two layers both claiming the legend would
  # overdraw identical symbols on the keys they can fill.
  expect_equal(unname(lengths(keys)), c(1L, 1L))
  expect_equal(sort(unname(unlist(keys))), c(1, 16))
})

test_that("na.rm = FALSE names the dropped occasion count", {
  skip_on_cran()
  res <- traj_fit()
  res$results$a_est[[2]] <- NA_real_

  expect_silent(ggplot2::ggplot_build(ssm_plot_trajectory(res)))
  expect_warning(
    ssm_plot_trajectory(res, na.rm = FALSE),
    "Removed 1 row with no defined displacement"
  )
})

# Error branches (AC6) --------------------------------------------------------

test_that("a non-SSM object is refused", {
  expect_error(ssm_plot_trajectory(data.frame(a = 1)))
})

test_that("an SSM object without occasions is refused informatively", {
  data("jz2017")
  set.seed(33)
  res <- ssm_analyze(jz2017, scales = 2:9, boots = 50)

  expect_error(ssm_plot_trajectory(res), "no occasions")
  expect_error(ssm_plot_trajectory(res), "ssm_analyze_long")
})

test_that("non-finite and non-scalar arguments are refused by name", {
  skip_on_cran()
  res <- traj_fit()

  # is.na() would let Inf through, and it would only surface as a cryptic
  # render-time failure that never names the argument (M32).
  expect_error(ssm_plot_trajectory(res, base_size = Inf), "base_size")
  expect_error(ssm_plot_trajectory(res, base_size = NA_real_), "base_size")
  expect_error(ssm_plot_trajectory(res, base_size = 0), "base_size")
  expect_error(ssm_plot_trajectory(res, base_size = c(10, 12)))
  expect_error(ssm_plot_trajectory(res, drop_xy = NA))
  expect_error(ssm_plot_trajectory(res, na.rm = "yes"))
})

test_that("an unrecognized argument warns rather than passing silently", {
  skip_on_cran()
  expect_warning(ssm_plot_trajectory(traj_fit(), colour = "red"), "disregarded")
})

# Rendered appearance ---------------------------------------------------------

test_that("the trajectory plot renders as expected", {
  skip_if_not_installed("vdiffr")
  # Secondary to the data-level assertions above: bootstrap CI positions are
  # BLAS-sensitive, so the baseline is a rendering guard, not the fence for any
  # acceptance criterion.
  skip_on_ci()
  res <- traj_fit()
  vdiffr::expect_doppelganger("trajectory ungrouped", ssm_plot_trajectory(res))
  vdiffr::expect_doppelganger(
    "trajectory grouped",
    ssm_plot_trajectory(traj_fit(grouping = "Gender"), drop_xy = TRUE)
  )
})
