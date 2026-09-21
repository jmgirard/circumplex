# The "Advanced Circumplex Visualization" vignette's latent-circumplex section
# draws its circle figures on a canvas whose rim carries the angles cpm_fit()
# estimated and nothing from the theoretical angles. The chunks are purled from
# the .Rmd.orig (vignette_chunk() in helper-vignette.R) and run here, so the
# figures the page ships are the objects inspected. The .orig is
# .Rbuildignore'd, so R CMD check and covr skip.

latent_plots <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    labels <- c("latent-fit", "latent-ticks", "latent-vectors", "latent-measure")
    chunks <- lapply(labels, vignette_chunk, file = "advanced-visualization.Rmd.orig")
    if (any(vapply(chunks, is.null, logical(1)))) return(NULL)
    # The chunks call ggplot2 unqualified, as the page attaches it.
    env <- new.env(parent = asNamespace("ggplot2"))
    plots <- list()
    for (i in seq_along(labels)) {
      value <- suppressWarnings(eval(parse(text = chunks[[i]]), envir = env))
      if (inherits(value, "ggplot")) plots[[labels[i]]] <- value
    }
    cache <<- list(env = env, plots = plots)
    cache
  }
})

skip_if_no_latent_plots <- function() {
  if (is.null(latent_plots())) {
    skip("vignette .Rmd.orig source unavailable in this build")
  }
}

expect_estimated_rim <- function(p, cpm) {
  built <- ggplot2::ggplot_build(p)
  scale_x <- built$layout$panel_scales_x[[1]]
  expect_equal(scale_x$get_breaks(), cpm$results$Angle, ignore_attr = TRUE)
  labels <- scale_x$get_labels()
  for (i in seq_len(nrow(cpm$results))) {
    expect_match(labels[i], cpm$results$Scale[i], fixed = TRUE)
    expect_match(labels[i], as.character(round(cpm$results$Angle[i])), fixed = TRUE)
  }
  expect_identical(p$coordinates$grid, "cartesian")
  # Angle_theory enters no layer: not as a column, not as a numeric column
  # equal to the theoretical angles.
  for (layer in p$layers) {
    d <- layer$data
    if (is.function(d) || inherits(d, "waiver")) next
    expect_false("Angle_theory" %in% names(d))
    for (col in names(d)) {
      if (is.numeric(d[[col]]) && length(d[[col]]) == nrow(cpm$results)) {
        expect_false(isTRUE(all.equal(sort(as.numeric(d[[col]])),
                                      sort(as.numeric(cpm$results$Angle_theory)))))
      }
    }
  }
}

layer_geoms <- function(p) {
  unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
}

test_that("the ticks figure is the estimated-angle canvas and nothing else", {
  skip_if_no_latent_plots()
  lp <- latent_plots()
  p <- lp$plots[["latent-ticks"]]
  expect_estimated_rim(p, lp$env$cpm)
  expect_identical(layer_geoms(p), "GeomBlank")
})

test_that("the vectors figure draws each measure as a point, a spoke and a label", {
  skip_if_no_latent_plots()
  lp <- latent_plots()
  p <- lp$plots[["latent-vectors"]]
  expect_estimated_rim(p, lp$env$cpm)
  ssm <- lp$env$ssm
  expect_identical(
    unname(sort(ssm$results$Label)),
    sort(c("NARPD", "ASPD", "HISPD", "AVPD", "SCZPD"))
  )
  geoms <- layer_geoms(p)
  expect_identical(geoms, c("GeomBlank", "GeomSsmPath", "GeomSsmPoint", "GeomText"))

  path <- p$layers[[2]]
  expect_null(path$aes_params$arrow)
  expect_null(path$geom_params$arrow)
  pd <- path$data
  for (lab in ssm$results$Label) {
    rows <- pd[pd$Label == lab, ]
    expect_equal(nrow(rows), 2L)
    expect_equal(sort(rows$amplitude), c(0, ssm$results$a_est[ssm$results$Label == lab]))
    expect_equal(unique(as.numeric(rows$displacement)),
                 as.numeric(ssm$results$d_est[ssm$results$Label == lab]))
  }

  point <- p$layers[[3]]
  expect_equal(nrow(point$data), 5L)
  expect_equal(point$data$a_est, ssm$results$a_est)
  expect_equal(point$data$d_est, ssm$results$d_est)
  expect_identical(rlang::as_label(point$mapping$amplitude), "a_est")
  expect_identical(rlang::as_label(point$mapping$displacement), "d_est")

  text <- p$layers[[4]]
  expect_identical(rlang::as_label(text$mapping$label), "Label")
  expect_identical(rlang::as_label(text$mapping$x), "d_est")
  expect_identical(rlang::as_label(text$mapping$y), "a_est")
  expect_identical(text$data$Label, ssm$results$Label)
  # The built text layer sits exactly on the points: labels are offset by
  # hjust/vjust, never by a changed position.
  built <- ggplot2::layer_data(p, 4)
  expect_equal(built$x, as.numeric(ssm$results$d_est))
  expect_equal(built$y, ssm$results$a_est)
})

test_that("the measure figure annotates one measure with its arc and two values", {
  skip_if_no_latent_plots()
  lp <- latent_plots()
  p <- lp$plots[["latent-measure"]]
  expect_estimated_rim(p, lp$env$cpm)
  ssm <- lp$env$ssm
  one <- ssm$results[ssm$results$Label == "SCZPD", ]
  a <- one$a_est
  d <- as.numeric(one$d_est)
  expect_gt(d, 180)

  geoms <- layer_geoms(p)
  expect_identical(
    geoms,
    c("GeomBlank", "GeomSsmPath", "GeomSsmPath", "GeomSsmPoint", "GeomText",
      "GeomText", "GeomText")
  )
  # The spoke: from the origin to the point.
  spoke <- p$layers[[2]]$data
  expect_equal(sort(spoke$amplitude), c(0, a))
  expect_equal(unique(as.numeric(spoke$displacement)), d)
  # The arc: one fixed amplitude, displacement increasing from 0 to d_est.
  arc <- p$layers[[3]]$data
  expect_length(unique(arc$amplitude), 1L)
  expect_gt(arc$amplitude[1], 0)
  expect_equal(arc$displacement[1], 0)
  expect_equal(arc$displacement[nrow(arc)], d)
  expect_true(all(diff(arc$displacement) > 0))
  # The point and its label sit on the measure.
  point <- p$layers[[4]]
  expect_equal(point$data$a_est, a)
  expect_equal(as.numeric(point$data$d_est), d)
  expect_identical(rlang::as_label(point$mapping$amplitude), "a_est")
  expect_identical(rlang::as_label(point$mapping$displacement), "d_est")
  expect_identical(p$layers[[5]]$data$Label, "SCZPD")
  # The two value labels: d_est to the nearest degree with a degree sign,
  # a_est to two decimals.
  labels <- unlist(lapply(p$layers[6:7], function(l) l$aes_params$label))
  expect_true(paste0("d = ", round(d), "°") %in% labels)
  expect_true(paste0("a = ", sprintf("%.2f", a)) %in% labels)
})
