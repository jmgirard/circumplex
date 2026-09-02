
# Graceful-degradation gate for the optional label-repelling feature. ggrepel is
# in Suggests (DESIGN.md dependency policy); only ssm_plot_circle(repel = TRUE)
# needs it, so the rest of the plotting family runs without it. Wrapped so the
# runtime gate can be exercised in tests via mocked bindings.
has_ggrepel <- function() {
  requireNamespace("ggrepel", quietly = TRUE)
}

#' Create a Circular Plot of SSM Results
#'
#' Take in the results of a Structural Summary Method analysis and plot the
#' point and interval estimate for each row (e.g., group or measure) in a
#' circular space quantified by displacement and amplitude.
#'
#' @param ssm_object Required. The output of `ssm_analyze()`.
#' @param amax A positive real number corresponding to the radius of the circle.
#'   It is used to scale the amplitude values and will determine which amplitude
#'   labels are drawn.
#' @param legend_font_size A positive real number corresponding to the size (in
#'   pt) of the text labels in the legend (default = 12).
#' @param scale_font_size A positive real number corresponding to the size (in
#'   pt) of the text labels for the amplitude and displacement scales (default =
#'   12).
#' @param drop_lowfit A logical determining whether profiles with low model fit
#'   (<.70) should be omitted or plotted with dashed borders (default = FALSE).
#' @param repel A logical determining whether each profile is labelled with a
#'   repelled text label (placed on the circumplex canvas by
#'   [coord_circumplex()], so labels avoid overlapping each other and the
#'   points) instead of distinguished by colour and a legend (default = FALSE).
#'   Requires the \pkg{ggrepel} package.
#' @param angle_labels A character vector specifying text labels to plot around
#'   the circle for each scale. Can also specify NULL to default to numerical
#'   angle labels or a vector of empty strings ("") to hide the labels. If not
#'   NULL, must have the same length and ordering as the `angles` argument to
#'   `ssm_analyze()`. (default = NULL)
#' @param palette A string corresponding to the palette to be used from
#'   ColorBrewer for the color and fill aesthetics. If set to NULL, all points
#'   will appear blue and no legend will be there (useful for showing the
#'   coverage of a high number of variables).
#' @param vary_shapes A logical determining whether profiles should each get
#'   their own shape or vary only by fill color. This only works when the number
#'   of profiles is five or less. (default = FALSE)
#' @param path A logical determining whether each series' movement across
#'   occasions is drawn as an arrowed path on the circle (default = `FALSE`).
#'   Requires an SSM object with occasions, from [ssm_analyze()] with the
#'   `occasions` argument or from [ssm_analyze_long()]; supplying `TRUE` for any
#'   other object is an error. Occasions are connected in the order they were
#'   supplied, never alphabetically, and the path is drawn the short way across
#'   the 0/360 boundary. An occasion whose displacement is undefined (a flat or
#'   zero-amplitude profile) breaks the path rather than being interpolated
#'   through. See [geom_ssm_path()] for the underlying layer.
#' @param ... Not used. Supplying an unrecognized argument produces a warning.
#' @return A ggplot variable containing a completed circular plot.
#' @family visualization functions
#' @export
#' @examples
#' # `boots` is lowered from its default of 2000 throughout these examples so
#' # they run quickly; a reported analysis should use the default.
#'
#' \donttest{
#' data("jz2017")
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = c("NARPD", "ASPD"),
#'   boots = 200
#' )
#' ssm_plot_circle(res)
#' }
ssm_plot_circle <- function(ssm_object,
                            amax = NULL,
                            legend_font_size = 12,
                            scale_font_size = 12,
                            drop_lowfit = FALSE,
                            repel = FALSE,
                            angle_labels = NULL,
                            palette = "Set2",
                            vary_shapes = FALSE,
                            path = FALSE,
                            ...) {

  chkDots(...)

  df <- ssm_object$results
  angles <- ssm_object$details$angles

  stopifnot(is_null_or_num(amax, n = 1))
  stopifnot(is_null_or_char(angle_labels, n = length(angles)))
  stopifnot(is_flag(path))

  # A movement path needs occasions to move between. Refuse early and name the
  # way to produce one, rather than drawing a pathless circle that silently
  # ignores the argument.
  if (path && is.null(ssm_object$details$occasions)) {
    stop(
      "`path = TRUE` needs an SSM object with occasions to draw a movement ",
      "path across; produce one with `ssm_analyze(occasions = )` or ",
      "`ssm_analyze_long()`.",
      call. = FALSE
    )
  }
  
  if (is.null(amax)) {
    amax <- pretty_max(ssm_object$results$a_uci)
  }
  
  if (ssm_object$details$contrast) {
    # A contrast row is a difference, not a position on the circle, so it never
    # gets drawn. The historical [1:2, ] slice is fine for the two-profile case
    # it was written for but truncates an occasions object to its first two
    # occasions; when a path is requested, drop only the contrast row (the last
    # one -- the same positional detector ssm_trajectory_frame() uses).
    df <- if (path) df[-nrow(df), , drop = FALSE] else df[1:2, ]
  }

  # Movement path across occasions. Built from `df` rather than the filtered
  # `df_plot` below on purpose: an occasion with an undefined displacement must
  # stay in the frame as NA so geom_ssm_path() BREAKS the path there. Dropping
  # the row instead would silently connect the occasions on either side of the
  # gap, drawing a movement that never happened.
  df_path <- NULL
  if (path) {
    # Occasions in details$occasions order -- the order they were supplied in,
    # never alphabetical, which puts T10 before T2 and reverses time.
    df_path <- df
    df_path[["Occasion"]] <-
      factor(df_path[["Occasion"]], levels = ssm_object$details$occasions)
    # One path per series: everything that is not the occasion identifies it.
    df_path[["Series"]] <- paste(df_path[["Group"]], df_path[["Measure"]])
    df_path <- df_path[order(df_path[["Series"]], df_path[["Occasion"]]), ]
  }

  # The amplitude/displacement-to-canvas transform (amplitude scaling and the
  # 0/360 polar mapping, with the seam wrap-around) is owned by
  # coord_circumplex(); geom_ssm_arc()/geom_ssm_point() just supply the SSM
  # aesthetics below.
  df_plot <- df

  # Profiles with an undefined location (flat or zero-amplitude scores:
  # d_est = NA) have no place on the circle. The arc/point geoms drop them, so
  # remove them up front and name them rather than let them vanish silently.
  # Uses the same plottability predicate as the geoms (ssm_has_location()).
  undefined <- !ssm_has_location(df_plot[["a_est"]], df_plot[["d_est"]])
  if (any(undefined)) {
    warning(
      "Profile(s) omitted for undefined displacement ",
      "(flat or zero-amplitude scores): ",
      paste(df_plot[["Label"]][undefined], collapse = ", "), ".",
      call. = FALSE
    )
    df_plot <- df_plot[!undefined, ]
    if (nrow(df_plot) < 1) {
      stop("After removing profiles with undefined displacement, ",
           "there were none left to plot.", call. = FALSE)
    }
  }

  if (!is.null(palette)) {
    df_plot[["Label"]] <- factor(
      df_plot[["Label"]],
      levels = unique(as.character(df_plot[["Label"]]))
    )
  } else {
    df_plot[["Label"]] <- factor("All")
  }
  
  n_labels <- nlevels(df_plot$Label)
  
  # Remove profiles with low model fit (unless overrided)
  n <- nrow(df_plot)
  if (drop_lowfit) {
    df_plot <- df_plot[df_plot$fit_est >= .70, ]
    if (nrow(df_plot) < 1) {
      stop("After removing profiles with low fit, there were none left to plot.")
    }
    # The path must honour the same removal. Blanking the occasion (rather than
    # dropping its row) makes the path BREAK there, exactly as it does at an
    # undefined displacement: dropping the row would connect the occasions on
    # either side, asserting a movement through a position the function just
    # said it would not show. Caught by review, M37.
    if (path) {
      lowfit <- !is.na(df_path$fit_est) & df_path$fit_est < .70
      df_path$a_est[lowfit] <- NA_real_
      df_path$d_est[lowfit] <- NA_real_
    }
  }
  df_plot[["lnty"]] <- ifelse(df_plot[["fit_est"]] >= .70, "solid", "dotted")
  
  ## Create circle base
  p <- ggcircumplex(
    angles = angles,
    labels = angle_labels,
    amax = amax,
    font_size = scale_font_size
  )
  
  ## Set color scales depending on palette
  if (is.null(palette)) {
    fill_color <- "#0072B2"
    p <- p +
      ggplot2::scale_color_manual(values = fill_color) +
      ggplot2::scale_fill_manual(values = fill_color) +
      ggplot2::guides(color = "none", fill = "none")
  } else {
    p <- p +
      ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::scale_fill_brewer(palette = palette)
  }
  
  p <- p +
    ggplot2::scale_linetype_identity() +
    ggplot2::theme(
      legend.position = ifelse(repel | n_labels == 1 || is.null(palette), "none", "right"),
      legend.text = ggplot2::element_text(size = legend_font_size)
    )
  
  ## Add arc bars
  p <- p +
    geom_ssm_arc(
      data = df_plot,
      mapping = ggplot2::aes(
        amplitude_min = .data$a_lci,
        amplitude_max = .data$a_uci,
        displacement_min = .data$d_lci,
        displacement_max = .data$d_uci,
        fill = .data$Label,
        color = .data$Label,
        linetype = .data$lnty
      ),
      alpha = 0.4,
      linewidth = 1
    )

  ## Add points
  if (vary_shapes) {
    stopifnot(n_labels <= 5)
    p <- p +
      geom_ssm_point(
        data = df_plot,
        mapping = ggplot2::aes(
          amplitude = .data$a_est,
          displacement = .data$d_est,
          fill = .data$Label,
          shape = .data$Label
        ),
        size = 3,
        color = "black"
      ) +
      ggplot2::scale_shape_manual(values = 21:(21 + n_labels - 1)) +
      ggplot2::guides(
        color = ggplot2::guide_legend("Profile"),
        fill = ggplot2::guide_legend("Profile"),
        shape = ggplot2::guide_legend("Profile")
      )
  } else {
    p <- p +
      geom_ssm_point(
        data = df_plot,
        mapping = ggplot2::aes(
          amplitude = .data$a_est,
          displacement = .data$d_est,
          fill = .data$Label
        ),
        shape = 21,
        size = 3,
        color = "black"
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend("Profile"),
        fill = ggplot2::guide_legend("Profile")
      )
  }

  ## Add the movement path LAST, so its arrowhead draws on top of the occasion
  ## markers. Underneath, the terminal arrowhead lands exactly where the final
  ## occasion's point sits and is covered by it completely -- the direction of
  ## time, which is the whole reason the path is drawn, becomes unreadable.
  ## Caught by the render-and-inspect pass, M37; the data-level tests and a
  ## vdiffr baseline both pass the version with the arrowhead hidden. The arrow
  ## is sized to clear a size-3 point marker for the same reason.
  if (path) {
    p <- p +
      geom_ssm_path(
        data = df_path,
        mapping = ggplot2::aes(
          amplitude = .data$a_est,
          displacement = .data$d_est,
          group = .data$Series
        ),
        colour = "grey30",
        linewidth = 0.6,
        arrow = ggplot2::arrow(
          length = ggplot2::unit(0.15, "inches"), type = "closed"
        )
      )
  }

  if (repel) {
    if (!has_ggrepel()) {
      stop(
        "`repel = TRUE` requires the 'ggrepel' package, which is not ",
        "installed. Install it with install.packages(\"ggrepel\").",
        call. = FALSE
      )
    }
    # Coord-aware label repelling (M31): map the labels to the same
    # amplitude/displacement aesthetics as the points and let coord_circumplex()
    # place them, so ggrepel repels in the rendered panel space. (The old branch
    # hand-computed canvas cartesian coordinates, which are meaningless once the
    # coord owns the transform.)
    p <- p +
      ggrepel::geom_label_repel(
        data = df_plot,
        mapping = ggplot2::aes(
          x = .data$d_est,
          y = .data$a_est,
          label = .data$Label
        ),
        size = legend_font_size / 2.8346438836889
      )
  }

  p
}


#' Create a Curve Plot of SSM Results
#'
#' Take in the results of a Structural Summary Method analysis and plot the
#' scores by angle and the estimated SSM curve.
#'
#' @param ssm_object Required. The results output of `ssm_analyze()`.
#' @param angle_labels Optional. Either NULL or a character vector that
#'   determines the x-axis labels. If NULL, the labels will be the angle
#'   numbers. If a character vector, must be the same length and in the same
#'   order as the `angles` argument to `ssm_analyze()` (default = NULL).
#' @param base_size Optional. A positive number corresponding to the base font
#'   size in pts (default = 11).
#' @param drop_lowfit Optional. A logical indicating whether to omit profiles with
#'   low fit (<.70) or include them with dashed lines (default = FALSE).
#' @param ... Not used. Supplying an unrecognized argument produces a warning.
#' @return A ggplot object depicting the SSM curve(s) of each profile.
#' @family visualization functions
#' @export
#' @examples
#' # `boots` is lowered from its default of 2000 throughout these examples so
#' # they run quickly; a reported analysis should use the default.
#'
#' \donttest{
#' data("jz2017")
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = 10:13,
#'   boots = 200
#' )
#' ssm_plot_curve(res)
#' ssm_plot_curve(res, angle_labels = PANO())
#' }
ssm_plot_curve <- function(ssm_object,
                           angle_labels = NULL,
                           base_size = 11,
                           drop_lowfit = FALSE,
                           ...) {

  stopifnot(inherits(ssm_object, "circumplex_ssm"))
  chkDots(...)

  results <- ssm_object$results
  scores <- ssm_object$scores
  angles <- ssm_object$details$angles

  stopifnot(is_num(base_size, n = 1) && base_size > 0)
  stopifnot(is_null_or_char(angle_labels, n = length(angles)))
  stopifnot(is_flag(drop_lowfit))

  # scale_x_circumplex() (added below) supplies the degree-formatted default
  # labels when angle_labels is NULL; here we only pick the axis title.
  xlabel <- if (is.null(angle_labels)) "Angle" else "Scale"

  # Drop the contrast row if contrast
  if (ssm_object$details$contrast) {
    results <- results[1:2, ]
    scores <- scores[1:2, ]
  }

  # Drop profiles with low fit if requested
  if (drop_lowfit) {
    idx <- results$fit_est >= .70
    results <- results[idx, ]
    scores <- scores[idx, ]
  }

  # Drop the info columns by name (occasions objects carry a fourth,
  # conditional-presence Occasion column; a positional -c(1:3) would leak it
  # into the scale columns and corrupt the reshape below)
  scores_only <-
    scores[, setdiff(names(scores), c("Label", "Group", "Measure", "Occasion"))]

  # Reshape scores to long format
  score_df <- data.frame(
    Label = rep(scores$Label, times = length(angles)),
    Scale = rep(colnames(scores_only), each = nrow(scores)),
    Angle = rep(angles, each = nrow(scores)),
    Score = as.vector(unlist(scores_only))
  )
  curve_fit <- function(p, x) {
    p$e_est + p$a_est * cos((x - p$d_est) * pi / 180)
  }
  all_angles <- seq(from = min(angles), to = max(angles), length.out = 100)
  param_list <- split(results, results$Label)
  pred_mat <- sapply(param_list, FUN = curve_fit, x = all_angles)

  pred_df <- data.frame(
    Label = rep(colnames(pred_mat), each = nrow(pred_mat)),
    Angle = rep(all_angles, times = ncol(pred_mat)),
    Score = as.vector(pred_mat)
  )
  pred_df <- merge(pred_df, results[c("Label", "fit_est")])
  pred_df$lnty <- ifelse(pred_df$fit_est >= .70, "solid", "dashed")

  # Create ggplot
  ggplot2::ggplot() +
    ggplot2::facet_wrap(~Label) +
    # Curve
    ggplot2::geom_line(
      data = pred_df,
      mapping = ggplot2::aes(
        x = .data$Angle,
        y = .data$Score,
        linetype = .data$lnty,
        color = .data$Label
      ),
      linewidth = 1.25
    ) +
    # Connectors
    ggplot2::geom_line(
      data = score_df,
      mapping = ggplot2::aes(
        x = .data$Angle,
        y = .data$Score,
        group = .data$Label
      ),
      color = "black"
    ) +
    # Points
    ggplot2::geom_point(
      data = score_df,
      mapping = ggplot2::aes(
        x = .data$Angle,
        y = .data$Score,
        group = .data$Label
      ),
      color = "black"
    ) +
    scale_x_circumplex(angles, labels = angle_labels) +
    ggplot2::scale_linetype_identity() +
    ggplot2::labs(x = xlabel) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}

#' Create a Difference Plot of SSM Contrast Results
#'
#' Take in the results of a Structural Summary Method analysis with pairwise
#' contrasts and plot the point and interval estimates for each parameter's
#' contrast (e.g., between groups or measures).
#'
#' @param ssm_object Required. The results output of \code{ssm_analyze()}.
#' @param drop_xy A logical determining whether the X-Value and Y-Value
#'   parameters should be removed from the plot (default = FALSE).
#' @param sig_color Optional. A string corresponding to the color to use to
#'   denote significant contrasts (default = "#fc8d62").
#' @param ns_color Optional. A string corresponding to the color to use to
#'   denote non-significant contrasts (default = "white").
#' @param linesize Optional. A positive number corresponding to the size of the
#'   point range elements in mm (default = 1.5).
#' @param fontsize Optional. A positive number corresponding to the size of the
#'   axis labels, numbers, and facet headings in pt (default = 12).
#' @param ... Not used. Supplying an unrecognized argument produces a warning.
#' @return A ggplot variable containing difference point-ranges faceted by SSM
#'   parameter. An interval that does not contain the value of zero has p<.05.
#' @family visualization functions
#' @export
#' @examples
#' # `boots` is lowered from its default of 2000 throughout these examples so
#' # they run quickly; a reported analysis should use the default.
#'
#' \donttest{
#' data("jz2017")
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = c("NARPD", "ASPD"),
#'   contrast = TRUE,
#'   boots = 200
#' )
#' ssm_plot_contrast(res)
#' }
ssm_plot_contrast <- function(ssm_object, drop_xy = FALSE,
                              sig_color = "#fc8d62", ns_color = "white",
                              linesize = 1.25, fontsize = 12, ...) {

  if (!isTRUE(ssm_object$details$contrast)) {
    # A gate-rejected latent contrast (ssm_sem(contrast = TRUE) whose
    # invariance gate failed) deliberately carries details$contrast = FALSE
    # so no inherited method renders a contrast; the refusal here must
    # restate that verdict, not contradict the user's contrast = TRUE call
    # with a bare condition failure.
    inv <- ssm_object$invariance
    if (isTRUE(inv$contrast_requested) && !isTRUE(inv$comparable)) {
      stop(
        "The requested latent contrast was not computed, so there is no ",
        "contrast to plot: ", inv$verdict, ". The object carries each ",
        "group's separate (configural) latent profile instead; see ",
        "print() for the invariance ladder.",
        call. = FALSE
      )
    }
    stop(
      "This SSM object contains no contrast to plot; request one with ",
      "`contrast = TRUE` (exactly two groups, two measures, or two ",
      "occasions).",
      call. = FALSE
    )
  }
  chkDots(...)

  # Prepare all estimates
  plabs <- c(
    e = expression(paste(Delta, " Elevation")),
    x = expression(paste(Delta, " X Value")),
    y = expression(paste(Delta, " Y Value")),
    a = expression(paste(Delta, " Amplitude")),
    d = expression(paste(Delta, " Displacement"))
  )
  pvals <- c("e", "x", "y", "a", "d")
  res <- ssm_object$results[nrow(ssm_object$results), ]

  plot_df <-
    data.frame(
      Parameter = factor(pvals, levels = pvals, labels = plabs),
      Difference = c(res$e_est, res$x_est, res$y_est, res$a_est, res$d_est),
      lci = c(res$e_lci, res$x_lci, res$y_lci, res$a_lci, res$d_lci),
      uci = c(res$e_uci, res$x_uci, res$y_uci, res$a_uci, res$d_uci)
    )

  plot_df$sig <- sign(plot_df$lci) == sign(plot_df$uci)

  # Drop x and y estimates if requested
  if (drop_xy) {
    plot_df <- plot_df[-c(2, 3), ]
  }

  p <-
    ggplot2::ggplot(plot_df) +
    ggplot2::facet_wrap(
      ~Parameter,
      nrow = 1,
      scales = "free",
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = linesize,
      color = "darkgray"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = "1",
        ymin = .data$lci,
        ymax = .data$uci
      ),
      linewidth = linesize,
      width = 0.15
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = "1",
        y = .data$Difference,
        fill = .data$sig
      ),
      size = linesize * 3,
      stroke = linesize,
      shape = 21
    ) +
    ggplot2::scale_fill_manual(
      name = "Significant",
      values = c("TRUE" = sig_color, "FALSE" = ns_color)
    ) +
    ggplot2::labs(y = paste0("Contrast (", res$Label, ")")) +
    ggplot2::theme_bw(base_size = fontsize) +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_line(linetype = "dashed"),
      axis.ticks.x = ggplot2::element_blank()
    )

  p
}

#' Create a circumplex plotting canvas
#'
#' Build an empty circular canvas -- the amplitude rings, displacement spokes,
#' and scale labels that circumplex figures are drawn on -- as a \pkg{ggplot2}
#' object. Additional layers (points, arcs, annotations) can be added to it
#' with `+`, so it serves as the reusable foundation for custom circumplex
#' visualizations. The package's own `ssm_plot_circle()` draws on the same
#' canvas.
#'
#' @param angles Optional. A numeric vector of the angular position (in
#'   degrees) of each circumplex scale, going counterclockwise from the right
#'   (default = `octants()`). Ignored if `instrument` is supplied.
#' @param labels Optional. Either `NULL` or a character vector of text labels
#'   to draw around the circle, one per angle and in the same order (default =
#'   `NULL`, which draws the numeric angles). If `instrument` is supplied,
#'   `NULL` uses the instrument's scale abbreviations.
#' @param amax Optional. A single positive number giving the amplitude at the
#'   outer ring, which sets the amplitude-axis labels; the center of the circle
#'   is fixed at amplitude 0 (default = 0.5).
#' @param font_size Optional. A single positive number giving the size (in pt)
#'   of the scale and amplitude labels (default = 12).
#' @param instrument Optional. Either `NULL` or a `circumplex_instrument`
#'   object (see `instrument()`). When supplied, the scale `angles` and (unless
#'   `labels` is given) the scale abbreviations are taken from the instrument
#'   (default = `NULL`).
#' @return A \pkg{ggplot2} object containing the empty circumplex canvas.
#' @family circumplex layers
#' @seealso [coord_circumplex()], which owns the transform this canvas is built
#'   on; [ssm_plot_circle()], which draws SSM results on this canvas.
#' @export
#' @examples
#' # A default octant canvas
#' ggcircumplex()
#'
#' # Label the scales with their circumplex pole abbreviations
#' ggcircumplex(octants(), labels = PANO())
#'
#' # Derive the angles and labels from a circumplex instrument
#' ggcircumplex(instrument = csip)
ggcircumplex <- function(angles = octants(), labels = NULL,
                         amax = 0.5, font_size = 12, instrument = NULL) {

  resolved <- resolve_circumplex_labels(angles, labels, instrument)
  stopifnot(is_num(amax, n = 1) && amax > 0)
  stopifnot(is_num(font_size, n = 1) && font_size > 0)

  ang <- resolved$angles
  lab <- resolved$labels
  if (is.null(lab)) lab <- circumplex_degree_labels(ang)

  # coord_circumplex() owns the amplitude->radius scaling and the polar
  # transform, so the canvas and any data layers added later share one amax and
  # cannot disagree. The displacement spokes/labels are the theta-axis breaks
  # (set here to the scale angles) and the amplitude rings are the r-axis
  # breaks, both drawn as themed panel furniture -- so `+ theme_*()` restyles
  # them. Note: no x-scale limits (they would censor a seam-straddling arc's
  # unwrapped xmax > 360 to NA); the coord's thetalim owns the [0, 360] range.
  # geom_blank establishes the [0, 360] x [0, amax] extent so the empty canvas's
  # rings/spokes/labels train and draw; it censors nothing, so a seam-straddling
  # arc added later (unwrapped xmax > 360) still extends the range freely.
  ggplot2::ggplot() +
    coord_circumplex(amax = amax, center = 0) +
    ggplot2::geom_blank(
      data = data.frame(.x = c(0, 360), .y = c(0, amax)),
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(breaks = ang, labels = lab) +
    ggplot2::scale_y_continuous(name = NULL) +
    theme_circumplex(base_size = font_size)
}

#' Circumplex canvas theme
#'
#' The \pkg{ggplot2} theme applied to the circumplex canvas built by
#' [ggcircumplex()]. It is built on [ggplot2::theme_minimal()] so that the
#' amplitude rings, displacement spokes, and labels drawn by [coord_circumplex()]
#' are themed panel furniture that respond to further theming. Apply it to a
#' custom circumplex plot, and add `+ theme_*()` or `+ theme()` on top to
#' restyle the canvas.
#'
#' @param base_size A single positive number giving the base font size (in pt)
#'   for the theme (default = 12).
#' @return A \pkg{ggplot2} theme object, to be added to a plot with `+`.
#' @family circumplex layers
#' @export
#' @examples
#' # Restyle the canvas with a larger base font
#' ggcircumplex(octants()) + theme_circumplex(base_size = 16)
theme_circumplex <- function(base_size = 12) {
  stopifnot(is_num(base_size, n = 1) && base_size > 0)
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray80"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
