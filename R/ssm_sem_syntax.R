# SEM-based SSM (M5): lavaan-syntax generator (T2).
#
# Emits a fixed-theoretical-angle circumplex measurement model as lavaan model
# syntax, and always attaches the ordinary-least-squares projection weights that
# the estimation layer (T3) uses to turn a profile into (e, x, y). See
# devel/m5-sem-design.md; section references below trace to it.
#
# Angles are FIXED theoretical constants everywhere here: they appear only as
# evaluated cos/sin numeric constants in the syntax, never as free parameters
# (spec section 1.2 / Q5.3). Freely estimated circumplex angles are Browne's
# model, which cpm_fit() owns.

# lavaan availability gate (spec section 7.4) ---------------------------------

# Wrapped so tests can mock it with testthat::local_mocked_bindings().
has_lavaan <- function() {
  requireNamespace("lavaan", quietly = TRUE)
}

# Graceful-degradation gate for the SEM feature family. Syntax GENERATION does
# not need lavaan (the string is inspectable and testable without it); only
# FITTING does. Fitting entry points (ssm_sem(), T3) call this first.
require_lavaan <- function() {
  if (!has_lavaan()) {
    stop(
      "The SEM-based SSM functions require the 'lavaan' package, which is not ",
      "installed. Install it with install.packages(\"lavaan\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# The OLS projection weights (spec section 2.1) -------------------------------

# W = (B'B)^{-1} B' for B the p x 3 design with rows (1, cos theta, sin theta),
# so that (e, x, y)' = W %*% profile. For equally spaced angles W equals the
# conventional closed form diag(1/p, 2/p, 2/p) %*% B' that ssm_parameters_cpp()
# applies; off the harmonic-balance condition the two genuinely differ, and the
# model-based layer uses the projection (never tabulated octant constants).
sem_ols_weights <- function(angles_rad, names) {
  b <- cbind(1, cos(angles_rad), sin(angles_rad))
  if (qr(b)$rank < 3L) {
    stop(
      "The angles do not span the cosine basis: at least three distinct, ",
      "non-collinear angles are required to define the SSM projection.",
      call. = FALSE
    )
  }
  w <- solve(crossprod(b), t(b))
  rownames(w) <- c("e", "x", "y")
  colnames(w) <- names
  w
}

# Input resolution (mirrors ssm_analyze()'s flexibility, spec section 3.5) -----

sem_resolve_scales_angles <- function(instrument, scales, angles) {
  if (!is.null(instrument)) {
    stopifnot(inherits(instrument, "circumplex_instrument"))
    scales <- as.character(instrument$Scales$Abbrev)
    angles <- as.numeric(instrument$Scales$Angle)
  } else {
    if (is.null(scales) || is.null(angles)) {
      stop(
        "Supply either `instrument`, or both `scales` and `angles`.",
        call. = FALSE
      )
    }
    stopifnot(is_char(scales), is_num(angles))
    if (length(scales) != length(angles)) {
      stop("`scales` and `angles` must have the same length.", call. = FALSE)
    }
  }
  list(scales = scales, angles = as.numeric(angles))
}

# Symbolic identification gate (spec section 3.4) ------------------------------

# Free-parameter counts (covariance/mean structure not needed here: T2 emits the
# single-group correlation-path model). scaled: a_i (p) + direction-constrained
# plane loadings (p effective) + residual variances (p) = 3p, with the g-plane
# covariances FIXED to zero -- freeing them makes the model locally
# unidentified exactly at phi_g = 0 (see the emitted syntax comment; found by
# T3's empirical identification check, recorded in devel/m5-sem-design.md
# section 12.3). strict: residual variances (p) + free 3x3 factor
# covariance (6) = p + 6. Each measure adds its variance (1) + three
# factor covariances (3); measures covary pairwise (m(m-1)/2). This coarse
# count gates syntax generation; the empirical local-identification check runs
# at fit time (T3).
sem_free_params <- function(model, p, m) {
  scale_block <- if (model == "scaled") 3L * p else p + 6L
  measure_block <- 4L * m + m * (m - 1L) / 2L
  scale_block + measure_block
}

# Formatting -------------------------------------------------------------------

# Full double precision so the fixed angles round-trip exactly.
fmt <- function(x) vapply(x, function(v) sprintf("%.17g", v), character(1))

# The generator ----------------------------------------------------------------

#' Generate lavaan syntax for a fixed-angle circumplex measurement model
#'
#' Emit \pkg{lavaan} model syntax for a structural-equation-model formulation of
#' the Structural Summary Method, with the circumplex scale angles held fixed at
#' their theoretical values. This is the syntax-generation layer of the
#' SEM-based SSM (see the package's design notes); it is a pure function of its
#' arguments and does **not** require \pkg{lavaan} to be installed (only fitting
#' the emitted model does).
#'
#' Two model tiers are available. The `"scaled"` tier frees a general saturation
#' and a circumplex saturation per scale while fixing each scale's angle; the
#' circumplex plane is held isotropic and orthogonal, and the general factor is
#' held orthogonal to the plane (freeing the general-plane covariances
#' alongside free saturations makes the model locally unidentified exactly at
#' zero covariance, so they cannot be estimated in this tier). The `"strict"`
#' tier fixes every loading to the unit cosine pattern
#' `(1, cos(angle), sin(angle))` and frees the 3x3 factor covariance matrix --
#' including the general-plane covariances, making it the tier that can model
#' a general factor leaning into the plane. The angles enter only as evaluated
#' cosine and sine constants; no angle is ever a free parameter.
#'
#' The returned string always carries a `weights` attribute: the
#' ordinary-least-squares projection matrix that maps a profile vector to the
#' structural summary coordinates `(e, x, y)`. For equally spaced angles this
#' equals the conventional closed-form estimator; for unequally spaced angles it
#' is the least-squares projection and can differ. The emitted syntax never
#' contains `:=` definitions for amplitude or displacement: those are nonlinear
#' functions whose confidence intervals must be constructed in-package, not by
#' \pkg{lavaan}'s delta method (which ignores the angular branch cut).
#'
#' @param instrument Optional. A `circumplex_instrument` object; its scale
#'   abbreviations and angles are used. Supply this or both `scales` and
#'   `angles`.
#' @param scales Optional. A character vector of circumplex scale (column)
#'   names. Ignored when `instrument` is supplied.
#' @param angles Optional. A numeric vector of scale angles in degrees, the same
#'   length as `scales`. Ignored when `instrument` is supplied.
#' @param measures Optional. Either `NULL` or a character vector of external
#'   measure (column) names to relate to the circumplex factors.
#' @param model Optional. The measurement-model tier: `"scaled"` (default) or
#'   `"strict"`. See Details.
#' @param include_defined Optional. Whether to append inspection-only `:=`
#'   definitions of the covariance-metric `(e, x, y)` coordinates for each
#'   measure. `NULL` (default) emits them automatically under the `"strict"`
#'   tier when at least one measure is present (there they are linear), and
#'   omits them otherwise. `TRUE` forces emission (an error under `"scaled"`,
#'   where they would be nonlinear); `FALSE` always suppresses them.
#' @return A single character string of \pkg{lavaan} model syntax, with
#'   attributes `angles` (a `circumplex_degree` vector), `scales`, `model`, and
#'   `weights` (the 3-by-p OLS projection matrix, rows `e`, `x`, `y`).
#' @seealso [ssm_analyze()] for the observed-data SSM.
#' @export
#' @examples
#' # Octant instrument, default scaled tier
#' syn <- ssm_sem_syntax(scales = paste0("s", 1:8), angles = octants())
#' attr(syn, "weights")
#' cat(syn)
ssm_sem_syntax <- function(instrument = NULL, scales = NULL, angles = NULL,
                           measures = NULL, model = c("scaled", "strict"),
                           include_defined = NULL) {
  model <- match.arg(model)
  stopifnot(is_null_or_char(measures))
  stopifnot(is.null(include_defined) || is_flag(include_defined))

  ra <- sem_resolve_scales_angles(instrument, scales, angles)
  scales <- ra$scales
  angles_deg <- ra$angles
  p <- length(scales)
  m <- length(measures)
  th <- as.numeric(as_radian(as_degree(angles_deg)))

  # Weights (also validates that B is full rank, spec section 2.1).
  weights <- sem_ols_weights(th, names = scales)

  # Identification gate (spec section 3.4). Observed moments span scales plus
  # measures; with m = 0 this reduces to p*(p + 1)/2.
  moments <- (p + m) * (p + m + 1) / 2
  df <- moments - sem_free_params(model, p, m)
  if (df < 0) {
    stop(
      sprintf(
        paste0(
          "The '%s' model is under-identified for %d scale(s)%s ",
          "(model-implied degrees of freedom = %g). Use more scales or the ",
          "'strict' tier."
        ),
        model, p,
        if (m > 0) sprintf(" and %d measure(s)", m) else "", df
      ),
      call. = FALSE
    )
  }

  # Resolve the inspection-lines default (spec section 3.5).
  if (is.null(include_defined)) {
    emit_defined <- (model == "strict" && m > 0)
  } else if (isTRUE(include_defined)) {
    if (model != "strict") {
      stop(
        "Inspection `:=` lines are only supported under model = \"strict\" ",
        "(under \"scaled\" they are nonlinear functions of free parameters).",
        call. = FALSE
      )
    }
    emit_defined <- m > 0
  } else {
    emit_defined <- FALSE
  }

  co <- fmt(cos(th))
  si <- fmt(sin(th))

  lines <- c(
    "# circumplex SSM measurement model (generated by ssm_sem_syntax())",
    sprintf(
      "# scales: %s", paste(scales, collapse = ", ")
    ),
    sprintf(
      "# angles (degrees): %s", paste(fmt(angles_deg), collapse = ", ")
    ),
    sprintf("# model tier: %s", model),
    ""
  )

  if (model == "scaled") {
    # The factors are identified by their fixed unit variances (below), so every
    # loading must stay free. lavaan's cfa()/sem() default (auto.fix.first) would
    # otherwise fix each factor's first loading to 1, silently fitting the wrong
    # model; the leading `NA*<first scale>` term explicitly frees it (lavaan
    # merges the repeated indicator term with its label), making the syntax
    # self-identifying under default settings.
    free_load <- function(factor, prefix) {
      terms <- sprintf("%s%d*%s", prefix, seq_len(p), scales)
      sprintf("%s =~ NA*%s + %s", factor, scales[[1]], paste(terms, collapse = " + "))
    }
    lines <- c(
      lines,
      "# general factor: free per-scale saturations",
      free_load("g", "a"),
      "# circumplex plane: loadings free but with each scale's angle fixed",
      free_load("cx", "lx"),
      free_load("cy", "ly"),
      "# fixed-angle direction constraints: sin(a)*lx - cos(a)*ly == 0",
      sprintf("0 == %s*lx%d - %s*ly%d", si, seq_len(p), co, seq_len(p)),
      "# isotropic orthonormal plane metric (plane scale absorbed by loadings)",
      "g ~~ 1*g",
      "cx ~~ 1*cx",
      "cy ~~ 1*cy",
      "cx ~~ 0*cy",
      "# general-plane covariances fixed to zero: with free per-scale",
      "# saturations, freeing these is locally unidentified exactly at",
      "# phi_g = 0 (the trade a_i +/- d*c_i*cos/sin(angle_i) <-> phi_g is",
      "# first-order flat there), so they cannot be estimated. To model a",
      "# general factor leaning into the plane, use the strict tier, whose",
      "# fixed loadings leave the full factor covariance matrix free.",
      "g ~~ 0*cx",
      "g ~~ 0*cy"
    )
  } else {
    lines <- c(
      lines,
      "# fixed unit-cosine loadings; free 3x3 factor covariance",
      sprintf(
        "g =~ %s",
        paste(sprintf("1*%s", scales), collapse = " + ")
      ),
      sprintf(
        "cx =~ %s",
        paste(sprintf("%s*%s", co, scales), collapse = " + ")
      ),
      sprintf(
        "cy =~ %s",
        paste(sprintf("%s*%s", si, scales), collapse = " + ")
      ),
      "g ~~ NA*g",
      "cx ~~ NA*cx",
      "cy ~~ NA*cy",
      "g ~~ cx",
      "g ~~ cy",
      "cx ~~ cy"
    )
  }

  # External measure block (spec section 3.3). Measures covary with the
  # circumplex factors only (measure-residual covariances stay fixed at 0 by
  # omission) and freely with each other. Covariance labels let the optional
  # inspection lines reference them.
  if (m > 0) {
    lines <- c(lines, "", "# external measure(s): related to circumplex factors")
    for (k in seq_len(m)) {
      lines <- c(
        lines,
        sprintf("%s ~~ mg%d*g", measures[[k]], k),
        sprintf("%s ~~ mcx%d*cx", measures[[k]], k),
        sprintf("%s ~~ mcy%d*cy", measures[[k]], k)
      )
    }
    if (m >= 2) {
      pairs <- utils::combn(seq_len(m), 2)
      lines <- c(
        lines,
        "# measures covary freely",
        sprintf("%s ~~ %s", measures[pairs[1, ]], measures[pairs[2, ]])
      )
    }
  }

  # Inspection-only covariance-metric coordinates (spec section 3.5). Under the
  # strict tier the fixed loadings make the W-projection of a measure's
  # model-implied covariance profile telescope to the three factor covariances.
  if (emit_defined) {
    lines <- c(
      lines,
      "",
      "# Inspection only (covariance metric): the OLS projection of each",
      "# measure's model-implied covariance profile. Under the strict tier the",
      "# fixed loadings make this equal the factor covariances. These are NOT",
      "# the reported latent SSM parameters (which transform the",
      "# correlation-metric profile), and their delta SEs are lavaan",
      "# approximations; the reported estimates and intervals come from the",
      "# circumplex package."
    )
    for (k in seq_len(m)) {
      lines <- c(
        lines,
        sprintf("cov_e%d := mg%d", k, k),
        sprintf("cov_x%d := mcx%d", k, k),
        sprintf("cov_y%d := mcy%d", k, k)
      )
    }
  }

  lines <- c(
    lines,
    "",
    "# NOTE: amplitude (a) and displacement (d) are deliberately NOT defined",
    "# here. They are nonlinear (sqrt / atan2) and their intervals must be",
    "# built in-package through circular quantiles, never via lavaan := or",
    "# delta-method CIs (which ignore the angular branch cut)."
  )

  syntax <- paste(lines, collapse = "\n")
  attr(syntax, "angles") <- as_degree(angles_deg)
  attr(syntax, "scales") <- scales
  attr(syntax, "model") <- model
  attr(syntax, "weights") <- weights
  syntax
}
