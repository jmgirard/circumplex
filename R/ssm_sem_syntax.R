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

# Free-parameter counts. Single group (covariance structure only, no mean
# structure): scaled = a_i (p) + direction-constrained plane loadings
# (p effective) + residual variances (p) = 3p, with the g-plane covariances
# FIXED to zero -- freeing them makes the model locally unidentified exactly at
# phi_g = 0 (see the emitted syntax comment; found by T3's empirical
# identification check, recorded in devel/m5-sem-design.md section 12.3).
# strict = residual variances (p) + free 3x3 factor covariance (6) = p + 6.
# Each measure adds its variance (1) + three factor covariances (3); measures
# covary pairwise (m(m-1)/2).
#
# Multi-group (n_groups = G >= 2): the invariance-rung counts of spec section
# 6.2 (as amended 2026-07-07 at T4: the g-plane covariances stay fixed to 0 in
# every group at every rung, so the scaled factor block frees only var(g_g)
# and the shared isotropic plane scale phi_g in the non-reference groups). The
# mean structure is emitted only when G >= 2, so scale/measure intercepts and
# latent means enter the count only there. These coarse counts gate syntax
# generation; the exact df is re-derived by lavaan in the T2 tests and the
# empirical local-identification check runs at fit time (T3/T4).
sem_free_params <- function(model, p, m, n_groups = 1L, invariance = "configural") {
  if (n_groups == 1L) {
    scale_block <- if (model == "scaled") 3L * p else p + 6L
    measure_block <- 4L * m + m * (m - 1L) / 2L
    return(scale_block + measure_block)
  }

  g <- n_groups
  measure_block <- g * (4L * m + m * (m - 1L) / 2L) # per-group measure block
  measure_int <- g * m # measure intercepts: free per group, all rungs

  # Loadings: strict fixes all (0 free); scaled frees 2p effective per group at
  # configural (a_i + direction-constrained plane), shared 2p from metric up.
  loadings <- if (model == "strict") {
    0L
  } else if (invariance == "configural") {
    g * 2L * p
  } else {
    2L * p
  }

  # Factor (co)variances: strict frees the full 3x3 Phi per group at every rung
  # (6 each); scaled fixes everything at configural, then frees var(g_g) and the
  # shared plane scale phi_g (2 params) in each non-reference group from metric.
  factor_cov <- if (model == "strict") {
    g * 6L
  } else if (invariance == "configural") {
    0L
  } else {
    (g - 1L) * 2L
  }

  # Residual variances: free per group, except shared under strict_residuals.
  residuals <- if (invariance == "strict_residuals") p else g * p

  # Scale intercepts: free per group, shared from scalar up.
  scale_int <- if (invariance %in% c("scalar", "strict_residuals")) p else g * p

  # Latent means: fixed 0 everywhere until scalar, then freed in non-ref groups.
  latent_means <- if (invariance %in% c("scalar", "strict_residuals")) {
    (g - 1L) * 3L
  } else {
    0L
  }

  loadings + factor_cov + residuals + scale_int + latent_means +
    measure_block + measure_int
}

# Formatting -------------------------------------------------------------------

# Full double precision so the fixed angles round-trip exactly.
fmt <- function(x) vapply(x, function(v) sprintf("%.17g", v), character(1))

# Snap the fixed-angle cos/sin loadings that are mathematically exact (0, +/-1)
# to those values before formatting. cos(pi/2) and friends evaluate to ~1e-16
# noise whose low-order bits differ across platforms' math libraries (Windows'
# libm vs others), which made the full-precision emission non-byte-portable and
# was numerically meaningless (a 90-degree scale's cx loading IS 0, not 6e-17).
# Only the exact special values are snapped; the genuinely irrational
# saturations (+/-0.7071...) keep full double precision and already agree
# across platforms.
snap_trig <- function(x, tol = 1e-9) {
  x[abs(x) < tol] <- 0
  x[abs(x - 1) < tol] <- 1
  x[abs(x + 1) < tol] <- -1
  x
}

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
#' @param n_groups Optional. A single positive whole number: the number of
#'   groups for a multi-group model. `1` (default) emits the single-group
#'   model. When `n_groups >= 2` the generator emits multi-group syntax with a
#'   mean structure and the invariance constraints selected by `invariance`;
#'   fitting it requires [lavaan::cfa()] called with `group = ` a grouping
#'   variable whose number of levels equals `n_groups`. Group ordering follows
#'   the factor-level order of that variable: the **first** level is the
#'   reference group, whose factor metric (and, from the scalar rung, whose
#'   latent means) is fixed.
#' @param invariance Optional. The cross-group invariance rung to emit when
#'   `n_groups >= 2`: `"configural"` (default), `"metric"`, `"scalar"`, or
#'   `"strict_residuals"`. Must be left at its default when `n_groups == 1`
#'   (supplying it there is an error). Under the `"strict"` tier the `"metric"`
#'   rung is vacuous (all loadings are fixed) and emits the configural
#'   structure with an explanatory comment. See the package design notes
#'   (section 6.2) for the adapted fixed-angle invariance ladder.
#' @param include_defined Optional. (Single-group models only: for
#'   `n_groups >= 2` the lines are unavailable and an explicit `TRUE` is
#'   ignored with a warning.) Whether to append inspection-only `:=`
#'   definitions of the covariance-metric `(e, x, y)` coordinates for each
#'   measure. `NULL` (default) emits them automatically under the `"strict"`
#'   tier when at least one measure is present (there they are linear), and
#'   omits them otherwise. `TRUE` forces emission (an error under `"scaled"`,
#'   where they would be nonlinear); `FALSE` always suppresses them.
#' @return A single character string of \pkg{lavaan} model syntax, with
#'   attributes `angles` (a `circumplex_degree` vector), `scales`, `model`,
#'   `weights` (the 3-by-p OLS projection matrix, rows `e`, `x`, `y`),
#'   `n_groups`, and (when `n_groups >= 2`) `invariance`.
#' @seealso [ssm_analyze()] for the observed-data SSM.
#' @export
#' @examples
#' # Octant instrument, default scaled tier
#' syn <- ssm_sem_syntax(scales = paste0("s", 1:8), angles = octants())
#' attr(syn, "weights")
#' cat(syn)
ssm_sem_syntax <- function(instrument = NULL, scales = NULL, angles = NULL,
                           measures = NULL, model = c("scaled", "strict"),
                           n_groups = 1, invariance = c("configural", "metric",
                             "scalar", "strict_residuals"),
                           include_defined = NULL) {
  model <- match.arg(model)
  stopifnot(is_null_or_char(measures))
  stopifnot(is.null(include_defined) || is_flag(include_defined))
  # n_groups: a single positive whole number.
  stopifnot(
    is_num(n_groups, n = 1L), is_count(n_groups), n_groups >= 1
  )
  n_groups <- as.integer(n_groups)
  # invariance is only meaningful for multi-group models. When n_groups == 1 it
  # must be left at its default; supplying it there is a clear error (rather
  # than silently ignored) so a single-group call never carries a rung it
  # cannot honor.
  if (n_groups == 1L) {
    if (!missing(invariance)) {
      stop(
        "`invariance` applies only to multi-group models; leave it at its ",
        "default when `n_groups` is 1.",
        call. = FALSE
      )
    }
    invariance <- "configural"
  } else {
    invariance <- match.arg(invariance)
  }

  ra <- sem_resolve_scales_angles(instrument, scales, angles)
  scales <- ra$scales
  angles_deg <- ra$angles
  p <- length(scales)
  m <- length(measures)
  th <- as.numeric(as_radian(as_degree(angles_deg)))

  # Weights (also validates that B is full rank, spec section 2.1).
  weights <- sem_ols_weights(th, names = scales)

  # Identification gate (spec section 3.4). Observed moments span scales plus
  # measures; single group with m = 0 reduces to p*(p + 1)/2. Multi-group fits
  # add a per-group mean vector (only emitted when n_groups >= 2), so each
  # group contributes (p+m)(p+m+1)/2 covariances plus (p+m) means.
  moment_block <- (p + m) * (p + m + 1) / 2
  moments <- if (n_groups == 1L) {
    moment_block
  } else {
    n_groups * (moment_block + (p + m))
  }
  df <- moments - sem_free_params(model, p, m, n_groups, invariance)
  if (df < 0) {
    stop(
      sprintf(
        paste0(
          "The '%s' model is under-identified for %d scale(s)%s%s ",
          "(model-implied degrees of freedom = %g). Use more scales or the ",
          "'strict' tier."
        ),
        model, p,
        if (m > 0) sprintf(" and %d measure(s)", m) else "",
        if (n_groups > 1L) {
          sprintf(" at the '%s' rung across %d groups", invariance, n_groups)
        } else {
          ""
        },
        df
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
  # The inspection `:=` lines reference single-group covariance labels (mg1,
  # mcx1, ...); under a multi-group model those labels become c()-vectors, so
  # the lines cannot be emitted there (the single-group inspection helper does
  # not generalize). An explicit `include_defined = TRUE` under "scaled" still
  # errors above; an explicit TRUE with n_groups >= 2 must not be a silent
  # no-op on a stated request.
  if (emit_defined && n_groups > 1L) {
    if (isTRUE(include_defined)) {
      warning(
        "The inspection `:=` lines are not available for multi-group ",
        "models (their labels do not generalize across groups); ",
        "`include_defined = TRUE` is ignored.",
        call. = FALSE
      )
    }
    emit_defined <- FALSE
  }

  co <- fmt(snap_trig(cos(th)))
  si <- fmt(snap_trig(sin(th)))

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
  if (n_groups > 1L) {
    lines <- c(
      lines[seq_len(length(lines) - 1L)],
      sprintf("# groups: %d", n_groups),
      sprintf("# invariance rung: %s", invariance),
      ""
    )
  }

  # Multi-group emission (spec sections 3.5 / 6.2) branches off here; the
  # single-group path below is preserved byte-for-byte.
  if (n_groups == 1L) {

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

  } else {
    # ---- Multi-group emission (n_groups = G >= 2, spec sections 3.5 / 6.2) ----
    g <- n_groups
    # c(v1, ..., vG) prefix helper: a shared plain label imposes cross-group
    # equality; a c() of distinct labels frees per group; numerics fix.
    cvec <- function(v) sprintf("c(%s)", paste(v, collapse = ","))

    # Rung switches (spec section 6.2 ladder).
    shared_loadings <- invariance != "configural"
    free_factor_var <- invariance != "configural"
    shared_intercepts <- invariance %in% c("scalar", "strict_residuals")
    free_latent_means <- shared_intercepts
    shared_residuals <- invariance == "strict_residuals"

    # Repeated-vector form of a cross-group-shared label: c(a1,a1,...)*x
    # imposes exactly the same equality as a plain a1*x, but states the intent
    # explicitly, so lavaan does not warn ("using a single label per parameter
    # in a multiple group setting implies imposing equality constraints across
    # all the groups; If this is not intended...").
    shared_lab <- function(lab) cvec(rep(lab, g))

    if (model == "scaled") {
      if (shared_loadings) {
        # Metric and up: repeated-vector shared labels impose cross-group
        # equality of the per-scale saturations and plane loadings; one set of
        # direction constraints (spec section 6.2, metric rung).
        free_load <- function(factor, prefix) {
          labs <- vapply(
            sprintf("%s%d", prefix, seq_len(p)), shared_lab, character(1)
          )
          terms <- sprintf("%s*%s", labs, scales)
          sprintf(
            "%s =~ %s*%s + %s", factor, cvec(rep("NA", g)), scales[[1]],
            paste(terms, collapse = " + ")
          )
        }
        lines <- c(
          lines,
          "# general factor: cross-group equal per-scale saturations",
          free_load("g", "a"),
          "# circumplex plane: cross-group equal loadings, angles fixed",
          free_load("cx", "lx"),
          free_load("cy", "ly"),
          "# fixed-angle direction constraints: sin(a)*lx - cos(a)*ly == 0",
          sprintf("0 == %s*lx%d - %s*ly%d", si, seq_len(p), co, seq_len(p))
        )
      } else {
        # Configural: distinct per-group labels free every loading per group,
        # with per-group per-scale direction constraints.
        load_cfg <- function(factor, prefix) {
          head <- sprintf(
            "%s =~ %s*%s", factor, cvec(rep("NA", g)), scales[[1]]
          )
          terms <- vapply(seq_len(p), function(i) {
            sprintf(
              "%s*%s", cvec(sprintf("%s%d_g%d", prefix, i, seq_len(g))),
              scales[[i]]
            )
          }, character(1))
          sprintf("%s + %s", head, paste(terms, collapse = " + "))
        }
        dir <- character(0)
        for (k in seq_len(g)) {
          dir <- c(dir, sprintf(
            "0 == %s*lx%d_g%d - %s*ly%d_g%d",
            si, seq_len(p), k, co, seq_len(p), k
          ))
        }
        lines <- c(
          lines,
          "# general factor: per-group free per-scale saturations (configural)",
          load_cfg("g", "a"),
          "# circumplex plane: per-group free loadings, angles fixed",
          load_cfg("cx", "lx"),
          load_cfg("cy", "ly"),
          "# fixed-angle direction constraints, per group and scale",
          dir
        )
      }

      # Factor (co)variances (spec section 6.2, amended 2026-07-07 at T4).
      if (free_factor_var) {
        # Every group's var(g_g) and plane scale phi_g carry a label; the
        # reference group's are fixed to 1 by the `== 1` constraints below.
        # lavaan forbids combining a numeric and a label in a single
        # multigroup modifier (lav_parse_modifier: "Combining labels and fixed
        # values in multigroup modifiers isn't allowed"), so the reference fix
        # cannot be an inline c(1, vg_g2, ...); it is a separate constraint.
        # The SAME plane-scale label vp_g<k> appears in the cx and cy lines of
        # group k: sharing it IS the per-group isotropy constraint
        # var(cx_gk) = var(cy_gk).
        vg <- cvec(sprintf("vg_g%d", seq_len(g)))
        vp <- cvec(sprintf("vp_g%d", seq_len(g)))
        lines <- c(
          lines,
          "# factor metric: non-reference groups free var(g_g) and a single",
          "# isotropic plane scale phi_g; reference group fixed by == below",
          sprintf("g ~~ %s*g", vg),
          sprintf("cx ~~ %s*cx", vp),
          sprintf("cy ~~ %s*cy", vp),
          "vg_g1 == 1",
          "vp_g1 == 1"
        )
      } else {
        ones <- cvec(rep("1", g))
        lines <- c(
          lines,
          "# isotropic orthonormal plane metric, fixed in every group",
          sprintf("g ~~ %s*g", ones),
          sprintf("cx ~~ %s*cx", ones),
          sprintf("cy ~~ %s*cy", ones)
        )
      }
      zeros <- cvec(rep("0", g))
      lines <- c(
        lines,
        sprintf("cx ~~ %s*cy", zeros),
        "# general-plane covariances fixed to zero: with free per-scale",
        "# saturations, freeing these is locally unidentified exactly at",
        "# phi_g = 0 (the trade a_i +/- d*c_i*cos/sin(angle_i) <-> phi_g is",
        "# first-order flat there), so they cannot be estimated. To model a",
        "# general factor leaning into the plane, use the strict tier, whose",
        "# fixed loadings leave the full factor covariance matrix free.",
        "# In multi-group fits they stay 0 in every group at every rung so the invariance ladder stays nested (spec section 6.2).",
        sprintf("g ~~ %s*cx", zeros),
        sprintf("g ~~ %s*cy", zeros)
      )
    } else {
      # Strict tier: all loadings fixed numerics (they replicate per group), and
      # the full 3x3 factor covariance is free per group at every rung.
      if (invariance == "metric") {
        lines <- c(
          lines,
          "# NOTE: under the strict tier the metric rung is vacuous (all",
          "# loadings are fixed); this emits the configural structure",
          "# (see spec section 6.2 table)."
        )
      }
      lines <- c(
        lines,
        "# fixed unit-cosine loadings; full 3x3 factor covariance free per group",
        sprintf("g =~ %s", paste(sprintf("1*%s", scales), collapse = " + ")),
        sprintf("cx =~ %s", paste(sprintf("%s*%s", co, scales), collapse = " + ")),
        sprintf("cy =~ %s", paste(sprintf("%s*%s", si, scales), collapse = " + ")),
        "g ~~ NA*g",
        "cx ~~ NA*cx",
        "cy ~~ NA*cy",
        "g ~~ cx",
        "g ~~ cy",
        "cx ~~ cy"
      )
    }

    # Residual variances: free per group by lavaan default (unlabeled), except
    # cross-group equal under strict_residuals.
    if (shared_residuals) {
      th_labs <- vapply(
        sprintf("th%d", seq_len(p)), shared_lab, character(1)
      )
      lines <- c(
        lines,
        "# residual variances: cross-group equal (strict residual invariance)",
        sprintf("%s ~~ %s*%s", scales, th_labs, scales)
      )
    }

    # External measure block: covariances with the factors are ALWAYS free per
    # group (never constrained across groups); measure variances and intercepts
    # free per group; measures covary freely pairwise per group.
    if (m > 0) {
      lines <- c(
        lines, "",
        "# external measure(s): group-varying covariances with the factors"
      )
      for (k in seq_len(m)) {
        lines <- c(
          lines,
          sprintf("%s ~~ %s*g", measures[[k]],
            cvec(sprintf("mg%d_g%d", k, seq_len(g)))),
          sprintf("%s ~~ %s*cx", measures[[k]],
            cvec(sprintf("mcx%d_g%d", k, seq_len(g)))),
          sprintf("%s ~~ %s*cy", measures[[k]],
            cvec(sprintf("mcy%d_g%d", k, seq_len(g))))
        )
      }
      if (m >= 2) {
        pairs <- utils::combn(seq_len(m), 2)
        lines <- c(
          lines,
          "# measures covary freely (per group)",
          sprintf("%s ~~ %s", measures[pairs[1, ]], measures[pairs[2, ]])
        )
      }
    }

    # Mean structure (emitted only for multi-group, spec section 6.2).
    lines <- c(lines, "", "# mean structure (multi-group)")
    if (shared_intercepts) {
      nu_labs <- vapply(
        sprintf("nu%d", seq_len(p)), shared_lab, character(1)
      )
      lines <- c(
        lines,
        "# scale intercepts: cross-group equal (scalar invariance)",
        sprintf("%s ~ %s*1", scales, nu_labs)
      )
    } else {
      nu <- vapply(seq_len(p), function(i) {
        cvec(sprintf("nu%d_g%d", i, seq_len(g)))
      }, character(1))
      lines <- c(
        lines,
        "# scale intercepts: free per group",
        sprintf("%s ~ %s*1", scales, nu)
      )
    }
    if (free_latent_means) {
      # As with the factor variances, each group's latent mean carries a label
      # and the reference group is fixed to 0 by a separate `== 0` constraint
      # (lavaan forbids a numeric-and-label c() modifier).
      lines <- c(
        lines,
        "# latent means: non-reference groups free; reference fixed 0 by == below",
        sprintf("g ~ %s*1", cvec(sprintf("alpha_g_g%d", seq_len(g)))),
        sprintf("cx ~ %s*1", cvec(sprintf("alpha_x_g%d", seq_len(g)))),
        sprintf("cy ~ %s*1", cvec(sprintf("alpha_y_g%d", seq_len(g)))),
        "alpha_g_g1 == 0",
        "alpha_x_g1 == 0",
        "alpha_y_g1 == 0"
      )
    } else {
      lines <- c(
        lines,
        "# latent means fixed to 0 in every group",
        sprintf("g ~ %s*1", cvec(rep("0", g))),
        sprintf("cx ~ %s*1", cvec(rep("0", g))),
        sprintf("cy ~ %s*1", cvec(rep("0", g)))
      )
    }
    if (m > 0) {
      lines <- c(
        lines,
        "# measure intercepts: free per group",
        sprintf("%s ~ 1", measures)
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
  attr(syntax, "n_groups") <- n_groups
  if (n_groups > 1L) {
    attr(syntax, "invariance") <- invariance
  }
  syntax
}
