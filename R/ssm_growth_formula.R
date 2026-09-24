# Growth-recipe input: the joint model as engine formulas (M151) --------------
# One fixed model, the one the coverage oracle validated
# (devel/m27-coverage-oracle.R), written in three engines' dialects. The
# builder takes no design options: a covariate, a polynomial time or another
# random-effects structure is a model the oracle never ran (D-064). The
# package fits nothing and calls no engine (D-060); the print method hands
# the user the call to paste.

#' The joint growth model on SSM coordinates as an engine's formulas
#'
#' Return the package's fixed joint growth model in the formula pieces one
#' mixed-model engine takes. The model reads the long table
#' [ssm_growth_data()] builds: one intercept and one linear time slope per
#' coordinate (`e`, `x`, `y`), a person-level random intercept per
#' coordinate with the three intercepts free to correlate, and a separate
#' residual variance per coordinate. Printing the object shows the complete
#' fit call for the engine and, below it, the lines that keep the fixed
#' effects and their covariance from the fit, or the posterior draws for
#' brms. Those are the inputs of the recipe's next step, the trajectory.
#'
#' The model has no options. Adding a covariate, a quadratic time term or
#' another random-effects structure changes the model whose interval
#' coverage the package validated, so the pieces are built for the user to
#' paste and, if they choose, to edit by hand with that in mind. That
#' validation ran the frequentist REML fit. The brms dialect fits the same
#' likelihood under brms's default priors, and its posterior intervals were
#' not part of it. The nlme call sets `na.action = na.omit`, since
#' `nlme::lme()` otherwise stops on a row whose `value` is `NA`; glmmTMB and
#' brms drop such rows by default.
#'
#' @param engine Optional. One of `"glmmTMB"` (default), `"nlme"` or
#'   `"brms"`. The engine is named only; it is not loaded or called.
#' @param time Optional. The name of the time column in the long table
#'   (default `"wave"`), the same `time` given to [ssm_growth_data()].
#' @param id Optional. The name of the person column in the long table
#'   (default `"person"`), the same `id` given to [ssm_growth_data()].
#'   `time` and `id` must each be one non-empty name, different from each
#'   other and from `dv` and `value`, the columns the long table reserves.
#'   A name that is not syntactic, such as `"my wave"`, is backticked in the
#'   formulas, so every name reads as one column and never as formula
#'   syntax.
#' @return A list of class `"circumplex_growth_formula"` with attributes
#'   `engine`, `time` and `id`. Its elements are formula objects, one per
#'   argument or formula part the engine's fit call takes. For `"glmmTMB"`: `formula`,
#'   `value ~ 0 + dv + dv:<time> + us(0 + dv | <id>)`, and `dispformula`,
#'   `~ 0 + dv`. For `"nlme"`: `fixed`, `value ~ 0 + dv + dv:<time>`,
#'   `random`, `~ 0 + dv | <id>`, and `weights`, `~ 1 | dv`, the form that
#'   `nlme::varIdent()` takes. For `"brms"`: `formula`,
#'   `value ~ 0 + dv + dv:<time> + (0 + dv | <id>)`, and `sigma`,
#'   `sigma ~ 0 + dv`, the two parts of a `brms::bf()` call.
#' @family growth functions
#' @export
#' @examples
#' ssm_growth_formula("glmmTMB", time = "wave", id = "person")
#' ssm_growth_formula("nlme")
#' ssm_growth_formula("brms")
ssm_growth_formula <- function(engine = c("glmmTMB", "nlme", "brms"),
                               time = "wave", id = "person") {

  engines <- c("glmmTMB", "nlme", "brms")
  if (!is_char(engine) || length(engine) == 0 ||
      !(identical(engine, engines) || (length(engine) == 1 &&
                                         engine %in% engines))) {
    stop("`engine` must be one of \"glmmTMB\", \"nlme\" or \"brms\".",
         call. = FALSE)
  }
  engine <- engine[1]
  check_growth_name(time, "time")
  check_growth_name(id, "id")
  if (identical(time, id)) {
    stop("`id` and `time` must name different columns.", call. = FALSE)
  }

  # A non-syntactic name is backticked so that it reads as one column, never
  # as formula syntax: `time = "wave + age"` names the column "wave + age",
  # and "my wave" parses.
  time_term <- backtick_name(time)
  id_term <- backtick_name(id)

  fixed <- paste0("value ~ 0 + dv + dv:", time_term)
  pieces <- switch(
    engine,
    glmmTMB = list(
      formula = paste0(fixed, " + us(0 + dv | ", id_term, ")"),
      dispformula = "~ 0 + dv"
    ),
    nlme = list(
      fixed = fixed,
      random = paste0("~ 0 + dv | ", id_term),
      weights = "~ 1 | dv"
    ),
    brms = list(
      formula = paste0(fixed, " + (0 + dv | ", id_term, ")"),
      sigma = "sigma ~ 0 + dv"
    )
  )
  out <- lapply(pieces, function(txt) stats::as.formula(txt, env = globalenv()))
  structure(out, engine = engine, time = time, id = id,
            class = "circumplex_growth_formula")
}

backtick_name <- function(x) {
  if (identical(make.names(x), x)) x else paste0("`", x, "`")
}

# A column name for the formula: one non-empty string that is not one of the
# long table's own fixed column names.
check_growth_name <- function(x, arg) {
  if (!is_char(x, n = 1) || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a single column name.", call. = FALSE)
  }
  if (x %in% c("dv", "value")) {
    stop("`", arg, "` cannot be \"", x, "\", a column the long table ",
         "reserves.", call. = FALSE)
  }
  invisible(x)
}

#' @rdname ssm_growth_formula
#' @param x An object of class `"circumplex_growth_formula"`.
#' @param ... Ignored (S3 consistency).
#' @method print circumplex_growth_formula
#' @export
print.circumplex_growth_formula <- function(x, ...) {
  cat(growth_formula_text(x), sep = "\n")
  invisible(x)
}

# The printed text: a two-line header, the engine's complete fit call on the
# long table `long`, and the line(s) that keep the fixed effects and their
# covariance, or the draws, for the trajectory step.
# Formulas are deparsed so the text and the objects cannot disagree; the
# deparser writes a one-sided formula as `~0 + dv`, respaced here to the
# `~ 0 + dv` the vignette and the engines' own documentation write.
growth_formula_text <- function(x) {
  engine <- attr(x, "engine")
  f <- vapply(x, function(f) {
    txt <- paste(deparse(f, width.cutoff = 500L), collapse = "")
    sub("^~", "~ ", txt)
  }, character(1))
  call_lines <- switch(
    engine,
    glmmTMB = c(
      "fit <- glmmTMB::glmmTMB(",
      paste0("  ", f[["formula"]], ","),
      paste0("  dispformula = ", f[["dispformula"]], ","),
      "  data = long,",
      "  REML = TRUE",
      ")"
    ),
    nlme = c(
      "fit <- nlme::lme(",
      paste0("  fixed = ", f[["fixed"]], ","),
      paste0("  random = ", f[["random"]], ","),
      paste0("  weights = nlme::varIdent(form = ", f[["weights"]], "),"),
      "  data = long,",
      "  na.action = na.omit,",
      "  method = \"REML\"",
      ")"
    ),
    brms = c(
      "fit <- brms::brm(",
      "  brms::bf(",
      paste0("    ", f[["formula"]], ","),
      paste0("    ", f[["sigma"]]),
      "  ),",
      "  data = long",
      ")"
    )
  )
  extract_lines <- switch(
    engine,
    glmmTMB = c(
      "coef <- glmmTMB::fixef(fit)$cond",
      "vcov <- as.matrix(vcov(fit)$cond)"
    ),
    nlme = c(
      "coef <- nlme::fixef(fit)",
      "vcov <- as.matrix(vcov(fit))"
    ),
    brms = "draws <- as.matrix(fit)"
  )
  c(
    paste0("Joint growth model on SSM coordinates, ", engine, " dialect."),
    paste0("Fit on the long table from ssm_growth_data(), then keep ",
           if (engine == "brms") "the draws" else "the fixed effects", ":"),
    "",
    call_lines,
    extract_lines
  )
}
