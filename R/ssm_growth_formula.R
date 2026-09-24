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
#' fit call for the engine and the line that extracts what
#' `ssm_trajectory()` takes from the fit.
#'
#' The model has no options. Adding a covariate, a quadratic time term or
#' another random-effects structure changes the model whose interval
#' coverage the package validated, so the pieces are built for the user to
#' paste and, if they choose, to edit by hand with that in mind.
#'
#' @param engine Optional. One of `"glmmTMB"` (default), `"nlme"` or
#'   `"brms"`. The engine is named only; it is not loaded or called.
#' @param time Optional. The name of the time column in the long table
#'   (default `"wave"`), the same `time` given to [ssm_growth_data()].
#' @param id Optional. The name of the person column in the long table
#'   (default `"person"`), the same `id` given to [ssm_growth_data()].
#' @return A list of class `"circumplex_growth_formula"` with attribute
#'   `engine`. Its elements are formula objects, named as the engine's fit
#'   function names its arguments. For `"glmmTMB"`: `formula`,
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

  fixed <- paste0("value ~ 0 + dv + dv:", time)
  pieces <- switch(
    engine,
    glmmTMB = list(
      formula = paste0(fixed, " + us(0 + dv | ", id, ")"),
      dispformula = "~ 0 + dv"
    ),
    nlme = list(
      fixed = fixed,
      random = paste0("~ 0 + dv | ", id),
      weights = "~ 1 | dv"
    ),
    brms = list(
      formula = paste0(fixed, " + (0 + dv | ", id, ")"),
      sigma = "sigma ~ 0 + dv"
    )
  )
  out <- lapply(pieces, function(txt) stats::as.formula(txt, env = globalenv()))
  structure(out, engine = engine, time = time, id = id,
            class = "circumplex_growth_formula")
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
