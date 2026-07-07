# S3 class for the SSM CI-trustworthiness diagnostic (M4/Z1). The full
# plain-language verdict wording and the amplitude-ladder analysis layer of
# spec sec. 5.2 land with M4/Z2; print()/summary() here report the verdict
# classifications and the coverage/guardrail tables.

# S3 Constructor
new_ci_accuracy <- function(coverage, guardrail, verdict, cpm, population,
                            details, call) {
  new_s3_scalar(
    coverage = coverage,
    guardrail = guardrail,
    verdict = verdict,
    cpm = cpm,
    population = population,
    details = details,
    call = call,
    class = "circumplex_ci_accuracy"
  )
}

# Print method: one verdict line per profile row (spec sec. 5: print shows the
# per-profile verdict lines only; summary adds the tables)
#' @method print circumplex_ci_accuracy
#' @export
print.circumplex_ci_accuracy <- function(x, digits = 3, ...) {
  d <- x$details
  cat(
    "\nSSM CI accuracy, simulated at your n and settings (",
    d$reps, " replications per condition; ",
    if (identical(d$method, "montecarlo")) "Monte Carlo" else "bootstrap",
    " intervals with ", d$boots, " replicates at level ", d$interval, ")\n",
    sep = ""
  )
  v <- x$verdict
  pretty <- c(e = "Elevation", a = "Amplitude",
              d_conditional = "Displacement (when certified)")
  for (lab in unique(v$Profile)) {
    vp <- v[v$Profile == lab, ]
    cat("\n# Profile [", lab, "]:\n", sep = "")
    for (pm in names(pretty)) {
      row <- vp[vp$Parameter == pm, ]
      if (nrow(row) == 0 || is.na(row$Class)) {
        cat("  ", format(pretty[[pm]], width = 30), " (not assessable)\n",
            sep = "")
      } else {
        cat("  ", format(pretty[[pm]], width = 30), " coverage ",
            format(round(row$Coverage * 100, 1), nsmall = 1), "% -- ",
            row$Class,
            if (!is.na(row$Direction)) paste0(" (", row$Direction,
                                              "-coverage)") else "",
            "\n", sep = "")
      }
    }
    ov <- vp$Class[vp$Parameter == "overall"]
    cat("  Overall: ",
        if (is.na(ov)) "not assessable" else toupper(ov), "\n", sep = "")
  }
  invisible(x)
}

# Summary method: details, the embedded structure fit, and the full coverage
# and guardrail tables
#' @method summary circumplex_ci_accuracy
#' @export
summary.circumplex_ci_accuracy <- function(object, digits = 3, ...) {
  d <- object$details
  cat(
    "\nStatistical Basis:\t", d$score_type, "Scores",
    "\nAssessed Engine:\t", d$method, "with", d$boots, "replicates",
    "\nConfidence Level:\t", d$interval,
    "\nSimulation Reps:\t", d$reps, "per condition",
    "\nAmplitude Ladder:\t", d$amplitude_factors,
    "\nPopulation Structure:\t",
    if (identical(d$structure, "cpm")) "Browne circular model (CPM)"
    else "observed correlations",
    "\nGroup Sizes:\t\t", paste0(names(d$n), " = ", d$n, collapse = ", "),
    "\nCertification Rule:\t round(a_lci, ", d$digits, ") > 0 (threshold ",
    format(d$threshold, scientific = FALSE), " amplitude units)",
    "\nElapsed:\t\t", round(d$elapsed, 1), "s\n",
    sep = " "
  )
  if (!is.null(d$cpm_diagnostics)) {
    cd <- d$cpm_diagnostics
    cat(
      "\nStructure note: population simulated from a Browne circular model ",
      "fit\n(m = ", cd$m, ", RMSEA = ", round(cd$rmsea, 3), ", SRMR = ",
      round(cd$srmr, 3), ")",
      if (!isTRUE(cd$accepted)) {
        "\nCAUTION: the structural model did not converge cleanly."
      } else "",
      if (length(cd$markers) > 0) {
        paste0("\nBoundary markers: ", paste(cd$markers, collapse = "; "), ".")
      } else "",
      "\n", sep = ""
    )
  }
  if (any(d$failed_reps > 0)) {
    cat("\nFailed simulation replicates by condition:\n")
    print(d$failed_reps)
  }
  cat("\nCoverage by profile, parameter, and amplitude condition:\n")
  cov <- object$coverage
  num <- vapply(cov, is.numeric, logical(1))
  cov[num] <- lapply(cov[num], round, digits = digits)
  print(cov, row.names = FALSE)
  cat("\nGuardrail operating characteristics:\n")
  gr <- object$guardrail
  num <- vapply(gr, is.numeric, logical(1))
  gr[num] <- lapply(gr[num], round, digits = digits)
  print(gr, row.names = FALSE)
  print(object)
  invisible(object)
}
