# S3 class for the SSM CI-trustworthiness diagnostic (M4/Z1 + Z2). print()
# shows the per-profile verdict blocks of spec sec. 5.2 (coverage lines, the
# guardrail false-certification caution, and the plain-language verdict);
# summary() adds the structure note with its downgrade annotations, the full
# coverage and guardrail tables, and the amplitude-ladder notes; plot() draws
# coverage across the ladder against the Bradley band.

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

# Percentage formatting shared by the verdict wording
ssm_ci_pct <- function(p, digits = 1) {
  paste0(format(round(p * 100, digits), nsmall = digits, trim = TRUE), "%")
}

# One wrapped output line with a fixed-width leader (verdict-block layout)
ssm_ci_cat_line <- function(leader, text, indent = 4, width = 78) {
  lead <- paste0(strrep(" ", indent), format(leader, width = 15))
  body <- strwrap(text, width = width - indent - 15)
  cat(lead, body[1], "\n", sep = "")
  for (b in body[-1]) {
    cat(strrep(" ", indent + 15), b, "\n", sep = "")
  }
}

# A wrapped paragraph at a fixed indent (verdict paragraph, notes)
ssm_ci_cat_para <- function(text, indent = 2, width = 78) {
  cat(strwrap(text, width = width, indent = indent, exdent = indent),
      sep = "\n")
}

# The guardrail false-certification caution decision (spec sec. 4.3/5.1):
# the caution fires when the 95% Wilson interval's LOWER bound on the c = 0
# certification rate exceeds the (1 - interval)/2 user-expectation benchmark
# -- an interval-based trigger, so Monte Carlo noise cannot fire it; the
# benchmark is what users read into "the amplitude CI excludes zero", never
# a nominal level (the CI-excludes-0 <-> level-alpha/2-test duality fails for
# a boundary-constrained nonnegative parameter).
ssm_ci_guardrail_caution <- function(cert_lci, benchmark) {
  !is.na(cert_lci) & cert_lci > benchmark
}

# The sec. 5.2 per-profile verdict blocks, shared by print() and summary():
# coverage lines for elevation, amplitude, and certified displacement; the
# guardrail caution line (when a c = 0 rung was run); and the plain-language
# verdict paragraph. Wording bar (spec sec. 5.2): an interval excluding zero
# is never described as a significance test.
ssm_ci_verdict_blocks <- function(x) {
  v <- x$verdict
  d <- x$details
  cov <- x$coverage
  gr <- x$guardrail
  engine <- paste0(
    ssm_ci_pct(d$interval, 0), " ",
    if (identical(d$method, "montecarlo")) "Monte Carlo" else "bootstrap",
    " CIs, ", d$boots, " replicates"
  )
  con_lab <- if (isTRUE(d$contrast)) names(d$row_n)[length(d$row_n)]

  for (lab in unique(v$Profile)) {
    vp <- v[v$Profile == lab, ]
    n_lab <- d$row_n[[lab]]
    is_con <- identical(lab, con_lab)
    cat(
      "\n  # ", if (is_con) "Contrast" else "Profile", " [", lab, "] (",
      if (!is_con) paste0("n = ", n_lab, "; "), engine, "):\n",
      sep = ""
    )

    # A profile's displacement verdict is certification-conditional
    # ("d_conditional"); the contrast's is unconditional ("d") -- M15-D1.
    dkey <- if (is_con) "d" else "d_conditional"
    cls <- list()
    for (pm in c("e", "a", dkey)) {
      row <- vp[vp$Parameter == pm, ]
      cls[[pm]] <- row
      is_dcond <- pm == "d_conditional"
      leader <- switch(pm, e = "Elevation", a = "Amplitude",
                       d = "Displacement", d_conditional = "Displacement")
      if (nrow(row) == 0 || is.na(row$Class)) {
        txt <- if (is_dcond) {
          "never certified at the as-estimated condition (not assessable)"
        } else {
          "not assessable"
        }
        ssm_ci_cat_line(leader, txt)
        next
      }
      shown <- if (row$Class == "inadequate") "INADEQUATE" else row$Class
      qual <- if (row$Class == "inadequate") {
        paste0(" (", row$Direction, "-coverage",
               if (pm == "a") ssm_ci_miss_phrase(cov, lab), ")")
      } else {
        ""
      }
      ssm_ci_cat_line(leader, paste0(
        "coverage ", ssm_ci_pct(row$Coverage),
        if (is_dcond) " when certified", " -- ", shown, qual
      ))
    }

    # Guardrail false-certification line: profiles only. print.circumplex_ssm()
    # gates a profile's displacement on "amplitude CI excludes zero" but applies
    # no such gate to a contrast, so the diagnostic reports no false-cert verdict
    # for the contrast row -- and, since M15-D1, no certification-conditional
    # displacement line either (the contrast's displacement line above is
    # unconditional). The retained contrast Cert_rate is object-only provenance.
    gr0 <- gr[gr$Profile == lab & gr$Condition == 0, ]
    guard_fired <- FALSE
    guard_rate <- NA_real_
    if (!is_con && nrow(gr0) == 1 && !is.na(gr0$Cert_rate)) {
      guard_rate <- gr0$Cert_rate
      # The stored decision (guardrail$Caution, computed once at run time)
      guard_fired <- isTRUE(gr0$Caution)
      ssm_ci_cat_line("Guardrail", if (guard_fired) {
        paste0(
          "if the true amplitude were zero, displacement would still be ",
          "certified ", ssm_ci_pct(guard_rate),
          " of the time -- the \"amplitude CI excludes zero\" rule is far ",
          "weaker than the ", ssm_ci_pct(gr0$Benchmark),
          " error rate its wording suggests"
        )
      } else {
        paste0(
          "under a truly zero amplitude, displacement would be certified ",
          ssm_ci_pct(guard_rate),
          " of the time (user-expectation benchmark ",
          ssm_ci_pct(gr0$Benchmark), ")"
        )
      })
    }

    ssm_ci_cat_para(
      ssm_ci_verdict_text(cls, guard_fired, guard_rate, dkey = dkey), indent = 2
    )
  }
  invisible(x)
}

# Miss-direction phrase for an inadequate amplitude verdict, from the
# one-sided decomposition at the as-estimated condition (the diagnostic
# signature of the near-zero percentile pathology: misses pile up on the
# truth-below-interval side)
ssm_ci_miss_phrase <- function(cov, lab) {
  a1 <- cov[cov$Profile == lab & cov$Parameter == "a" & cov$Condition == 1, ]
  if (nrow(a1) != 1 || is.na(a1$Left_miss)) {
    return("")
  }
  if (a1$Left_miss > 2 * a1$Right_miss) {
    "; misses are almost all below the interval: the amplitude CI tends to sit above the truth"
  } else if (a1$Right_miss > 2 * a1$Left_miss) {
    "; misses are almost all above the interval: the amplitude CI tends to sit below the truth"
  } else {
    "; misses fall on both sides of the interval"
  }
}

# Assemble the plain-language verdict paragraph (spec sec. 5.2) from the
# three classifications and the guardrail caution. The headline is CAUTION
# whenever any coverage verdict is inadequate OR the false-certification
# caution fired (sec. 5.1: the caution triggers the CAUTION wording).
# `dkey` selects the displacement verdict key: "d_conditional" for a profile
# (certification-conditional) or "d" for the contrast (unconditional; M15-D1).
# `certified` drives the "when certified" / "certified displacement" wording,
# which is omitted for the contrast.
ssm_ci_verdict_text <- function(cls, guard_fired, guard_rate,
                                dkey = "d_conditional") {
  certified <- dkey == "d_conditional"
  d_label <- if (certified) "certified displacement" else "displacement"
  labmap <- c(e = "elevation", a = "amplitude")
  labmap[[dkey]] <- d_label
  class_of <- function(pm) {
    row <- cls[[pm]]
    if (is.null(row) || nrow(row) == 0) NA_character_ else row$Class
  }
  dir_of <- function(pm) {
    row <- cls[[pm]]
    if (is.null(row) || nrow(row) == 0) NA_character_ else row$Direction
  }
  cl <- vapply(c("e", "a", dkey), class_of, character(1))
  if (all(is.na(cl)) && !guard_fired) {
    return("Verdict: not assessable at this number of replications.")
  }

  bad <- character(0)
  if (identical(cl[["e"]], "inadequate")) {
    bad <- c(bad, if (identical(dir_of("e"), "under")) {
      "elevation CIs cover less often than nominal at this sample size"
    } else {
      "elevation CIs cover more often than nominal (they are conservative)"
    })
  }
  a_under <- identical(cl[["a"]], "inadequate") &&
    identical(dir_of("a"), "under")
  if (identical(cl[["a"]], "inadequate")) {
    bad <- c(bad, if (a_under) {
      "amplitude CIs are less reliable than nominal at this sample size"
    } else {
      "amplitude CIs cover more often than nominal (they are conservative)"
    })
  }
  if (identical(cl[[dkey]], "inadequate")) {
    bad <- c(bad, if (identical(dir_of(dkey), "under")) {
      paste0("displacement CIs mis-cover", if (certified) " even when certified")
    } else {
      paste0("displacement CIs over-cover", if (certified) " when certified")
    })
  }
  if (guard_fired) {
    bad <- c(bad, if (!is.na(guard_rate) && guard_rate >= 0.5) {
      "the interpretability guardrail provides almost no protection against a truly zero amplitude"
    } else {
      "the interpretability guardrail certifies a truly zero amplitude more often than its wording suggests"
    })
  }

  headline <- if (length(bad) > 0) {
    "CAUTION"
  } else if (any(cl == "borderline", na.rm = TRUE)) {
    "BORDERLINE"
  } else {
    "ADEQUATE"
  }

  join_and <- function(x) {
    if (length(x) == 1) {
      return(x)
    }
    paste0(paste(x[-length(x)], collapse = ", "),
           if (length(x) > 2) ",", " and ", x[length(x)])
  }

  sentences <- character(0)
  if (length(bad) > 0) {
    sentences <- paste0(join_and(bad), ".")
    if (identical(cl[[dkey]], "adequate")) {
      sentences <- c(sentences, paste0(
        "Displacement CIs are trustworthy",
        if (certified) " when certified", "."
      ))
    }
  } else if (!any(cl == "borderline", na.rm = TRUE)) {
    ok <- labmap[names(cl)[which(cl == "adequate")]]
    sentences <- paste0(
      "coverage is consistent with the nominal level for ", join_and(ok),
      " at this sample size."
    )
  }
  bord <- labmap[names(cl)[which(cl == "borderline")]]
  if (length(bord) > 0) {
    sentences <- c(sentences, paste0(
      join_and(bord), " coverage ", if (length(bord) > 1) "rates are"
      else "is", " borderline at this number of replications; a larger ",
      "`reps` would sharpen the verdict."
    ))
  }
  if (a_under || guard_fired) {
    sentences <- c(sentences, paste0(
      "Consider a larger sample or treat near-zero amplitudes as ",
      "inconclusive rather than absent."
    ))
  }

  # The first sentence continues the "Verdict: HEADLINE --" lead-in
  # (lowercase); later sentences stand alone
  upper_first <- function(s) {
    substr(s, 1, 1) <- toupper(substr(s, 1, 1))
    s
  }
  if (length(sentences) > 1) {
    sentences[-1] <- vapply(sentences[-1], upper_first, character(1),
                            USE.NAMES = FALSE)
  }

  paste0("Verdict: ", headline, " -- ", paste(sentences, collapse = " "))
}

# Structure note with the sec. 5.2 downgrade annotations, in severity order:
# acceptance failure, poor global fit (benchmark constants in
# R/ssm_ci_accuracy.R, cited there), boundary markers, large PSD repair. The
# cpm-vs-observed sensitivity comparison is deliberately cross-call (one call
# assesses one population), so the advice names the other configuration
# instead of pretending to have run it.
ssm_ci_structure_note <- function(object) {
  d <- object$details
  if (identical(d$structure, "observed")) {
    ssm_ci_cat_para(paste0(
      "Structure note: population built from the observed pooled ",
      "within-group correlations (sensitivity configuration). Compare with ",
      "the default structure = \"cpm\" run: if the verdicts differ, ",
      "structure uncertainty is itself material."
    ), indent = 0)
  } else if (!is.null(d$cpm_diagnostics)) {
    cd <- d$cpm_diagnostics
    ssm_ci_cat_para(paste0(
      "Structure note: population simulated from a Browne circular model ",
      "fit (m = ", cd$m, ", RMSEA = ", round(cd$rmsea, 3), ", SRMR = ",
      round(cd$srmr, 3), ")."
    ), indent = 0)
    if (!isTRUE(cd$accepted)) {
      ssm_ci_cat_para(paste0(
        "CAUTION: verdict unreliable -- the structural model did not ",
        "converge cleanly."
      ), indent = 2)
    } else if (is.na(cd$rmsea) || cd$rmsea > ssm_ci_rmsea_poor) {
      ssm_ci_cat_para(paste0(
        "CAUTION: the structural model fits poorly (RMSEA > ",
        ssm_ci_rmsea_poor, "; Browne & Cudeck, 1993), so the simulated ",
        "population may misrepresent your data. Rerun with structure = ",
        "\"observed\" as a sensitivity check: if the verdicts differ, ",
        "structure uncertainty is itself material."
      ), indent = 2)
    } else if (cd$rmsea <= ssm_ci_rmsea_reasonable &&
               !is.na(cd$srmr) && cd$srmr <= ssm_ci_srmr_good) {
      ssm_ci_cat_para(paste0(
        "The structure fits adequately (RMSEA <= ",
        ssm_ci_rmsea_reasonable, ", Browne & Cudeck, 1993; SRMR <= ",
        ssm_ci_srmr_good, ", Hu & Bentler, 1999), so the simulated ",
        "population is a reasonable stand-in for yours."
      ), indent = 2)
    } else {
      ssm_ci_cat_para(paste0(
        "The structural fit is marginal (RMSEA above ",
        ssm_ci_rmsea_reasonable, " or SRMR above ", ssm_ci_srmr_good,
        "); consider rerunning with structure = \"observed\" as a ",
        "sensitivity check: if the verdicts differ, structure uncertainty ",
        "is itself material."
      ), indent = 2)
    }
    if (length(cd$markers) > 0) {
      ssm_ci_cat_para(paste0(
        "Boundary markers: ", paste(cd$markers, collapse = "; "), "."
      ), indent = 2)
    }
  }
  max_delta <- d$max_psd_delta
  if (!is.null(max_delta) && is.finite(max_delta) &&
      max_delta > ssm_ci_psd_warn) {
    ssm_ci_cat_para(paste0(
      "The positive-semidefiniteness repair altered a population ",
      "correlation by up to ", round(max_delta, 4),
      " (> ", ssm_ci_psd_warn, "); population realism is reduced."
    ), indent = 2)
  }
  invisible(object)
}

# Print method: header plus the sec. 5.2 per-profile verdict blocks
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
  ssm_ci_verdict_blocks(x)
  invisible(x)
}

# Summary method: details, the structure note with downgrade annotations,
# the verdict blocks, and the full coverage and guardrail tables
#' Summarize the accuracy of SSM confidence intervals
#'
#' Print the full report of an [ssm_ci_accuracy()] run: the assessed
#' configuration, a structure note describing the simulated population (with
#' cautions when the structural model converged badly or fits poorly --
#' benchmarks per Browne & Cudeck, 1993, and Hu & Bentler, 1999), the
#' per-profile verdict blocks (coverage of elevation, amplitude, and
#' certification-conditional displacement classified against Bradley's
#' liberal band; the guardrail false-certification caution), and the
#' coverage and guardrail tables across the amplitude ladder.
#'
#' @param object A `circumplex_ci_accuracy` object from [ssm_ci_accuracy()].
#' @param digits Number of digits to which table entries are rounded
#'   (default = 3).
#' @param ... Currently ignored.
#' @return The object, invisibly.
#' @references Browne, M. W., & Cudeck, R. (1993). Alternative ways of
#'   assessing model fit. In K. A. Bollen & J. S. Long (Eds.), \emph{Testing
#'   structural equation models} (pp. 136-162). Sage.
#'
#'   Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
#'   covariance structure analysis: Conventional criteria versus new
#'   alternatives. \emph{Structural Equation Modeling, 6}(1), 1-55.
#' @method summary circumplex_ci_accuracy
#' @export
summary.circumplex_ci_accuracy <- function(object, digits = 3, ...) {
  d <- object$details
  cat(
    "\nStatistical Basis:\t", d$score_type, "Scores",
    "\nAssessed Engine:\t", d$method, "with", d$boots, "replicates",
    "\nConfidence Level:\t", d$interval,
    "\nSimulation Reps:\t", d$reps, "per condition",
    # The full simulated ladder (margin rung included), so this line always
    # enumerates the Condition values in the tables below
    "\nAmplitude Ladder:\t", round(d$conditions, 3),
    "\nPopulation Structure:\t",
    if (identical(d$structure, "cpm")) "Browne circular model (CPM)"
    else "observed correlations",
    "\nGroup Sizes:\t\t", paste0(names(d$n), " = ", d$n, collapse = ", "),
    "\nCertification Rule:\t",
    paste0("round(a_lci, ", d$digits, ") > 0 (threshold ",
           format(d$threshold, scientific = FALSE), " amplitude units)"),
    "\nElapsed:\t\t", round(d$elapsed, 1), "s\n\n",
    sep = " "
  )
  ssm_ci_structure_note(object)
  if (length(d$near_zero_rows) > 0) {
    ssm_ci_cat_para(paste0(
      "Near-zero regime: the amplitude estimate of profile",
      if (length(d$near_zero_rows) > 1) "s" else "", " ",
      paste0("[", d$near_zero_rows, "]", collapse = ", "),
      " is below half its own CI width, so your analysis already sits in ",
      "the amplitude-near-zero regime",
      if (!is.null(d$margin_rung)) {
        paste0(
          "; an absolute rung at the certification margin (c = ",
          round(d$margin_rung, 2),
          ", population amplitude = the observed CI half-width) was added ",
          "to the ladder."
        )
      } else {
        "."
      }
    ), indent = 0)
  }
  if (any(d$failed_reps > 0)) {
    cat("\nFailed simulation replicates by condition:\n")
    print(d$failed_reps)
  }

  cat(
    "\nCI trustworthiness at the as-estimated condition (c = 1), classified",
    "\nagainst Bradley's (1978) liberal band via 95% Wilson intervals:\n",
    sep = ""
  )
  ssm_ci_verdict_blocks(object)

  round_numeric <- function(df) {
    num <- vapply(df, is.numeric, logical(1))
    df[num] <- lapply(df[num], round, digits = digits)
    df
  }
  cat("\nCoverage by profile, parameter, and amplitude condition:\n")
  print(round_numeric(object$coverage), row.names = FALSE)
  if (any(object$coverage$Structural)) {
    ssm_ci_cat_para(paste0(
      "Note: amplitude coverage on rows flagged Structural is structurally ",
      "0 (a percentile interval of strictly positive amplitude replicates ",
      "cannot contain a zero truth) -- a theorem, not a measurement; the ",
      "informative near-zero rungs are the small c > 0 ones."
    ), indent = 2)
  }
  cat("\nGuardrail operating characteristics:\n")
  gr <- round_numeric(object$guardrail)
  # The certification threshold (0.5 * 10^-digits amplitude units) would
  # round to 0 at the display precision; keep it exact
  gr$Threshold <- object$guardrail$Threshold
  print(gr, row.names = FALSE)
  invisible(object)
}

# ---- plot -------------------------------------------------------------------

#' Plot SSM CI accuracy across the amplitude ladder
#'
#' Draw the empirical coverage from an [ssm_ci_accuracy()] run against its
#' amplitude-ladder conditions: one panel per SSM parameter (including
#' displacement conditional on guardrail certification), one line per profile
#' row, with 95% Wilson score intervals as error bars, Bradley's (1978)
#' liberal robustness band shaded, and the nominal confidence level as a
#' dashed line. Amplitude rungs whose coverage is structurally zero (a
#' percentile interval of strictly positive amplitude replicates cannot
#' contain a zero truth; see [ssm_ci_accuracy()]) are drawn as open symbols.
#' This is a Cartesian diagnostic plot, not a circumplex figure.
#'
#' @param x A `circumplex_ci_accuracy` object from [ssm_ci_accuracy()].
#' @param ... Currently ignored.
#' @return A \pkg{ggplot2} object.
#' @family ssm functions
#' @family visualization functions
#' @method plot circumplex_ci_accuracy
#' @export
#' @examples
#' \donttest{
#' data("jz2017")
#' set.seed(12345)
#' res <- ssm_analyze(
#'   jz2017[1:200, ],
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   boots = 100
#' )
#' set.seed(23456)
#' acc <- ssm_ci_accuracy(res, reps = 25)
#' plot(acc)
#' }
plot.circumplex_ci_accuracy <- function(x, ...) {
  cov <- x$coverage
  nominal <- x$details$interval

  base <- cov[, c("Profile", "Parameter", "Condition", "Coverage", "N_reps",
                  "Structural")]
  dcond <- cov[cov$Parameter == "d", ]
  dcond <- data.frame(
    Profile = dcond$Profile, Parameter = "d_cert",
    Condition = dcond$Condition, Coverage = dcond$Coverage_conditional,
    N_reps = dcond$N_conditional, Structural = FALSE,
    stringsAsFactors = FALSE
  )
  # The "Displacement (certified)" panel is a presentation surface, so it
  # follows print's profiles-only certification stance (M15-D1): the contrast's
  # displacement is unconditional and appears only in the "Displacement" panel.
  if (isTRUE(x$details$contrast)) {
    con_lab <- names(x$details$row_n)[length(x$details$row_n)]
    dcond <- dcond[dcond$Profile != con_lab, , drop = FALSE]
  }
  df <- rbind(base, dcond)
  df <- df[!is.na(df$Coverage) & df$N_reps > 0, , drop = FALSE]
  wl <- t(vapply(seq_len(nrow(df)), function(i) {
    ssm_ci_wilson(round(df$Coverage[i] * df$N_reps[i]), df$N_reps[i])
  }, numeric(2)))
  df$Wilson_lci <- wl[, 1]
  df$Wilson_uci <- wl[, 2]

  panel_labels <- c(
    e = "Elevation", x = "X-value", y = "Y-value", a = "Amplitude",
    d = "Displacement", d_cert = "Displacement (certified)"
  )
  df$Panel <- factor(panel_labels[df$Parameter], levels = panel_labels)
  df$Profile <- factor(df$Profile, levels = unique(cov$Profile))

  alpha_lev <- 1 - nominal
  caption <- if (any(df$Structural)) {
    "Open symbol: structurally zero coverage (a percentile amplitude interval cannot contain a zero truth)."
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$Condition, y = .data$Coverage,
                 color = .data$Profile, group = .data$Profile)
  ) +
    ggplot2::annotate(
      "rect", xmin = -Inf, xmax = Inf,
      ymin = 1 - 1.5 * alpha_lev, ymax = 1 - 0.5 * alpha_lev,
      fill = "grey50", alpha = 0.2
    ) +
    ggplot2::geom_hline(yintercept = nominal, linetype = "dashed",
                        color = "grey30") +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$Wilson_lci, ymax = .data$Wilson_uci),
      width = 0.03, na.rm = TRUE
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$Structural), size = 2,
                        na.rm = TRUE) +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1),
                                guide = "none") +
    # drop = FALSE: a parameter with no plottable coverage (e.g. displacement
    # never certified -- the near-zero regime this diagnostic targets) shows
    # as an empty panel rather than silently vanishing
    ggplot2::facet_wrap(~Panel, drop = FALSE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Population amplitude factor (c)",
      y = "Empirical coverage",
      color = "Profile",
      caption = caption
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
