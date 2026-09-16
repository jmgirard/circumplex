# Fixtures for the printed-caution width tests.
#
# One entry per emitter row in the M131 census. Each entry names the emitter
# and holds a quoted expression that, when evaluated, prints the output that
# carries that caution or note. The same list is read by
# test-print-width.R and by tools/m131-caution-word-parity.R, so the width
# test and the word-for-word comparison against the pre-change output can
# never drift onto different fixtures.
#
# Adding an emitter means adding a row here. A row whose expression stops
# firing its caution is caught by caution_fixture_fires(), which every width
# test runs first: a fixture that prints nothing would otherwise pass every
# width assertion silently.

# Capture what an expression prints, at a given console width. The width
# option is restored on the way out, including when the expression fails, so
# one broken fixture cannot leave every later test running at the wrong width.
caution_fixture_output <- function(expr, width, env = parent.frame()) {
  old <- options(width = width)
  on.exit(options(old), add = TRUE)
  utils::capture.output(eval(expr, envir = env))
}

# The marker text that proves a fixture fired its caution, not merely printed.
#
# Whitespace is collapsed on both sides before the match. A caution now wraps
# to the reader's width, so a marker phrase falls across a line break at one
# width and not at another, and the continuation indent would otherwise sit
# in the middle of the phrase. Without this, a marker could pass at width 120
# and fail at width 60 while the caution printed correctly at both.
caution_fixture_fires <- function(lines, marker) {
  squash <- function(x) paste(unlist(strsplit(x, "[ \t\r\n]+")), collapse = " ")
  grepl(squash(marker), squash(paste(lines, collapse = " ")), fixed = TRUE)
}

# Lines of printed output wider than `width` display columns.
caution_overlong_lines <- function(lines, width) {
  lines[nchar(lines, type = "width") > width]
}

# ---- fixture registry (M131 T3) ---------------------------------------------
#
# One entry per emitter row in the M131 census, as a NAMED LIST keyed
# "rowNN_<slug>". Each entry is list(build, print, marker):
#   build  -- a function of no arguments returning the object to print (or
#             NULL where the row prints directly, with no object of its own).
#   print  -- a function taking that object (ignored where build() is NULL)
#             and printing the caution.
#   marker -- a distinctive literal fragment of the caution's own text, used
#             to prove the fixture actually fired it (not merely printed
#             something).
#
# tests/testthat/test-print-width.R iterates this list, one test_that() block
# per entry. tools/m131-caution-word-parity.R (M131 T4) reads the same list to
# compare printed words against a pre-change build, so the fixture-building
# code lives here -- reachable from outside a test_that block -- rather than
# inline in the test file.
#
# Several rows share one cheap base object, mutated per row by setting
# `$details`/`$results`/`$coverage`/`$guardrail` fields directly -- the same
# direct-field approach the repo's own tests use to reach these branches
# (e.g. `fit$details$heywood <- TRUE`). Each base is built at most once per
# caution_fixtures() call (memoized in a private environment), and a list
# copy is cheap in R, so mutating a fetched copy never disturbs sibling rows.
caution_fixtures <- function() {
  cache <- new.env(parent = emptyenv())

  # A cheap analytic-CI CPM fit on the bundled jz2017 octants. Real (not
  # simulated) fit, but ci_method = "analytic" skips the bootstrap entirely,
  # so this is a single fast ML optimization.
  cpm_base <- function() {
    if (is.null(cache$cpm)) {
      data("jz2017", package = "circumplex", envir = environment())
      cache$cpm <- cpm_fit(
        jz2017, scales = PANO(), angles = as.numeric(octants()),
        ci_method = "analytic"
      )
    }
    cache$cpm
  }

  # A cheap axes_reliability() fit: 8 octant scales, 2 items each (16 items),
  # N = 300, no missingness. Two items per scale is the fewest the fitter
  # accepts, and eight octant scales are what the rest of the suite uses, at a
  # size where lavaan converges quickly.
  axes_base <- function() {
    if (is.null(cache$axes)) {
      if (!requireNamespace("lavaan", quietly = TRUE)) {
        testthat::skip("lavaan not installed")
      }
      oct <- octants()
      set.seed(1)
      dat <- axes_simulate(300L, oct, 2L, xi1 = .20, xi2 = .05, zeta1 = .08)
      inames <- sprintf("i%02d", seq_len(ncol(dat)))
      colnames(dat) <- inames
      items <- split(inames, rep(seq_along(oct), each = 2L))
      cache$axes <- suppressMessages(
        axes_reliability(dat, items = items, angles = oct)
      )
    }
    cache$axes
  }

  # A cheap bootstrap ssm_analyze() fit (small N, small boots) for the
  # print.circumplex_ssm() guardrail notes.
  ssm_base <- function() {
    if (is.null(cache$ssm)) {
      data("jz2017", package = "circumplex", envir = environment())
      set.seed(1)
      cache$ssm <- ssm_analyze(jz2017[1:80, ], scales = PANO(), boots = 20)
    }
    cache$ssm
  }

  # A cheap ssm_ci_accuracy() run with structure = "observed" (no CPM fit) and
  # a single amplitude condition (c = 1): the base for every row whose note
  # does not itself need the CPM structure note or a c = 0 rung.
  ci_base <- function() {
    if (is.null(cache$ci)) {
      data("jz2017", package = "circumplex", envir = environment())
      set.seed(1)
      obj <- ssm_analyze(jz2017[1:150, ], scales = PANO(), boots = 40)
      set.seed(2)
      cache$ci <- ssm_ci_accuracy(obj, reps = 3, amplitude_factors = 1,
                                  structure = "observed")
    }
    cache$ci
  }

  # As ci_base(), but with a c = 0 rung too, for the guardrail lines that only
  # exist at Condition == 0.
  ci_guardrail_base <- function() {
    if (is.null(cache$ci_guard)) {
      data("jz2017", package = "circumplex", envir = environment())
      set.seed(1)
      obj <- ssm_analyze(jz2017[1:150, ], scales = PANO(), boots = 40)
      set.seed(2)
      cache$ci_guard <- ssm_ci_accuracy(
        obj, reps = 3, amplitude_factors = c(1, 0), structure = "observed"
      )
    }
    cache$ci_guard
  }

  # The default structure = "cpm" path: one real (small) CPM fit, for the
  # rows whose note reads $details$cpm_diagnostics.
  ci_cpm_base <- function() {
    if (is.null(cache$ci_cpm)) {
      data("jz2017", package = "circumplex", envir = environment())
      set.seed(1)
      obj <- ssm_analyze(jz2017[1:150, ], scales = PANO(), boots = 40)
      set.seed(2)
      cache$ci_cpm <- ssm_ci_accuracy(obj, reps = 3, amplitude_factors = 1)
    }
    cache$ci_cpm
  }

  # A contrast ssm_ci_accuracy() run, for the paired-contrast caveat (which
  # needs $details$contrast TRUE and a named row_n with the contrast last).
  ci_contrast_base <- function() {
    if (is.null(cache$ci_contrast)) {
      data("jz2017", package = "circumplex", envir = environment())
      set.seed(1)
      obj <- ssm_analyze(jz2017[1:150, ], scales = PANO(), grouping = "Gender",
                         contrast = TRUE, boots = 40)
      set.seed(2)
      cache$ci_contrast <- ssm_ci_accuracy(
        obj, reps = 3, amplitude_factors = 1, structure = "observed"
      )
    }
    cache$ci_contrast
  }

  list(
    # ---- Class 1: cpm_oop.R print()/summary() notes (H mechanism) ---------
    row01_convergence = list(
      build = function() {
        x <- cpm_base()
        x$details$accepted <- FALSE
        x$details$gradient_norm <- 0.5
        x
      },
      print = print,
      marker = "convergence acceptance criterion"
    ),
    row02_heywood = list(
      build = function() {
        x <- cpm_base()
        x$details$heywood <- TRUE
        x
      },
      print = print,
      marker = "Heywood-type solution"
    ),
    row03_sigma_pathology = list(
      build = function() {
        x <- cpm_base()
        x$details$sigma_pathology <- TRUE
        x
      },
      print = print,
      marker = "departs materially from 1"
    ),
    row04_removed_harmonics = list(
      build = function() {
        x <- cpm_base()
        x$details$removed_harmonics <- 1L
        x
      },
      print = print,
      marker = "were on the zero boundary and removed"
    ),
    row05_near_tied_optima = list(
      build = function() {
        x <- cpm_base()
        x$details$multimodal <- TRUE
        x
      },
      print = print,
      marker = "competing near-tied optima"
    ),
    row06_discarded_boots = list(
      build = function() {
        x <- cpm_base()
        x$details$ci_method <- "bootstrap"
        x$details$boots <- 100L
        x$details$boots_used <- 90L
        x$details$boots_degenerate <- 5L
        x$details$boots_nonconvergent <- 5L
        x
      },
      print = print,
      marker = "bootstrap resamples were"
    ),
    # ---- Class 2: cpm_oop.R bootstrap fired-marker note (G70 mechanism) ---
    row07_bootstrap_marker_note = list(
      build = function() {
        x <- cpm_base()
        x$details$ci_method <- "bootstrap"
        x$details$heywood <- TRUE
        x
      },
      print = summary,
      marker = "boundary/weak-identification markers fired"
    ),
    row08_analytic_ci_n = list(
      build = function() {
        x <- cpm_base()
        x$details$N <- 100
        x
      },
      print = summary,
      marker = "analytic (Wald) confidence intervals may materially mis-cover"
    ),
    row09_analytic_ci_marker = list(
      build = function() {
        x <- cpm_base()
        x$details$N <- 10000
        x$details$heywood <- TRUE
        x
      },
      print = summary,
      marker = "near a parameter boundary"
    ),
    row10_free_scaling_no_ci = list(
      build = function() {
        x <- cpm_base()
        x$details$scaling <- "free"
        x$details$N <- 100000
        x
      },
      print = summary,
      marker = "carry no confidence interval"
    ),
    row11_low_model_fit = list(
      build = function() {
        x <- ssm_base()
        x$results$fit_est[[1]] <- 0.5
        x
      },
      print = print,
      marker = "model fit is inadequate"
    ),
    row12_amplitude_not_certified = list(
      build = function() {
        x <- ssm_base()
        x$results$a_lci[[1]] <- 0
        x$results$a_uci[[1]] <- 1
        x
      },
      print = print,
      marker = "amplitude CI lower bound is under"
    ),
    row13_draws_not_certified = list(
      build = function() {
        theta <- as.numeric(octants()) * pi / 180
        draws <- rbind(
          1 + 2 * cos(theta) + 2 * sin(theta),
          2 - 1 * cos(theta) + 0 * sin(theta)
        )
        x <- ssm_draws(draws, angles = octants())
        x$results$a_lci[[1]] <- 0
        x$results$a_uci[[1]] <- 1
        x
      },
      print = print,
      marker = "amplitude CrI lower bound is under"
    ),
    row14_uncalibrated_nv = list(
      build = function() {
        data("jz2017", package = "circumplex", envir = environment())
        fit_structure(jz2017, scales = PANO()[1:6])
      },
      print = print,
      marker = "no interpretive cutoffs are calibrated"
    ),
    row15_heuristic_caveat = list(
      build = function() {
        data("jz2017", package = "circumplex", envir = environment())
        fit_structure(jz2017, scales = PANO())
      },
      print = print,
      marker = "heuristic likelihood classifications"
    ),
    row16_boundary_solution = list(
      build = function() {
        x <- axes_base()
        x$details$boundary <- TRUE
        x
      },
      print = print,
      marker = "boundary solution was reached"
    ),
    row17_equal_axes_reliability = list(
      build = function() {
        x <- axes_base()
        x$details$boundary <- FALSE
        x$results$reliability[[2]] <- x$results$reliability[[1]]
        x
      },
      print = print,
      marker = "share one axes-variance estimate"
    ),
    row18_nb_na_cormat = list(
      build = function() {
        x <- axes_base()
        x$details$input <- "cormat"
        x
      },
      print = print,
      marker = "needs the raw item scores"
    ),
    row19_nb_na_fiml = list(
      build = function() {
        x <- axes_base()
        x$details$nb_reason <- "fiml"
        x
      },
      print = print,
      marker = "every respondent"
    ),
    row20_nb_na_single_item = list(
      build = function() {
        x <- axes_base()
        x$details$nb_reason <- "single_item"
        x
      },
      print = print,
      marker = "carrying only one item"
    ),
    row21_se_correction_failed = list(
      build = function() {
        x <- axes_base()
        x$details$se_correction_failed <- "singular"
        x$components$SE <- NA_real_
        x
      },
      print = print,
      marker = "could not be computed"
    ),
    row22_correlation_as_covariance = list(
      build = function() axes_base(),
      print = print,
      marker = "Cudeck, 1989"
    ),
    row23_se_corrected_numbers = list(
      build = function() axes_base(),
      print = print,
      marker = "adjusted to the correlation metric"
    ),
    row24_fiml_se_caveat = list(
      build = function() {
        x <- axes_base()
        x$details$missing <- "fiml"
        x
      },
      print = print,
      marker = "observed-information SEs on the standardized metric"
    ),
    row25_scaled_fit = list(
      build = function() axes_base(),
      print = summary,
      marker = "chisq, pvalue, rmsea and cfi are scaled"
    ),
    row26_fit_scaling_failed = list(
      build = function() {
        x <- axes_base()
        x$details$fit_scaling_failed <- "unidentified"
        x$fit[c("chisq", "pvalue", "rmsea", "cfi")] <- NA_real_
        x
      },
      print = summary,
      marker = "could not be scaled to the"
    ),

    # ---- Class 4: ssm_ci_cat_line() (L mechanism) --------------------------
    row27_guardrail_false_cert = list(
      build = function() {
        x <- ci_guardrail_base()
        x$guardrail$Caution[x$guardrail$Condition == 0] <- TRUE
        x
      },
      print = print,
      marker = "far more often than the"
    ),
    row28_plain_language_verdict = list(
      build = function() ci_guardrail_base(),
      print = print,
      marker = "Verdict:"
    ),

    # ---- Class 3: ssm_ci_cat_para() (S78 mechanism) ------------------------
    row29_occasions_structure = list(
      build = function() {
        x <- ci_base()
        x$details$occ_k <- 2L
        x
      },
      print = summary,
      marker = "population simulated from a multivariate normal with"
    ),
    row30_occasions_rank_deficient = list(
      build = function() {
        x <- ci_base()
        x$details$occ_k <- 2L
        x$details$rank_deficiency <- list(G1 = list(deficient = TRUE))
        x
      },
      print = summary,
      marker = "rank-deficient in group"
    ),
    row31_observed_structure_sensitivity = list(
      build = function() ci_base(),
      print = summary,
      marker = "sensitivity configuration"
    ),
    row32_cpm_structure_fit = list(
      build = function() ci_cpm_base(),
      print = summary,
      marker = "Browne circular model fit"
    ),
    row33_cpm_not_converged = list(
      build = function() {
        x <- ci_cpm_base()
        x$details$cpm_diagnostics$accepted <- FALSE
        x
      },
      print = summary,
      marker = "verdict unreliable"
    ),
    row34_cpm_poor_fit = list(
      build = function() {
        x <- ci_cpm_base()
        x$details$cpm_diagnostics$accepted <- TRUE
        x$details$cpm_diagnostics$rmsea <- 0.5
        x
      },
      print = summary,
      marker = "the structural model fits poorly"
    ),
    row35_cpm_adequate_fit = list(
      build = function() {
        x <- ci_cpm_base()
        x$details$cpm_diagnostics$accepted <- TRUE
        x$details$cpm_diagnostics$rmsea <- 0.03
        x$details$cpm_diagnostics$srmr <- 0.02
        x
      },
      print = summary,
      marker = "structure fits adequately"
    ),
    row36_cpm_marginal_fit = list(
      build = function() {
        x <- ci_cpm_base()
        x$details$cpm_diagnostics$accepted <- TRUE
        x$details$cpm_diagnostics$rmsea <- 0.09
        x$details$cpm_diagnostics$srmr <- 0.09
        x
      },
      print = summary,
      marker = "structural fit is marginal"
    ),
    row37_boundary_markers = list(
      build = function() {
        x <- ci_cpm_base()
        x$details$cpm_diagnostics$accepted <- TRUE
        x$details$cpm_diagnostics$rmsea <- 0.03
        x$details$cpm_diagnostics$srmr <- 0.02
        x$details$cpm_diagnostics$markers <- c("Heywood communality")
        x
      },
      print = summary,
      marker = "Boundary markers:"
    ),
    row38_psd_repair = list(
      build = function() {
        x <- ci_base()
        x$details$max_psd_delta <- 0.05
        x
      },
      print = summary,
      marker = "positive-semidefiniteness repair"
    ),
    row39_near_zero_amplitude = list(
      build = function() {
        x <- ci_base()
        x$details$near_zero_rows <- "Row1"
        x
      },
      print = summary,
      marker = "Near-zero regime:"
    ),
    row40_structural_coverage_zero = list(
      build = function() {
        x <- ci_base()
        x$coverage$Structural[[1]] <- TRUE
        x
      },
      print = summary,
      marker = "structurally"
    ),
    row41_paired_contrast_caveat = list(
      build = function() {
        x <- ci_contrast_base()
        x$details$occ_k <- 2L
        con_lab <- names(x$details$row_n)[length(x$details$row_n)]
        idx <- x$guardrail$Profile == con_lab & x$guardrail$Condition == 1
        x$guardrail$Cert_rate[idx] <- 0.5
        x
      },
      print = summary,
      marker = "Paired-contrast caveat"
    )
  )
}
