# Fixtures for the printed invariance-ladder block of a grouped ssm_sem()
# result (M130): one fitted case per verdict arm of sem_fit_ladder() and per
# Delta-CFI scope branch. Each case is fitted once per test run and cached,
# because several tests read the same fit. The populations are built with
# sem_pop() (helper-ssm-sem.R), so they need no test-file helpers.

ladder_oct <- as.numeric(octants())

# Two groups under metric invariance; `eps` multiplies group B's circumplex
# saturations by (1 + eps * cos(2 * theta)), a second-harmonic pattern
# violation (eps = 0 is exactly metric-invariant). Same construction as
# dcfi_pop_2g() in test-ssm_sem_groups.R.
ladder_pop <- function(eps = 0) {
  a <- seq(0.5, 0.7, length.out = 8)
  cc <- seq(0.65, 0.55, length.out = 8)
  d1 <- 40 * pi / 180
  d2 <- 110 * pi / 180
  g_a <- sem_pop(
    a, cc, seq(0.3, 0.6, length.out = 8), ladder_oct,
    cbind(c(0.2, 0.4 * cos(d1), 0.4 * sin(d1))),
    v_m = 1
  )
  g_b <- sem_pop(
    a, cc * (1 + eps * cos(2 * ladder_oct * pi / 180)),
    seq(0.4, 0.7, length.out = 8), ladder_oct,
    cbind(c(0.25, 0.35 * cos(d2), 0.35 * sin(d2))),
    v_m = 1
  )
  list(A = g_a$sigma, B = g_b$sigma)
}

# Loadings fixed at (1, cos, sin), as the strict tier assumes, with one measure
ladder_pop_strict <- function() {
  g <- sem_pop(
    rep(1, 8), rep(1, 8), seq(0.3, 0.6, length.out = 8), ladder_oct,
    cbind(c(0.2, 0.3 * cos(0.7), 0.3 * sin(0.7))),
    v_m = 1, phi = diag(c(0.8, 0.5, 0.5))
  )
  list(A = g$sigma, B = g$sigma)
}

# Raw data for any number of groups. `shift` adds item-specific intercept
# differences to every group after the first, which breaks scalar invariance.
ladder_sim <- function(sigmas, n_per, seed, shift = NULL) {
  set.seed(seed)
  dat <- do.call(rbind, lapply(names(sigmas), function(g) {
    sig <- sigmas[[g]]
    x <- as.data.frame(matrix(rnorm(n_per * ncol(sig)), n_per) %*% chol(sig))
    colnames(x) <- colnames(sig)
    x$grp <- g
    x
  }))
  if (!is.null(shift)) {
    later <- dat$grp != names(sigmas)[[1]]
    sc <- paste0("s", 1:8)
    dat[later, sc] <- sweep(dat[later, sc], 2, shift, "+")
  }
  dat
}

# lavaan's nested test returning no statistic: a two-row table whose
# difference columns are NA, the shape sem_fit_ladder() indexes
ladder_na_lrt <- function(...) {
  data.frame(
    `Chisq diff` = c(NA_real_, NA_real_), `Df diff` = c(NA_real_, NA_real_),
    `Pr(>Chisq)` = c(NA_real_, NA_real_), check.names = FALSE
  )
}

ladder_case_names <- c(
  "configural", "vacuous", "vacuous_above", "retained_one", "retained_two",
  "retained_above", "rejected_contrast", "rejected_plain", "untestable_contrast",
  "untestable_plain", "gls", "groups3_ml", "groups3_mlr"
)

# Fit one case: the result, the warning messages it raised and, when it has
# one, the ssm_plot_contrast() error message
ladder_fit_case <- function(name) {
  sc <- paste0("s", 1:8)
  # The seed formal is `rng`, because `se = ` in `...` would partially match
  # a formal named `seed`
  fit <- function(dat, rng, ...) {
    set.seed(rng)
    ssm_sem(dat, scales = sc, grouping = "grp", boots = 20, ...)
  }
  run <- function() {
    switch(name,
      configural = fit(ladder_sim(ladder_pop(0), 400, 11), 1,
        measures = "m1", invariance = "configural", estimator = "ML"
      ),
      vacuous = fit(ladder_sim(ladder_pop_strict(), 600, 32), 2,
        measures = "m1", contrast = TRUE, model = "strict", estimator = "ML"
      ),
      vacuous_above = fit(
        ladder_sim(ladder_pop_strict(), 600, 32,
          shift = seq(0.5, 1.2, length.out = 8)
        ), 3,
        measures = "m1", contrast = TRUE, model = "strict",
        invariance = "scalar", estimator = "ML"
      ),
      retained_one = fit(ladder_sim(ladder_pop(0), 400, 17), 4,
        measures = "m1", contrast = TRUE
      ),
      retained_two = fit(ladder_sim(ladder_pop(0), 400, 16), 5,
        contrast = TRUE
      ),
      retained_above = fit(
        ladder_sim(ladder_pop(0), 700, 23,
          shift = seq(0.5, 1.2, length.out = 8)
        ), 6,
        measures = "m1", contrast = TRUE, invariance = "scalar"
      ),
      rejected_contrast = fit(ladder_sim(ladder_pop(0.35), 700, 61), 7,
        measures = "m1", contrast = TRUE, estimator = "ML"
      ),
      rejected_plain = fit(ladder_sim(ladder_pop(0.35), 700, 61), 7,
        measures = "m1", estimator = "ML"
      ),
      untestable_contrast = testthat::with_mocked_bindings(
        fit(ladder_sim(ladder_pop(0), 400, 11), 8,
          measures = "m1", contrast = TRUE, estimator = "ML"
        ),
        lavTestLRT = ladder_na_lrt, .package = "lavaan"
      ),
      untestable_plain = testthat::with_mocked_bindings(
        fit(ladder_sim(ladder_pop(0), 400, 11), 8,
          measures = "m1", estimator = "ML"
        ),
        lavTestLRT = ladder_na_lrt, .package = "lavaan"
      ),
      gls = fit(ladder_sim(ladder_pop(0), 300, 11), 9,
        measures = "m1", estimator = "GLS", se = "standard"
      ),
      groups3_ml = fit(
        ladder_sim(list(
          A = ladder_pop(0)$A, B = ladder_pop(0.02)$B,
          C = ladder_pop(-0.02)$B
        ), 400, 41), 10,
        measures = "m1", estimator = "ML"
      ),
      groups3_mlr = fit(
        ladder_sim(list(
          A = ladder_pop(0)$A, B = ladder_pop(0.02)$B,
          C = ladder_pop(-0.02)$B
        ), 400, 41), 11,
        measures = "m1"
      ),
      stop("unknown ladder case: ", name)
    )
  }
  warnings <- character(0)
  res <- withCallingHandlers(run(), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  plot_error <- tryCatch(
    {
      ssm_plot_contrast(res)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
  list(res = res, warnings = warnings, plot_error = plot_error)
}

ladder_cache <- new.env(parent = emptyenv())

ladder_case <- function(name) {
  if (!exists(name, envir = ladder_cache, inherits = FALSE)) {
    assign(name, ladder_fit_case(name), envir = ladder_cache)
  }
  get(name, envir = ladder_cache, inherits = FALSE)
}

# The ladder block as print() shows it, from the "Invariance ladder" line to
# the line before the first "# Profile" line
ladder_block_lines <- function(res, width = 80) {
  out <- withr::with_options(
    list(width = width), utils::capture.output(print(res))
  )
  start <- grep("^Invariance ladder", out)
  expect_length(start, 1)
  end <- grep("^# Profile", out)
  end <- end[end > start][[1]] - 1
  while (end > start && !nzchar(trimws(out[[end]]))) {
    end <- end - 1
  }
  out[start:end]
}
