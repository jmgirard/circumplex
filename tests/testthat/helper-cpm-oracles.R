# Shared CPM oracle fixtures (M4/B6). Sourced by testthat for every test
# file; used by test-cpm_oracles.R (the validation battery) and
# test-cpm_api.R (the coverage-calibrated summary() caution).
#
# Published-value provenance: see the header of test-cpm_oracles.R (all
# values transcribed 2026-07-06 from Grassi, Luccio & Di Blas, 2010, BRM
# 42(1), 55-73; second independent human re-read of the Grassi values
# completed 2026-07-19 by Jeff -- see that header for what it corrected).

# ---- fixtures ----------------------------------------------------------------

# Vocational interest scales, Table 1 (p. 58; = Browne, 1992, Table 2), N = 175.
cpm_oracle_voc <- function() {
  nm <- c("Health", "Science", "Technology", "Trades",
          "BusinessOperations", "BusinessContact", "Social")
  R <- matrix(0, 7, 7, dimnames = list(nm, nm))
  R[lower.tri(R)] <- c(
    .654, .453, .251, .122, .218, .496,
          .644, .440, .158, .210, .264,
                .757, .551, .570, .366,
                      .493, .463, .202,
                            .754, .471,
                                  .650)
  R <- R + t(R)
  diag(R) <- 1
  list(R = R, N = 175, names = nm,
       # Table 2 model 1a m = 1 angles, whole degrees (inputs/start values
       # only, never expectations; they are the mirror of the Appendix A
       # direction -- the paper itself prints both, cf. its "360-ang. pos.")
       th_start = c(0, 55, 112, 123, 192, 210, 269))
}

# Appendix A (pp. 70-72), unconstrained m = 1: full-precision estimates.
# Every vector below is stored in Table-1 variable order. The Appendix prints
# these blocks in its OWN order -- Health, Social, BusinessContact,
# BusinessOperations, Trades, Technology, Science, i.e. ascending in its
# mirrored angle -- so a value read off the page must be re-mapped by scale
# before it is compared here (2026-07-19 re-read).
cpm_oracle_voc_appendix <- function() {
  list(
    theta = c(0, 305.35328, 247.82980, 237.38218, 168.30615, 149.83787,
              91.25973),
    theta_se = c(0, 9.01111, 7.35838, 9.44904, 9.08050, 7.95016, 8.72929),
    v = c(0.15438, 0.51654, 0.03945, 0.63153, 0.54550, 0.13449, 0.44771),
    v_se = c(0.13759, 0.12755, 0.04238, 0.13854, 0.12125, 0.05959, 0.13865),
    z = c(0.91358, 0.81222, 1.00102, 0.79058, 0.79269, 0.92497, 0.84376),
    beta = c(0.6378, 0.3622),
    mcsc = 0.276,                       # rho(180)
    Fhat = 0.089815,                    # iteration trace, "final value"
    Tstat = 15.63, df = 7, pvalue = 0.029,
    F0 = 0.049, F0_ci = c(0.005, 0.139),
    rmsea = 0.084, rmsea_ci = c(0.026, 0.141),
    null_chisq = 747.663, null_df = 21,
    tli = 0.964, cfi = 0.988, srmr = 0.04,
    var_ratios = c(0.963, 1.000, 1.042, 1.020, 0.971, 0.971, 1.031),
    # communality indices rho-hat(x_i, c_i) (Browne, 1992, Eq. 4) with their
    # 95% CIs, in Table-1 variable order
    comm = c(.93, .81, .98, .78, .80, .94, .83),
    comm_ci = rbind(c(.73, .99), c(.74, .87), c(.87, 1), c(.71, .84),
                    c(.74, .86), c(.87, .97), c(.74, .90))
  )
}

# Verbal ability tests, Listing 7 (p. 68; Guttman, 1954, p. 282, also Browne,
# 1992, p. 470), N = 1046 (Listing 8). Used for input-refusal behavior only.
cpm_oracle_verbal <- function() {
  nm <- c("Spelling", "Punctuation", "Grammar", "Vocabulary", "Literature",
          "ForeignLiterature")
  R <- matrix(0, 6, 6, dimnames = list(nm, nm))
  R[lower.tri(R)] <- c(
    .621, .564, .476, .394, .389,
          .742, .503, .461, .411,
                .577, .472, .429,
                      .688, .548,
                            .639)
  R <- R + t(R)
  diag(R) <- 1
  list(R = R, N = 1046, names = nm)
}

# Shortest circular distance in degrees.
cpm_angdiff_deg <- function(a, b) {
  d <- (a - b) %% 360
  pmin(d, 360 - d)
}

# Max angular discrepancy after trying both reflections about the reference
# (design sec. 2.1/6.5: published angles are compared reference-relative and
# mirror-aware, never as raw orientations).
cpm_mirror_diff_deg <- function(ours, pub, ref = 1) {
  refl <- (2 * ours[ref] - ours) %% 360
  min(max(cpm_angdiff_deg(ours, pub)), max(cpm_angdiff_deg(refl, pub)))
}

# In-family population matrix with well-separated identification (non-octant
# angles, varied zeta, interior beta): Hessian condition ~1e3, no boundary.
# The smallest beta (.15) matches the coverage oracle's interior
# configuration and keeps clear margin above summary()'s 0.10
# boundary-proximity marker (a .10 truth sat exactly on the strict < 0.10
# threshold, a platform-dependent knife edge -- B6 review fix).
cpm_clean_truth <- function() {
  list(angles = c(360, 40, 95, 150, 190, 230, 285, 330),
       zeta = c(.85, .7, .8, .65, .75, .8, .7, .6),
       beta = c(.35, .30, .20, .15))
}

# The battery's shared unconstrained m = 1 fit of the vocational matrix,
# computed once per test run (several test blocks consume the identical fit).
cpm_oracle_voc_fit <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      voc <- cpm_oracle_voc()
      cache <<- cpm_fit(cormat = voc$R, scales = voc$names,
                        angles = voc$th_start, n = voc$N, m = 1)
    }
    cache
  }
})
