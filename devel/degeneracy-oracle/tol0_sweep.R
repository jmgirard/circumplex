# The tol = 0 sweep: does the per-fit certificate catch every matrix the
# default solve() tolerance used to refuse? (M147; D-061; RR22 rec 9.)
#
# Run from the repo root:  Rscript devel/degeneracy-oracle/tol0_sweep.R
# Needs python3 (devel/degeneracy-oracle/exact_oracle.py, standard library
# only). Writes tol0-sweep-results.rds and tol0-sweep-summary.md beside this
# file; both are regenerable from committed material alone.
#
# WHAT IS MEASURED. axes_pricing_core() inverts the information matrix
# Delta'V Delta. With R's default solve() tolerance that inversion refuses
# "unidentified" wherever LAPACK's reciprocal condition estimate falls below
# .Machine$double.eps, a threshold RR22 measured straddling real platforms at
# counterexample B. M147 replaces that gate by `tol = 0` on the certified path
# and lets the certificate judge. This script measures, at every matrix of
# four stated families, what each tolerance does and what the certificate and
# the exact-rational oracle say where the two tolerances disagree.
#
# THE SCRIPT IS DESIGN-AGNOSTIC. It reads the shipped axes_pricing_core() and
# builds a tolerance-parametrised replica of it from the function's own text:
# where the shipped function already carries a `tol` formal (after M147 T4)
# the replica IS the shipped function; where it does not (at HEAD before T4)
# the one `solve(info)` call is rewritten to `solve(info, tol = tol)` and
# nothing else changes, which the builder asserts. Every column below is then
# read off the replica at both tolerances, off the shipped predicate as it
# stands, and off the same predicate with the core rebound to tol = 0 (the
# "tol0 world"; a no-op once the shipped core defaults to 0). So the script
# runs unchanged at HEAD (the baseline, T1) and after the core change (T5).
#
# FAMILIES (the domain enumerates itself; an empty family fails (d)):
#   cert    the six committed certificate cases: the five cos()-built anchors
#           of tests/testthat/test-axes-certificate.R and counterexample B
#           (tests/testthat/fixtures/rb18-counterexample-b.rds).
#   q4      the RR18 Q4 fixture-perturbation sweep, S_t = t*S_B + (1-t)*I at
#           the three t of devel/degeneracy-oracle/exact_oracle.R.
#   m106    the M106 reachable families A (p = 8 and p = 24) and C (p = 4)
#           and the near-duplicate family B (p = 9), each on an item-error
#           grid eps = 10^seq(-1, -12, by = -0.5) that crosses the floor;
#           family A at 6 and 12 equally spaced scales (one item each) on the
#           same grid (RR24 rec 3); and ONE p = 64 row (8 octant scales, 8
#           items, item error 0.3, above the floor) whose certificate wall
#           time is recorded and which is asserted outside the region, so the
#           exact oracle is never handed a 64 x 64 pipeline.
#   random  a seeded model-implied family Sigma = xi1*C + xi2*J + zeta1*B +
#           diag(e), cov2cor'd, at p in {4, 8, 9, 16, 24} (the M106 shapes:
#           four cardinal scales; one, two and three items per octant scale;
#           octants plus a duplicate-angle ninth item), with xi1 ~ U(.2,.5),
#           xi2 ~ U(0,.3), zeta1 ~ U(0,.3) where two or more items share a
#           scale and 0 otherwise, and e_i = level * exp(N(0, .5)) with
#           log10(level) ~ U(-12, -1). Draws per p: 40, 40, 40, 30, 20; plus
#           4 rows per p with a NEGATIVE component (zeta1 = -0.05 where a
#           scale shares items, else xi2 = -0.02), the boundary class
#           axes_is_boundary() names (RR24 rec 3). set.seed(20260921) once
#           before the family.
#   blocks  the crossed-block family fitting zeta2: axes_crossed_blocks()
#           layouts at 4 and 8 scales with 2 and 3 items, zeta2 in {0.1,
#           0.25}, xi1 = .3, xi2 = .2, zeta1 = .1, on the m106 item-error
#           grid; zeta1 and zeta2 fitted per the package's own predicates.
#   struct  designs singular in exact arithmetic: one item per scale with
#           zeta1 forced fitted at item errors {.5, .3, .1, .03, .01, 1e-3,
#           1e-4} (the same-scale matrix IS the identity), and two antipodal
#           blocks over the four cardinal scales with zeta2 forced fitted at
#           item errors {.3, .1, .03} (C = 2B - Z up to cosine rounding).
#           FAMILY MEMBERSHIP DOES NOT DECIDE REGION MEMBERSHIP; the core's
#           answer does (below): the identity rows are refused on an exact
#           structural ground and leave the region, the antipodal rows are
#           cosine-inexact and stay in it.
#
# COLUMNS, per matrix: family, id, p, n_scales, spacing
# (angles_spacing_status() of the distinct scale angles), zeta1 and zeta2
# (fitted in the derivative set), api_zeta1 and api_zeta2 (what
# axes_fits_zeta1() / axes_fits_zeta2() would fit for the design), min_items
# (smallest item count on any scale), kappa (eigenvalue ratio), floor (the
# criterion's answer, "NULL" where it admits), rcond_sigma, rcond_info
# (computed here from the information matrix, independently of the core),
# structural (the design carries an exact ground: a bit-identical pair among
# the component matrices or a component identical to the identity),
# default_outcome (the replica at tol = .Machine$double.eps: "inverted",
# "refused", "sigma-singular", "structural"), tol0_outcome (the replica at
# tol = 0: "inverted", "exact-singular" (LAPACK dgesv INFO > 0), "nonfinite",
# "other-error", "sigma-singular", "structural"), cert_secs (the
# certificate's wall time, recorded only), bit_identical (si, sim and acov identical across the two
# tolerances where both inverted), shipped_reason (axes_corrected_se()'s
# `reason` with the shipped core, "computes" where NULL), shipped_naive_reason,
# tol0_reason and tol0_naive_reason (the same in the tol0 world), cert_se,
# cert_cval, cert_ratio (the certificate in the tol0 world, where tol = 0
# inverted), and in the REGION -- default refused, tol = 0 inverted, and no
# structural ground (no oracle is called outside it) -- oracle_status, true_se, true_cval, true_ratio (the exact-rational oracle's
# relative errors of the tol0 doubles), and under_report (the certificate is
# graded, i.e. below its sentinel 1, and some field sits below its true
# error).
#
# PRE-REGISTERED ACCEPTANCE (M147 AC2), evaluated on the committed run and
# printed at the end:
#   (a) At every REGION matrix whose tol0 double pricing produced numbers:
#       the oracle ran, no under-report, and either tol0_reason is
#       "uncertified" or every true error is at or below delta_star (1e-4).
#       At a REGION matrix whose double pricing refused after the inversion
#       (a nonpositive quadratic form, "indefinite"), tol0_reason is not
#       "computes". At every matrix BOTH tolerances refused whose floor answer
#       is neither "indefinite" nor "singular", tol0_reason is "unidentified".
#   (b1) At every struct-family matrix rcond_info is below the selector
#        threshold sqrt(.Machine$double.eps) divided by 1e4.
#   (b2) Every matrix the floor admits ("NULL") whose recorded columns show a
#        design the exported API admits (n_scales >= 4, spacing "ok", zeta1
#        == api_zeta1, zeta2 == api_zeta2) and that the selector routes
#        (rcond_info below the threshold) passes its certificate (tol0_reason
#        "computes"); the list is non-empty; at least one blocks-family
#        matrix with api_zeta2 TRUE is floor-admitted in that domain; the
#        p = 3 rows are listed separately as out of domain.
#   (c) Every matrix inverted under the default tolerance has bit_identical
#       TRUE. Expected to hold BY CONSTRUCTION: La_solve factorises once and
#       checks rcond afterwards, so `tol` cannot change the factorisation
#       (RR24 B5); kept as a tripwire against a future solve().
#   (d) No family is empty, and the run holds at least one p = 64 row, at
#       least one row each at 6 and 12 scales, and at least one row with a
#       negative zeta1 or xi2. The p = 64 row is outside the region.
# At HEAD (before T4) the last clause of (a) was expected to FAIL: the floor
# consulted the certificate ahead of the pricing there, so a zero pivot
# surfaced as the sentinel's "uncertified" rather than as "unidentified".
# That baseline (commit 7856f70d) is what T2's brief read; the acceptance
# binds the T5 run.
#
# ALSO REPORTED, outside the acceptance: every matrix with floor "NULL" and
# rcond_sigma below .Machine$double.eps (a hit means solve(sigma)'s default
# tolerance can bite above the floor -- a candidate row, per the plan's Out
# clause), and every REGION matrix whose oracle failed (an exactly singular
# information matrix in rationals that tol = 0 nonetheless inverted -- the
# reopening class D-061 names).

suppressMessages(pkgload::load_all(quiet = TRUE))
source(file.path("tests", "testthat", "helper-m106-degeneracy.R"))

N <- 600
EPS <- .Machine$double.eps
DELTA_STAR <- axes_degeneracy_delta_star
OUT_RDS <- file.path("devel", "degeneracy-oracle", "tol0-sweep-results.rds")
OUT_MD  <- file.path("devel", "degeneracy-oracle", "tol0-sweep-summary.md")
FIXTURE <- file.path("tests", "testthat", "fixtures", "rb18-counterexample-b.rds")
PY <- file.path("devel", "degeneracy-oracle", "exact_oracle.py")

# ---- the tolerance-parametrised replica of the shipped core -----------------
ns <- asNamespace("circumplex")
shipped_core <- get("axes_pricing_core", envir = ns)
shipped_has_tol <- "tol" %in% names(formals(shipped_core))
replica <- if (shipped_has_tol) {
  shipped_core
} else {
  f <- shipped_core
  attr(f, "srcref") <- NULL           # deparse the parse tree, not the source text
  txt <- deparse(f, width.cutoff = 500L)
  hit <- grepl("solve(info)", txt, fixed = TRUE)
  stopifnot("exactly one solve(info) call in the shipped core" = sum(hit) == 1L)
  txt[hit] <- sub("solve(info)", "solve(info, tol = tol)", txt[hit], fixed = TRUE)
  stopifnot(grepl("^function \\(sigma, d\\)", txt[1L]))
  txt[1L] <- sub("function (sigma, d)", "function (sigma, d, tol = 0)", txt[1L],
                 fixed = TRUE)
  g <- eval(parse(text = txt, keep.source = FALSE))
  environment(g) <- environment(shipped_core)
  g
}

# Evaluate `expr` with the shipped core rebound to tol = 0. A no-op once the
# shipped core defaults to 0; before that the rebinding is what lets the
# certificate and both surfaces be measured in the world M147 ships.
tol0_world <- function(expr) {
  if (shipped_has_tol) return(expr)
  wrapper <- function(sigma, d, ...) replica(sigma, d, tol = 0)
  assignInNamespace("axes_pricing_core", wrapper, ns = "circumplex")
  on.exit(assignInNamespace("axes_pricing_core", shipped_core, ns = "circumplex"),
          add = TRUE)
  expr
}

# The information matrix, as the core forms it, for the direct solve() whose
# error class names the tol = 0 outcome. Cross-checked below against the
# replica's own verdict at every matrix.
info_of <- function(sigma, d) {
  si <- solve(sigma)
  sim <- lapply(d$mats, function(m) si %*% m)
  q <- length(sim)
  info <- matrix(0, q, q)
  for (s in seq_len(q)) for (t in s:q) {
    info[s, t] <- info[t, s] <- 0.5 * sum(sim[[s]] * t(sim[[t]]))
  }
  info
}

# ---- the exact-rational oracle (the same handover exact_oracle.R makes) ----
hex_dump <- function(S, mats, n_comp, df, baseline_df) {
  f <- tempfile(fileext = ".txt")
  h <- function(v) paste(sprintf("%a", as.numeric(v)), collapse = " ")
  writeLines(c(
    sprintf("P: %d", nrow(S)), sprintf("N: %d", N), sprintf("DF: %d", df),
    sprintf("BASELINE_DF: %d", baseline_df), sprintf("NCOMP: %d", n_comp),
    sprintf("Q: %d", length(mats)), sprintf("S: %s", h(S)),
    vapply(seq_along(mats), function(i) sprintf("M%d: %s", i, h(mats[[i]])), "")
  ), f)
  f
}
exact <- function(S, d, df, baseline_df) {
  out <- suppressWarnings(system2("python3",
                 c(PY, hex_dump(S, d$mats, d$n_comp, df, baseline_df)),
                 stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    return(list(status = paste0("failed: ", utils::tail(out, 1L))))
  }
  key <- sub(":.*$", "", out)
  val <- trimws(sub("^[A-Z_0-9]+:", "", out))
  res <- as.list(stats::setNames(suppressWarnings(as.numeric(val)), key))
  need <- c("EXACT_CVAL", "EXACT_SE1", "EXACT_RATIO1")
  if (!all(need %in% names(res))) {
    return(list(status = "failed: oracle output missing keys"))
  }
  res$status <- "ok"
  res
}

# ---- the families -----------------------------------------------------------
case <- function(family, id, S, ang, scale, block = NULL,
                 fit_zeta1 = NULL, fit_zeta2 = NULL, neg = FALSE) {
  api_z1 <- axes_fits_zeta1(split(seq_along(scale), scale))
  api_z2 <- axes_fits_zeta2(ang, scale, block)
  z1 <- if (is.null(fit_zeta1)) api_z1 else fit_zeta1
  z2 <- if (is.null(fit_zeta2)) api_z2 else fit_zeta2
  d <- axes_se_derivs(ang, scale, block, z1, z2)
  comp <- d$mats[seq_len(d$n_comp)]
  structural <- any(duplicated(comp)) ||
    any(vapply(comp, identical, TRUE, diag(nrow(S))))
  list(family = family, id = id, S = S, ang = ang, scale = scale,
       block = block, d = d, zeta1 = z1, zeta2 = z2, api_zeta1 = api_z1,
       api_zeta2 = api_z2, structural = structural, neg = neg,
       n_scales = length(unique(scale)),
       spacing = angles_spacing_status(unique(ang)),
       min_items = min(lengths(split(seq_along(scale), scale))))
}
oct <- as.numeric(octants())

fam_cert <- function() {
  fx <- readRDS(FIXTURE)
  c(
    list(case("cert", "a4", m106_family_a(2.4e-4, 1L), oct, as.character(1:8)),
         case("cert", "a5", m106_family_a(2.4e-5, 1L), oct, as.character(1:8)),
         case("cert", "c4", m106_family_c(1.2e-5), c(90, 180, 270, 360),
              as.character(1:4)),
         case("cert", "b9a", m106_family_b(7e-5), c(oct, oct[1L]),
              as.character(c(1:8, 1L))),
         case("cert", "b9b", m106_family_b(7e-6), c(oct, oct[1L]),
              as.character(c(1:8, 1L))),
         case("cert", "cxb", fx$S, as.numeric(fx$ia), c("A", "B", "C")))
  )
}

fam_q4 <- function() {
  fx <- readRDS(FIXTURE)
  lapply(c(1 - 2.5e-5, 1 - 2.5e-4, 1 - 2.5e-3), function(tt) {
    St <- tt * fx$S + (1 - tt) * diag(nrow(fx$S))
    dimnames(St) <- dimnames(fx$S)
    case("q4", sprintf("t=%.7f", tt), St, as.numeric(fx$ia), c("A", "B", "C"))
  })
}

M106_GRID <- 10^seq(-1, -12, by = -0.5)
# Family A at k equally spaced scales, one item each (the octant builder
# generalised; k = 8 reproduces m106_family_a(e, 1L) up to the angle set).
k_family_a <- function(eps, k, xi1 = 0.3, xi2 = 0.3) {
  ang <- seq(360 / k, 360, by = 360 / k)
  rad <- ang * pi / 180
  sg <- xi1 * outer(rad, rad, function(u, v) cos(u - v)) +
    xi2 * matrix(1, k, k) + eps * diag(k)
  nms <- paste0("i", seq_len(k))
  dimnames(sg) <- list(nms, nms)
  list(S = stats::cov2cor(sg), ang = ang, scale = as.character(seq_len(k)))
}
fam_m106 <- function() {
  out <- list()
  big <- m106_family_a(0.3, 8L)
  out <- c(out, list(case("m106", "A64 eps=3.0e-01", big, rep(oct, each = 8L),
                          as.character(rep(1:8, each = 8L)))))
  for (e in M106_GRID) {
    for (k in c(6L, 12L)) {
      g <- k_family_a(e, k)
      out <- c(out, list(case("m106", sprintf("A%d eps=%.1e", k, e), g$S,
                              g$ang, g$scale)))
    }
    tag <- sprintf("eps=%.1e", e)
    out <- c(out, list(
      case("m106", paste0("A8 ", tag), m106_family_a(e, 1L), oct, as.character(1:8)),
      case("m106", paste0("A24 ", tag), m106_family_a(e, 3L),
           rep(oct, each = 3L), as.character(rep(1:8, each = 3L))),
      case("m106", paste0("C4 ", tag), m106_family_c(e), c(90, 180, 270, 360),
           as.character(1:4)),
      case("m106", paste0("B9 ", tag), m106_family_b(e), c(oct, oct[1L]),
           as.character(c(1:8, 1L)))
    ))
  }
  out
}

RANDOM_SEED <- 20260921
RANDOM_DRAWS <- c(`4` = 40L, `8` = 40L, `9` = 40L, `16` = 30L, `24` = 20L)
random_shape <- function(p) {
  switch(as.character(p),
    `4` = list(ang = c(90, 180, 270, 360), scale = as.character(1:4)),
    `8` = list(ang = oct, scale = as.character(1:8)),
    `9` = list(ang = c(oct, oct[1L]), scale = as.character(c(1:8, 1L))),
    `16` = list(ang = rep(oct, each = 2L), scale = as.character(rep(1:8, each = 2L))),
    `24` = list(ang = rep(oct, each = 3L), scale = as.character(rep(1:8, each = 3L)))
  )
}
fam_random <- function() {
  set.seed(RANDOM_SEED)
  out <- list()
  for (p in c(4L, 8L, 9L, 16L, 24L)) {
    sh <- random_shape(p)
    shares <- any(duplicated(sh$scale))
    for (k in seq_len(RANDOM_DRAWS[[as.character(p)]])) {
      xi1 <- stats::runif(1, 0.2, 0.5)
      xi2 <- stats::runif(1, 0, 0.3)
      zeta1 <- if (shares) stats::runif(1, 0, 0.3) else 0
      level <- 10^stats::runif(1, -12, -1)
      e <- level * exp(stats::rnorm(p, 0, 0.5))
      rad <- sh$ang * pi / 180
      cm <- outer(rad, rad, function(u, v) cos(u - v))
      bm <- outer(sh$scale, sh$scale, "==") * 1
      sg <- xi1 * cm + xi2 * matrix(1, p, p) + zeta1 * bm + diag(e)
      nms <- paste0("i", seq_len(p))
      dimnames(sg) <- list(nms, nms)
      out <- c(out, list(case("random", sprintf("p%d #%02d level=%.1e", p, k, level),
                              stats::cov2cor(sg), sh$ang, sh$scale)))
    }
    # The boundary class: a negative component estimate, as a converged fit
    # can return (axes_is_boundary()). Non-PD draws are what the floor's
    # "indefinite" arm exists for and are kept, not resampled.
    for (k in seq_len(4L)) {
      xi1 <- stats::runif(1, 0.2, 0.5)
      xi2 <- if (shares) stats::runif(1, 0, 0.3) else -0.02
      zeta1 <- if (shares) -0.05 else 0
      level <- 10^stats::runif(1, -6, -1)
      e <- level * exp(stats::rnorm(p, 0, 0.5))
      rad <- sh$ang * pi / 180
      cm <- outer(rad, rad, function(u, v) cos(u - v))
      bm <- outer(sh$scale, sh$scale, "==") * 1
      sg <- xi1 * cm + xi2 * matrix(1, p, p) + zeta1 * bm + diag(e)
      nms <- paste0("i", seq_len(p))
      dimnames(sg) <- list(nms, nms)
      S <- if (all(diag(sg) > 0)) stats::cov2cor(sg) else sg
      out <- c(out, list(case("random", sprintf("p%d neg#%d level=%.1e", p, k, level),
                              S, sh$ang, sh$scale, neg = TRUE)))
    }
  }
  out
}

# The crossed-block family (RR24 rec 3): the fifth component, on layouts the
# exported API reaches through `blocks`.
fam_blocks <- function() {
  out <- list()
  for (k in c(4L, 8L)) for (n in c(2L, 3L)) for (z2 in c(0.1, 0.25)) {
    ang_k <- if (k == 4L) c(90, 180, 270, 360) else oct
    bl <- axes_crossed_blocks(k, n)
    for (e in M106_GRID) {
      pop <- axes_population_cor(ang_k, n, xi1 = .3, xi2 = .2, zeta1 = .1,
                                 zeta2 = z2, item_block = bl)
      sg <- pop$sigma
      diag(sg) <- .3 + .2 + .1 + z2 + e
      p <- nrow(sg)
      nms <- paste0("i", seq_len(p))
      dimnames(sg) <- list(nms, nms)
      out <- c(out, list(case("blocks", sprintf("k%d n%d z2=%.2f eps=%.1e", k, n, z2, e),
                              stats::cov2cor(sg), rep(ang_k, each = n),
                              as.character(rep(seq_len(k), each = n)), block = bl)))
    }
  }
  out
}

# Designs singular in exact arithmetic (RR24 section 2(a) table).
fam_struct <- function() {
  out <- list()
  for (e in c(.5, .3, .1, .03, .01, 1e-3, 1e-4)) {
    out <- c(out, list(case("struct", sprintf("identity zeta1 eps=%.0e", e),
                            m106_family_a(e, 1L), oct, as.character(1:8),
                            fit_zeta1 = TRUE)))
  }
  ang4 <- c(90, 180, 270, 360)
  bl <- c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L)
  for (e in c(.3, .1, .03)) {
    pop <- axes_population_cor(ang4, 2L, xi1 = .3, xi2 = .2, zeta1 = .1,
                               zeta2 = .15, item_block = bl)
    sg <- pop$sigma
    diag(sg) <- .3 + .2 + .1 + .15 + e
    nms <- paste0("i", 1:8)
    dimnames(sg) <- list(nms, nms)
    out <- c(out, list(case("struct", sprintf("antipodal zeta2 eps=%.0e", e),
                            stats::cov2cor(sg), rep(ang4, each = 2L),
                            as.character(rep(1:4, each = 2L)), block = bl,
                            fit_zeta1 = TRUE, fit_zeta2 = TRUE)))
  }
  out
}

# ---- one matrix ---------------------------------------------------------------
surface_reason <- function(cs) {
  got <- suppressWarnings(axes_corrected_se(
    cs$S, rownames(cs$S), cs$ang, cs$scale, cs$block, n = N,
    fit_zeta1 = cs$zeta1, fit_zeta2 = cs$zeta2))
  list(reason = if (is.null(got$reason)) "computes" else got$reason,
       naive = if (is.null(got$naive_reason)) "computes" else got$naive_reason)
}

measure <- function(cs) {
  S <- cs$S; d <- cs$d; p <- nrow(S)
  fl <- axes_sigma_degenerate(S)
  row <- list(
    family = cs$family, id = cs$id, p = p, n_scales = cs$n_scales,
    spacing = cs$spacing, zeta1 = cs$zeta1, zeta2 = cs$zeta2,
    api_zeta1 = cs$api_zeta1, api_zeta2 = cs$api_zeta2, neg = cs$neg,
    min_items = cs$min_items, kappa = m106_kappa(S),
    floor = if (is.null(fl)) "NULL" else fl,
    rcond_sigma = rcond(S), rcond_info = NA_real_, structural = cs$structural,
    default_outcome = NA_character_, tol0_outcome = NA_character_,
    cert_secs = NA_real_,
    bit_identical = NA, replica_agrees = NA,
    shipped_reason = NA_character_, shipped_naive_reason = NA_character_,
    tol0_reason = NA_character_, tol0_naive_reason = NA_character_,
    cert_se = NA_real_, cert_cval = NA_real_, cert_ratio = NA_real_,
    oracle_status = NA_character_,
    true_se = NA_real_, true_cval = NA_real_, true_ratio = NA_real_,
    under_report = NA
  )

  core_def <- replica(S, d, tol = EPS)
  core_0 <- replica(S, d, tol = 0)
  if (identical(core_def, "singular")) {
    row$default_outcome <- row$tol0_outcome <- "sigma-singular"
  } else if (cs$structural) {
    # An exact ground: the shipped core refuses ahead of any inversion (after
    # M147 T4), and the direct solve() below is still run for rcond_info --
    # (b1) reads it -- but the row is outside the region by definition.
    row$default_outcome <- row$tol0_outcome <- "structural"
    info <- tryCatch(info_of(S, d), error = function(e) NULL)
    row$rcond_info <- if (is.null(info)) NA_real_ else
      tryCatch(rcond(info), error = function(e) NA_real_)
  } else {
    row$default_outcome <- if (is.character(core_def)) "refused" else "inverted"
    info <- info_of(S, d)
    row$rcond_info <- tryCatch(rcond(info), error = function(e) NA_real_)
    direct <- tryCatch(solve(info, tol = 0), error = function(e) e)
    row$tol0_outcome <- if (inherits(direct, "error")) {
      if (grepl("exactly singular", conditionMessage(direct))) "exact-singular"
      else "other-error"
    } else if (!all(is.finite(direct))) "nonfinite" else "inverted"
    # The replica's own verdict agrees with the direct solve() unless the core
    # refused ahead of the inversion (the duplicate-pair check, after T4).
    row$replica_agrees <- identical(is.character(core_0),
                                    row$tol0_outcome != "inverted")
    if (!is.character(core_def) && !is.character(core_0)) {
      row$bit_identical <- identical(core_def[c("si", "sim", "acov")],
                                     core_0[c("si", "sim", "acov")])
    }
  }

  sr <- surface_reason(cs)
  row$shipped_reason <- sr$reason
  row$shipped_naive_reason <- sr$naive

  tol0_world({
    tr <- surface_reason(cs)
    row$tol0_reason <- tr$reason
    row$tol0_naive_reason <- tr$naive
    if (identical(row$tol0_outcome, "inverted")) {
      t_cert <- Sys.time()
      cert <- suppressWarnings(axes_accuracy_certificate(S, d))
      row$cert_secs <- as.numeric(Sys.time() - t_cert, units = "secs")
      row$cert_se <- cert$se
      row$cert_cval <- cert$cval
      row$cert_ratio <- cert$fiml_ratio
      if (identical(row$default_outcome, "refused")) {
        df <- p * (p + 1) / 2 - length(d$mats)
        pr <- suppressWarnings(axes_se_pricing(S, d, N))
        uu <- suppressWarnings(axes_u_pricing(S, d))
        ex <- if (is.character(pr) || is.character(uu)) {
          # The inversion succeeded but the quadratic forms did not price
          # (a nonpositive variance, "indefinite"): no double number exists
          # to measure, and the surface refuses on that literal.
          list(status = paste0("double pricing refused: ",
                               paste(Filter(is.character, list(pr, uu)),
                                     collapse = ", ")))
        } else {
          exact(S, d, df, p * (p - 1) / 2)
        }
        row$oracle_status <- ex$status
        if (identical(ex$status, "ok")) {
          se_ex <- vapply(seq_len(d$n_comp),
                          function(i) ex[[sprintf("EXACT_SE%d", i)]], 0)
          rt_ex <- vapply(seq_len(d$n_comp),
                          function(i) ex[[sprintf("EXACT_RATIO%d", i)]], 0)
          row$true_se <- max(abs(se_ex - pr$corrected) / abs(se_ex))
          row$true_cval <- abs(ex[["EXACT_CVAL"]] - axes_u_pricing(S, d) / df) /
            abs(ex[["EXACT_CVAL"]])
          row$true_ratio <- max(abs(rt_ex - pr$corrected / pr$naive) / abs(rt_ex))
          graded <- max(cert$se, cert$cval, cert$fiml_ratio) < 1
          row$under_report <- graded && (cert$se < row$true_se ||
                                         cert$cval < row$true_cval ||
                                         cert$fiml_ratio < row$true_ratio)
        }
      }
    }
  })
  as.data.frame(row, stringsAsFactors = FALSE)
}

# ---- run ------------------------------------------------------------------------
# TOL0_REPORT_ONLY=1 re-renders the summary from the committed results without
# measuring again (the full run costs about an hour, most of it the exact
# oracle at p = 24 in the region). The provenance block travels in the .rds.
t0 <- Sys.time()
if (nzchar(Sys.getenv("TOL0_REPORT_ONLY"))) {
  res <- readRDS(OUT_RDS)
} else {
cases <- c(fam_cert(), fam_q4(), fam_m106(), fam_random(), fam_blocks(), fam_struct())
cat(sprintf("%d matrices in six families\n", length(cases)))
rows <- vector("list", length(cases))
for (i in seq_along(cases)) {
  rows[[i]] <- measure(cases[[i]])
  if (i %% 25 == 0) cat(sprintf("  %d done (%.0f s)\n", i,
                                as.numeric(Sys.time() - t0, units = "secs")))
}
res <- do.call(rbind, rows)
attr(res, "provenance") <- list(
  script = "devel/degeneracy-oracle/tol0_sweep.R",
  commit = tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
                    error = function(e) NA_character_),
  date = format(Sys.Date()),
  shipped_core_has_tol = shipped_has_tol,
  seed = RANDOM_SEED, draws = RANDOM_DRAWS,
  platform = R.version$platform, r = R.version.string,
  blas = sessionInfo()$BLAS, lapack = sessionInfo()$LAPACK
)
saveRDS(res, OUT_RDS)
}
prov <- attr(res, "provenance")

# ---- the pre-registered acceptance --------------------------------------------
region <- res[res$default_outcome %in% "refused" & res$tol0_outcome %in% "inverted", ]
both <- res[res$default_outcome %in% "refused" & !(res$tol0_outcome %in% "inverted"), ]
priced <- region[!startsWith(region$oracle_status, "double pricing refused"), ]
unpriced <- region[startsWith(region$oracle_status, "double pricing refused"), ]
a1 <- nrow(region) > 0 && all(priced$oracle_status %in% "ok") &&
  !any(priced$under_report) &&
  all(priced$tol0_reason == "uncertified" |
        (priced$true_se <= DELTA_STAR & priced$true_cval <= DELTA_STAR &
           priced$true_ratio <= DELTA_STAR)) &&
  all(unpriced$tol0_reason != "computes")
both_pd <- both[!(both$floor %in% c("indefinite", "singular")), ]
a2 <- all(both_pd$tol0_reason == "unidentified")
THETA <- sqrt(EPS)
struct <- res[res$family == "struct", ]
b1 <- nrow(struct) > 0 && all(!is.na(struct$rcond_info) & struct$rcond_info < THETA / 1e4)
api_ok <- res$n_scales >= 4L & res$spacing == "ok" &
  res$zeta1 == res$api_zeta1 & res$zeta2 == res$api_zeta2
domain <- res[res$floor == "NULL" & api_ok, ]
routed <- domain[!is.na(domain$rcond_info) & domain$rcond_info < THETA, ]
p3 <- res[res$floor == "NULL" & res$n_scales < 4L &
            !is.na(res$rcond_info) & res$rcond_info < THETA, ]
b2 <- nrow(routed) > 0 && all(routed$tol0_reason == "computes") &&
  any(domain$family == "blocks" & domain$api_zeta2)
inv <- res[res$default_outcome %in% "inverted", ]
cc <- nrow(inv) > 0 && all(inv$bit_identical)
fam_n <- table(factor(res$family, levels = c("cert", "q4", "m106", "random", "blocks", "struct")))
p64 <- res[res$p == 64L, ]
dd <- all(fam_n > 0) && nrow(p64) > 0 &&
  !any(p64$default_outcome %in% "refused" & p64$tol0_outcome %in% "inverted") &&
  any(res$n_scales == 6L) && any(res$n_scales == 12L) && any(res$neg)
sigma_hits <- res[res$floor == "NULL" & res$rcond_sigma < EPS, ]
oracle_fail <- region[!(region$oracle_status %in% "ok") &
                        !startsWith(region$oracle_status, "double pricing refused"), ]
moved <- res[res$shipped_reason == "uncertified" & res$tol0_reason == "computes", ]
disagree <- res[!is.na(res$replica_agrees) & !res$replica_agrees, ]

verdict <- function(x) if (isTRUE(x)) "PASS" else "FAIL"
fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.3g", x))
md <- c(
  "# tol = 0 sweep: results",
  "",
  sprintf("Generated by `devel/degeneracy-oracle/tol0_sweep.R` at commit `%s` on %s (%s, %s; BLAS %s).",
          prov$commit, prov$date, prov$r, prov$platform, basename(prov$blas)),
  sprintf("Shipped `axes_pricing_core()` carried a `tol` formal at that commit: %s.",
          prov$shipped_core_has_tol),
  "",
  "## Domain",
  "",
  sprintf("| family | matrices | floor NULL | default refused | tol = 0 inverted of those | both refused |"),
  "|---|---|---|---|---|---|",
  vapply(names(fam_n), function(f) {
    r <- res[res$family == f, ]
    sprintf("| %s | %d | %d | %d | %d | %d |", f, nrow(r), sum(r$floor == "NULL"),
            sum(r$default_outcome %in% "refused"),
            sum(r$default_outcome %in% "refused" & r$tol0_outcome %in% "inverted"),
            sum(r$default_outcome %in% "refused" & !(r$tol0_outcome %in% "inverted")))
  }, ""),
  "",
  "## Pre-registered acceptance",
  "",
  sprintf("- (a) region (%d matrices, %d of them priced by the tol0 doubles): oracle ran at every priced one, no under-report, and each refuses `uncertified` or is inside delta_star, and every unpriced one refuses: **%s**; both-refused (%d matrices) all `unidentified`: **%s**",
          nrow(region), nrow(priced), verdict(a1), nrow(both), verdict(a2)),
  sprintf("- (b1) struct family (%d matrices): rcond(info) below sqrt(eps)/1e4 = %s at every one: **%s** (max %s)",
          nrow(struct), fmt(THETA / 1e4), verdict(b1), fmt(suppressWarnings(max(struct$rcond_info)))),
  sprintf("- (b2) floor-admitted, API-admitted and selector-routed (%d matrices, of %d in domain, %d blocks-family rows with zeta2 in domain): every one computes: **%s**",
          nrow(routed), nrow(domain), sum(domain$family == "blocks" & domain$api_zeta2), verdict(b2)),
  sprintf("- (c) bit-identical `si`, `sim`, `acov` across tolerances at every default-inverted matrix (%d): **%s**",
          nrow(inv), verdict(cc)),
  sprintf("- (d) no family empty, p = 64 row present and outside the region, 6- and 12-scale rows, negative-component rows: **%s** (p = 64 certificate %s s)",
          verdict(dd), fmt(suppressWarnings(max(p64$cert_secs)))),
  "",
  "## The routed reachable list (b2)",
  "",
  "| family | id | p | rcond(info) | floor | tol0 | cert se | cert cval | cert ratio |",
  "|---|---|---|---|---|---|---|---|---|",
  if (nrow(routed)) vapply(seq_len(nrow(routed)), function(i) {
    r <- routed[i, ]
    sprintf("| %s | %s | %d | %s | %s | %s | %s | %s | %s |", r$family, r$id, r$p,
            fmt(r$rcond_info), r$floor, r$tol0_reason, fmt(r$cert_se),
            fmt(r$cert_cval), fmt(r$cert_ratio))
  }, "") else "(empty)",
  "",
  sprintf("Out of domain (fewer than four scales), floor-admitted and below the threshold: %d rows%s",
          nrow(p3), if (nrow(p3)) paste0(" -- ", paste(sprintf("%s (rcond %s, cert cval %s)", p3$id, fmt(p3$rcond_info), fmt(p3$cert_cval)), collapse = "; ")) else ""),
  "",
  "## Outside the acceptance",
  "",
  sprintf("- floor-admitted matrices with rcond(sigma) below eps: %d", nrow(sigma_hits)),
  sprintf("- region matrices whose exact oracle failed (structurally singular information matrix inverted under tol = 0): %d", nrow(oracle_fail)),
  sprintf("- matrices where the replica's verdict disagrees with the direct solve(info, tol = 0): %d", nrow(disagree)),
  sprintf("- matrices refused `uncertified` by the shipped predicate that compute in the tol0 world: %d (largest true error among them %s)",
          nrow(moved), if (nrow(moved)) fmt(max(pmax(moved$true_se, moved$true_cval, moved$true_ratio))) else "none"),
  "",
  "## The region: default refused, tol = 0 inverted",
  "",
  "| family | id | p | kappa | rcond(info) | shipped | tol0 | cert se | cert cval | cert ratio | true se | true cval | true ratio | under-report |",
  "|---|---|---|---|---|---|---|---|---|---|---|---|---|---|",
  if (nrow(region)) vapply(seq_len(nrow(region)), function(i) {
    r <- region[i, ]
    sprintf("| %s | %s | %d | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
            r$family, r$id, r$p, fmt(r$kappa), fmt(r$rcond_info), r$shipped_reason,
            r$tol0_reason, fmt(r$cert_se), fmt(r$cert_cval), fmt(r$cert_ratio),
            fmt(r$true_se), fmt(r$true_cval), fmt(r$true_ratio),
            if (is.na(r$under_report)) r$oracle_status else as.character(r$under_report))
  }, "") else "(empty)",
  "",
  "## Both tolerances refused",
  "",
  "| family | id | p | kappa | rcond(info) | tol0 outcome | shipped | tol0 |",
  "|---|---|---|---|---|---|---|---|",
  if (nrow(both)) vapply(seq_len(nrow(both)), function(i) {
    r <- both[i, ]
    sprintf("| %s | %s | %d | %s | %s | %s | %s | %s |", r$family, r$id, r$p,
            fmt(r$kappa), fmt(r$rcond_info), r$tol0_outcome, r$shipped_reason,
            r$tol0_reason)
  }, "") else "(empty)",
  ""
)
writeLines(md, OUT_MD)
cat(md, sep = "\n")
cat(sprintf("\n%.0f s\n", as.numeric(Sys.time() - t0, units = "secs")))
if (!(a1 && a2 && b1 && b2 && cc && dd)) quit(status = 1L)
