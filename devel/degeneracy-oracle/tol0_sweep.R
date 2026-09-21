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
#           grid eps = 10^seq(-1, -12, by = -0.5) that crosses the floor.
#   random  a seeded model-implied family Sigma = xi1*C + xi2*J + zeta1*B +
#           diag(e), cov2cor'd, at p in {4, 8, 9, 16, 24} (the M106 shapes:
#           four cardinal scales; one, two and three items per octant scale;
#           octants plus a duplicate-angle ninth item), with xi1 ~ U(.2,.5),
#           xi2 ~ U(0,.3), zeta1 ~ U(0,.3) where two or more items share a
#           scale and 0 otherwise, and e_i = level * exp(N(0, .5)) with
#           log10(level) ~ U(-12, -1). Draws per p: 40, 40, 40, 30, 20.
#           set.seed(20260921) once before the family.
#
# COLUMNS, per matrix: family, id, p, zeta1 (fitted?), min_items (smallest
# item count on any scale), kappa (eigenvalue ratio), floor (the criterion's
# answer, "NULL" where it admits), rcond_sigma, rcond_info, default_outcome
# (the replica at tol = .Machine$double.eps: "inverted", "refused",
# "sigma-singular"), tol0_outcome (the replica at tol = 0: "inverted",
# "exact-singular" (LAPACK dgesv INFO > 0), "nonfinite", "other-error",
# "sigma-singular"), bit_identical (si, sim and acov identical across the two
# tolerances where both inverted), shipped_reason (axes_corrected_se()'s
# `reason` with the shipped core, "computes" where NULL), shipped_naive_reason,
# tol0_reason and tol0_naive_reason (the same in the tol0 world), cert_se,
# cert_cval, cert_ratio (the certificate in the tol0 world, where tol = 0
# inverted), and in the REGION -- default refused, tol = 0 inverted --
# oracle_status, true_se, true_cval, true_ratio (the exact-rational oracle's
# relative errors of the tol0 doubles), and under_report (the certificate is
# graded, i.e. below its sentinel 1, and some field sits below its true
# error).
#
# PRE-REGISTERED ACCEPTANCE (M147 AC2), evaluated on the committed run and
# printed at the end:
#   (a) At every REGION matrix: no under-report, and either tol0_reason is
#       "uncertified" or every true error is at or below delta_star (1e-4).
#       At every matrix BOTH tolerances refused, tol0_reason is "unidentified".
#   (b) Every matrix the floor admits ("NULL") whose design fits zeta1 only
#       with two or more items on every scale was inverted under both
#       tolerances, with rcond_info >= 1e4 * .Machine$double.eps.
#   (c) Every matrix inverted under the default tolerance has bit_identical
#       TRUE.
#   (d) No family is empty.
# At HEAD (before T4) the second clause of (a) is expected to FAIL: the floor
# consults the certificate ahead of the pricing there, so a zero pivot
# surfaces as the sentinel's "uncertified" rather than as "unidentified".
# That baseline is what T2's brief reads; the acceptance binds the T5 run.
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
case <- function(family, id, S, ang, scale) {
  z <- axes_fits_zeta1(split(seq_along(scale), scale))
  d <- axes_se_derivs(ang, scale, NULL, z, FALSE)
  list(family = family, id = id, S = S, ang = ang, scale = scale, d = d,
       zeta1 = z, min_items = min(lengths(split(seq_along(scale), scale))))
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
fam_m106 <- function() {
  out <- list()
  for (e in M106_GRID) {
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
  }
  out
}

# ---- one matrix ---------------------------------------------------------------
surface_reason <- function(cs) {
  got <- suppressWarnings(axes_corrected_se(
    cs$S, rownames(cs$S), cs$ang, cs$scale, n = N,
    fit_zeta1 = cs$zeta1, fit_zeta2 = FALSE))
  list(reason = if (is.null(got$reason)) "computes" else got$reason,
       naive = if (is.null(got$naive_reason)) "computes" else got$naive_reason)
}

measure <- function(cs) {
  S <- cs$S; d <- cs$d; p <- nrow(S)
  fl <- axes_sigma_degenerate(S)
  row <- list(
    family = cs$family, id = cs$id, p = p, zeta1 = cs$zeta1,
    min_items = cs$min_items, kappa = m106_kappa(S),
    floor = if (is.null(fl)) "NULL" else fl,
    rcond_sigma = rcond(S), rcond_info = NA_real_,
    default_outcome = NA_character_, tol0_outcome = NA_character_,
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
      cert <- suppressWarnings(axes_accuracy_certificate(S, d))
      row$cert_se <- cert$se
      row$cert_cval <- cert$cval
      row$cert_ratio <- cert$fiml_ratio
      if (identical(row$default_outcome, "refused")) {
        df <- p * (p + 1) / 2 - length(d$mats)
        ex <- exact(S, d, df, p * (p - 1) / 2)
        row$oracle_status <- ex$status
        if (identical(ex$status, "ok")) {
          pr <- axes_se_pricing(S, d, N)
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
cases <- c(fam_cert(), fam_q4(), fam_m106(), fam_random())
cat(sprintf("%d matrices in four families\n", length(cases)))
t0 <- Sys.time()
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

# ---- the pre-registered acceptance --------------------------------------------
region <- res[res$default_outcome %in% "refused" & res$tol0_outcome %in% "inverted", ]
both <- res[res$default_outcome %in% "refused" & !(res$tol0_outcome %in% "inverted"), ]
a1 <- nrow(region) > 0 && all(region$oracle_status %in% "ok") &&
  !any(region$under_report) &&
  all(region$tol0_reason == "uncertified" |
        (region$true_se <= DELTA_STAR & region$true_cval <= DELTA_STAR &
           region$true_ratio <= DELTA_STAR))
a2 <- all(both$tol0_reason == "unidentified")
covered <- res[res$floor == "NULL" & (!res$zeta1 | res$min_items >= 2L), ]
b <- nrow(covered) > 0 && all(covered$default_outcome == "inverted" &
                                 covered$tol0_outcome == "inverted" &
                                 covered$rcond_info >= 1e4 * EPS)
inv <- res[res$default_outcome %in% "inverted", ]
cc <- nrow(inv) > 0 && all(inv$bit_identical)
fam_n <- table(factor(res$family, levels = c("cert", "q4", "m106", "random")))
dd <- all(fam_n > 0)
sigma_hits <- res[res$floor == "NULL" & res$rcond_sigma < EPS, ]
oracle_fail <- region[!(region$oracle_status %in% "ok"), ]
disagree <- res[!is.na(res$replica_agrees) & !res$replica_agrees, ]

verdict <- function(x) if (isTRUE(x)) "PASS" else "FAIL"
fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.3g", x))
md <- c(
  "# tol = 0 sweep: results",
  "",
  sprintf("Generated by `devel/degeneracy-oracle/tol0_sweep.R` at commit `%s` on %s (%s, %s; BLAS %s).",
          attr(res, "provenance")$commit, attr(res, "provenance")$date,
          R.version.string, R.version$platform, basename(sessionInfo()$BLAS)),
  sprintf("Shipped `axes_pricing_core()` carries a `tol` formal: %s.", shipped_has_tol),
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
  sprintf("- (a) region (%d matrices): oracle ran at all, no under-report, and each refuses `uncertified` or is inside delta_star: **%s**; both-refused (%d matrices) all `unidentified`: **%s**",
          nrow(region), verdict(a1), nrow(both), verdict(a2)),
  sprintf("- (b) floor-admitted designs with zeta1 fitted only at two or more items per scale (%d matrices): inverted under both tolerances with rcond(info) >= 1e4 eps: **%s** (min rcond(info) %s)",
          nrow(covered), verdict(b), fmt(suppressWarnings(min(covered$rcond_info)))),
  sprintf("- (c) bit-identical `si`, `sim`, `acov` across tolerances at every default-inverted matrix (%d): **%s**",
          nrow(inv), verdict(cc)),
  sprintf("- (d) no family empty: **%s**", verdict(dd)),
  "",
  "## Outside the acceptance",
  "",
  sprintf("- floor-admitted matrices with rcond(sigma) below eps: %d", nrow(sigma_hits)),
  sprintf("- region matrices whose exact oracle failed (structurally singular information matrix inverted under tol = 0): %d", nrow(oracle_fail)),
  sprintf("- matrices where the replica's verdict disagrees with the direct solve(info, tol = 0): %d", nrow(disagree)),
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
if (!(a1 && a2 && b && cc && dd)) quit(status = 1L)
