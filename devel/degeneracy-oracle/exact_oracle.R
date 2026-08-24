# Exact-oracle driver for the M89 degeneracy criterion (RR18 BC7 / M89 AC4).
#
# Run from the repo root:  Rscript devel/degeneracy-oracle/exact_oracle.R
#
# Reproduces, from committed material only, the two measurements that re-cut
# M89: at the committed counterexample the shipped corrected SEs are wrong by
# ~3.4% while `reason` is NULL, and the true scaling factor is POSITIVE where
# double precision returns -0.216 and refuses "indefinite". It then runs the
# RR18 Q4 sweep, showing the double-precision SE error tracks p*kappa(R)^2*eps.
#
# The exact arithmetic lives in exact_oracle.py (Python standard library only;
# not a package dependency -- devel/ is .Rbuildignore'd). This script supplies
# its inputs and checks its output against the pinned anchors.
#
# EVERY SETTING THE ANCHORS DEPEND ON IS NAMED HERE. The committed fixture
# tests/testthat/fixtures/rb18-counterexample-b.rds carries only the matrix `S`
# and the item angles `ia`; the rest of the model is not in it, and the anchors
# are not reproducible without these (M89 AC6). The fixture read is the PACKAGED
# copy, and since M108 it is the only copy: the duplicate under cairn/reviews/
# was deleted with the byte-identity guard that fenced the two against each
# other, so this script reads nothing under cairn/ and runs with that directory
# absent (M108 AC5).
FIXTURE      <- "tests/testthat/fixtures/rb18-counterexample-b.rds"
ITEM_SCALE   <- c("A", "B", "C")  # one item per scale, p = 3
ITEM_BLOCK   <- NULL              # no blocks; only consulted when FIT_ZETA2
FIT_ZETA1    <- FALSE             # so q = 5 (xi1, xi2, and p error matrices)
FIT_ZETA2    <- FALSE
N            <- 600               # SEs scale as 1/sqrt(N); the anchors are at 600
DF           <- 1                 # p*(p+1)/2 - q = 6 - 5
BASELINE_DF  <- 3                 # p*(p-1)/2

# Pinned anchors (M89 AC4). Tolerances are the criterion's own.
ANCHOR_CVAL <- c(value = 0.05554788, tol = 1e-7)
ANCHOR_SE   <- rbind(c(value = 0.1476340, tol = 1e-6),
                     c(value = 0.1443740, tol = 1e-6))

suppressMessages(pkgload::load_all(quiet = TRUE))
py <- file.path("devel", "degeneracy-oracle", "exact_oracle.py")

# Hand the exact bits over: %a round-trips a double exactly, decimal does not,
# and at kappa ~ 6.7e6 the lost bits change the answer's sign.
# `df` and `baseline_df` are arguments rather than reads of the globals above.
# The globals are the p = 3 FIXTURE's counts; every other case has its own, and
# EXACT_CVAL divides by df, so a case priced under the fixture's counts would
# report a cval for a model it is not. The reachable family below does read
# EXACT_CVAL, at each case's own counts (M106 T11).
hex_dump <- function(S, mats, n_comp, df, baseline_df) {
  f <- tempfile(fileext = ".txt")
  h <- function(v) paste(sprintf("%a", as.numeric(v)), collapse = " ")
  lines <- c(
    sprintf("P: %d", nrow(S)), sprintf("N: %d", N), sprintf("DF: %d", df),
    sprintf("BASELINE_DF: %d", baseline_df), sprintf("NCOMP: %d", n_comp),
    sprintf("Q: %d", length(mats)), sprintf("S: %s", h(S)),
    vapply(seq_along(mats), function(i) sprintf("M%d: %s", i, h(mats[[i]])), "")
  )
  writeLines(lines, f)
  f
}

# The same two identities axes_scaling_factor() guards on: df is the count of
# overidentifying restrictions, baseline_df the independence model's.
df_of <- function(S, d) nrow(S) * (nrow(S) + 1) / 2 - length(d$mats)
baseline_df_of <- function(S) nrow(S) * (nrow(S) - 1) / 2

exact <- function(S, d, df = DF, baseline_df = BASELINE_DF) {
  out <- system2("python3",
                 c(py, hex_dump(S, d$mats, d$n_comp, df, baseline_df)),
                 stdout = TRUE)
  vals <- as.numeric(sub("^[A-Z_0-9]+: ", "", out))
  stats::setNames(vals, sub(":.*$", "", out))
}

kappa_of <- function(S) {
  ev <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
  ev[1] / ev[length(ev)]
}

fx <- readRDS(FIXTURE)
S  <- fx$S
ang <- fx$ia
nm <- rownames(S)
d  <- axes_se_derivs(ang, ITEM_SCALE, ITEM_BLOCK, FIT_ZETA1, FIT_ZETA2)

stopifnot(isTRUE(all.equal(cov2cor(S), S)))  # unit diagonal: the metrics coincide

cat("== Counterexample B ==\n")
cat(sprintf("kappa(S) = %.6g   (identical in both metrics)\n", kappa_of(S)))
cat(sprintf("criterion accepts it: %s\n",
            is.null(axes_sigma_degenerate(S))))

ex <- exact(S, d)
dbl_se <- axes_se_pricing(S, d, N)$corrected
dbl_sf <- suppressWarnings(
  axes_scaling_factor(S, nm, ang, ITEM_SCALE, ITEM_BLOCK,
                      fit_zeta1 = FIT_ZETA1, fit_zeta2 = FIT_ZETA2,
                      df = DF, baseline_df = BASELINE_DF)
)

# The cval the shipped double-precision code computes BEFORE its refusal
# discards it. axes_scaling_factor() returns a reason and no number wherever
# the criterion trips, so a refused case has no other route to the double
# value being priced (M106 T11). This used to be a transcribed copy of
# R/axes_scaled_fit.R's arithmetic, kept in step by a comment; since M108 that
# arithmetic has one definition -- axes_u_pricing(), which the shipped path and
# the certificate's reference route both read -- so the copy is gone and this
# calls it. The `/ df` is what axes_scaling_factor() does with the numerator.
double_cval <- function(S, d, df) axes_u_pricing(S, d) / df

cat(sprintf("\ncval   exact %+.12g | double %+.12g | shipped reason: %s\n",
            ex[["EXACT_CVAL"]], double_cval(S, d, DF),
            if (is.null(dbl_sf$reason)) "NULL" else dbl_sf$reason))
cat(sprintf("       the exact value is POSITIVE: the refusal is a cancellation sign-flip\n"))
cat(sprintf("       (exact tr_vg %.10g - proj %.10g; amplification %.4g)\n",
            ex[["EXACT_TR_VG"]], ex[["EXACT_PROJ"]],
            (abs(ex[["EXACT_TR_VG"]]) + abs(ex[["EXACT_PROJ"]])) /
              abs(ex[["EXACT_TR_VG"]] - ex[["EXACT_PROJ"]])))

cat("\ncorrected SEs (reported with reason NULL):\n")
b_se_rel <- 0
for (i in seq_along(dbl_se)) {
  e <- ex[[sprintf("EXACT_SE%d", i)]]
  b_se_rel <- max(b_se_rel, abs(e - dbl_se[[i]]) / e)
  cat(sprintf("  comp %d: exact %.12g | double %.12g | rel.err %.3e\n",
              i, e, dbl_se[[i]], abs(e - dbl_se[[i]]) / e))
}

# --- M108: the per-fit certificate beside the error it estimates -------------
#
# axes_accuracy_certificate() never sees the exact values; it reads the shipped
# double pricing against its own double-double replay of the same pipeline.
# What this section measures is whether that estimate brackets the error the
# exact-rational oracle MEASURES -- at or above it (no under-report, which is
# the licensing failure) and within CERT_CEILING of it (M108 AC2's
# pre-registered window, which the a-priori bound it replaces misses by 5 to 8
# decades). The certificate is n-free and df-free by construction, so nothing
# here hands it either.
CERT_CEILING <- 1e3
CERT_EXPECTED <- 12L   # six geometries x {SE, cval}
cert_ok <- TRUE
cert_n <- 0L
cert_line <- function(lbl, cert, true_rel) {
  cert_n <<- cert_n + 1L
  if (true_rel == 0) {
    # An exactly priced case: the shipped route committed no error, so there is
    # no ratio to form and the certificate can only report its own floor. That
    # is the certificate being RIGHT, not a failure -- before M108's AC2
    # amendment the Inf ratio here printed FAIL.
    ok <- cert > 0 && is.finite(cert)
    cert_ok <<- cert_ok && ok
    cat(sprintf("  %-10s true %9.3e | certificate %9.3e | exact case, floor only\n",
                lbl, true_rel, cert))
    return(invisible(NULL))
  }
  ratio <- cert / true_rel
  cert_ok <<- cert_ok && is.finite(ratio) && ratio >= 1 && ratio <= CERT_CEILING
  cat(sprintf("  %-10s true %9.3e | certificate %9.3e | ratio %8.3g\n",
              lbl, true_rel, cert, ratio))
}

cat("\ncertificate at counterexample B:\n")
cert_b <- axes_accuracy_certificate(S, d)
cert_line("SE", cert_b$se, b_se_rel)
cert_line("cval", cert_b$cval,
          abs(ex[["EXACT_CVAL"]] - double_cval(S, d, DF)) / abs(ex[["EXACT_CVAL"]]))

# --- anchor checks (M89 AC4/AC6) --------------------------------------------
ok <- abs(ex[["EXACT_CVAL"]] - ANCHOR_CVAL[["value"]]) <= ANCHOR_CVAL[["tol"]]
for (i in 1:2) {
  ok <- ok && abs(ex[[sprintf("EXACT_SE%d", i)]] - ANCHOR_SE[i, "value"]) <=
    ANCHOR_SE[i, "tol"]
}

# --- RR18 Q4 sweep: does the double error track p*kappa^2*eps? ---------------
cat("\n== Q4 sweep: S_t = t*S_B + (1-t)*I ==\n")
cat("       t          kappa(R)     rel.err     p*kappa^2*eps   ratio\n")
sweep_ok <- TRUE
for (tt in c(1 - 2.5e-5, 1 - 2.5e-4, 1 - 2.5e-3)) {
  St <- tt * S + (1 - tt) * diag(nrow(S))
  dimnames(St) <- dimnames(S)
  ext <- exact(St, d)
  dt <- axes_se_pricing(St, d, N)$corrected
  rel <- max(abs(c(ext[["EXACT_SE1"]], ext[["EXACT_SE2"]]) - dt) /
               c(ext[["EXACT_SE1"]], ext[["EXACT_SE2"]]))
  bound <- nrow(S) * kappa_of(St)^2 * .Machine$double.eps
  ratio <- rel / bound
  sweep_ok <- sweep_ok && ratio <= 10 && ratio >= 0.1
  cat(sprintf("  %.7f   %10.4g   %9.3e   %11.3e   %6.3g\n",
              tt, kappa_of(St), rel, bound, ratio))
}

# --- M106 / RR19 B2: the REACHABLE-geometry family ---------------------------
#
# Everything above is measured at counterexample B, and B is not a matrix this
# criterion can be handed in production: it is p = 3 with df = 1 while
# axes_reliability() requires four scales, and it sits 25 units off the model
# manifold at its own stated configuration. So the sweep above establishes that
# the bound is TIGHT AT B -- a property of that fixture, not of the criterion.
# Its pass window (ratio in [0.1, 10]) encodes exactly that and must not be
# applied here.
#
# This family is model-implied -- Sigma = xi1*C + xi2*J + zeta1*Bm + diag(eps),
# the form every lavaan-fitted Sigma-hat has -- at dimensions the exported API
# actually reaches. What it asserts is the OPPOSITE property: that in reachable
# geometry the bound stays decades away from the error it stands for. If a
# future change puts a reachable design into B's coupling regime, this is what
# reddens.
#
# The window is three decades below 1. Running this script on 2026-08-22
# measured attainment across the five cases below at 6.8e-8 to 3.8e-7, so 1e-3
# sits three to four decades looser than anything measured -- deliberately,
# because a bar set at the measured value is a bar calibrated on one machine.
REACHABLE_WINDOW <- 1e-3

reachable_family <- function(eps, per_scale, xi1 = 0.3, xi2 = 0.3, zeta1 = 0) {
  oct <- c(90, 135, 180, 225, 270, 315, 360, 45)
  ang <- rep(oct, each = per_scale)
  sid <- rep(seq_along(oct), each = per_scale)
  rad <- ang * pi / 180
  cm <- outer(rad, rad, function(u, v) cos(u - v))
  bm <- outer(sid, sid, "==") * 1
  sg <- xi1 * cm + xi2 * matrix(1, length(ang), length(ang)) +
    zeta1 * bm + eps * diag(length(ang))
  nms <- paste0("i", seq_along(ang))
  dimnames(sg) <- list(nms, nms)
  list(S = cov2cor(sg), ang = ang, scale = as.character(sid))
}

# Near-duplicate geometry: a ninth item sharing scale 1's angle, with the
# pair's item errors driven down together. This is M89 F3's own case -- the
# refusal that motivated M106 -- so the family covers the shape the
# recalibration was made for, not only well-spread designs.
near_duplicate_family <- function(pair_eps, xi1 = 0.3, xi2 = 0.2,
                                  zeta1 = 0.2, other_eps = 0.30) {
  oct <- c(90, 135, 180, 225, 270, 315, 360, 45)
  ang <- c(oct, oct[1])
  sid <- c(seq_along(oct), 1L)
  rad <- ang * pi / 180
  cm <- outer(rad, rad, function(u, v) cos(u - v))
  bm <- outer(sid, sid, "==") * 1
  ev <- rep(other_eps, length(ang))
  ev[c(1L, length(ang))] <- pair_eps
  sg <- xi1 * cm + xi2 * matrix(1, length(ang), length(ang)) +
    zeta1 * bm + diag(ev)
  nms <- paste0("i", seq_along(ang))
  dimnames(sg) <- list(nms, nms)
  list(S = cov2cor(sg), ang = ang, scale = as.character(sid))
}

cat("\n== M106 reachable-geometry family: is the bound decades LOOSE here? ==\n")
cat("  construction                p     kappa(R)     rel.err     bound        ratio     cval rel.err\n")
reach_ok <- TRUE
reach_cases <- list(
  list(lbl = "family A, 1 item/scale ", g = reachable_family(2.4e-4, 1L)),
  list(lbl = "family A, 1 item/scale ", g = reachable_family(2.4e-5, 1L)),
  list(lbl = "family C, p = 4 minimum", g = local({
    ang <- c(90, 180, 270, 360); rad <- ang * pi / 180
    cm <- outer(rad, rad, function(u, v) cos(u - v))
    sg <- 0.3 * cm + 0.3 * matrix(1, 4, 4) + 1.2e-5 * diag(4)
    nms <- paste0("i", 1:4); dimnames(sg) <- list(nms, nms)
    list(S = cov2cor(sg), ang = ang, scale = as.character(1:4))
  })),
  list(lbl = "near-duplicate r=.9999 ", g = near_duplicate_family(7e-5)),
  list(lbl = "near-duplicate r=.99999", g = near_duplicate_family(7e-6))
)
for (cs in reach_cases) {
  g <- cs$g
  pr <- nrow(g$S)
  # zeta1 is read off the case's own item map with the package's own predicate,
  # never inherited from the p = 3 fixture's FIT_ZETA1 above. The near-duplicate
  # cases put two items on scale 1, so axes_fits_zeta1() is TRUE for them and
  # the exported path fits a scale-specificity component the fixture's model
  # does not have; pricing them at FALSE measured a model axes_reliability()
  # would never fit, which is not the reachable geometry this family claims.
  zr <- axes_fits_zeta1(split(seq_along(g$scale), g$scale))
  dr <- axes_se_derivs(g$ang, g$scale, NULL, zr, FALSE)
  dfr <- df_of(g$S, dr)
  exr <- exact(g$S, dr, dfr, baseline_df_of(g$S))
  dtr <- axes_se_pricing(g$S, dr, N)$corrected
  exv <- vapply(seq_along(dtr), function(i) exr[[sprintf("EXACT_SE%d", i)]], 0)
  rel <- max(abs(exv - dtr) / abs(exv))
  bnd <- pr * kappa_of(g$S)^2 * .Machine$double.eps
  rat <- rel / bnd
  # The SCALING surface's own quantity, measured at each case's own df (M106
  # T11). The SE target is extended to cval by fiat (see the premises beside
  # axes_degeneracy_tau), so what that extension costs is only visible if cval
  # is priced against the exact oracle in the same reachable geometries.
  cvr <- abs(exr[["EXACT_CVAL"]] - double_cval(g$S, dr, dfr)) /
    abs(exr[["EXACT_CVAL"]])
  reach_ok <- reach_ok && rat <= REACHABLE_WINDOW
  cat(sprintf("  %s  %3d  %10.4g   %9.3e   %10.3e   %8.2e   %9.3e\n",
              cs$lbl, pr, kappa_of(g$S), rel, bnd, rat, cvr))
  crt <- axes_accuracy_certificate(g$S, dr)
  cert_line("  SE", crt$se, rel)
  cert_line("  cval", crt$cval, cvr)
}

# The count is asserted, not asserted-in-a-label: `cert_ok` starts TRUE and is
# only ever falsified INSIDE cert_line(), so a truncated or empty case list
# would otherwise print PASS at "all six geometries" having checked none.
cert_ok <- cert_ok && identical(cert_n, CERT_EXPECTED)
cat(sprintf("\nANCHORS: %s\nSWEEP (within a factor of 10 of the bound): %s\nREACHABLE (attainment below %.0e): %s\nCERTIFICATE (%d of %d ratios checked, each in [1, %.0e], at all six geometries): %s\n",
            if (ok) "PASS" else "FAIL", if (sweep_ok) "PASS" else "FAIL",
            REACHABLE_WINDOW, if (reach_ok) "PASS" else "FAIL",
            cert_n, CERT_EXPECTED, CERT_CEILING, if (cert_ok) "PASS" else "FAIL"))
if (!ok || !sweep_ok || !reach_ok || !cert_ok) quit(status = 1L)
