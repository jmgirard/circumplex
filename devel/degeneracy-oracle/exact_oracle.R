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
# cairn/reviews/rb18-counterexample-b.rds carries only the matrix `S` and the
# item angles `ia`; the rest of the model is not in it, and the anchors are not
# reproducible without these (M89 AC6):
FIXTURE      <- "cairn/reviews/rb18-counterexample-b.rds"
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
hex_dump <- function(S, mats, n_comp) {
  f <- tempfile(fileext = ".txt")
  h <- function(v) paste(sprintf("%a", as.numeric(v)), collapse = " ")
  lines <- c(
    sprintf("P: %d", nrow(S)), sprintf("N: %d", N), sprintf("DF: %d", DF),
    sprintf("BASELINE_DF: %d", BASELINE_DF), sprintf("NCOMP: %d", n_comp),
    sprintf("Q: %d", length(mats)), sprintf("S: %s", h(S)),
    vapply(seq_along(mats), function(i) sprintf("M%d: %s", i, h(mats[[i]])), "")
  )
  writeLines(lines, f)
  f
}

exact <- function(S, d) {
  out <- system2("python3", c(py, hex_dump(S, d$mats, d$n_comp)), stdout = TRUE)
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

cat(sprintf("\ncval   exact %+.12g | double %+.12g | shipped reason: %s\n",
            ex[["EXACT_CVAL"]], (function() {
              # the double value the shipped code computes, before its refusal
              si <- solve(S); sim <- lapply(d$mats, function(m) si %*% m)
              q <- length(sim)
              info <- matrix(0, q, q)
              for (s in seq_len(q)) for (t in s:q)
                info[s, t] <- info[t, s] <- 0.5 * sum(sim[[s]] * t(sim[[t]]))
              acov <- solve(info)
              up <- upper.tri(S); rho <- S[up]
              tr_vg <- sum(1 - si[up] * rho * (1 - rho^2))
              ys <- lapply(sim, function(sm) {
                w <- 0.5 * sm %*% si
                diag(w) <- diag(w) - diag(S %*% w)
                w %*% S
              })
              b <- matrix(0, q, q)
              for (s in seq_len(q)) for (t in s:q)
                b[s, t] <- b[t, s] <- 2 * sum(ys[[s]] * t(ys[[t]]))
              (tr_vg - sum(acov * b)) / DF
            })(),
            if (is.null(dbl_sf$reason)) "NULL" else dbl_sf$reason))
cat(sprintf("       the exact value is POSITIVE: the refusal is a cancellation sign-flip\n"))
cat(sprintf("       (exact tr_vg %.10g - proj %.10g; amplification %.4g)\n",
            ex[["EXACT_TR_VG"]], ex[["EXACT_PROJ"]],
            (abs(ex[["EXACT_TR_VG"]]) + abs(ex[["EXACT_PROJ"]])) /
              abs(ex[["EXACT_TR_VG"]] - ex[["EXACT_PROJ"]])))

cat("\ncorrected SEs (reported with reason NULL):\n")
for (i in seq_along(dbl_se)) {
  e <- ex[[sprintf("EXACT_SE%d", i)]]
  cat(sprintf("  comp %d: exact %.12g | double %.12g | rel.err %.3e\n",
              i, e, dbl_se[[i]], abs(e - dbl_se[[i]]) / e))
}

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
cat("  construction                p     kappa(R)     rel.err     bound        ratio\n")
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
  exr <- exact(g$S, dr)
  dtr <- axes_se_pricing(g$S, dr, N)$corrected
  exv <- vapply(seq_along(dtr), function(i) exr[[sprintf("EXACT_SE%d", i)]], 0)
  rel <- max(abs(exv - dtr) / abs(exv))
  bnd <- pr * kappa_of(g$S)^2 * .Machine$double.eps
  rat <- rel / bnd
  reach_ok <- reach_ok && rat <= REACHABLE_WINDOW
  cat(sprintf("  %s  %3d  %10.4g   %9.3e   %10.3e   %8.2e\n",
              cs$lbl, pr, kappa_of(g$S), rel, bnd, rat))
}

cat(sprintf("\nANCHORS: %s\nSWEEP (within a factor of 10 of the bound): %s\nREACHABLE (attainment below %.0e): %s\n",
            if (ok) "PASS" else "FAIL", if (sweep_ok) "PASS" else "FAIL",
            REACHABLE_WINDOW, if (reach_ok) "PASS" else "FAIL"))
if (!ok || !sweep_ok || !reach_ok) quit(status = 1L)
