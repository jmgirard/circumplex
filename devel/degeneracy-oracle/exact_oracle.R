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

cat(sprintf("\nANCHORS: %s\nSWEEP (within a factor of 10 of the bound): %s\n",
            if (ok) "PASS" else "FAIL", if (sweep_ok) "PASS" else "FAIL"))
if (!ok || !sweep_ok) quit(status = 1L)
