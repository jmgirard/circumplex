# The a-posteriori per-fit accuracy certificate (M108; D-051, RR21).
#
# What is under test is axes_accuracy_certificate(): given the matrix a fit is
# priced at, it estimates the relative error that fit's corrected component SEs
# and its `cval` carry. The mechanism is in R/axes_certificate.R; these are its
# validation layers.
#
# ORACLE RECORDS (DESIGN.md "Oracle records"; IP3's two-independent-types bar).
# The certificate's number is backed by two independent oracle TYPES:
#
#   EXACT-RATIONAL -- devel/degeneracy-oracle/exact_oracle.py prices this model
#     in Python `fractions` (exact integer arithmetic, no floating point
#     anywhere), driven by devel/degeneracy-oracle/exact_oracle.R, which is the
#     committed generator for every value pinned below and reproduces them from
#     committed material alone. It cannot run in this suite (python3 is not a
#     package dependency and devel/ does not ship), so what is committed here
#     is its output.
#
#     WHAT is committed changed at M115. Until then it was a relative ERROR per
#     case -- one machine's measurement, so every assertion reading one sat
#     behind a bit-identity gate on the shipped pricing and skipped wherever
#     that pricing moved. Since M115 it is the EXACT quadratic forms
#     themselves, as hi/lo double pairs: those describe the matrix rather than
#     a machine, so each machine measures its OWN error against them and the
#     bracket asserts everywhere. See the block at `cert_frozen` for the one
#     precondition that survives and why.
#
#   CLOSED-FORM -- the hand-derived configurations in the last two tests of
#     this file, derived from the definitions and committed as exact fractions.
#     Neither shares code, library or pipeline with the Python oracle or with
#     the route under test. The first is dyadic throughout, so the shipped
#     route is exact there and the certificate is pinned at its floor; the
#     second (M113) sits at a configuration whose shipped route is wrong by
#     about 1e-12 on the quotient, so the `fiml_ratio` field is checked where
#     there is an error to catch and not only at its floor.
#
# A third layer ships alongside and is deliberately NOT counted as a type: the
# planted-perturbation invariants below assert the comparison's sensitivity
# with no external truth at all.


# ---- the six anchor geometries ----------------------------------------------
#
# Constructed here with the helper-m106-degeneracy.R builders and in the oracle
# script with its own copies of the same closed forms, at the same parameters.
# `kappa` is pinned per case as the fingerprint that ties the two: a builder
# edit that moved a geometry would leave the frozen EXACT_* figures describing a
# matrix this file no longer builds, and kappa is what reddens then.
cert_anchors <- function() {
  list(
    list(id = "a4", lbl = "family A, p = 8, kappa 1e4",
         r = m106_family_a(2.4e-4, 1L), scale = as.character(1:8),
         ang = as.numeric(octants()), kappa = 1.000e4),
    list(id = "a5", lbl = "family A, p = 8, kappa 1e5",
         r = m106_family_a(2.4e-5, 1L), scale = as.character(1:8),
         ang = as.numeric(octants()), kappa = 1.000e5),
    list(id = "c4", lbl = "family C, p = 4 minimum",
         r = m106_family_c(1.2e-5), scale = as.character(1:4),
         ang = c(90, 180, 270, 360), kappa = 1.000e5),
    list(id = "b9a", lbl = "near-duplicate r = .9999",
         r = m106_family_b(7e-5), scale = as.character(c(1:8, 1L)),
         ang = c(as.numeric(octants()), as.numeric(octants())[[1L]]),
         kappa = 2.874e4),
    list(id = "b9b", lbl = "near-duplicate r = .99999",
         r = m106_family_b(7e-6), scale = as.character(c(1:8, 1L)),
         ang = c(as.numeric(octants()), as.numeric(octants())[[1L]]),
         kappa = 2.874e5)
  )
}

# The derivative set the oracle priced each case at: zeta1 is read off the
# case's own item map with the package's own predicate, never assumed -- the
# near-duplicate cases put two items on one scale, so they fit a component the
# spread designs do not.
cert_derivs <- function(cs) {
  z <- axes_fits_zeta1(split(seq_along(cs$scale), cs$scale))
  axes_se_derivs(cs$ang, cs$scale, NULL, z, FALSE)
}


# ---- the exact quadratic forms, and the one precondition left --------------
#
# WHAT CHANGED AT M115, and why. Until M115 this file committed a relative
# ERROR per case: a number measured on ONE machine, and a yardstick for no
# other. Every case therefore sat behind a bit-identity gate on the shipped
# double pricing and SKIPPED wherever that pricing differed by a bit -- so the
# bracket asserted on the machine that froze the figures and reported nothing
# at all on windows-latest (CI run 32752082137), which is the reach this
# milestone exists to restore.
#
# What is committed instead is the EXACT `v`, `v_naive` and `u` -- properties
# of the MATRIX, not of a machine -- each as a hi/lo double pair carrying about
# 106 significant bits, ten decades finer than the double-precision errors
# measured against them. Every machine now prices its own doubles against those
# and brackets ITS OWN error, which is what the bracket claimed to do all
# along. Produced by the exact-rational oracle in
# devel/degeneracy-oracle/exact_oracle.py; that script and its R driver remain
# the committed generator.
#
# ONE HALF OF THE OLD PRECONDITION SURVIVES: `sig`, the anchor matrix's upper
# triangle. The exact values describe THAT matrix, so a machine whose `cos()`
# builds a different one has no yardstick here and the case skips naming that
# reason. Unlike the shipped pricing this half is not expected to move -- these
# matrices are `cos()` at octant angles plus simple arithmetic -- and whether it
# ever fires off the authoring machine is read from the pull request's CI run,
# not assumed here.
#
# THE DERIVATIVE SET is pinned through `sig` and the exact values together. It
# is too large to commit entry by entry (27 matrices of 8x8 at the p = 8
# anchors), but the committed `v`, `v_naive` and `u` were priced from a
# derivative set the oracle builds from the same closed forms, so a derivative
# set that drifted here would move this machine's doubles AWAY from them and
# redden the bracket rather than hide inside it.
#
# What is still deliberately NOT pinned is the double-double reference route.
# That route is the artifact under test, and an expectation derived from the
# artifact under test is blind in the dimension it derives: an earlier draft of
# the old gate did pin it, and the planted defect that stops the route carrying
# low-order words then made these cases SKIP instead of redden -- the defect
# hid inside the precondition meant to protect the comparison.
#
# This block does NOT replace the kappa fingerprint. kappa is asserted OUTSIDE
# the precondition, so a builder edit that moved a geometry still REDDENS.
#
# REGENERATE the whole block -- matrix and exact values in one pass, so the two
# cannot come to describe different matrices -- from the repo root with:
#   CERT_EMIT=1 Rscript devel/degeneracy-oracle/exact_oracle.R
# and paste its trailing `cert_frozen` block over the one below.
#
# Measured on the authoring machine (macOS, arm64) on 2026-08-30 by that
# script, for orientation only -- no assertion reads these figures, which is
# the whole point of the change: the six geometries' certificate-over-true-
# error ratios ran 9.83 to 10.00 against the ceiling of that day, 1e3 --
# since lowered to the `cert_ceiling` of 100 committed below -- and the true
# SE errors ran 5.9e-14 (family A at kappa 1e4) to 3.4e-02 (counterexample B).

cert_frozen <- list(
  a4 = list(
    sig = c("0x1.b4d8379580e2p-1", "0x1.ffcb979800d1bp-2", "0x1.b4d8379580e2p-1", 
    "0x1.2bcd8009ffbebp-3", "0x1.ffcb979800d1bp-2", "0x1.b4d8379580e2p-1", 
    "0x0p+0", "0x1.2bcd8009ffbebp-3", "0x1.ffcb979800d1bp-2", "0x1.b4d8379580e2p-1", 
    "0x1.2bcd8009ffbe6p-3", "0x0p+0", "0x1.2bcd8009ffbebp-3", "0x1.ffcb979800d1bp-2", 
    "0x1.b4d8379580e2p-1", "0x1.ffcb979800d1ap-2", "0x1.2bcd8009ffbe6p-3", 
    "0x0p+0", "0x1.2bcd8009ffbebp-3", "0x1.ffcb979800d1bp-2", "0x1.b4d8379580e2p-1", 
    "0x1.b4d8379580e2p-1", "0x1.ffcb979800d1bp-2", "0x1.2bcd8009ffbebp-3", 
    "0x0p+0", "0x1.2bcd8009ffbe6p-3", "0x1.ffcb979800d1ap-2", "0x1.b4d8379580e2p-1"
    ),
    v_hi = c("0x1.7fb171557665bp-3", "0x1.7fe5cdf0c4d5bp-3"),
    v_lo = c("-0x1.a352bdd9f74f6p-64", "0x1.b53cb2eed0db5p-60"),
    vn_hi = c("0x1.ffcb9978a7aa0p-3", "0x1.ffb16679a59dfp-2"),
    vn_lo = c("-0x1.21464d4a961bep-57", "-0x1.d0e261a64002ep-57"),
    u_hi = "0x1.136add72ce9f6p+11",
    u_lo = "-0x1.21c8c1ce2e1b1p-44"
  ),
  a5 = list(
    sig = c("0x1.b50079a0feb12p-1", "0x1.fffac1e05c131p-2", "0x1.b50079a0feb12p-1", 
    "0x1.2be920fd7587ep-3", "0x1.fffac1e05c131p-2", "0x1.b50079a0feb12p-1", 
    "0x0p+0", "0x1.2be920fd7587ep-3", "0x1.fffac1e05c131p-2", "0x1.b50079a0feb12p-1", 
    "0x1.2be920fd75879p-3", "0x0p+0", "0x1.2be920fd7587ep-3", "0x1.fffac1e05c131p-2", 
    "0x1.b50079a0feb12p-1", "0x1.fffac1e05c12fp-2", "0x1.2be920fd75879p-3", 
    "0x0p+0", "0x1.2be920fd7587ep-3", "0x1.fffac1e05c131p-2", "0x1.b50079a0feb12p-1", 
    "0x1.b50079a0feb12p-1", "0x1.fffac1e05c131p-2", "0x1.2be920fd7587ep-3", 
    "0x0p+0", "0x1.2be920fd75879p-3", "0x1.fffac1e05c12fp-2", "0x1.b50079a0feb12p-1"
    ),
    v_hi = c("0x1.7ff822f445090p-3", "0x1.7ffd60f5ad28bp-3"),
    v_lo = c("-0x1.936d3acc2b778p-60", "-0x1.d0ddf75c29bcep-57"),
    vn_hi = c("0x1.fffac1e52b6ddp-3", "0x1.fff822d8710d2p-2"),
    vn_lo = c("-0x1.4c20a635f2019p-60", "-0x1.2d8da398995fep-56"),
    u_hi = "0x1.560b55f7e8335p+14",
    u_lo = "-0x1.8089fbf774868p-41"
  ),
  c4 = list(
    sig = c("0x1.fffd60ecbe7bp-2", "0x0p+0", "0x1.fffd60ecbe7bp-2", "0x1.fffd60ecbe7aep-2", 
    "0x0p+0", "0x1.fffd60ecbe7bp-2"),
    v_hi = c("0x1.7ffd60fdec50dp-3", "0x1.800000036f8e8p-3"),
    v_lo = c("0x1.659fc12469cbep-57", "-0x1.d8e8dce3de8afp-57"),
    vn_hi = c("0x1.000000036f930p-2", "0x1.fffd60ee76447p-2"),
    vn_lo = c("-0x1.a2bab17f99d60p-56", "0x1.2ea2a74033150p-56"),
    u_hi = "0x1.4001f74d274cbp+1",
    u_lo = "-0x1.e2f9fd70264fcp-53"
  ),
  b9a = list(
    sig = c("0x1.f86394ae0e824p-2", "0x1.e98a8996b5f8ap-3", "0x1.a605f0a4c5714p-2", 
    "-0x1.db2162eb1137p-7", "0x1.999999999999bp-3", "0x1.a605f0a4c5714p-2", 
    "-0x1.e98a8996b5f86p-4", "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3", 
    "0x1.a605f0a4c5714p-2", "-0x1.db2162eb113aap-7", "-0x1.9999999999998p-4", 
    "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3", "0x1.a605f0a4c5714p-2", 
    "0x1.e98a8996b5f86p-3", "-0x1.8d8ae1657af7p-7", "-0x1.9999999999998p-4", 
    "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3", "0x1.a605f0a4c5714p-2", 
    "0x1.f86394ae0e824p-2", "0x1.999999999999bp-3", "-0x1.8d8ae1657af4p-7", 
    "-0x1.9999999999998p-4", "-0x1.8d8ae1657af7p-7", "0x1.9999999999998p-3", 
    "0x1.a605f0a4c5714p-2", "0x1.fff2e4e46e7a8p-1", "0x1.f86394ae0e824p-2", 
    "0x1.e98a8996b5f8ap-3", "-0x1.db2162eb1137p-7", "-0x1.e98a8996b5f86p-4", 
    "-0x1.db2162eb113aap-7", "0x1.e98a8996b5f86p-3", "0x1.f86394ae0e824p-2"
    ),
    v_hi = c("0x1.eecf3c6f2786bp-4", "0x1.ad14d01e8a4b2p-4", "0x1.dd93c7c70363ep-2"
    ),
    v_lo = c("-0x1.010402a050219p-59", "-0x1.463576282f2f1p-59", "0x1.49f589c2481b9p-59"
    ),
    vn_hi = c("0x1.8be3fa47bb758p-3", "0x1.2aa9076ab71a7p-3", "0x1.47c7f04c468b2p-1"
    ),
    vn_lo = c("0x1.ee5c78f087d78p-57", "0x1.f6c3bae380ad2p-57", "0x1.37f6e0038c159p-56"
    ),
    u_hi = "0x1.dd66dd08b9283p+4",
    u_lo = "0x1.b39ce6efa592dp-51"
  ),
  b9b = list(
    sig = c("0x1.f86964229da49p-2", "0x1.e9902d41e6beep-3", "0x1.a605f0a4c5714p-2", 
    "-0x1.db26dc16dcb8p-7", "0x1.999999999999bp-3", "0x1.a605f0a4c5714p-2", 
    "-0x1.e9902d41e6bebp-4", "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3", 
    "0x1.a605f0a4c5714p-2", "-0x1.db26dc16dcbb9p-7", "-0x1.9999999999998p-4", 
    "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3", "0x1.a605f0a4c5714p-2", 
    "0x1.e9902d41e6bebp-3", "-0x1.8d8ae1657af7p-7", "-0x1.9999999999998p-4", 
    "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3", "0x1.a605f0a4c5714p-2", 
    "0x1.f86964229da49p-2", "0x1.999999999999bp-3", "-0x1.8d8ae1657af4p-7", 
    "-0x1.9999999999998p-4", "-0x1.8d8ae1657af7p-7", "0x1.9999999999998p-3", 
    "0x1.a605f0a4c5714p-2", "0x1.fffeb07583584p-1", "0x1.f86964229da49p-2", 
    "0x1.e9902d41e6beep-3", "-0x1.db26dc16dcb8p-7", "-0x1.e9902d41e6bebp-4", 
    "-0x1.db26dc16dcbb9p-7", "0x1.e9902d41e6bebp-3", "0x1.f86964229da49p-2"
    ),
    v_hi = c("0x1.eed0c2cfd9376p-4", "0x1.ad165233741c0p-4", "0x1.dda97a116808fp-2"
    ),
    v_lo = c("-0x1.d0ee322f62ab4p-58", "0x1.e441ae46cfc85p-59", "0x1.2a1c6d337b0e1p-56"
    ),
    vn_hi = c("0x1.8be582ecd5915p-3", "0x1.2aaa260cb9c24p-3", "0x1.47c9e3d75bdb6p-1"
    ),
    vn_lo = c("-0x1.f8a3754824057p-57", "0x1.2f8ab83d3d8bfp-60", "-0x1.dbf1617a6c6cdp-56"
    ),
    u_hi = "0x1.dd66a60c11651p+4",
    u_lo = "0x1.a97ef4aa07aebp-50"
  ),
  cxb = list(
    sig = c("-0x1.ac70f5bf320e9p-1", "0x1.a2ad9ad37693p-1", "-0x1.ffb4667563093p-1"
    ),
    v_hi = c("0x1.a27aa6fa81289p+3", "0x1.9033b1b503c27p+3"),
    v_lo = c("0x1.14a44927d1499p-52", "0x1.6dd7ad9921fd4p-54"),
    vn_hi = c("0x1.d7e81cc594451p+5", "0x1.bd654f98f5a5bp+5"),
    vn_lo = c("-0x1.8bc7343b184ebp-49", "0x1.df2138cd8f0a9p-50"),
    u_hi = "0x1.c70c587f1f6ecp-5",
    u_lo = "-0x1.af973c7509042p-59"
  )
)

cert_hex <- function(x) sprintf("%a", as.vector(x))

# The relative error of one shipped double against the exact value hi + lo.
# Written as ((hat - hi) - lo) so no precision is thrown away: wherever the two
# agree to within a factor of two -- every quantity here bar `u` at
# counterexample B, whose relative error is of order one (0.42 on
# ubuntu-latest, 4.89 on macOS arm64, both measured 2026-08-30 by the two runs
# recorded in M115's file) -- `hat - hi` is exact, and the low word then lands
# on a difference that still has all its bits. At B the subtraction is ordinary
# and loses nothing that matters against an error that large.
cert_rel <- function(hat, hi, lo) ((hat - hi) - lo) / (hi + lo)

# A variance's relative error converted to its square root's. This is the exact
# identity sqrt(1 + e) - 1, written with the cancellation at small `e` removed
# rather than the first-order e/2 -- the conversion between what is committed
# above (pre-root quadratic forms) and what users are handed (an SE, and the
# quotient of two SEs). `n` cancels out of both exactly, so none appears.
cert_root_rel <- function(e) abs(e / (sqrt(1 + e) + 1))

# The certificate's floor, `safety factor * 2 * eps`, with the factor WRITTEN
# DOWN rather than read from axes_certificate_safety_factor (M115 AC4). An
# expectation derived from the constant it is checking cannot notice that
# constant move: with the factor read from the package, raising it from 10 to
# 100 shifted estimate and expectation together and every assertion in this
# file stayed green. Every other site that needs the floor reads this one.
cert_floor <- 10 * 2 * .Machine$double.eps

# The bracket's UPPER end: how far above a fit's true error the certificate is
# allowed to sit before the estimate counts as an overstatement. Written down
# here, and NOT read from axes_certificate_safety_factor, for the reason the
# floor above is not: an expectation reading the constant it is checking moves
# with it and notices nothing.
#
# 100 is ten times the safety factor `10` written down above. The certificate
# is a bound times that factor, so a ratio at the factor is what it is built to
# deliver and the room above it is what a machine rounding the other way needs.
# M108 pre-registered 1e3 before any measurement existed; every measurement
# since has come in two decades under it, which is a ceiling nothing can
# reach. Measured 2026-08-30 on aarch64-apple-darwin23, R 4.6.1, reference
# BLAS (range corrected 2026-08-31, M116 first return, F5): eighteen
# estimate-over-true-error ratios across the six priced cases and three
# fields, all between 9.829339 (`cxb se`) and 10.0025192 (`a4 fiml_ratio`),
# reproduced by pricing each case through axes_v_pricing() and
# axes_u_pricing() against the exact values committed below and dividing
# axes_accuracy_certificate()'s field by the result -- the same two steps
# cert_true_error() and the per-case tests below already take.
cert_ceiling <- 100

# THIS MACHINE's own relative error at one case, against the committed exact
# values. Returns NULL only where the shipped pricing refused, having already
# failed; skips where this machine builds a different matrix.
cert_true_error <- function(id, sigma, d) {
  fz <- cert_frozen[[id]]
  # THE MATRIX CHECK COMES FIRST, before the shipped pricing is even called.
  # The refusal branch below calls a refusal a regression, and that conclusion
  # is only warranted once this machine is known to build the geometry the
  # regression would be against.
  #
  # Compared as DOUBLES, not as the text `%a` prints. `?sprintf` states that
  # `%a` is not uniquely defined across platforms -- trailing zeros and the
  # leading digit are both at the C library's discretion -- so a machine
  # agreeing on every bit but formatting differently would skip all six cases,
  # which is exactly the vacuity M115 exists to remove. `as.numeric()` on a hex
  # literal routes through R's own R_strtod rather than the platform's
  # formatter, and round-trips every value committed here exactly (none is a
  # denormal, the one place that parse loses bits).
  if (!identical(sigma[upper.tri(sigma)], as.numeric(fz$sig))) {
    testthat::skip(paste0(
      "this machine does not build the anchor matrix at case '", id, "' bit ",
      "for bit, so the exact quadratic forms committed for that matrix are ",
      "not a yardstick for this one"
    ))
  }
  v <- axes_v_pricing(sigma, d)
  u <- axes_u_pricing(sigma, d)
  # A REFUSAL from the shipped pricing is NOT a failure to reproduce. Every
  # one of these six geometries is admitted -- axes_sigma_degenerate() passes
  # on each -- so a refusal here is a regression in axes_pricing_core(), and
  # folding it into the skip above would turn that red green. Fail on it, and
  # let the caller carry on: the certificate returns its sentinel at a refusal,
  # which reddens the bracket too.
  if (is.character(v) || is.character(u)) {
    testthat::fail(paste0(
      "the shipped pricing REFUSES at case '", id, "' (",
      paste(Filter(is.character, list(v, u)), collapse = ", "),
      ") -- an admitted geometry, so this is a regression, not a platform ",
      "difference"
    ))
    return(NULL)
  }
  dv <- cert_rel(v$corrected, as.numeric(fz$v_hi), as.numeric(fz$v_lo))
  dn <- cert_rel(v$naive, as.numeric(fz$vn_hi), as.numeric(fz$vn_lo))
  du <- cert_rel(u, as.numeric(fz$u_hi), as.numeric(fz$u_lo))
  list(
    # Aggregated by MAX over components, as the certificate's own estimands
    # are: the reported SE vector refuses as a unit, so the worst component is
    # what a gate has to protect.
    se = max(cert_root_rel(dv)),
    # `cval` is the numerator divided by df, and df divides out of a relative
    # error, so the numerator's error IS cval's.
    cval = abs(du),
    # The quotient's own error, formed from the two arms' rather than from a
    # quotient of doubles: (1 + dv)/(1 + dn) - 1 = (dv - dn)/(1 + dn), which
    # takes no difference between two nearly equal large numbers.
    ratio = max(cert_root_rel((dv - dn) / (1 + dn)))
  )
}

# The bracket, for one field, against an error measured on the running machine.
#
# Two branches, and NEITHER is a skip -- that is the change (M115). Where the
# certificate sits at its floor it is reporting that the fit committed no error
# worth stating, and there is no ratio to form; what is asserted there is that
# the machine agrees, because a true error ABOVE what the floor certifies is
# exactly the under-report this instrument exists to prevent. Where it sits
# above its floor both halves of the bracket run: at least the measured error,
# and at most `cert_ceiling` times it.
#
# `lbl` names the field AND the case, and is carried into every expectation:
# a cross-platform failure arrives as two bare numbers otherwise, naming
# neither -- which is how the ubuntu-latest failure that returned this
# milestone at its first review gate had to be traced by line number.
cert_bracket <- function(est, true_rel, lbl) {
  if (identical(est, cert_floor)) {
    expect_lte(true_rel, cert_floor, label = paste0(lbl, ": true error"))
  } else {
    expect_gte(est, true_rel, label = paste0(lbl, ": estimate"))
    # The upper end never sits below the certificate's own floor: every field
    # is fac * max(delta, 2 * eps), so the floor is the smallest value the
    # certificate can report and asking for less is asking for a value it
    # cannot produce. Without this, a measured error of zero degenerates the
    # bound to `est <= 0`, and a broken platform's sentinel then reports
    # "1 is not less than or equal to 0" -- a comparison against zero rather
    # than against anything the certificate promises (M116 review, F2). The
    # failure set is unchanged: where the bound was ceiling * true_rel it
    # still is, and where it was below the floor no estimate could sit
    # between the two anyway.
    expect_lte(est, max(cert_ceiling * true_rel, cert_floor),
               label = paste0(lbl, ": estimate"))
  }
}


test_that("AC3: the anchor case list is not empty", {
  # Without this, emptying cert_anchors() would take every bracket assertion in
  # this file with it -- the per-case tests below are GENERATED from that list,
  # and a loop over nothing generates nothing and reports PASS. The count is
  # written down rather than derived from the list it is checking.
  expect_length(cert_anchors(), 5L)
  expect_identical(vapply(cert_anchors(), `[[`, "", "id"),
                   c("a4", "a5", "c4", "b9a", "b9b"))
  # ... and every one of them, plus counterexample B, has exact values to be
  # priced against.
  expect_identical(sort(names(cert_frozen)),
                   sort(c("a4", "a5", "c4", "b9a", "b9b", "cxb")))
  # ... and each case's committed arrays are the length that case needs.
  # cert_rel() is elementwise, so a regeneration pasted in truncated would be
  # RECYCLED to length and every entry compared against the wrong exact value.
  # Both counts are written down rather than read from cert_frozen: `p` from
  # the matrix each case builds (28 = choose(8, 2), and so on down), and the
  # component count from the case's design.
  cert_shape <- list(a4 = c(8L, 2L), a5 = c(8L, 2L), c4 = c(4L, 2L),
                     b9a = c(9L, 3L), b9b = c(9L, 3L), cxb = c(3L, 2L))
  for (id in names(cert_shape)) {
    fz <- cert_frozen[[id]]
    expect_length(fz$sig, choose(cert_shape[[id]][[1L]], 2L))
    for (fld in c("v_hi", "v_lo", "vn_hi", "vn_lo")) {
      expect_identical(length(fz[[fld]]), cert_shape[[id]][[2L]],
                       label = paste0(id, " ", fld, " length"))
    }
    expect_length(fz$u_hi, 1L)
    expect_length(fz$u_lo, 1L)
  }

  # ... and each case's MATRIX is the size the table above says (M116). The
  # `p` column was checked only against `cert_frozen`'s own committed arrays
  # until now -- one committed artifact against another, with the thing that
  # BUILDS the matrix never asked. cert_true_error()'s precondition compares
  # upper triangles elementwise, so a builder returning a matrix of a
  # different dimension makes that comparison unequal by length and the case
  # SKIPS, calling a builder edit a platform difference. Asserted here,
  # outside every precondition, it reddens instead. Counterexample B's saved
  # matrix goes through the same precondition, so it is pinned too.
  for (cs in cert_anchors()) {
    expect_identical(dim(cs$r),
                     rep(cert_shape[[cs$id]][[1L]], 2L),
                     label = paste0(cs$id, " matrix dim"))
  }
  cxb_sigma <- readRDS(test_path("fixtures", "rb18-counterexample-b.rds"))$S
  expect_identical(dim(cxb_sigma), rep(cert_shape$cxb[[1L]], 2L),
                   label = "cxb matrix dim")
})


# One test PER CASE, deliberately: `skip()` abandons the whole `test_that()` it
# fires in, so a single loop would let one non-reproducing case take the other
# four with it -- and the criterion's "a skip on some platform is expected"
# only means anything if the cases skip independently.
for (cert_case in cert_anchors()) {
  test_that(paste0("AC2: the estimate brackets THIS machine's own error -- ",
                   cert_case$lbl), {
    cs <- cert_case
    d <- cert_derivs(cs)
    # OUTSIDE the precondition: a builder edit that moved this geometry must
    # redden here, never skip, because then the exact values committed above
    # would be describing a matrix this file no longer builds.
    expect_equal(m106_kappa(cs$r), cs$kappa, tolerance = 1e-3, label = cs$lbl)

    true_rel <- cert_true_error(cs$id, cs$r, d)
    if (is.null(true_rel)) return()
    cert <- axes_accuracy_certificate(cs$r, d)

    cert_bracket(cert$se, true_rel$se, paste0(cs$id, " se"))
    cert_bracket(cert$cval, true_rel$cval, paste0(cs$id, " cval"))
    cert_bracket(cert$fiml_ratio, true_rel$ratio, paste0(cs$id, " fiml_ratio"))
  })
}


test_that("AC2/AC3: at counterexample B the estimate brackets a 3.4%-wrong SE", {
  # The one committed matrix on which the shipped corrected SEs are measurably
  # wrong (3.413e-02) while the pre-M89 criterion reported them with reason
  # NULL, and on which the double-precision cval comes out sign-flipped (exact
  # +0.0555, double -0.216 -- relative error 4.890). Frozen from the same
  # oracle run. Provenance of the fixture itself is at its first read site in
  # test-axes-scaled-fit.R.
  fx <- readRDS(test_path("fixtures", "rb18-counterexample-b.rds"))
  d <- axes_se_derivs(fx$ia, c("A", "B", "C"), NULL, FALSE, FALSE)
  # OUTSIDE the precondition, for the same reason kappa is outside it at the
  # five anchors: where the gate below fires this test would otherwise assert
  # nothing at all -- not even that the fixture is still the 3.4%-wrong matrix
  # the frozen figures were measured on. The literal is committed here rather
  # than read from the fixture's own `kappa` field, which would be blind in
  # the dimension it derives.
  expect_equal(m106_kappa(fx$S), 6654372.506, tolerance = 1e-6)

  true_rel <- cert_true_error("cxb", fx$S, d)
  if (is.null(true_rel)) return()
  cert <- axes_accuracy_certificate(fx$S, d)

  cert_bracket(cert$se, true_rel$se, "cxb se")
  cert_bracket(cert$cval, true_rel$cval, "cxb cval")
  cert_bracket(cert$fiml_ratio, true_rel$ratio, "cxb fiml_ratio")

  # The wrongness itself, asserted rather than described: this is the one
  # committed matrix on which double precision misses the stated accuracy
  # target while the pre-M89 criterion reported the SEs with reason NULL.
  #
  # Asserted against delta_star -- the package's own target -- and NOT as a
  # window around a measured figure. A first draft here wrote decades taken
  # from the authoring machine (SE error in (1e-2, 1e-1), cval error above 1),
  # and both reddened on ubuntu-latest, which prices this matrix through a
  # different BLAS and measures 0.124 and 0.42 where macOS measures 0.0341 and
  # 4.890. At an ill-conditioned matrix the size of the rounding error IS a
  # property of the machine; what is a property of the MATRIX is that no
  # machine gets within the target on it. So only that is claimed, and the
  # margin is three decades or more on both machines seen so far.
  expect_gt(true_rel$se, axes_degeneracy_delta_star)
  expect_gt(true_rel$cval, axes_degeneracy_delta_star)
})


test_that("AC3: the estimate discriminates the reachable cases from counterexample B", {
  # The discrimination is the point of the instrument: the same threshold that
  # passes every reachable geometry must refuse B. delta_star = 1e-4 is the
  # stated accuracy target the criterion already carries.
  for (cs in cert_anchors()) {
    cert <- axes_accuracy_certificate(cs$r, cert_derivs(cs))
    expect_lt(cert$se, axes_degeneracy_delta_star)
    expect_lt(cert$cval, axes_degeneracy_delta_star)
    expect_lt(cert$fiml_ratio, axes_degeneracy_delta_star)
  }

  fx <- readRDS(test_path("fixtures", "rb18-counterexample-b.rds"))
  d <- axes_se_derivs(fx$ia, c("A", "B", "C"), NULL, FALSE, FALSE)
  cert <- axes_accuracy_certificate(fx$S, d)
  expect_gt(cert$se, axes_degeneracy_delta_star)
  expect_gt(cert$cval, axes_degeneracy_delta_star)
})


test_that("AC1: the estimate is finite and non-negative across the admitted domain", {
  # "Admitted" is AC1's own scope: every matrix on which the criterion's
  # "singular" and "indefinite" limbs pass -- which includes the whole
  # ill-conditioned band, up to and past the current floor, and the
  # roundoff-negative corner where no positive-definiteness assumption is
  # available.
  cases <- list(
    well_conditioned = m106_family_a(0.3, 1L),
    at_the_floor = m106_family_a(2.4e-5, 1L),
    past_the_floor = m106_family_b(7e-7),
    machine_singular = m106_family_b(0),
    roundoff_negative = m106_planted(8L, -0.5 * m106_band(8L))
  )
  for (nm in names(cases)) {
    r <- cases[[nm]]
    expect_false(identical(axes_sigma_degenerate(r), "singular"), label = nm)
    expect_false(identical(axes_sigma_degenerate(r), "indefinite"), label = nm)

    p <- nrow(r)
    sid <- if (nm %in% c("past_the_floor", "machine_singular")) {
      as.character(c(1:8, 1L))
    } else {
      as.character(seq_len(p))
    }
    ang <- if (p == 9L) {
      c(as.numeric(octants()), as.numeric(octants())[[1L]])
    } else {
      rep(as.numeric(octants()), length.out = p)
    }
    d <- axes_se_derivs(ang, sid, NULL,
                        axes_fits_zeta1(split(seq_len(p), sid)), FALSE)
    cert <- axes_accuracy_certificate(r, d)
    expect_true(is.finite(cert$se) && cert$se >= 0, label = nm)
    expect_true(is.finite(cert$cval) && cert$cval >= 0, label = nm)
    # The third field is asserted here for the same reason as the other two,
    # and for one more: axes_degeneracy_refusal() compares the MAX over all
    # three against the target, and a NaN there would make that comparison NA
    # and raise from inside a helper obliged to refuse rather than error.
    expect_true(is.finite(cert$fiml_ratio) && cert$fiml_ratio >= 0, label = nm)
  }
})


test_that("AC1: the estimate cannot depend on the typed sample size", {
  # Structural, not tested-into-existence: the certificate is computed from the
  # pre-square-root quadratic forms, in which n never appears, and takes no `n`
  # argument at all. The formals assertion is what pins that -- an `n` added
  # later reddens here.
  expect_false("n" %in% names(formals(axes_accuracy_certificate)))

  # The other half is the estimand: the quantity being estimated is itself
  # n-invariant, so there is nothing for an n-free estimate to miss. Measured
  # on the case whose error is largest among the reachable five, so the
  # comparison is not swamped by the one division and one square root that DO
  # carry n (about 2 ulp, which is what the certificate's 2*eps floor covers).
  cs <- cert_anchors()[[5L]]
  d <- cert_derivs(cs)
  ref <- axes_dd_pricing(cs$r, d)
  true_rel <- function(n) {
    se <- axes_se_pricing(cs$r, d, n)$corrected
    truth <- sqrt(dd_to_double(ref$v) / n)
    max(abs(se - truth) / truth)
  }
  small <- true_rel(100)
  large <- true_rel(5e5)
  expect_lt(abs(small - large) / large, 1e-3)
  expect_identical(axes_accuracy_certificate(cs$r, d),
                   axes_accuracy_certificate(cs$r, d))
})


test_that("AC1: the refusal predicate and its warning both read the quotient", {
  # The failure this pins is a SPLIT between the two: until M113 the predicate
  # and the note each took their own max, and the note's set was the smaller
  # one. A fit refused on a field the note did not read would be told an
  # "estimated relative error" BELOW the target it had just been refused
  # against -- a warning contradicting its own refusal.
  #
  # Driven by a certificate whose first two fields sit INSIDE the target and
  # whose third sits outside it, so only the third can produce the refusal.
  r <- m106_family_b(7e-7)
  d <- axes_se_derivs(c(as.numeric(octants()), as.numeric(octants())[[1L]]),
                      as.character(c(1:8, 1L)), NULL, TRUE, FALSE)
  # The certificate is consulted only on this limb, so the case must reach it.
  expect_identical(axes_sigma_degenerate(r), "ill_conditioned")

  planted <- list(se = 1e-9, cval = 1e-9, fiml_ratio = 1e-2)
  expect_lt(max(planted$se, planted$cval), axes_degeneracy_delta_star)
  expect_gt(planted$fiml_ratio, axes_degeneracy_delta_star)
  testthat::local_mocked_bindings(
    axes_accuracy_certificate = function(sigma, d) planted,
    .package = "circumplex"
  )
  got <- axes_degeneracy_refusal(r, d)
  expect_identical(got$reason, "uncertified")
  # ... and the warning names the field that refused it, not a smaller max.
  expect_match(axes_degeneracy_note(got, r), "estimated relative error 0.01",
               fixed = TRUE)
})


test_that("the sentinel is returned where there is nothing to certify", {
  # 1 means "no digits certified": finite, non-negative, and four decades above
  # delta_star, so a gate keyed to the certificate fails closed on it (GP2).
  # Fired here through the double route's own refusal -- the same failure the
  # shipped pricing reports as "unidentified", at which point there is no
  # reported number left to certify.
  d <- axes_se_derivs(as.numeric(octants()), as.character(1:8), NULL,
                      FALSE, FALSE)
  r <- m106_family_a(2.4e-4, 1L)
  # A derivative set with a duplicated matrix makes the information matrix
  # exactly rank-deficient, which is where the shipped route gives up.
  d_dup <- d
  d_dup$mats[[length(d_dup$mats)]] <- d_dup$mats[[1L]]
  expect_identical(axes_v_pricing(r, d_dup), "unidentified")
  expect_identical(axes_accuracy_certificate(r, d_dup),
                   list(se = 1, cval = 1, fiml_ratio = 1))

  # And through the self-test: an arithmetic that defeats the error-free
  # transforms must degrade to the sentinel, never to a certificate computed
  # with them.
  testthat::local_mocked_bindings(
    axes_dd_selftest = function() FALSE,
    .package = "circumplex"
  )
  expect_identical(axes_accuracy_certificate(r, d),
                   list(se = 1, cval = 1, fiml_ratio = 1))
})


test_that("dd_solve() returns its sentinel, never a condition, on a column
           with no finite pivot", {
  # Regression, M108 review: which.max() on an all-NA column returns
  # integer(0), so the finiteness test on the selected pivot evaluated to NA
  # and the `if` errored -- the sentinel path raising a condition instead of
  # taking it. Two ways in: NaN directly, and an overflow to NaN produced by
  # the dd arithmetic itself on finite input (1e308 + 1e308).
  expect_null(dd_solve(dd_of(matrix(NaN, 2, 2))))
  expect_null(dd_solve(dd_of(matrix(1e308, 2, 2))))
  # ... and the ordinary path still inverts.
  expect_identical(dd_solve(dd_of(diag(2)))$hi, diag(2))
})


test_that("the estimate tracks a planted perturbation of the shipped values", {
  # A sensitivity invariant, not an oracle: it asserts the comparison is wired
  # to the shipped numbers and responds monotonically, with no external truth
  # at all. The shipped double route is captured and its output multiplied by
  # (1 + delta); the certificate must then read at least F*delta/2 on the SE
  # side and F*delta on the cval side, less the true error already present.
  cs <- cert_anchors()[[1L]]
  d <- cert_derivs(cs)
  real_v <- axes_v_pricing
  real_u <- axes_u_pricing
  # The error this geometry ALREADY carries is the slack either bound needs:
  # the planted delta lands on top of it, so the response is delta plus or
  # minus what was there before. Taken from the unperturbed call rather than
  # assumed, so the window stays as tight as the case allows (here about a
  # part in a thousand at the smallest delta).
  base <- axes_accuracy_certificate(cs$r, d)
  for (delta in c(1e-10, 1e-8, 1e-4, 1e-2)) {
    testthat::local_mocked_bindings(
      axes_v_pricing = function(sigma, dd) {
        out <- real_v(sigma, dd)
        out$corrected <- out$corrected * (1 + delta)
        out
      },
      axes_u_pricing = function(sigma, dd) real_u(sigma, dd) * (1 + delta),
      .package = "circumplex"
    )
    cert <- axes_accuracy_certificate(cs$r, d)
    # The safety factor WRITTEN DOWN, not read from the package: an
    # expectation computed from the constant it is checking moves with it and
    # can never notice it change (M115 AC4).
    f <- 10
    # Two slacks, both derived rather than tuned: the (1 +/- delta) factor is
    # the cross term (perturbing by (1 + delta) scales the error already there
    # by the same factor), and f*eps covers the single rounding the planted
    # multiplication itself commits, which lands directly in the relative
    # error being read.
    slack <- f * .Machine$double.eps
    expect_gte(cert$se, (f * delta / 2 - base$se) * (1 - delta) - slack)
    expect_lte(cert$se, (f * delta / 2 + base$se) * (1 + delta) + slack)
    expect_gte(cert$cval, (f * delta - base$cval) * (1 - delta) - slack)
    expect_lte(cert$cval, (f * delta + base$cval) * (1 + delta) + slack)
  }
})


test_that("the reference route lands on hand-derived exact values (closed-form oracle)", {
  # THE SECOND ORACLE TYPE (IP3; RR21 section 4). One configuration small
  # enough to price by hand, driven through the internal seam, with the matrix
  # and the derivative matrix chosen so that EVERY intermediate is a dyadic
  # rational -- a fraction with a power-of-two denominator, hence exactly
  # representable in binary floating point. The exact answers are therefore
  # exact doubles, and the reference route must land on them bit for bit, with
  # its low word exactly zero. Nothing here shares code, library or pipeline
  # with the Python exact-rational oracle or with the route under test.
  #
  # THE DERIVATION, by hand from the definitions in R/axes_corrected_se.R and
  # R/axes_scaled_fit.R. Take p = 2, one derivative matrix (q = 1), which is
  # also the one fitted component:
  #
  #   S    = [[1, 1/2], [1/2, 5/4]]      det S = 5/4 - 1/4 = 1
  #   S^-1 = [[5/4, -1/2], [-1/2, 1]]    (det 1, so the inverse is dyadic)
  #   M    = [[0, 0], [0, 1]]
  #
  #   X = S^-1 M         = [[0, -1/2], [0, 1]]
  #   info = 0.5*sum(X * t(X)) = 0.5*(0 + 2*(-1/2)(0) + 1) = 1/2
  #   acov = 1/info      = 2                      (dyadic: info is 2^-1)
  #
  #   W  = 0.5 * S^-1 (M*acov) S^-1 = X S^-1 = [[1/4, -1/2], [-1/2, 1]]
  #   Wc = W with the diagonal replaced by -rowSums(Wc0 * S), Wc0 = W less its
  #        diagonal:  rowSums = (-1/4, -1/4), so Wc = [[1/4, -1/2],
  #                                                    [-1/2, 1/4]]
  #   Wc S = [[0, -1/2], [-3/8, 1/16]]
  #   v = 2*sum(WcS * t(WcS)) = 2*(0 + 2*(-1/2)(-3/8) + 1/256)
  #     = 2*(3/8 + 1/256) = 97/128
  #
  #   The naive arm skips the Jacobian substitution -- W itself, not Wc:
  #   W S = [[1/4 - 1/4, 1/8 - 5/8], [-1/2 + 1/2, -1/4 + 5/4]]
  #       = [[0, -1/2], [0, 1]]
  #   v_naive = 2*sum(WS * t(WS)) = 2*(0 + (-1/2)(0) + (0)(-1/2) + 1) = 2
  #   so the quotient v/v_naive = 97/256, dyadic like everything else.
  #
  #   rho = S[1,2] = 1/2,  (S^-1)[1,2] = -1/2
  #   tr_vg = 1 - (-1/2)(1/2)(1 - 1/4) = 1 + 3/16 = 19/16
  #   Wy = 0.5 * X S^-1 = [[1/8, -1/4], [-1/4, 1/2]]; diag(S Wy) = (0, 1/2),
  #     so Wy's diagonal becomes (1/8 - 0, 1/2 - 1/2) = (1/8, 0)
  #   Y = Wy S = [[0, -1/4], [-1/4, -1/8]]
  #   B = 2*sum(Y * t(Y)) = 2*(0 + 2*(-1/4)(-1/4) + 1/64) = 9/32
  #   u = tr_vg - acov*B = 19/16 - 2*(9/32) = 19/16 - 9/16 = 5/8
  s <- rbind(c(1, 1 / 2), c(1 / 2, 5 / 4))
  d <- list(mats = list(rbind(c(0, 0), c(0, 1))), components = "m", n_comp = 1L)

  ref <- axes_dd_pricing(s, d)
  expect_identical(ref$v$hi, 97 / 128)
  expect_identical(ref$v$lo, 0)
  expect_identical(ref$v_naive$hi, 2)
  expect_identical(ref$v_naive$lo, 0)
  expect_identical(ref$u$hi, 5 / 8)
  expect_identical(ref$u$lo, 0)

  # THE CERTIFICATE, against THIS MACHINE's error rather than against an
  # asserted zero (M116). What stood here was three identity checks, one per
  # certificate field, each against `cert_floor` -- saying the machine running
  # them commits no error at all at this configuration, a claim about the
  # machine made without measuring it. On the authoring machine
  # the shipped route is indeed exact here, so those three passed while
  # checking nothing about the estimate: the floor is the certificate's own
  # constant, and reporting it is what the certificate does whenever its
  # bound is small, exact route or not.
  #
  # The same two-branch bracket the anchors use replaces them, fed by errors
  # measured against the fractions committed above -- the only exact values
  # in scope here, and derived by hand rather than from any route. Where the
  # machine is exact the floor branch asserts its measured error is under the
  # floor; where it is not, both halves of the bracket run.
  #
  # NO exactness identity on the shipped route stands here any more (M116
  # first return, F1). Three did -- expect_identical() of $corrected, $naive
  # and the u pricing against the fractions above -- and they were the one
  # claim in this file about a machine rather than a matrix: on any run that
  # reached the brackets they had already pinned every measured error to
  # exactly zero, which reduced both bracket branches to the identity checks
  # the brackets replaced. With them gone the brackets are the site's
  # assertions: a machine whose shipped route drifts here is judged by its
  # measured error -- green under the floor, red where the certificate
  # under-reports -- instead of reddening on a bit-level platform difference
  # the certificate prices correctly.
  hat <- axes_v_pricing(s, d)
  dv <- cert_rel(hat$corrected, 97 / 128, 0)
  dn <- cert_rel(hat$naive, 2, 0)
  du <- cert_rel(axes_u_pricing(s, d), 5 / 8, 0)
  cert <- axes_accuracy_certificate(s, d)
  cert_bracket(cert$se, max(cert_root_rel(dv)), "closed-form dyadic se")
  cert_bracket(cert$cval, abs(du), "closed-form dyadic cval")
  cert_bracket(cert$fiml_ratio, max(cert_root_rel((dv - dn) / (1 + dn))),
               "closed-form dyadic fiml_ratio")
})


test_that("the quotient's replay lands on hand-derived exact values where the shipped route is WRONG (closed-form oracle)", {
  # THE SECOND ORACLE TYPE FOR THE `fiml_ratio` FIELD (M113 AC3; IP3). Same
  # type as the test above -- one configuration priced by hand from the
  # definitions in R/axes_corrected_se.R, committed as literal fractions,
  # sharing no code, no library and no pipeline with the Python exact-rational
  # oracle or with the route under test -- but at a configuration the shipped
  # double route gets WRONG. The test above sits where every intermediate is
  # dyadic, so the shipped error is zero and the certificate can only be
  # asserted to report its floor; a second type that never meets an error
  # validates the field in letter and not in substance for a number printed to
  # users. Here the shipped quotient is off by about 1e-12 and the field has
  # something to catch.
  #
  # THE DERIVATION, by hand, for the whole family S = [[1, r], [r, r^2 + D]]
  # with the single derivative matrix M = [[0, 0], [0, 1]] (q = 1, and that one
  # matrix is also the one fitted component). Write s = r^2 + D, so det S = D:
  #
  #   S^-1  = (1/D) [[s, -r], [-r, 1]]
  #   X     = S^-1 M = (1/D) [[0, -r], [0, 1]]
  #   info  = 0.5*sum(X * t(X)) = 0.5*(2*(-r/D)*0 + (1/D)^2) = 1/(2 D^2)
  #   acov  = 2 D^2
  #   W     = 0.5 * S^-1 (M*acov) S^-1 = D^2 * X S^-1 = [[r^2, -r], [-r, 1]]
  #
  #   NAIVE arm -- W itself, no Jacobian substitution:
  #   W S   = [[0, r^3 - r s], [0, s - r^2]] = [[0, -r D], [0, D]]
  #   v_naive = 2*sum(WS * t(WS)) = 2*(0 + (-rD)*0 + 0*(-rD) + D^2) = 2 D^2
  #
  #   CORRECTED arm -- Wc is W with its diagonal replaced by
  #   -rowSums(Wc0 * S), Wc0 being W less its diagonal:
  #   Wc0 * S has off-diagonal -r*r, so rowSums = (-r^2, -r^2) and
  #   Wc    = [[r^2, -r], [-r, r^2]]
  #   Wc S  = [[0, -r D], [r(r^2 - 1), r^2 (s - 1)]]
  #   v = 2*sum(WcS * t(WcS)) = 2*(2*(-rD)*r(r^2 - 1) + r^4 (s-1)^2)
  #     = 2*(2 r^2 D (1 - r^2) + r^4 (s - 1)^2)
  #
  # AT r = 3/8 AND D = 13/2^20 -- so s = 9/64 + 13/2^20 = 147469/2^20, and
  # kappa(S) is about 1.0e5, inside the admitted domain:
  #
  #   v_naive = 2 * (13/2^20)^2 = 169/2^39
  #   2 r^2 D (1 - r^2) = 2*(9/64)*(13/2^20)*(55/64) = 12870/2^32
  #   s - 1 = -901107/2^20, and 901107^2 = 811993825449, so
  #   r^4 (s-1)^2 = (81/2^12)*(811993825449/2^40) = 65771499861369/2^52
  #   v = 12870/2^31 + 65771499861369/2^51
  #     = (12870*2^20 + 65771499861369)/2^51
  #     = (13495173120 + 65771499861369)/2^51 = 65784995034489/2^51
  #
  # Both numerators are under 2^53, so both exact values are exact DOUBLES --
  # which is what lets the replay be checked against them directly. Their
  # QUOTIENT is not: 65784995034489/692224 has a factor of 169 in its
  # denominator, and that is the point. The shipped route divides by an inexact
  # 1/D at every entry of S^-1 and lands off the truth.
  s_val <- 147469 / 2^20
  s <- rbind(c(1, 3 / 8), c(3 / 8, s_val))
  d <- list(mats = list(rbind(c(0, 0), c(0, 1))), components = "m",
            n_comp = 1L)
  v_exact <- 65784995034489 / 2^51
  v_naive_exact <- 169 / 2^39
  # Admitted: the certificate is not being asked about a matrix the criterion
  # would have refused outright.
  expect_null(axes_sigma_degenerate(s))

  # THE REPLAY, against the hand-derived truth. The dd route touches no LAPACK
  # and no BLAS -- it is R-level `+`, `-`, `*` and `/` on doubles throughout --
  # so this half is deterministic and platform-independent, and it is asserted
  # unconditionally. The low words are not zero here (the exact quotient is not
  # dyadic), so what is pinned is the value the route delivers.
  ref <- axes_dd_pricing(s, d)
  expect_identical(dd_to_double(ref$v), v_exact)
  expect_identical(dd_to_double(ref$v_naive), v_naive_exact)

  # THE SHIPPED ROUTE, against the same truth: it is wrong, and it is wrong on
  # the naive arm -- the quotient's DENOMINATOR, which no other field prices.
  q_exact <- v_exact / v_naive_exact
  hat <- axes_v_pricing(s, d)
  true_rel <- abs(sqrt(hat$corrected / hat$naive) - sqrt(q_exact)) /
    sqrt(q_exact)
  # THE SE FIELD, against THIS MACHINE's error on the corrected arm (M116).
  # An identity check of the `se` field against `cert_floor` stood here,
  # saying the machine running it prices the corrected arm exactly -- which
  # the authoring machine does not: it commits 1.19e-16 there (measured 2026-08-30, aarch64-apple-
  # darwin23), under the floor but not zero. The line passed anyway, because
  # reporting the floor is what the certificate does whenever its bound is
  # small; it never touched the arm it named. The bracket does, against
  # `v_exact` -- the hand-derived value committed above, the one exact
  # quantity this configuration has for that arm.
  cert <- axes_accuracy_certificate(s, d)
  floor_est <- cert_floor
  cert_bracket(cert$se, max(cert_root_rel(cert_rel(hat$corrected, v_exact, 0))),
               "closed-form quotient se")

  # `cval` gets NO assertion here, and that is the deliberate half. The hand
  # derivation above covers `v` and `v_naive` only -- there is no exact `u`
  # committed for this configuration -- so there is nothing to measure the
  # machine's cval error against, and the identity check of the `cval` field
  # against `cert_floor` that stood here asserted a zero it could not have
  # checked.
  # Pricing cval here needs its own hand derivation, which is its own
  # correctness surface; the five anchors and counterexample B already price
  # the field against exact values.

  # THE SHIPPED-ERROR HALF, AND WHY IT IS PLATFORM-DEPENDENT (M113 review; the
  # windows-latest red on CI run 33329301066). The replay half above needs no
  # platform agreement -- it is R-level `+`, `-`, `*` and `/` throughout -- but
  # the half below reads whatever error the shipped route commits here, and
  # that is a property of the machine's LAPACK, not of the configuration.
  # windows-latest prices this matrix EXACTLY (true_rel 0, against 5.6e-13 on
  # macOS and ubuntu), which is why the assertions cannot be written as a
  # bracket around a fixed error.
  #
  # M113 SKIPPED here, which left this half asserting nothing on the platform
  # that most needed watching. M115 replaces the skip with the same two-branch
  # bracket the anchors use: where the machine committed an error the field
  # brackets it, and where the machine was exact the floor is asserted against
  # the machine's own measured zero. Neither branch is empty, and a run in
  # which this test asserts nothing is no longer reachable.
  cert_bracket(cert$fiml_ratio, true_rel, "fiml_ratio")

  # ... and what the case DISCRIMINATES, on each branch. Where there is an
  # error, the estimate this fit gets comes from the new field alone: before
  # M113 this configuration was certified at 4.4e-15 with a quotient wrong by
  # 5.6e-13. Where there is none, the three fields must agree at the floor --
  # a worst-of that disagreed with its own inputs would be the drift
  # axes_certificate_worst() exists to prevent, and it is asserted on both
  # branches rather than only on the one this machine happens to take.
  if (identical(cert$fiml_ratio, cert_floor)) {
    expect_identical(axes_certificate_worst(cert), cert_floor)
  } else {
    expect_gt(cert$fiml_ratio, floor_est)
    expect_identical(axes_certificate_worst(cert), cert$fiml_ratio)
  }
})


test_that("AC6: a condition inside the certificate refuses at both surfaces, never errors", {
  # The certificate is consulted from inside two helpers whose contract is to
  # REFUSE -- a named reason, NA numbers, exactly one warning -- on a matrix
  # they cannot price. It is a large arithmetic surface (a hand-rolled inverse,
  # a compensated arithmetic, and the shipped pricing it replays), and until
  # M113 it was called bare: a condition raised anywhere inside it propagated
  # out as an error neither caller has a handler for. The fence turns that into
  # the sentinel, which refuses (the sentinel is four decades past the accuracy
  # target).
  #
  # TWO ROUTES, run one at a time, because they take different paths through
  # the fence: a `stop()` reaches the tryCatch handler, while a route failure
  # returns the sentinel normally and never raises at all. Both must land on
  # the same refusal at both surfaces, or the two surfaces could disagree about
  # whether this fit is certified.
  r <- m106_family_b(7e-7)
  nm9 <- paste0("i", 1:9)
  ang9 <- c(as.numeric(octants()), as.numeric(octants())[[1L]])
  sid9 <- as.character(c(1:8, 1L))
  dimnames(r) <- list(nm9, nm9)
  expect_identical(axes_sigma_degenerate(r), "ill_conditioned")

  both_surfaces_refuse <- function(lbl) {
    w_se <- testthat::capture_warnings(
      got <- axes_corrected_se(r, nm9, ang9, sid9, n = 600,
                               fit_zeta1 = TRUE, fit_zeta2 = FALSE)
    )
    expect_identical(got$reason, "uncertified", label = lbl)
    expect_true(all(is.na(got$corrected)), label = lbl)
    expect_length(w_se, 1L)

    w_sf <- testthat::capture_warnings(
      gsf <- axes_scaling_factor(r, nm9, ang9, sid9, fit_zeta1 = TRUE,
                                 fit_zeta2 = FALSE, df = 33L,
                                 baseline_df = 36L)
    )
    expect_identical(gsf$reason, "uncertified", label = lbl)
    expect_length(w_sf, 1L)
  }

  # ROUTE 1 -- a raised error from inside the certificate's own body.
  local({
    testthat::local_mocked_bindings(
      axes_accuracy_certificate = function(sigma, d) {
        stop("planted condition from inside the certificate", call. = FALSE)
      },
      .package = "circumplex"
    )
    both_surfaces_refuse("stop()")
  })

  # ROUTE 2 -- a non-error route failure: the self-test reports that this
  # machine's arithmetic defeats the error-free transforms, so the certificate
  # returns its sentinel without raising anything at all.
  local({
    testthat::local_mocked_bindings(
      axes_dd_selftest = function() FALSE,
      .package = "circumplex"
    )
    both_surfaces_refuse("route failure")
  })
})
