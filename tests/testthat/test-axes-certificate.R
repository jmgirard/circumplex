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
#     route can be exact there -- on every machine measured so far it is, and
#     the certificate lands on its floor (nothing pins it there; M116); the
#     second (M113) sits at a configuration whose shipped route is wrong by
#     about 1e-12 on the quotient, so the `fiml_ratio` field is checked where
#     there is an error to catch and not only at its floor.
#
# A third layer ships alongside and is deliberately NOT counted as a type: the
# planted-perturbation invariants below assert the comparison's sensitivity
# with no external truth at all.
#
# THE RULE THIS FILE IS HELD TO (M122; D-055). Every assertion here is one of
# three things:
#
#   (a) a property of a committed matrix or of the exact oracle;
#   (b) this machine's own measurement, bracketed by a machine-independent
#       bound;
#   (c) an exhaustive disposition -- every outcome the shipped route can take
#       at that matrix is enumerated, and each branch asserts.
#
# "The shipped route does X here", with no branch for it doing otherwise, is a
# FROZEN MEASUREMENT and is disallowed, however many machines have been seen
# doing X. Five escalations to outside review returned the same defect in a
# different disguise each time -- a measured error, a bit pattern, a decade
# window, and finally the premise that one committed matrix always prices,
# which cost the 2.0.1 release its second pre-test rejection, at the third
# platform-exact failure site this package has hit. This rule is what
# ends the series, and it is checked at review rather than by any assertion.


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
# error ratios ran 9.83 to 10.0025 against the ceiling of that day, 1e3 --
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
#
# THE DENOMINATOR IS REFUSED WHERE IT IS ZERO (M122, from the Known-fragilities
# list). A relative error against an exact zero is not defined, and what this
# expression returns there is Inf, -Inf or NaN depending on the sign of a
# numerator that is itself a rounding artifact. Every quantity committed in
# this file is nonzero, so nothing reaches it today; the guard is here because
# the assertion added at counterexample B divides by a committed value on a
# route where no shipped number exists to sanity-check the result, and a
# silent NaN there passes an `expect_lt()` as NA rather than reddening.
cert_rel <- function(hat, hi, lo) {
  if (any(hi + lo == 0)) {
    stop("cert_rel(): the exact value is zero, so there is no relative error ",
         "to form -- this quantity needs an absolute-error comparison",
         call. = FALSE)
  }
  ((hat - hi) - lo) / (hi + lo)
}

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


# Both constants above are written against a package safety factor of 10, and
# neither reads it -- deliberately, for the reason each comment gives. That
# leaves the harness free to drift away from the package: move
# axes_certificate_safety_factor and cert_floor keeps certifying the old floor
# while cert_ceiling keeps allowing the old overstatement, both describing a
# certificate that no longer exists. This is the one assertion that ties the
# two together, and the only one that names the cause when they part (M118).
#
# The drift is not silent today, in either direction -- measured 2026-08-31 on
# aarch64-apple-darwin23 by setting the package constant and re-running this
# file. At 100 the per-case brackets and both closed-form oracle tests go red
# as well (8 failures); at 2 the planted-perturbation test does (9 failures).
# What every one of those reports is a bracket or an oracle missing its
# expected value, with the moved constant nowhere in the message; this
# assertion is what turns that into "the safety factor is not 10". It is also
# the layer that survives a future site being added whose own bracket happens
# to absorb the move.
test_that("AC3: the harness's written-down safety factor is the package's", {
  expect_identical(axes_certificate_safety_factor, 10)
})


# ---- WHAT ACTUALLY GOT PRICED (M118) ---------------------------------------
#
# Every bracket assertion in this file is reached through cert_true_error(),
# and that function SKIPS a case whose anchor matrix this machine does not
# build bit for bit. The per-case tests below are one test_that() each so that
# those skips are independent -- but a machine on which all six skip runs this
# file green with not one bracket asserted, and nothing in the file says so.
# Until now the only detector was a human reading the pull request's CI log
# for skip counts.
#
# So each case records what became of it, on every path, and a test after the
# per-case tests reads the record back. The environment is file-local and
# written from INSIDE cert_true_error(): a case that skips leaves its reason
# here before skip() unwinds its test_that(), and a case that is priced says
# so only after every value it prices has been computed.
cert_dispositions <- new.env(parent = emptyenv())

# THE VOCABULARY IS PINNED (M122). Until now a disposition was whatever string
# the recording site happened to write, and the detector picked one out with
# `== "priced"`: a typo at either end makes that comparison quietly false, and
# a run in which nothing was priced then reports the same green as a run in
# which everything was. These four constants are the whole vocabulary, and
# cert_record() refuses anything outside it -- a typo is red with a reason
# instead of green on nothing.
#
# The DETAIL is carried separately from the disposition for the same reason:
# a skip reason and a refusal literal are prose, and folding them into the
# disposition string is what made the set unpinnable.
cert_disp <- c(
  priced   = "priced",
  refused  = "refused -- unidentified",
  skipped  = "skipped",
  mismatch = "matrix mismatch"
)

cert_record <- function(id, disposition, detail = "") {
  if (!isTRUE(disposition %in% cert_disp)) {
    stop("cert_record(): '", disposition, "' is not one of the pinned ",
         "dispositions (", paste(cert_disp, collapse = "; "), ")",
         call. = FALSE)
  }
  assign(id, list(disposition = disposition, detail = detail),
         envir = cert_dispositions)
  invisible(disposition)
}

cert_disposition <- function(id) {
  if (exists(id, envir = cert_dispositions, inherits = FALSE)) {
    get(id, envir = cert_dispositions, inherits = FALSE)$disposition
  } else {
    "never reached"
  }
}

cert_detail <- function(id) {
  if (exists(id, envir = cert_dispositions, inherits = FALSE)) {
    get(id, envir = cert_dispositions, inherits = FALSE)$detail
  } else {
    ""
  }
}


# ---- WHAT EACH CASE'S MATRIX IS, AND WHERE A REFUSAL IS ADMITTED (M122) -----
#
# Two facts per case, both committed, both read by cert_true_error() in place
# of a case name. Naming a case in an `if` is how an admission written for one
# matrix quietly widens to another; naming a PROPERTY means widening it is a
# visible edit to this table.
#
# ORIGIN -- how the matrix reaches the comparison. The five anchors are built
# here from `cos()` at octant differences, so a machine whose libm rounds one
# of those differently builds a different matrix and has no yardstick: that is
# a skip. Counterexample B is read from committed BYTES (a fixture), so a
# mismatch there is not a platform difference at all -- it means the fixture or
# the frozen block moved -- and the honest verdict is a failure. Measured
# 2026-09-05: of the eight distinct cosines the anchor builders use, exactly
# one sits within 0.05 of a unit in the last place of a rounding boundary
# (cos(3.9269908169872414) = -0.7071067811865477, margin 0.0396 ulp), and it
# moves four of the five anchors; the four-variable case c4 moves only on
# cos(0) and cos(pi), both a full half ulp from any boundary.
#
# RCOND_BAND -- the interval the information matrix's reciprocal condition
# estimate occupies at this case, under one-ulp perturbation of the matrix.
# Where that band CONTAINS .Machine$double.eps, `solve(info)`'s success is a
# property of the platform's LU roundoff rather than of the matrix, and a
# refusal is therefore admitted. Where it does not -- every anchor, whose
# rcond sits decades above eps -- a refusal stays a regression.
#
# The band committed for cxb was measured by RR22 over 300 one-ulp neighbours
# of the fixture (min 1.667e-16, median 2.41e-16, max 3.35e-16), and BOTH ends
# have since been straddled by real platforms rather than by perturbation
# alone: 2.6008e-16 on aarch64-apple-darwin23 with reference BLAS, which
# prices, and 2.0494e-16 on aarch64-unknown-linux-gnu with OpenBLAS 0.3.33,
# which refuses (both measured 2026-09-05, the second in tools/arm64's
# container). eps is 2.220446e-16, between the two.
cert_admission <- list(
  a4  = list(origin = "cos-built",       rcond_band = NULL),
  a5  = list(origin = "cos-built",       rcond_band = NULL),
  c4  = list(origin = "cos-built",       rcond_band = NULL),
  b9a = list(origin = "cos-built",       rcond_band = NULL),
  b9b = list(origin = "cos-built",       rcond_band = NULL),
  cxb = list(origin = "committed-bytes", rcond_band = c(1.667e-16, 3.35e-16))
)

# A matrix this machine builds itself can differ from the committed one
# innocently; committed bytes cannot.
cert_matrix_is_built <- function(id) {
  identical(cert_admission[[id]]$origin, "cos-built")
}

# The admission, read off the committed band rather than off the case name:
# a refusal is admitted exactly where the band straddles this machine's eps.
cert_refusal_admitted <- function(id) {
  band <- cert_admission[[id]]$rcond_band
  !is.null(band) &&
    band[[1L]] <= .Machine$double.eps &&
    .Machine$double.eps <= band[[2L]]
}

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
    reason <- paste0(
      "this machine does not build the matrix at case '", id, "' bit for ",
      "bit, so the exact quadratic forms committed for that matrix are not a ",
      "yardstick for this one"
    )
    # WHICH VERDICT depends on where the matrix came from, not on which case
    # this is (M122). A cos()-built anchor can differ innocently -- a libm one
    # ulp away at one octant difference -- and the yardstick is then simply
    # absent, which is a skip. A matrix read from committed bytes cannot
    # differ innocently: a mismatch there means the fixture or the frozen
    # block moved, and calling that a platform difference would hide the one
    # edit this comparison exists to catch.
    #
    # RECORDED BEFORE skip(), which unwinds this case's test_that() and would
    # otherwise leave the case looking like one that was never reached.
    if (cert_matrix_is_built(id)) {
      cert_record(id, cert_disp[["skipped"]], reason)
      testthat::skip(reason)
    }
    cert_record(id, cert_disp[["mismatch"]], reason)
    testthat::fail(paste0(
      "the matrix at case '", id, "' is read from committed bytes and no ",
      "longer matches the exact values committed beside it -- ", reason
    ))
    return(NULL)
  }
  v <- axes_v_pricing(sigma, d)
  u <- axes_u_pricing(sigma, d)
  # A REFUSAL from the shipped pricing IS A ROUTE, not a failure to reproduce
  # -- but only where the case's committed conditioning band says the platform
  # decides it (M122; D-055). At the five anchors that band is absent: their
  # information matrices sit decades clear of eps, they price on every platform
  # measured, and a refusal there is a regression in axes_pricing_core() which
  # this fail() is what catches. At counterexample B the band straddles eps,
  # measured on two real platforms in both directions, so a refusal is one of
  # the two outcomes the matrix admits and the caller asserts it exhaustively.
  #
  # Either way NULL comes back, and the recorded disposition is what tells the
  # caller which of the two it was; the certificate returns its sentinel at a
  # refusal, which reddens a bracket applied to it.
  if (is.character(v) || is.character(u)) {
    literals <- paste(Filter(is.character, list(v, u)), collapse = ", ")
    if (!cert_refusal_admitted(id)) {
      cert_record(id, cert_disp[["refused"]], literals)
      testthat::fail(paste0(
        "the shipped pricing REFUSES at case '", id, "' (", literals,
        ") -- no conditioning band is committed for this case, so this is a ",
        "regression, not a platform difference"
      ))
      return(NULL)
    }
    # THE REFUSAL'S IDENTITY, asserted here rather than left to the caller:
    # the admission is for the LU gate giving up on a matrix whose condition
    # straddles eps, which surfaces as "unidentified" from the acov inversion.
    # "singular" would mean solve(sigma) itself failed -- rcond(sigma) is
    # 1.39e-7 at B, five decades inside double range, so that is a regression;
    # "indefinite" lives downstream of both functions called here, so it would
    # mean a wiring change. Either fails, and so does a refusal from only one
    # of the two.
    expect_identical(v, "unidentified", label = paste0(id, " v pricing"))
    expect_identical(u, "unidentified", label = paste0(id, " u pricing"))
    cert_record(id, cert_disp[["refused"]], literals)
    return(NULL)
  }
  dv <- cert_rel(v$corrected, as.numeric(fz$v_hi), as.numeric(fz$v_lo))
  dn <- cert_rel(v$naive, as.numeric(fz$vn_hi), as.numeric(fz$vn_lo))
  du <- cert_rel(u, as.numeric(fz$u_hi), as.numeric(fz$u_lo))
  # AFTER the pricing, not before: what is claimed is that this case was
  # measured against its committed exact values, not that it was attempted.
  cert_record(id, cert_disp[["priced"]])
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
# `at_floor` SAYS WHICH BRANCH IS TAKEN, and says it as a comparison rather
# than as a value coincidence (M122, from the Known-fragilities list). What
# stood here was `identical(est, cert_floor)`: bit-equality with a constant
# the harness writes down and the package computes separately, so the branch
# turned on two numbers happening to agree. Below the floor -- reachable by
# lowering the package's safety factor -- that test is FALSE and the two-sided
# bracket ran against an estimate the certificate cannot actually produce,
# which is the wrong report. The floor is the smallest value the certificate
# emits, so "at or below it" is the condition meant all along; it is an
# argument so a caller that knows which branch it expects can say so and have
# the other one fail.
cert_bracket <- function(est, true_rel, lbl, at_floor = est <= cert_floor) {
  if (at_floor) {
    expect_lte(true_rel, cert_floor, label = paste0(lbl, ": true error"))
  } else {
    expect_gte(est, true_rel, label = paste0(lbl, ": estimate"))
    # The upper end never sits below the certificate's own floor: every field
    # is fac * max(delta, 2 * eps) -- delta halved first for the two
    # root-converted fields -- so the floor is the smallest value the
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
  skip_on_cran()
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


test_that("AC2/AC3: counterexample B is refused on every route, and bracketed where it prices", {
  # The one committed matrix on which the shipped corrected SEs are measurably
  # wrong (3.413e-02 on the machine that froze the figures) while the pre-M89
  # criterion reported them with reason NULL, and on which the
  # double-precision cval comes out sign-flipped (exact +0.0555, double
  # -0.216). Frozen from the same oracle run. Provenance of the fixture itself
  # is at its first read site in test-axes-scaled-fit.R.
  #
  # WHAT THIS TEST CLAIMS CHANGED AT M122 (D-055). It used to claim "B prices,
  # and the estimate brackets an error of about 3.4%". That is a fact about
  # one machine: `solve(info)`'s outcome here is decided by the platform's LU
  # roundoff -- rcond(info) is 2.6008e-16 on macOS/arm64 with reference BLAS
  # and 2.0494e-16 on linux-arm64 with OpenBLAS, straddling eps = 2.220446e-16
  # -- so the shipped pricing prices on the first and refuses on the second,
  # and CRAN's linux-arm64 pre-test rejected 2.0.1 on exactly the fail() this
  # file used to raise there. The claim is now: B is refused `uncertified` on
  # EVERY route, the route taken is one of the two the committed conditioning
  # band admits, and where a value exists the certificate brackets it. Both
  # branches assert; neither can be empty.
  fx <- readRDS(test_path("fixtures", "rb18-counterexample-b.rds"))
  d <- axes_se_derivs(fx$ia, c("A", "B", "C"), NULL, FALSE, FALSE)

  # ---- outside both routes -------------------------------------------------
  #
  # Outside the precondition, for the same reason kappa is outside it at the
  # five anchors: where the checks below divide, this test would otherwise
  # assert nothing at all -- not even that the fixture is still the matrix the
  # frozen figures were measured on. The literal is committed here rather than
  # read from the fixture's own `kappa` field, which would be blind in the
  # dimension it derives.
  expect_equal(m106_kappa(fx$S), 6654372.506, tolerance = 1e-6)
  # ... and that B still reaches the certificate's limb rather than one of the
  # two literals that refuse without consulting it. Asserted here as well as
  # in the refusal suite, which CRAN skips.
  expect_identical(axes_sigma_degenerate(fx$S), "ill_conditioned")

  true_rel <- cert_true_error("cxb", fx$S, d)
  cert <- axes_accuracy_certificate(fx$S, d)

  # THE EXACT-RATIONAL ORACLE'S REACH AT B, ON BOTH ROUTES (M122). The bracket
  # has two halves -- the shipped route's error against exact truth, and the
  # certificate's estimate against that error -- and on the refusing route the
  # first half is empty, which is the whole five-line loss the old shape took.
  # The certificate's REFERENCE route is not: axes_dd_pricing() is R-level
  # `+`, `-`, `*` and `/` on doubles throughout, touching neither BLAS nor
  # LAPACK, and it computes at B on every route (measured at all 97 refusing
  # neighbours, RR22). Its agreement with the committed exact values is the
  # yardstick the certificate would have used, and it needs no shipped value.
  #
  # The comment at the head of this file records a decision NOT to pin the dd
  # route. That decision is about using it as a PRECONDITION -- a gate that
  # skips, which is how a planted defect in the route once hid -- and not
  # about asserting it against truth derived elsewhere; the two closed-form
  # oracle tests below already do exactly that.
  #
  # TWO BOUNDS, because the two quantities fail differently.
  #
  # `v` and `v_naive`: the bound is HALF a unit in the last place of the exact
  # value, which is to say the reference route must deliver that value's
  # correctly rounded double. Measured 2026-09-05 by `dd_ulp()` below,
  # bit-identical on aarch64-apple-darwin23 and on
  # aarch64-unknown-linux-gnu/OpenBLAS: 0.135 and 0.045 ulp for `v`, 0.387 and
  # 0.234 for `v_naive`.
  #
  # `u`: at B it is a difference of two quantities of size about one that
  # comes out 0.0555, so a rounding of either operand is amplified by about
  # eighteen in RELATIVE terms while staying the same size in ABSOLUTE ones.
  # The bound is therefore absolute -- one ulp of the operands' own scale --
  # and the measurement is 3.76e-17 against a bound of 1.11e-16, again
  # identical on both platforms. Stated as a relative bound this would be
  # 6.1 ulp, a figure with no derivation behind it.
  fz <- cert_frozen$cxb
  ref <- axes_dd_pricing(fx$S, d)
  # ONE UNIT IN THE LAST PLACE OF `hi`, which is what "ulp" has to mean here.
  # This divided the RELATIVE error by 2^-53 until the M122 review (finding 1),
  # which is an ulp count only where the mantissa is exactly 2: across the four
  # quantities committed at B one such unit is 1.085 to 1.279 true ulp, so a
  # bound written as half an ulp was really demanding 0.391 to 0.461 of one.
  # Taken against ulp(hi) directly, the bound means what it says.
  dd_ulp <- function(hat, hi, lo) {
    if (any(hi == 0)) {
      stop("dd_ulp(): the exact value is zero, so it has no last place",
           call. = FALSE)
    }
    abs((hat - hi) - lo) / 2^(floor(log2(abs(hi))) - 52)
  }
  expect_lt(max(dd_ulp(dd_to_double(ref$v),
                       as.numeric(fz$v_hi), as.numeric(fz$v_lo))), 0.5,
            label = "cxb dd-vs-exact v (ulp)")
  expect_lt(max(dd_ulp(dd_to_double(ref$v_naive),
                       as.numeric(fz$vn_hi), as.numeric(fz$vn_lo))), 0.5,
            label = "cxb dd-vs-exact v_naive (ulp)")
  expect_lt(abs((dd_to_double(ref$u) - as.numeric(fz$u_hi)) -
                  as.numeric(fz$u_lo)), 2^-53,
            label = "cxb dd-vs-exact u (absolute)")

  # No machine gets within the accuracy target on this matrix. On the refusing
  # route the certificate says so with its sentinel (four decades past the
  # target); on the priced route with a graded estimate. Either way the
  # worst-of is what the refusal predicate reads, so this is the claim that
  # holds on both.
  expect_gt(axes_certificate_worst(cert), axes_degeneracy_delta_star)

  # ---- the two admitted routes --------------------------------------------
  #
  # Branched on the RECORDED disposition rather than on `is.null(true_rel)`:
  # a matrix mismatch also returns NULL, and it must not be mistaken for a
  # refusal and have the refusal's assertions run against a priced value.
  disp <- cert_disposition("cxb")

  if (identical(disp, cert_disp[["refused"]])) {
    # THE REFUSING ROUTE. cert_true_error() has already asserted the refusal's
    # identity ("unidentified" from both `v` and `u`). What is left is the
    # contract that follows from it: the certificate degrades to its sentinel
    # -- D-051's promise, asserted here for the first time at a matrix that
    # reaches it naturally rather than through a planted duplicate derivative
    # -- and the predicate users actually depend on still refuses.
    expect_identical(cert, list(se = 1, cval = 1, fiml_ratio = 1))
    expect_identical(axes_degeneracy_refusal(fx$S, d)$reason, "uncertified")

  } else if (identical(disp, cert_disp[["priced"]])) {
    # THE PRICED ROUTE, unchanged: the three brackets, and the wrongness
    # itself asserted rather than described.
    cert_bracket(cert$se, true_rel$se, "cxb se")
    cert_bracket(cert$cval, true_rel$cval, "cxb cval")
    cert_bracket(cert$fiml_ratio, true_rel$ratio, "cxb fiml_ratio")

    # Asserted against delta_star -- the package's own target -- and NOT as a
    # window around a measured figure. A first draft here wrote decades taken
    # from the authoring machine (SE error in (1e-2, 1e-1), cval error above
    # 1), and both reddened on ubuntu-latest, which prices this matrix through
    # a different BLAS and measures 0.124 and 0.42 where macOS measures 0.0341
    # and 4.890. At an ill-conditioned matrix the size of the rounding error
    # IS a property of the machine; what is a property of the MATRIX is that
    # no machine gets within the target on it.
    expect_gt(true_rel$se, axes_degeneracy_delta_star)
    expect_gt(true_rel$cval, axes_degeneracy_delta_star)

  } else {
    # Neither route: cert_true_error() has already failed (the matrix no
    # longer matches its committed bytes). Say which state this was, so the
    # report names it rather than leaving a test that asserted only the
    # outside-both checks.
    testthat::fail(paste0(
      "counterexample B took neither admitted route -- disposition '", disp,
      "' (", cert_detail("cxb"), ")"
    ))
  }
})


test_that("AC1: every case reached an admitted disposition, and the anchors were priced", {
  # THE DETECTOR (M118, rewritten at M122). Every bracket assertion above runs
  # only for a case cert_true_error() actually priced; a case whose anchor
  # matrix this machine builds differently skips instead, and each skip
  # abandons only its own test_that(). So a machine on which all of them skip
  # reports skips and zero failures -- a green file with nothing measured
  # against the exact values it commits. That state is what this test turns
  # red, and since M122 it also turns red on the state the counterexample-B
  # fix creates: every case REFUSING, which the old "at least one priced"
  # clause would have caught only by accident.
  #
  # It reads the dispositions the cases recorded as they ran, rather than
  # re-pricing anything: re-pricing here would be a second copy of the
  # precondition, and would skip in exactly the runs this test exists to
  # catch.
  #
  # The case list is pinned at six -- five anchors plus counterexample B --
  # because this test's own domain is a list that can empty: with no cases
  # enumerated there would be nothing to count and no priced case to want.
  anchors <- vapply(cert_anchors(), `[[`, "", "id")
  ids <- c(anchors, "cxb")
  expect_length(ids, 6L)

  dispositions <- vapply(ids, cert_disposition, "")
  table_line <- paste0(ids, " = ", dispositions, collapse = "; ")

  # THE TABLE IS EMITTED ON EVERY RUN, green ones included (M122). Until now
  # the only way to learn which cases were actually measured was a human
  # reading a CI log for skip counts, and a green arm64 or CRAN log said
  # nothing at all. `info` prints on failure only, so the table goes out
  # through a message, which testthat's reporters carry.
  message("certificate cases: ", table_line)

  # EVERY DISPOSITION IS ONE OF THE PINNED FOUR. A case that never ran records
  # nothing and reads back "never reached", which is outside the set and fails
  # here -- as does a typo at any recording site.
  for (id in ids) {
    expect_true(cert_disposition(id) %in% cert_disp,
                label = paste0(id, " disposition '", cert_disposition(id),
                               "' (", table_line, ")"))
  }

  # COUNTEREXAMPLE B took one of its two admitted routes. It is read from
  # committed bytes, so "skipped" is not available to it and a mismatch is a
  # failure; this is the clause that notices if that ever stops being true.
  expect_true(cert_disposition("cxb") %in%
                cert_disp[c("priced", "refused")],
              label = paste0("cxb disposition (", table_line, ")"))

  # AT LEAST ONE ANCHOR WAS PRICED -- the clause that fails on the all-skip
  # run, and the one clause here with CRAN exposure, since the five anchors
  # are built from cos() and a libm one ulp away at an octant difference
  # builds a different matrix.
  #
  # KEPT CRAN-LIVE, on measurement (2026-09-05). Of the eight distinct cosines
  # the builders use, exactly one sits within 0.05 ulp of a rounding boundary
  # (cos(3.9269908169872414), margin 0.0396 ulp), and it moves four of the
  # five anchors. The fifth, c4, moves only on cos(0) and cos(pi), each a full
  # half ulp from any boundary and at an extremum where cos is flat. All five
  # skipping together therefore takes a libm wrong at both sites. CRAN's
  # r-release-macos-x86_64 log for 2.0.0 -- the flavor whose cos(225 degrees)
  # rounds differently -- reports no anchor-matrix skip, and all five priced
  # on linux-arm64/OpenBLAS in tools/arm64's container.
  priced <- anchors[dispositions[anchors] == cert_disp[["priced"]]]
  expect_gt(length(priced), 0L,
            label = paste0("anchors priced (", table_line, ")"))
})


test_that("AC5: the disposition vocabulary is closed", {
  # cert_record() is the only writer, and a string outside the pinned four
  # must not reach the environment at all -- otherwise the detector's
  # membership check above is reading a value the recorder already accepted,
  # and the two would have to agree by convention rather than by construction.
  expect_error(cert_record("probe", "priced!"), "pinned dispositions")
  expect_identical(cert_disposition("probe"), "never reached")
  expect_length(cert_disp, 4L)
  expect_identical(sort(names(cert_disp)),
                   c("mismatch", "priced", "refused", "skipped"))
})


test_that("AC7: the two harness helpers select their branches on a stated condition", {
  # cert_bracket() and cert_rel() are the two helpers M122 repaired, and both
  # repairs are invisible from the cases above -- the branch the anchors take
  # and the denominators they divide by are unchanged by either. So each is
  # probed directly, with the defect its repair removes.
  #
  # cert_bracket(): the floor branch used to be chosen by bit-equality with
  # `cert_floor`, so an estimate BELOW the floor took the two-sided branch
  # instead. The probe is exactly that value.
  #
  # WHAT THIS PROBE DOES AND DOES NOT SHOW (M122 review, finding 3). It shows
  # the floor branch running and reddening on an under-report. It does NOT
  # discriminate the repair, and no probe can: below the floor the old branch
  # failed iff `true_rel > est` and the new one fails iff
  # `true_rel > cert_floor`, so the new failure set is a strict SUBSET of the
  # old one -- the change is more lenient there, not stricter. What the repair
  # buys is a truthful report rather than a wider one: `identical()` made the
  # branch turn on two numbers happening to agree, and ran the two-sided
  # bracket against an estimate the certificate cannot emit. The argument
  # probes below are what discriminate the new signature.
  #
  # Probed through the CONDITION each branch signals rather than through
  # expect_success()/expect_failure(), which count expectations: the two-sided
  # branch fires two (both halves of the bracket) and the floor branch one, so
  # a count-based helper reports the branch's arity instead of its verdict.
  bracket_reddens <- function(...) {
    expect_condition(cert_bracket(...), class = "expectation_failure")
  }
  bracket_passes <- function(...) {
    expect_no_condition(cert_bracket(...), class = "expectation_failure")
  }
  below_floor <- cert_floor / 2
  bracket_reddens(below_floor, cert_floor * 4, "probe below floor")
  # ... and the argument overrides the default in both directions, so a caller
  # that knows which branch it wants gets that branch and not another.
  bracket_passes(1e-3, 1e-5, "probe two-sided", at_floor = FALSE)
  bracket_reddens(1e-3, 1e-5, "probe forced floor", at_floor = TRUE)

  # cert_rel(): a zero exact value has no relative error, and the expression
  # returned NaN or an infinity there rather than saying so.
  expect_error(cert_rel(1e-16, 0, 0), "the exact value is zero")
  expect_error(cert_rel(c(1, 2), c(1, 0), c(0, 0)), "the exact value is zero")
  # ... and an ordinary quantity still divides.
  expect_equal(cert_rel(2 + 2^-51, 2, 0), 2^-52)
})


test_that("AC3: the estimate discriminates the reachable cases from counterexample B", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  hat_u <- axes_u_pricing(s, d)

  # THE SHIPPED VALUES THEMSELVES, pinned (M118). The brackets below judge
  # this machine's MEASURED error, so they pass wherever the certificate
  # prices that error correctly -- including where the shipped route has
  # regressed and the certificate honestly reports how wrong it now is. With
  # only the brackets here, a shipped pricing regression at this configuration
  # is green across the whole file.
  #
  # What stood here before M116 was three expect_identical() checks against
  # these same fractions. They were deleted because bit-identity pinned every
  # measured error to exactly zero and so collapsed both bracket branches into
  # themselves. A TOLERANCE avoids that collapse -- the brackets below still
  # run on their own measured error -- while a real regression, which moves
  # these quantities by far more than a few units in the last place, reddens
  # here.
  #
  # The tolerance is WRITTEN DOWN as `4 * 2^-53` (two units in the last place
  # at 1.0) and deliberately NOT read from cert_floor or from
  # axes_certificate_safety_factor: an expectation defined by the harness
  # constant it sits beside weakens whenever that constant is raised, which is
  # the failure M115 AC4 recorded for the floor itself.
  #
  # It is NOT, however, as wide as the brackets below. Measured 2026-08-31 on
  # aarch64-apple-darwin23: this machine's shipped route is exact here (all
  # three relative errors 0), so cert$se, cert$cval and cert$fiml_ratio are
  # each identical() to cert_floor and all three cert_bracket() calls take the
  # at-the-floor branch, whose live assertion is `true_rel <= cert_floor` =
  # 4.4408921e-15. These pins redden at 4.4408921e-16 -- ten times tighter. A
  # platform whose shipped route differs here by three to twenty ulp would
  # pass the bracket, which prices that difference correctly, and fail these
  # pins. Only this machine has been measured at this configuration; the size
  # of the headroom these pins should carry is open (M118 review, [O] F1).
  expect_equal(hat$corrected, 97 / 128, tolerance = 4 * 2^-53)
  expect_equal(hat$naive, 2, tolerance = 4 * 2^-53)
  expect_equal(hat_u, 5 / 8, tolerance = 4 * 2^-53)

  dv <- cert_rel(hat$corrected, 97 / 128, 0)
  dn <- cert_rel(hat$naive, 2, 0)
  du <- cert_rel(hat_u, 5 / 8, 0)
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
