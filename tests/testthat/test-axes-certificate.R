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
#   FROZEN -- the exact-rational oracle. devel/degeneracy-oracle/exact_oracle.py
#     prices this model in Python `fractions` (exact integer arithmetic, no
#     floating point anywhere), driven by devel/degeneracy-oracle/exact_oracle.R,
#     which is the committed generator for every EXACT_* figure pinned below and
#     reproduces them from committed material alone. It cannot run in this suite
#     (python3 is not a package dependency and devel/ does not ship), so its
#     measurements are frozen here as literals, each tagged with the case that
#     produced it. Regenerate with, from the repo root:
#       Rscript devel/degeneracy-oracle/exact_oracle.R
#     Values below are that script's output on 2026-08-24, extended on
#     2026-08-30 with the `fiml_ratio` field's measured error at each case
#     (M113); it prints the true relative error and the certificate side by
#     side and fails if any of the eighteen ratios leaves [1, its field's
#     ceiling]. Each frozen error is a measurement of ONE
#     matrix priced on ONE machine, so every assertion that reads one sits
#     behind the bit-identity precondition below -- see the block at
#     `cert_frozen` for what that pins and why (M108 AC2, amended).
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
         ang = as.numeric(octants()),
         kappa = 1.000e4, se = 5.889e-14, cval = 2.096e-14,
         ratio = 5.964e-14),
    list(id = "a5", lbl = "family A, p = 8, kappa 1e5",
         r = m106_family_a(2.4e-5, 1L), scale = as.character(1:8),
         ang = as.numeric(octants()),
         kappa = 1.000e5, se = 3.004e-12, cval = 2.205e-13,
         ratio = 1.938e-12),
    list(id = "c4", lbl = "family C, p = 4 minimum",
         r = m106_family_c(1.2e-5), scale = as.character(1:4),
         ang = c(90, 180, 270, 360),
         kappa = 1.000e5, se = 6.459e-13, cval = 1.124e-08,
         ratio = 5.681e-13),
    list(id = "b9a", lbl = "near-duplicate r = .9999",
         r = m106_family_b(7e-5), scale = as.character(c(1:8, 1L)),
         ang = c(as.numeric(octants()), as.numeric(octants())[[1L]]),
         kappa = 2.874e4, se = 6.302e-13, cval = 7.245e-14,
         ratio = 1.069e-12),
    list(id = "b9b", lbl = "near-duplicate r = .99999",
         r = m106_family_b(7e-6), scale = as.character(c(1:8, 1L)),
         ang = c(as.numeric(octants()), as.numeric(octants())[[1L]]),
         kappa = 2.874e5, se = 1.126e-11, cval = 1.488e-12,
         ratio = 6.682e-12)
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


# ---- the bit-identity precondition (M108 AC2, amended) ----------------------
#
# A frozen relative error describes ONE matrix priced on ONE machine. Neither
# half of it is bit-portable: the anchor matrices are built through `cos()`,
# and the shipped pricing runs through `solve()` and `%*%`. The branch measured
# both halves moving -- windows-latest read 2e-12 against this file's 3.004e-12
# floor at the p = 8 / kappa 1e5 anchor (CI run 32752082137), and on one machine
# a 1-ulp perturbation of that anchor moves the estimate by two decades. So the
# bracket below is asserted only where the running machine reproduces, EXACTLY,
# the numbers the frozen figures were measured against. Where it does not, the
# case skips naming that reason: the certificate is not wrong there, the frozen
# yardstick simply is not that machine's yardstick.
#
# Three things are pinned per case, all as `%a` hex, which round-trips through
# `as.numeric()` bit for bit:
#
#   sig  the anchor matrix's upper triangle -- the input, directly.
#   dbl  the shipped double-precision `v` and `u` -- the numerator of the
#        frozen error, and the one quantity a different LAPACK moves.
#
# The DERIVATIVE SET is pinned through `dbl` rather than directly. It is too
# large to commit entry by entry (27 matrices of 8x8 at the p = 8 anchors), and
# `cos()` reaches it too -- but the shipped `v` and `u` are computed FROM it, so
# a derivative set that drifted would have to drift in a way that cancels
# exactly out of both to leave `dbl` bit-identical.
#
# What is deliberately NOT pinned here is the double-double reference route.
# That route is the artifact under test, and an expectation derived from the
# artifact under test is blind in the dimension it derives: an earlier draft of
# this gate did pin it, and the planted defect that stops the route carrying
# low-order words then made these cases SKIP instead of redden -- the defect
# hid inside the precondition meant to protect the comparison. Measured, not
# reasoned: that plant reddens again with the route unpinned.
#
# Reproduce both and the certificate is determined, so the bracket holds
# deterministically rather than by luck. Regenerate after any deliberate change
# to a builder or to the pricing, from the repo root:
#   Rscript devel/degeneracy-oracle/exact_oracle.R   (re-measures the errors)
# and re-emit these literals with `sprintf("%a", ...)` at the two sites above.
#
# This gate does NOT replace the kappa fingerprint. kappa is asserted OUTSIDE
# it, so a builder edit that moved a geometry still REDDENS; only last-bit
# drift skips. A run in which every case skips does not satisfy AC2.

cert_frozen <- list(
  a4 = list(
    sig = c("0x1.b4d8379580e2p-1", "0x1.ffcb979800d1bp-2",
    "0x1.b4d8379580e2p-1", "0x1.2bcd8009ffbebp-3", "0x1.ffcb979800d1bp-2",
    "0x1.b4d8379580e2p-1", "0x0p+0", "0x1.2bcd8009ffbebp-3",
    "0x1.ffcb979800d1bp-2", "0x1.b4d8379580e2p-1", "0x1.2bcd8009ffbe6p-3",
    "0x0p+0", "0x1.2bcd8009ffbebp-3", "0x1.ffcb979800d1bp-2",
    "0x1.b4d8379580e2p-1", "0x1.ffcb979800d1ap-2", "0x1.2bcd8009ffbe6p-3",
    "0x0p+0", "0x1.2bcd8009ffbebp-3", "0x1.ffcb979800d1bp-2",
    "0x1.b4d8379580e2p-1", "0x1.b4d8379580e2p-1", "0x1.ffcb979800d1bp-2",
    "0x1.2bcd8009ffbebp-3", "0x0p+0", "0x1.2bcd8009ffbe6p-3",
    "0x1.ffcb979800d1ap-2", "0x1.b4d8379580e2p-1"),
    dbl = c("0x1.7fb171557680fp-3", "0x1.7fe5cdf0c5074p-3",
    "0x1.136add72cea5bp+11"),
    naive = c("0x1.ffcb9978a7d92p-3", "0x1.ffb16679a59cdp-2")
  ),
  a5 = list(
    sig = c("0x1.b50079a0feb12p-1", "0x1.fffac1e05c131p-2",
    "0x1.b50079a0feb12p-1", "0x1.2be920fd7587ep-3", "0x1.fffac1e05c131p-2",
    "0x1.b50079a0feb12p-1", "0x0p+0", "0x1.2be920fd7587ep-3",
    "0x1.fffac1e05c131p-2", "0x1.b50079a0feb12p-1", "0x1.2be920fd75879p-3",
    "0x0p+0", "0x1.2be920fd7587ep-3", "0x1.fffac1e05c131p-2",
    "0x1.b50079a0feb12p-1", "0x1.fffac1e05c12fp-2", "0x1.2be920fd75879p-3",
    "0x0p+0", "0x1.2be920fd7587ep-3", "0x1.fffac1e05c131p-2",
    "0x1.b50079a0feb12p-1", "0x1.b50079a0feb12p-1", "0x1.fffac1e05c131p-2",
    "0x1.2be920fd7587ep-3", "0x0p+0", "0x1.2be920fd75879p-3",
    "0x1.fffac1e05c12fp-2", "0x1.b50079a0feb12p-1"),
    dbl = c("0x1.7ff822f445536p-3", "0x1.7ffd60f5b7113p-3",
    "0x1.560b55f7e7e06p+14"),
    naive = c("0x1.fffac1e52ba79p-3", "0x1.fff822d875bccp-2")
  ),
  c4 = list(
    sig = c("0x1.fffd60ecbe7bp-2", "0x0p+0", "0x1.fffd60ecbe7bp-2",
    "0x1.fffd60ecbe7aep-2", "0x0p+0", "0x1.fffd60ecbe7bp-2"),
    dbl = c("0x1.7ffd60fdec1a2p-3", "0x1.8000000371affp-3",
    "0x1.4001f78982p+1"),
    naive = c("0x1.0000000370ae4p-2", "0x1.fffd60ee7951cp-2")
  ),
  b9a = list(
    sig = c("0x1.f86394ae0e824p-2", "0x1.e98a8996b5f8ap-3",
    "0x1.a605f0a4c5714p-2", "-0x1.db2162eb1137p-7", "0x1.999999999999bp-3",
    "0x1.a605f0a4c5714p-2", "-0x1.e98a8996b5f86p-4", "-0x1.8d8ae1657af4p-7",
    "0x1.999999999999bp-3", "0x1.a605f0a4c5714p-2", "-0x1.db2162eb113aap-7",
    "-0x1.9999999999998p-4", "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3",
    "0x1.a605f0a4c5714p-2", "0x1.e98a8996b5f86p-3", "-0x1.8d8ae1657af7p-7",
    "-0x1.9999999999998p-4", "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3",
    "0x1.a605f0a4c5714p-2", "0x1.f86394ae0e824p-2", "0x1.999999999999bp-3",
    "-0x1.8d8ae1657af4p-7", "-0x1.9999999999998p-4", "-0x1.8d8ae1657af7p-7",
    "0x1.9999999999998p-3", "0x1.a605f0a4c5714p-2", "0x1.fff2e4e46e7a8p-1",
    "0x1.f86394ae0e824p-2", "0x1.e98a8996b5f8ap-3", "-0x1.db2162eb1137p-7",
    "-0x1.e98a8996b5f86p-4", "-0x1.db2162eb113aap-7", "0x1.e98a8996b5f86p-3",
    "0x1.f86394ae0e824p-2"),
    dbl = c("0x1.eecf3c6f253c2p-4", "0x1.ad14d01e89aecp-4",
    "0x1.dd93c7c700ce1p-2", "0x1.dd66dd08b94e4p+4"),
    naive = c("0x1.8be3fa47badbcp-3", "0x1.2aa9076ab6738p-3",
    "0x1.47c7f04c47c7cp-1")
  ),
  b9b = list(
    sig = c("0x1.f86964229da49p-2", "0x1.e9902d41e6beep-3",
    "0x1.a605f0a4c5714p-2", "-0x1.db26dc16dcb8p-7", "0x1.999999999999bp-3",
    "0x1.a605f0a4c5714p-2", "-0x1.e9902d41e6bebp-4", "-0x1.8d8ae1657af4p-7",
    "0x1.999999999999bp-3", "0x1.a605f0a4c5714p-2", "-0x1.db26dc16dcbb9p-7",
    "-0x1.9999999999998p-4", "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3",
    "0x1.a605f0a4c5714p-2", "0x1.e9902d41e6bebp-3", "-0x1.8d8ae1657af7p-7",
    "-0x1.9999999999998p-4", "-0x1.8d8ae1657af4p-7", "0x1.999999999999bp-3",
    "0x1.a605f0a4c5714p-2", "0x1.f86964229da49p-2", "0x1.999999999999bp-3",
    "-0x1.8d8ae1657af4p-7", "-0x1.9999999999998p-4", "-0x1.8d8ae1657af7p-7",
    "0x1.9999999999998p-3", "0x1.a605f0a4c5714p-2", "0x1.fffeb07583584p-1",
    "0x1.f86964229da49p-2", "0x1.e9902d41e6beep-3", "-0x1.db26dc16dcb8p-7",
    "-0x1.e9902d41e6bebp-4", "-0x1.db26dc16dcbb9p-7", "0x1.e9902d41e6bebp-3",
    "0x1.f86964229da49p-2"),
    dbl = c("0x1.eed0c2cfe3ae3p-4", "0x1.ad1652334a978p-4",
    "0x1.dda97a1176d68p-2", "0x1.dd66a60c14724p+4"),
    naive = c("0x1.8be582ecd2207p-3", "0x1.2aaa260c9ef1cp-3",
    "0x1.47c9e3d753337p-1")
  ),
  cxb = list(
    sig = c("-0x1.ac70f5bf320e9p-1", "0x1.a2ad9ad37693p-1",
    "-0x1.ffb4667563093p-1"),
    dbl = c("0x1.86663bff23322p+3", "0x1.758572325cbeep+3",
    "-0x1.ba7d520282p-3"),
    naive = c("0x1.b90291590661cp+5", "0x1.a03bff6d802f8p+5")
  )
)

cert_hex <- function(x) sprintf("%a", as.vector(x))

cert_skip_unless_reproduced <- function(id, sigma, d) {
  fz <- cert_frozen[[id]]
  v <- axes_v_pricing(sigma, d)
  u <- axes_u_pricing(sigma, d)
  # A REFUSAL from the shipped pricing is NOT a failure to reproduce. Every
  # one of these six geometries is admitted -- axes_sigma_degenerate() passes
  # on each -- so a refusal here is a regression in axes_pricing_core(), and
  # folding it into the skip below would turn that red green. Fail on it, and
  # let the caller carry on: the certificate returns its sentinel at a refusal,
  # which reddens the bracket too.
  if (is.character(v) || is.character(u)) {
    testthat::fail(paste0(
      "the shipped pricing REFUSES at case '", id, "' (",
      paste(Filter(is.character, list(v, u)), collapse = ", "),
      ") -- an admitted geometry, so this is a regression, not a platform ",
      "difference"
    ))
    return(invisible(FALSE))
  }
  bad <- character(0)
  if (!identical(cert_hex(sigma[upper.tri(sigma)]), fz$sig)) {
    bad <- c(bad, "the anchor matrix")
  }
  if (!identical(c(cert_hex(v$corrected), cert_hex(u)), fz$dbl)) {
    bad <- c(bad, "the shipped double pricing")
  }
  # The NAIVE arm is pinned too (M113 review, [O] F5). It is not part of the
  # `se` or `cval` yardsticks, so M108 had no reason to freeze it; the `ratio`
  # anchors this file gained are a function of it, and a machine reproducing
  # `corrected` and `u` bit for bit but not `naive` would have compared them
  # against another machine's measurement instead of skipping.
  if (!identical(cert_hex(v$naive), fz$naive)) {
    bad <- c(bad, "the shipped naive arm")
  }
  if (length(bad) == 0L) return(invisible(TRUE))
  testthat::skip(paste0(
    "this machine does not reproduce ", paste(bad, collapse = " or "),
    " bit for bit at case '", id, "', so the frozen relative error measured ",
    "on the oracle's machine is not a yardstick for it"
  ))
}


# One test PER CASE, deliberately: `skip()` abandons the whole `test_that()` it
# fires in, so a single loop would let one non-reproducing case take the other
# four with it -- and the criterion's "a skip on some platform is expected"
# only means anything if the cases skip independently.
for (cert_case in cert_anchors()) {
  test_that(paste0("AC2: the estimate brackets the exact oracle's error -- ",
                   cert_case$lbl), {
    cs <- cert_case
    d <- cert_derivs(cs)
    # OUTSIDE the precondition: a builder edit that moved this geometry must
    # redden here, never skip, because then the frozen figures below would be
    # describing a matrix this file no longer builds.
    expect_equal(m106_kappa(cs$r), cs$kappa, tolerance = 1e-3, label = cs$lbl)

    cert_skip_unless_reproduced(cs$id, cs$r, d)
    cert <- axes_accuracy_certificate(cs$r, d)

    # AT LEAST the measured error: an estimate below it is an under-report,
    # which is the licensing failure the certificate exists to prevent and the
    # evidence class D-051 records as reopening the mechanism.
    expect_gte(cert$se, cs$se)
    expect_gte(cert$cval, cs$cval)
    expect_gte(cert$fiml_ratio, cs$ratio)
    # ... and at most 1e3 times it, the ceiling M108 pre-registered before any
    # measurement. The a-priori bound this replaces overstates by 5 to 8
    # decades, so this half is what falsifies a certificate that merely
    # restates that bound.
    expect_lte(cert$se, 1e3 * cs$se)
    expect_lte(cert$cval, 1e3 * cs$cval)
    expect_lte(cert$fiml_ratio, 1e3 * cs$ratio)
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
  cert_skip_unless_reproduced("cxb", fx$S, d)
  cert <- axes_accuracy_certificate(fx$S, d)

  expect_gte(cert$se, 3.413e-02)
  expect_lte(cert$se, 1e3 * 3.413e-02)
  expect_gte(cert$cval, 4.890)
  expect_lte(cert$cval, 1e3 * 4.890)
  expect_gte(cert$fiml_ratio, 8.712e-04)
  expect_lte(cert$fiml_ratio, 1e3 * 8.712e-04)
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
    f <- axes_certificate_safety_factor
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

  # Every intermediate being dyadic, the SHIPPED double route is exact here
  # too -- so the committed error at this configuration is known to be zero,
  # and the certificate must report its floor and nothing more. That is the
  # level assertion this type exists to make: a reference route drifting off
  # the truth would move the estimate off the floor.
  expect_identical(axes_v_pricing(s, d)$corrected, 97 / 128)
  expect_identical(axes_v_pricing(s, d)$naive, 2)
  expect_identical(axes_u_pricing(s, d), 5 / 8)
  cert <- axes_accuracy_certificate(s, d)
  floor_est <- axes_certificate_safety_factor * 2 * .Machine$double.eps
  expect_identical(cert$se, floor_est)
  expect_identical(cert$cval, floor_est)
  expect_identical(cert$fiml_ratio, floor_est)
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
  # The corrected arm and the cval numerator are both priced to within a
  # floor's worth here on every platform measured, which is what makes the
  # estimate this fit gets come from the new field alone. Asserted before the
  # precondition below, so they still run where that one steps aside.
  cert <- axes_accuracy_certificate(s, d)
  floor_est <- axes_certificate_safety_factor * 2 * .Machine$double.eps
  expect_identical(cert$se, floor_est)
  expect_identical(cert$cval, floor_est)

  # THE PRECONDITION ON THE SHIPPED-ERROR HALF (M113 review; the windows-latest
  # red on CI run 33329301066). The replay half above needs no platform
  # agreement -- it is R-level `+`, `-`, `*` and `/` throughout -- but the half
  # below needs the shipped route to be WRONG here, and that is a property of
  # the machine's LAPACK, not of the configuration. windows-latest prices this
  # matrix EXACTLY (true_rel 0, against 5.6e-13 on macOS and ubuntu), so the
  # three assertions below had nothing to bracket and reddened the package for
  # a platform being more accurate than the one the case was derived on.
  #
  # Skipping is the honest report and not a widening: a machine with a real
  # error still runs every assertion, and asserting a floor against a floor is
  # the emptiness this case exists to avoid, so it is not worth reddening for.
  # The threshold is the certificate's own floor, `fac * max(delta_q/2, 2*eps)`,
  # which bites until delta_q exceeds 4 * eps -- not the 2 * eps its expression
  # reads. This is the platform-reach problem M115 exists to widen, reached at
  # a second surface; a run in which this half skips does not satisfy AC3.
  if (true_rel <= 4 * .Machine$double.eps) {
    testthat::skip(paste0(
      "the shipped double route is exact at this configuration on this ",
      "machine (measured relative error ", format(true_rel), "), so there is ",
      "no committed error for the `fiml_ratio` field to bracket here; the ",
      "replay half above is asserted unconditionally and has run"
    ))
  }

  # The field brackets a REAL error: at least the measured one (an estimate
  # below it is the under-report the certificate exists to prevent) and within
  # the same pre-registered decade the other two fields carry.
  expect_gte(cert$fiml_ratio, true_rel)
  expect_lte(cert$fiml_ratio, 1e3 * true_rel)

  # ... and the case DISCRIMINATES: the estimate this fit gets comes from the
  # new field alone. Before M113 this configuration was certified at 4.4e-15
  # with a quotient wrong by 5.6e-13.
  expect_gt(cert$fiml_ratio, floor_est)
  expect_identical(axes_certificate_worst(cert), cert$fiml_ratio)
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
