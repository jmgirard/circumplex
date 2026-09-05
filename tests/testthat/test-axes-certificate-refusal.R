# The refusal region the per-fit certificate decides (M111).
#
# Until M111 the degeneracy criterion's floor WAS the refusal: every matrix
# below sqrt(p*eps/tau) in eigenvalue ratio was refused at both surfaces,
# whatever the arithmetic actually did on it. M108 built the instrument that
# can tell those cases apart -- a per-fit certificate estimating the relative
# error THIS fit's numbers carry -- and this file pins what M111 did with it:
#
#   AC2  the five reachable geometries the exact-rational oracle prices all
#        COMPUTE end-to-end, including the two the floor used to refuse.
#   AC3  the one committed matrix whose reported numbers were measured WRONG
#        still refuses, at both surfaces, under one shared literal.
#
# AC2 and AC3's sentinel case are asserted through axes_reliability() with the
# matrix injected at axes_fitted_cov() -- the one seam both consumers read. The
# injection is not a convenience: the exported path REFITS, so pricing a fit's
# own Sigma-hat would price a different matrix than the oracle enumerated, and
# the certificate's whole claim is about a named matrix (M111 plan work log).
# AC3's graded case cannot take that seam and says why at its own test.


# The five reachable-geometry cases devel/degeneracy-oracle/exact_oracle.R
# enumerates, built from the same helper builders at the same parameters --
# see test-axes-certificate.R, which pins each one's kappa and the oracle's
# frozen error beside it.
m111_reachable <- function() {
  oct <- as.numeric(octants())
  list(
    list(id = "a4", lbl = "family A, p = 8, kappa 1e4",
         r = m106_family_a(2.4e-4, 1L), fit_on = m106_family_a(0.3, 1L),
         scale = as.character(1:8), ang = oct),
    list(id = "a5", lbl = "family A, p = 8, kappa 1e5",
         r = m106_family_a(2.4e-5, 1L), fit_on = m106_family_a(0.3, 1L),
         scale = as.character(1:8), ang = oct),
    list(id = "c4", lbl = "family C, p = 4 minimum",
         r = m106_family_c(1.2e-5), fit_on = m106_family_c(0.3),
         scale = as.character(1:4), ang = c(90, 180, 270, 360)),
    list(id = "b9a", lbl = "near-duplicate r = .9999",
         r = m106_family_b(7e-5), fit_on = m106_family_b(0.3),
         scale = as.character(c(1:8, 1L)), ang = c(oct, oct[[1L]])),
    list(id = "b9b", lbl = "near-duplicate r = .99999",
         r = m106_family_b(7e-6), fit_on = m106_family_b(0.3),
         scale = as.character(c(1:8, 1L)), ang = c(oct, oct[[1L]]))
  )
}


test_that("M111 AC2: every reachable geometry the oracle prices computes at both surfaces", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  cases <- m111_reachable()
  # The domain this test runs over, asserted rather than assumed: a builder
  # edit that emptied or shortened the case list would leave every assertion
  # below vacuously satisfied, which is the silent-empty-domain failure the
  # repo has now hit twice (M107, M109).
  expect_length(cases, 5L)

  refused_before <- character()
  for (cs in cases) {
    p <- nrow(cs$r)
    inames <- rownames(cs$r)
    items <- split(inames, cs$scale)
    # Which side of the OLD floor this case sits on, recorded per case so the
    # test states what it is actually proving. Two of the five are below it:
    # before M111 those two refused here.
    if (identical(axes_sigma_degenerate(cs$r), "ill_conditioned")) {
      refused_before <- c(refused_before, cs$id)
    }

    # The fit runs on a well-conditioned sibling of the SAME design and the
    # oracle's own matrix is injected at the seam. Fitting the anchor itself
    # does not work at these condition numbers -- lavaan fails to converge at
    # the p = 8 kappa 1e5 case -- and would price a refitted Sigma-hat rather
    # than the matrix the oracle enumerated, which is the substitution the
    # seam exists to prevent.
    expect_null(axes_sigma_degenerate(cs$fit_on), label = cs$lbl)
    expect_identical(dimnames(cs$fit_on), dimnames(cs$r))
    local_mocked_bindings(axes_fitted_cov = function(fit) cs$r)
    w <- testthat::capture_warnings(
      res <- suppressMessages(
        axes_reliability(cormat = cs$fit_on, items = items,
                         angles = as.numeric(unique(cs$ang)), n = 600L)
      )
    )

    # Neither surface refused, and neither warned.
    expect_null(res$details$se_correction_failed, label = cs$lbl)
    expect_null(res$details$fit_scaling_failed, label = cs$lbl)
    expect_length(grep("could not be computed", w), 0L)

    # The corrected component SEs are finite. The epsilon row carries no
    # single reported item-error SE on any path, so the assertion is over the
    # rows that have one.
    se_rows <- res$components$Symbol != "epsilon"
    expect_true(all(is.finite(res$components$SE[se_rows])), label = cs$lbl)
    expect_true(any(se_rows), label = cs$lbl)

    # ...and cval is finite: the scaling factor is cval divided by df, so a
    # finite positive factor is that quantity reaching the user.
    expect_true(is.finite(res$details$scaling_factor[["model"]]), label = cs$lbl)
    expect_gt(res$details$scaling_factor[["model"]], 0)
    expect_true(is.finite(res$fit$chisq), label = cs$lbl)
  }

  # The two cases this milestone actually moved, named rather than counted:
  # both sit below the a-priori floor, and both compute above.
  expect_setequal(refused_before, c("a5", "b9b"))
})


test_that("M111 AC3 (sentinel route): a p = 24 near-duplicate fit refuses through axes_reliability()", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  # The route the exported surface can actually take. The counterexample below
  # is p = 3 and cannot ride this seam at all (axes_reliability() refuses fewer
  # than four scales), so without this case nothing would pin the new literal
  # on what a user's own call returns.
  #
  # Three items per octant scale with the item-error variance driven to 1.5e-9
  # makes each scale's items near-duplicates of one another. The certificate
  # returns its sentinel here rather than a graded estimate -- both pricing
  # routes fail on a matrix this close to rank-deficient -- which is the
  # fail-closed arm (GP2), and a different mechanism than the counterexample's
  # graded 4.9e+1. Both must refuse, so both are asserted.
  bad <- m106_family_a(1.5e-9, 3L)
  expect_identical(nrow(bad), 24L)
  expect_identical(axes_sigma_degenerate(bad), "ill_conditioned")

  oct <- as.numeric(octants())
  scale_id <- rep(seq_along(oct), each = 3L)
  items <- split(rownames(bad), scale_id)
  # The fit itself runs on a well-conditioned sibling of the same design, so
  # what is under test is the refusal on the injected matrix and not lavaan's
  # behaviour on a near-singular one.
  clean <- m106_family_a(0.3, 3L)
  dimnames(clean) <- dimnames(bad)
  expect_null(axes_sigma_degenerate(clean))

  local_mocked_bindings(axes_fitted_cov = function(fit) bad)
  w <- testthat::capture_warnings(
    res <- suppressMessages(
      axes_reliability(cormat = clean, items = items, angles = oct, n = 600L)
    )
  )

  # One shared literal at both surfaces, distinct from the two that refuse
  # without consulting the certificate.
  expect_identical(res$details$se_correction_failed, "uncertified")
  expect_identical(res$details$fit_scaling_failed, "uncertified")
  expect_false(res$details$se_correction_failed %in%
                 c("indefinite", "singular"))

  # Each surface warns once, naming that literal, and each warning carries the
  # fit's own estimate rather than the refusal alone (M111 gate).
  expect_length(grep("uncertified", w, fixed = TRUE), 2L)
  expect_true(any(grepl("standard errors could not be computed", w)))
  expect_true(any(grepl("scaled fit statistics could not be computed", w)))
  expect_length(grep("estimated relative error", w, fixed = TRUE), 2L)
  # WHICH route this case takes, asserted rather than described (M111 review
  # F5). The certificate's sentinel is exactly 1 and prints as "1" at the
  # note's two significant digits; a graded estimate could not. Without this
  # the file's two AC3 cases could drift onto the same route with every other
  # assertion still green, collapsing "one on each route" silently.
  expect_length(grep("estimated relative error 1;", w, fixed = TRUE), 2L)

  # A unit refusal: the one reason speaks for all three SE vectors (M91).
  expect_null(res$details$naive_reason)

  # Everything the refusal covers is NA together; df and srmr still report.
  expect_true(all(is.na(res$components$SE)))
  expect_identical(res$fit$chisq, NA_real_)
  expect_identical(res$fit$pvalue, NA_real_)
  expect_identical(res$fit$rmsea, NA_real_)
  expect_identical(res$fit$cfi, NA_real_)
  expect_true(is.finite(res$fit$df))
  expect_true(is.finite(res$fit$srmr))
})


test_that("M111 AC3: the committed counterexample refuses at both surfaces, on either route", {
  skip_on_cran()
  # The one matrix on record whose reported corrected SEs were measured WRONG
  # -- by 3.4% with reason NULL under the pre-M89 floor (RR18). Both decades
  # past the 1e-4 accuracy target, so the refusal stands here where M111
  # lifted it for the reachable geometries above.
  #
  # WHICH CERTIFICATE THIS CASE GETS IS A PLATFORM FACT (M122; D-055). This
  # test was written as the GRADED half of a pair -- the case above takes the
  # sentinel route, this one a graded 4.9e+1 -- and it asserted that
  # difference by requiring the warning NOT to read "estimated relative error
  # 1;". But `solve(info)` at this matrix succeeds or fails on the platform's
  # LU roundoff (rcond(info) 2.6008e-16 on macOS/arm64, 2.0494e-16 on
  # linux-arm64/OpenBLAS, straddling eps), and where it fails the certificate
  # degrades to its sentinel and the warning reads exactly that. The pairing
  # is still asserted, but on the route this machine actually takes: the two
  # cases must not BOTH be graded and must not BOTH be sentinel, which is what
  # the pairing was ever about. See test-axes-certificate.R for the same
  # repair at the bracket suite.
  #
  # Priced by the two surfaces DIRECTLY. At p = 3 with df = 1 it is unreachable
  # through the exported path, which requires four scales, so the
  # axes_fitted_cov() seam would realign it to four item names it does not
  # carry and error. Fixture provenance is documented at its other reader,
  # test-axes-scaled-fit.R.
  fx <- readRDS(test_path("fixtures", "rb18-counterexample-b.rds"))
  # The precondition, asserted so a fixture change cannot make this pass for a
  # different reason: the criterion still calls it ill-conditioned, which is
  # the arm M111 handed to the certificate, and not one of the two literals
  # that refuse without consulting it.
  expect_identical(axes_sigma_degenerate(fx$S), "ill_conditioned")
  S <- fx$S
  scl <- c("A", "B", "C")
  wse <- testthat::capture_warnings(
    se <- axes_corrected_se(S, rownames(S), as.numeric(fx$ia), scl,
                            n = 600, fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  )
  wsf <- testthat::capture_warnings(
    sf <- axes_scaling_factor(S, rownames(S), as.numeric(fx$ia), scl,
                              fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                              df = 1, baseline_df = 3)
  )
  # WHICH route, asserted rather than described (M111 review F5), and now
  # exhaustively over the two the matrix admits (M122). The note is present on
  # both routes and carries an estimate on both; what differs is whether that
  # estimate is the sentinel, which prints as exactly "1" at the note's two
  # significant digits. The digits of a GRADED estimate are NOT pinned -- they
  # are the fixture's own arithmetic, which reproduces bit for bit only on the
  # platform that produced it (pinning 49 reddened ubuntu and windows at
  # M111's own review gate, macOS green).
  #
  # The route is read from the shipped pricing rather than from the warning
  # text, so the two are independent: the pricing decides, and the text is
  # then required to agree with it. Reading the route off the text and
  # asserting the text would assert nothing.
  refused <- is.character(axes_v_pricing(S, axes_se_derivs(
    as.numeric(fx$ia), scl, NULL, FALSE, FALSE)))
  expect_length(grep("estimated relative error ", wse, fixed = TRUE), 1L)
  expect_length(grep("estimated relative error ", wsf, fixed = TRUE), 1L)
  sentinel_notes <- c(grep("estimated relative error 1;", wse, fixed = TRUE),
                      grep("estimated relative error 1;", wsf, fixed = TRUE))
  if (refused) {
    # The shipped pricing gave up, so there is no reported number to have a
    # relative error and the sentinel is the only truthful estimate. Both
    # surfaces must print it -- one printing a graded figure would mean the
    # two disagree about whether this fit was certified.
    expect_length(sentinel_notes, 2L)
  } else {
    # A value exists, so the estimate is graded and neither surface may print
    # the sentinel. This is also what keeps the pairing with the case above
    # meaningful: that one reaches the sentinel through a rank-deficient
    # derivative set, on every platform.
    expect_length(sentinel_notes, 0L)
  }
  # One shared literal, both surfaces, distinct from the two that refuse
  # without the certificate -- M89's nestedness contract holding across it.
  expect_identical(se$reason, "uncertified")
  expect_identical(sf$reason, "uncertified")
  expect_identical(se$reason, sf$reason)
  expect_false(se$reason %in% c("indefinite", "singular"))
  expect_true(all(is.na(se$corrected)))
  expect_identical(sf$scale, NA_real_)
  # A unit refusal: the one reason speaks for all three SE vectors (M91).
  expect_null(se$naive_reason)
})


# ---- AC4: the two limbs the certificate is never asked about -----------------
#
# M111 handed ONE of the criterion's three answers to the certificate. The
# other two refuse exactly as before, and this is where that is fenced at the
# SURFACES rather than at the criterion function. The criterion's own returns
# stay pinned by the near-threshold probes in test-axes-scaled-fit.R (M89 AC3,
# M106 AC3), which M111 left untouched; those probes cannot see a surface, so
# a wiring change that routed "indefinite" through the certificate would leave
# every one of them green.

# The df pair a p x p map with this derivative set implies. Computed rather
# than hard-coded so a p change cannot silently turn a refusal into the
# scaling surface's df_mismatch door and pass for the wrong reason.
m111_dfs <- function(p, ang, scl) {
  d <- axes_se_derivs(ang, scl, NULL, FALSE, FALSE)
  list(df = p * (p + 1) / 2 - length(d$mats), baseline_df = p * (p - 1) / 2)
}

# Both surfaces' reasons for one matrix, as a pair.
m111_both <- function(sigma, ang, scl) {
  p <- nrow(sigma)
  di <- m111_dfs(p, ang, scl)
  se <- suppressWarnings(
    axes_corrected_se(sigma, rownames(sigma), ang, scl,
                      n = 600, fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  )
  sf <- suppressWarnings(
    axes_scaling_factor(sigma, rownames(sigma), ang, scl, NULL,
                        fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                        df = di$df, baseline_df = di$baseline_df)
  )
  list(se = se$reason, sf = sf$reason)
}

test_that("M111 AC4: 'indefinite' and 'singular' still refuse at both surfaces, unchanged", {
  skip_on_cran()
  # Two p, and two spectral forms at each. The forms differ in more than
  # scale: form 1 plants a single eigenvalue in a rotated basis (the whole
  # rest of the spectrum flat at 1), form 2 subtracts a rank-one projector
  # from the identity, so the negative direction is a uniform mixture of every
  # item rather than a rotated coordinate. A wiring change keyed to either
  # shape alone would survive one of them.
  for (p in c(4L, 8L)) {
    ang <- as.numeric(octants())[seq_len(p)] * (8 / p)
    scl <- as.character(seq_len(p))
    band <- m106_band(p)

    planted <- function(mult) m106_planted(p, -band * mult)
    rank_one <- function(mult) {
      v <- rep(1 / sqrt(p), p)
      s <- diag(p) - (1 + band * mult) * tcrossprod(v)
      s <- (s + t(s)) / 2
      dimnames(s) <- list(paste0("i", seq_len(p)), paste0("i", seq_len(p)))
      s
    }

    for (form in list(list(nm = "planted", f = planted),
                      list(nm = "rank-one", f = rank_one))) {
      lab <- sprintf("p %d, %s", p, form$nm)

      # PAST the partition boundary: decisively negative, so the refusal is a
      # statement about the user's model and no arithmetic certificate is
      # consulted. Both surfaces, same literal.
      deep <- form$f(100)
      expect_identical(axes_sigma_degenerate(deep), "indefinite", label = lab)
      r <- m111_both(deep, ang, scl)
      expect_identical(r$se, "indefinite", label = lab)
      expect_identical(r$sf, "indefinite", label = lab)

      # INSIDE the band, the other side of the same boundary: the criterion
      # declines to call it a model defect, so this is the arm M111 routed to
      # the certificate. The literal must NOT be "indefinite" here -- that is
      # what makes the assertion above about the boundary rather than about
      # negativity in general.
      shallow <- form$f(0.5)
      expect_identical(axes_sigma_degenerate(shallow), "ill_conditioned",
                       label = lab)
      r <- m111_both(shallow, ang, scl)
      expect_false(identical(r$se, "indefinite"), label = lab)
      expect_false(identical(r$sf, "indefinite"), label = lab)
      expect_identical(r$se, r$sf, label = lab)
    }

    # "singular" -- non-finite entries, which eigen() cannot decompose and the
    # certificate cannot price. Refused at both surfaces on its own literal,
    # ahead of any certificate call.
    nonfin <- m106_planted(p, 0.5)
    nonfin[2L, 3L] <- nonfin[3L, 2L] <- NA_real_
    expect_identical(axes_sigma_degenerate(nonfin), "singular")
    r <- m111_both(nonfin, ang, scl)
    expect_identical(r$se, "singular", label = sprintf("p %d, non-finite", p))
    expect_identical(r$sf, "singular", label = sprintf("p %d, non-finite", p))
  }
})


# ---- M114 AC1: the shared predicate, fenced against the per-surface split ----
#
# WHAT IS UNDER TEST. axes_degeneracy_refusal() reads ONE number for both
# surfaces -- axes_certificate_worst(), the max over the certificate's three
# fields -- rather than letting each surface read the field it produces. That
# was the M111 gate's choice over the per-surface alternative, on the grounds
# that gating each surface on its own field would let one surface compute while
# the other refuses the same matrix, which is the split M89's nestedness
# contract exists to prevent. Nothing failed if the max were replaced by that
# alternative (M111 review F4): every case committed before this milestone has
# all three fields on one side of the target, so both readings agree on all of
# them.
#
# WHERE THE STRADDLE COMES FROM. T1 searched the three anchor families' stated
# parameter space and a neighbourhood of the committed counterexample for an
# input whose fields land on opposite sides of the target. Families A and B do
# not produce one at any parameter tried: their fields climb together to ~1e-7
# and then both pricing routes fail at once, so all three become the sentinel 1
# with nothing in between. Family C does, and not marginally. At p = 4 -- the
# minimum design the exported API accepts -- the cval sum cancels decades
# earlier than the SE quadratic forms do, so across a band of item-error
# variances the scaling factor's estimate is past the target while the SE
# vector's and the quotient's are three decades inside it. The margins are what
# make this worth committing rather than stubbing: a platform whose arithmetic
# differs would have to move a field by three decades to unmake the straddle.
m114_straddle_eps <- c(3e-9, 5e-9, 8e-9)
m114_straddle_sigma <- function(eps) m106_family_c(eps, xi1 = 0.1, xi2 = 0.3)

test_that("M114 AC1: an input whose fields straddle the target refuses at BOTH surfaces", {
  skip_on_cran()
  ang <- c(90, 180, 270, 360)
  scl <- as.character(1:4)
  d <- axes_se_derivs(ang, scl, NULL, FALSE, FALSE)
  # Computed by the same helper the M111 cases use, not hard-coded: a p change
  # must not silently turn a refusal into the scaling surface's df_mismatch
  # door and pass for the wrong reason (M114 review [O] F8).
  di <- m111_dfs(4L, ang, scl)
  ds <- axes_degeneracy_delta_star

  # WHICH members of the band straddle HERE, named rather than counted. The
  # certificate's fields are this machine's own arithmetic, and a platform that
  # prices these matrices differently could push a member onto the sentinel
  # route, where all three fields become 1 and the straddle is gone (the M113
  # windows-latest lesson, reached at a third surface). So the band is measured
  # first and the assertions below run over what it actually yields.
  # The band this test runs over, asserted rather than assumed, per this file's
  # convention at "M111 AC2" above: a shortened band would quietly reduce what
  # the loop below covers.
  expect_length(m114_straddle_eps, 3L)

  # A LOGICAL MASK, not a set of formatted numbers (M114 review [O] F1). Round-
  # tripping eps through format() would compare a scalar's rendering with a
  # vector's, and format() pads a numeric VECTOR to a common mantissa width --
  # so on a widened band the membership test could match nothing while the
  # non-emptiness guard below still passed, emptying the assertion loop
  # silently. Indexing by mask cannot drift that way.
  straddling <- logical(length(m114_straddle_eps))
  for (i in seq_along(m114_straddle_eps)) {
    eps <- m114_straddle_eps[[i]]
    sigma <- m114_straddle_sigma(eps)
    # The precondition: the criterion routes this matrix to the certificate,
    # rather than to one of the two literals that refuse without consulting it.
    expect_identical(axes_sigma_degenerate(sigma), "ill_conditioned",
                     label = sprintf("eps %g: criterion verdict", eps))
    cert <- axes_accuracy_certificate(sigma, d)
    straddling[[i]] <- cert$cval > ds && cert$se <= ds && cert$fiml_ratio <= ds
  }
  # The domain is not allowed to empty silently. If every member degenerated,
  # this criterion has nothing to assert on this machine and must say so rather
  # than pass vacuously.
  expect_true(any(straddling),
              label = "at least one band member straddles the target")

  for (eps in m114_straddle_eps[straddling]) {
    lab <- sprintf("family C p = 4, eps %g", eps)
    sigma <- m114_straddle_sigma(eps)
    cert <- axes_accuracy_certificate(sigma, d)

    # The straddle itself, stated as the two facts that make it one: the
    # scaling surface's own field is past the target, and BOTH of the SE
    # helper's own fields are inside it. This is the configuration on which the
    # shared max and the per-surface reading disagree.
    expect_gt(cert$cval, ds)
    expect_lte(cert$se, ds)
    expect_lte(cert$fiml_ratio, ds)

    wse <- testthat::capture_warnings(
      se <- axes_corrected_se(sigma, rownames(sigma), ang, scl, NULL, n = 600,
                              fit_zeta1 = FALSE, fit_zeta2 = FALSE)
    )
    wsf <- testthat::capture_warnings(
      sf <- axes_scaling_factor(sigma, rownames(sigma), ang, scl, NULL,
                                fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                                df = di$df, baseline_df = di$baseline_df)
    )

    # BOTH surfaces refuse, under one literal. The SE helper is the one the
    # per-surface reading would let compute: neither of the fields it produces
    # is past the target, and it refuses anyway because the decision is not
    # its own field's to make.
    expect_identical(se$reason, "uncertified", label = sprintf("%s: SE helper", lab))
    expect_identical(sf$reason, "uncertified", label = sprintf("%s: scaling", lab))
    expect_true(all(is.na(se$corrected)), label = lab)
    expect_identical(sf$scale, NA_real_, label = lab)
    # A unit refusal: the one reason speaks for all three SE vectors (M91).
    expect_null(se$naive_reason, label = lab)

    # One warning each, and the estimate each carries is the field the refusal
    # was actually made on -- the note reads axes_certificate_worst() too, so a
    # surface cannot be refused on one number and shown another. Asserted as
    # the route rather than the digits, which are this machine's arithmetic:
    # the note is present and it is NOT the sentinel's 1.
    expect_length(wse, 1L)
    expect_length(wsf, 1L)
    expect_length(grep("estimated relative error ", wse, fixed = TRUE), 1L)
    expect_length(grep("estimated relative error ", wsf, fixed = TRUE), 1L)
    expect_length(grep("estimated relative error 1;", wse, fixed = TRUE), 0L)
    expect_length(grep("estimated relative error 1;", wsf, fixed = TRUE), 0L)
  }
})


# The same fence, per field, on a stubbed certificate. The case above is the
# real input AC1 asks for and it exercises exactly one of the three fields --
# `cval`, the only one the search found on the far side alone. This one covers
# the other two, and covers all three with fields that are exact rather than
# measured, so what it pins is the PREDICATE's arithmetic and not some
# machine's rounding near a threshold.
test_that("M114 AC1: a straddling certificate refuses at BOTH surfaces, whichever field is the bad one", {
  # The matrix only has to reach the certificate branch and be one BOTH
  # surfaces can otherwise price; the stub supplies the numbers. That rules out
  # the committed counterexample, whose scaling surface trips the `cval <= 0`
  # backstop whatever the certificate says. Reachable geometry a5 -- family A
  # at p = 8, kappa 1e5 -- is below the criterion's floor and computes at both
  # surfaces, which is what the M111 AC2 test above asserts of it end-to-end.
  # That test is not what makes this one honest, though: it can skip when
  # lavaan is absent, so the liveness argument rests on the all-fields-inside
  # baseline below, which is run here (M114 review [O] F7).
  sigma <- m106_family_a(2.4e-5, 1L)
  ang <- as.numeric(octants())
  scl <- as.character(1:8)
  di <- m111_dfs(nrow(sigma), ang, scl)
  expect_identical(axes_sigma_degenerate(sigma), "ill_conditioned")

  # THE FIELD SET THE STUBS BELOW MUST COVER, pinned to the certificate's own
  # sentinel rather than to this file's memory of it (M114 review [O] F2).
  # axes_certificate_worst() is a max over the fields it is handed, and
  # `max(1, 2, NULL)` is 2 -- so a fourth field added the way M113 added
  # `fiml_ratio` would leave every hand-written stub here silently short, the
  # new field pinned by nothing while all six straddle assertions stayed green.
  # This is the same drift axes_certificate_sentinel() exists to prevent at the
  # call site; asserting against it makes a field addition redden HERE.
  expect_named(axes_certificate_sentinel(), c("se", "cval", "fiml_ratio"))

  ds <- axes_degeneracy_delta_star
  lo <- ds / 1e4                       # four decades inside the target
  hi <- ds * 1e2                       # two decades past it

  both <- function(cert) {
    local_mocked_bindings(axes_accuracy_certificate = function(sigma, d) cert)
    wse <- testthat::capture_warnings(
      se <- axes_corrected_se(sigma, rownames(sigma), ang, scl, NULL,
                              n = 600, fit_zeta1 = FALSE, fit_zeta2 = FALSE)
    )
    wsf <- testthat::capture_warnings(
      sf <- axes_scaling_factor(sigma, rownames(sigma), ang, scl, NULL,
                                fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                                df = di$df, baseline_df = di$baseline_df)
    )
    list(se = se$reason, sf = sf$reason, wse = wse, wsf = wsf)
  }

  # THE STUB IS LIVE, and shown to be by the direction that cannot be the
  # matrix's own doing: this geometry COMPUTES at both surfaces unstubbed (AC2
  # above), and with every field stubbed past the target it refuses at both.
  # Without this the cases below could be reading the matrix rather than the
  # certificate and stay green under any predicate at all.
  r <- both(list(se = hi, cval = hi, fiml_ratio = hi))
  expect_identical(r$se, "uncertified", label = "all three past: SE helper")
  expect_identical(r$sf, "uncertified", label = "all three past: scaling")

  # The baseline in the other direction: every field inside the target and
  # both surfaces compute, so a refusal below is a refusal the stub caused.
  r <- both(list(se = lo, cval = lo, fiml_ratio = lo))
  expect_null(r$se, label = "all three inside: SE helper")
  expect_null(r$sf, label = "all three inside: scaling surface")

  # THE STRADDLES, one per field. In each, exactly one field is past the target
  # and the other two are four decades inside it, so the shared max refuses and
  # a per-surface reading would not. The FIELDS ARE NAMED, not counted: which
  # field is bad is the whole content of the case, because each one belongs to
  # a different surface under the alternative this fences.
  #
  #   `cval`        the scaling surface's own field. Under the alternative the
  #                 SE helper reads max(se, fiml_ratio) and COMPUTES -- so this
  #                 case is what reddens at the SE helper.
  #   `se`          the SE helper's own. The scaling surface reads cval alone
  #                 and computes -- this case reddens at the scaling surface.
  #   `fiml_ratio`  also the SE helper's (axes_corrected_se() returns the
  #                 quotient). Same redden as `se`, at the scaling surface, and
  #                 it is asserted separately because a mutation could easily
  #                 carry `se` and drop the quotient.
  straddles <- list(
    list(field = "cval",
         cert = list(se = lo, cval = hi, fiml_ratio = lo)),
    list(field = "se",
         cert = list(se = hi, cval = lo, fiml_ratio = lo)),
    list(field = "fiml_ratio",
         cert = list(se = lo, cval = lo, fiml_ratio = hi))
  )
  # One case per field, asserted rather than assumed: a dropped entry would
  # retire a field's coverage with nothing to show for it.
  expect_length(straddles, 3L)
  for (case in straddles) {
    lab <- sprintf("only `%s` past the target", case$field)
    r <- both(case$cert)

    # One shared literal, both surfaces -- M89's nestedness contract, which is
    # what the shared max buys.
    expect_identical(r$se, "uncertified", label = sprintf("%s: SE helper", lab))
    expect_identical(r$sf, "uncertified",
                     label = sprintf("%s: scaling surface", lab))

    # One warning per refusal, at each surface -- the contract M90 AC7 states,
    # asserted here too so a second warning carrying the note could not pass
    # for the one (M114 review [O] F9).
    expect_identical(length(r$wse), 1L,
                     label = sprintf("%s: SE helper warning count", lab))
    expect_identical(length(r$wsf), 1L,
                     label = sprintf("%s: scaling warning count", lab))

    # And the number the user is shown is the same one the refusal was made
    # against -- the note reads axes_certificate_worst() too, so a field the
    # predicate refused on cannot be a field the note declines to print. The
    # expected text is DERIVED from `hi` rather than spelled "0.01", so a
    # change to axes_degeneracy_delta_star cannot fail these greps for a reason
    # that has nothing to do with what they test (M114 review [O] F5).
    note <- sprintf("estimated relative error %.2g", hi)
    expect_identical(length(grep(note, r$wse, fixed = TRUE)), 1L,
                     label = sprintf("%s: SE helper note", lab))
    expect_identical(length(grep(note, r$wsf, fixed = TRUE)), 1L,
                     label = sprintf("%s: scaling note", lab))
  }
})


# ---- M117: the refusal decision is priced once per checked fit ---------------
#
# Both surfaces call axes_degeneracy_refusal() on the same realigned cov2cor
# matrix with the same derivative set, so until M117 a fit the floor sent to
# the certificate paid for the double-double replay twice, once per surface.
# axes_reliability() now makes the decision once (axes_shared_refusal) and
# hands it to both through their `refusal` argument. What is pinned here:
# exactly ONE certificate evaluation per exported call (AC1), each surface
# still pricing its own certificate when called standalone with the argument
# absent (AC2), and one estimate in every warning on both sides of the seam
# (AC3).

# The estimate substring a refusal warning carries, extracted so two warnings
# can be compared as numbers-as-printed rather than by whole-message identity.
m117_estimate <- function(w) {
  m <- regmatches(w, regexpr("estimated relative error [0-9.e+-]+", w))
  expect_length(m, 1L)
  m
}

test_that("M117 AC1: the certificate is evaluated exactly once per checked axes_reliability() call, on both missing paths", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  # The M89 AC6 injection idiom: the fit runs on clean data and the
  # ill-conditioned matrix is injected at axes_fitted_cov(), the one seam both
  # consumers read. The matrix is this file's own sentinel-route case, renamed
  # to the fixture's item names so realignment lands.
  oct <- octants()
  set.seed(70)
  dat <- axes_simulate(600L, oct, 3L, xi1 = .20, xi2 = .05, zeta1 = .08)
  inames <- sprintf("i%02d", seq_len(ncol(dat)))
  colnames(dat) <- inames
  items <- split(inames, rep(seq_along(oct), each = 3L))
  expect_length(inames, 24L)
  bad <- m106_family_a(1.5e-9, 3L)
  dimnames(bad) <- list(inames, inames)
  # The precondition that routes this matrix to the certificate at all: the
  # criterion answers "ill_conditioned" for it, the one arm M111 handed over.
  expect_identical(axes_sigma_degenerate(bad), "ill_conditioned")

  # Item-level holes so the fiml path exercises its own estimation route
  # rather than degenerating to the listwise one.
  holed <- dat
  for (j in 1:6) holed[sample(seq_len(nrow(holed)), 40L), j] <- NA_real_

  count <- 0L
  real_cert <- axes_accuracy_certificate
  local_mocked_bindings(
    axes_fitted_cov = function(fit) bad,
    axes_accuracy_certificate = function(sigma, d) {
      count <<- count + 1L
      real_cert(sigma, d)
    }
  )

  for (path in c("listwise", "fiml")) {
    count <- 0L
    w <- testthat::capture_warnings(
      suppressMessages(
        if (path == "fiml") {
          axes_reliability(holed, items = items, angles = oct,
                           missing = "fiml")
        } else {
          axes_reliability(dat, items = items, angles = oct)
        }
      )
    )
    # BOTH surfaces still refuse -- proof the one evaluation fed two
    # consumers, not that one consumer stopped consulting the certificate.
    expect_length(grep("uncertified", w, fixed = TRUE), 2L)
    expect_true(any(grepl("standard errors could not be computed", w)),
                label = path)
    expect_true(any(grepl("scaled fit statistics could not be computed", w)),
                label = path)
    expect_identical(count, 1L, label = path)
  }
})

test_that("M117 AC2 + AC3 (both sides of the seam): standalone surfaces price their own certificate; every warning carries one estimate", {
  # The committed graded-route matrix: p = 3, unreachable through the exported
  # path, so the two surfaces are fired DIRECTLY -- which is exactly the
  # standalone shape AC2 binds. Provenance documented at its other readers.
  fx <- readRDS(test_path("fixtures", "rb18-counterexample-b.rds"))
  S <- fx$S
  ang <- as.numeric(fx$ia)
  scl <- c("A", "B", "C")
  expect_identical(axes_sigma_degenerate(S), "ill_conditioned")

  count <- 0L
  real_cert <- axes_accuracy_certificate
  local_mocked_bindings(
    axes_accuracy_certificate = function(sigma, d) {
      count <<- count + 1L
      real_cert(sigma, d)
    }
  )

  # AC2: each surface, called WITHOUT the argument, evaluates the certificate
  # itself (the trace moves by one per call) and refuses "uncertified".
  wse <- testthat::capture_warnings(
    se <- axes_corrected_se(S, rownames(S), ang, scl, n = 600,
                            fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  )
  expect_identical(count, 1L)
  wsf <- testthat::capture_warnings(
    sf <- axes_scaling_factor(S, rownames(S), ang, scl,
                              fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                              df = 1, baseline_df = 3)
  )
  expect_identical(count, 2L)
  expect_identical(se$reason, "uncertified")
  expect_identical(sf$reason, "uncertified")

  # The warned estimate is derived from THE certificate for this matrix and
  # derivative set -- recomputed here independently of either surface -- not
  # merely some number in the right format.
  d <- axes_se_derivs(ang, scl, NULL, FALSE, FALSE)
  want <- sprintf("estimated relative error %.2g",
                  axes_certificate_worst(real_cert(stats::cov2cor(S), d)))
  expect_match(wse, want, fixed = TRUE)
  expect_match(wsf, want, fixed = TRUE)

  # The seam side: axes_shared_refusal() decides once, both surfaces receive
  # the decision, and the trace does not move -- neither reprices.
  ref <- axes_shared_refusal(S, rownames(S), ang, scl, NULL,
                             fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  expect_identical(count, 3L)  # the shared decision itself priced it once
  expect_identical(ref$reason, "uncertified")
  wse2 <- testthat::capture_warnings(
    se2 <- axes_corrected_se(S, rownames(S), ang, scl, n = 600,
                             fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                             refusal = ref)
  )
  wsf2 <- testthat::capture_warnings(
    sf2 <- axes_scaling_factor(S, rownames(S), ang, scl,
                               fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                               df = 1, baseline_df = 3, refusal = ref)
  )
  expect_identical(count, 3L)
  expect_identical(se2$reason, "uncertified")
  expect_identical(sf2$reason, "uncertified")

  # AC3: one estimate, all four warnings -- across the two surfaces AND
  # across the two sides of the seam.
  ests <- c(m117_estimate(wse), m117_estimate(wsf),
            m117_estimate(wse2), m117_estimate(wsf2))
  expect_identical(unique(ests), ests[[1L]])
})


test_that("M117 review F1: a refusal priced for a different matrix is refused, not consumed", {
  # Without the consumption guard this pairing is silent and fail-open: the
  # degenerate matrix's own criterion refuses it "uncertified", but a clean
  # matrix's precomputed decision carries reason = NULL, so both surfaces
  # would report populated numbers with no warning at all.
  fx <- readRDS(test_path("fixtures", "rb18-counterexample-b.rds"))
  S <- fx$S
  ang <- as.numeric(fx$ia)
  scl <- c("A", "B", "C")
  clean <- diag(3)
  clean[1, 2] <- clean[2, 1] <- 0.3
  clean[1, 3] <- clean[3, 1] <- 0.2
  clean[2, 3] <- clean[3, 2] <- 0.25
  dimnames(clean) <- dimnames(S)

  bad_ref <- axes_shared_refusal(clean, rownames(S), ang, scl,
                                 fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  # The pairing is only dangerous because the two decisions DISAGREE: pin that
  # the borrowed decision is the permissive one, so the guard is what stands
  # between the caller and a reported number.
  expect_null(bad_ref$reason)
  own_ref <- axes_shared_refusal(S, rownames(S), ang, scl,
                                 fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  expect_identical(own_ref$reason, "uncertified")

  expect_error(
    axes_corrected_se(S, rownames(S), ang, scl, n = 600,
                      fit_zeta1 = FALSE, fit_zeta2 = FALSE, refusal = bad_ref),
    "priced for a different matrix"
  )
  expect_error(
    axes_scaling_factor(S, rownames(S), ang, scl,
                        fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                        df = 1, baseline_df = 3, refusal = bad_ref),
    "priced for a different matrix"
  )

  # The passing control: the MATCHED decision passes the guard and still
  # refuses "uncertified" -- the guard rejects mispairing, not the seam.
  expect_warning(
    se <- axes_corrected_se(S, rownames(S), ang, scl, n = 600,
                            fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                            refusal = own_ref),
    "uncertified"
  )
  expect_identical(se$reason, "uncertified")

  # F5: `item_block` takes a default, so the named-argument spelling both
  # callees accept works here too.
  expect_identical(
    axes_shared_refusal(S, rownames(S), ang, scl,
                        fit_zeta1 = FALSE, fit_zeta2 = FALSE),
    own_ref
  )
})
