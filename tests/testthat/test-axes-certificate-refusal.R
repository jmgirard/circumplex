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


test_that("M111 AC3 (graded route): the committed counterexample refuses at both surfaces", {
  # The one matrix on record whose reported corrected SEs were measured WRONG
  # -- by 3.4% with reason NULL under the pre-M89 floor (RR18). Its certificate
  # is graded rather than sentinel: 3.4e-1 for the SEs and 4.9e+1 for cval,
  # both decades past the 1e-4 accuracy target, so the refusal stands here
  # where M111 lifted it for the reachable geometries above.
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
  se <- suppressWarnings(
    axes_corrected_se(S, rownames(S), as.numeric(fx$ia), scl,
                      n = 600, fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  )
  sf <- suppressWarnings(
    axes_scaling_factor(S, rownames(S), as.numeric(fx$ia), scl,
                        fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                        df = 1, baseline_df = 3)
  )
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
