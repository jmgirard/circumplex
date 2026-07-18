# Trajectory plotting for occasions objects (M33) ------------------------------
# A Cartesian companion to the circular canvas: SSM parameters plotted against
# occasion, one facet per parameter. Everything statistically load-bearing
# happens here in the reshape, not in the ggplot call -- the occasion ordering
# and the displacement branch are wrong-answer channels that render without
# error, so they are pinned at the data level and tested there.

# Panel titles, in canonical parameter order. Doubles as the panel factor's
# level set, so a dropped parameter cannot silently reorder the facets.
ssm_trajectory_panels <- function() {
  c(
    e = "Elevation",
    x = "X-value",
    y = "Y-value",
    a = "Amplitude",
    d = "Displacement"
  )
}

# Unwrap a temporally ordered displacement series onto a continuous branch,
# bridging occasions whose displacement is undefined.
#
# angle_unwrap() propagates NA from a missing wave *onward* (cumsum), which is
# the right policy for a series whose later values are genuinely
# branch-ambiguous. For a plot it is the wrong one: a single flat occasion
# would blank the rest of the trajectory rather than leaving a gap at the flat
# occasion. So unwrap the defined occasions as a sequence and reinsert NA at
# the gaps. The assumption this widens is angle_unwrap()'s own -- that the
# profile rotates less than a half-turn between consecutive *defined*
# occasions -- applied across the gap instead of across one step; it is
# documented on ssm_plot_trajectory() because no data can verify it.
ssm_unwrap_gapped <- function(x) {
  ok <- !is.na(x)
  out <- rep(NA_real_, length(x))
  if (any(ok)) out[ok] <- angle_unwrap(as.numeric(x[ok]))
  out
}

# Place a CI bound on its estimate's unwrapped branch.
#
# Non-contrast displacement bounds are each independently wrapped into
# [0, 360] by quantile.circumplex_radian() (R/ssm_bootstrap.R), so a
# seam-straddling interval is stored with lower > upper. The bound belongs at
# its *signed* circular distance from its own estimate -- never at the
# estimate's branch offset, which throws a straddling bound a full turn off and
# inverts the ribbon (LESSONS M27; the same expression the growth vignette and
# the M27 coverage oracle use).
ssm_bound_on_branch <- function(bound, est, branch) {
  branch + (((as.numeric(bound) - as.numeric(est) + 180) %% 360) - 180)
}

# Reshape an occasions object into the long per-panel frame the trajectory plot
# draws: one row per (Group, Occasion, Parameter).
ssm_trajectory_frame <- function(ssm_object, drop_xy = FALSE) {
  results <- ssm_object$results
  details <- ssm_object$details

  # Drop the contrast row. It is the last row when details$contrast (the
  # positional detector print.circumplex_ssm() uses -- there is no boolean
  # column), it is not a time point, and its displacement rides the opposite
  # branch convention (circumplex_contrast_radian: already contiguous, may be
  # negative or exceed 360). ssm_plot_circle()'s df[1:2, ] slice is not reusable
  # here -- it truncates k > 2 and grouped objects.
  if (isTRUE(details$contrast)) {
    results <- results[-nrow(results), , drop = FALSE]
  }

  # details$occasions is the canonical order (the occasions list order, or the
  # long path's factor levels / first-appearance order). results$Occasion is
  # character, so mapping it to a discrete scale without this factoring lets
  # ggplot2 re-sort it alphabetically -- which flips a T10/T2 pair and silently
  # reverses the trajectory's time axis.
  results$Occasion <- factor(results$Occasion, levels = details$occasions)
  results$Group <- factor(results$Group, levels = unique(results$Group))

  # A profile has a defined displacement iff it has a location; the shared
  # predicate keeps this agreeing with the circular geoms rather than rolling a
  # second is.na() criterion. A flat (zero-amplitude) occasion fails it.
  located <- ssm_has_location(results$a_est, results$d_est)
  results$d_est[!located] <- NA_real_

  # Displacement onto a continuous branch, per group series, in occasion order.
  # Done before the melt so the unwrap sees the temporally ordered sequence.
  results <- results[order(results$Group, results$Occasion), , drop = FALSE]
  by_group <- split(seq_len(nrow(results)), results$Group, drop = TRUE)
  d_branch <- rep(NA_real_, nrow(results))
  d_low <- rep(NA_real_, nrow(results))
  d_high <- rep(NA_real_, nrow(results))
  for (idx in by_group) {
    est <- as.numeric(results$d_est[idx])
    branch <- ssm_unwrap_gapped(est)
    d_branch[idx] <- branch
    d_low[idx] <- ssm_bound_on_branch(results$d_lci[idx], est, branch)
    d_high[idx] <- ssm_bound_on_branch(results$d_uci[idx], est, branch)
  }
  results$d_est <- d_branch
  results$d_lci <- d_low
  results$d_uci <- d_high

  params <- names(ssm_trajectory_panels())
  if (drop_xy) params <- setdiff(params, c("x", "y"))

  # D-007 displacement-interpretability guardrail, per profile row: a pure
  # function of the amplitude CI pair. Carried on every row so the plot can
  # mark it where it applies (the displacement panel) without a second join.
  certified <- ssm_certified(results$a_lci, results$a_uci)

  out <- do.call(rbind, lapply(params, function(p) {
    data.frame(
      Group = results$Group,
      Occasion = results$Occasion,
      Parameter = p,
      est = as.numeric(results[[paste0(p, "_est")]]),
      lci = as.numeric(results[[paste0(p, "_lci")]]),
      uci = as.numeric(results[[paste0(p, "_uci")]]),
      Certified = certified,
      stringsAsFactors = FALSE
    )
  }))

  panels <- ssm_trajectory_panels()
  out$Panel <- factor(panels[out$Parameter], levels = panels[params])
  out
}
