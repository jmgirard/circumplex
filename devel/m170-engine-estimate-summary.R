# M170 T7: summary functions that read devel/m170-engine-estimate-results.rds
# and write the generated part of the record devel/m170-engine-estimate.md,
# between its `<!-- generated:begin -->` and `<!-- generated:end -->`
# markers. Every figure in that part is computed here from the results file;
# the prose around it is written by hand and cites these tables.
# Usage: Rscript devel/m170-engine-estimate-summary.R

res <- readRDS(file.path("devel", "m170-engine-estimate-results.rds"))
record <- file.path("devel", "m170-engine-estimate.md")

fmt <- function(x, d = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d))
}
md_table <- function(df) {
  head <- paste0("| ", paste(names(df), collapse = " | "), " |")
  rule <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(head, rule, rows)
}
cells <- names(res$metrics)
# H's shares in a cell whose saturation verdict is a fail are lower bounds on
# the share of fits with a lower discrepancy; they carry a ">= " prefix
lb <- function(x, k, d = 3) {
  paste0(if (!res$metrics[[k]]$saturation$pass) ">= " else "", fmt(x, d))
}
H_NOTE <- paste("H's figures describe H, the study's bounded reference search,",
                "never the global minimum of F. A \">=\" marks H's total lower",
                "share in a cell whose saturation verdict is a fail: a lower",
                "bound.")
cell_label <- function(key) {
  m <- regmatches(key, regexec("^(\\w+)_N(\\d+)$", key))[[1]]
  sprintf("%s N = %s", m[2], m[3])
}

# ---- run facts ----------------------------------------------------------------

run_facts <- function() {
  c(sprintf("- Results file written %s at commit %s on %s. Uncommitted paths at start: %d.",
            res$date, substr(res$commit, 1, 8), res$machine,
            length(res$git_status)),
    sprintf("- R = %d replicates per cell (pilot rule), K = %d. H's random starts: %s.",
            res$reps, res$K, res$random),
    sprintf("- Pilot: commit %s, projected %s CPU-hours.",
            substr(res$pilot$commit, 1, 8),
            fmt(1.2 * sum(res$reps * res$pilot$cpu$replicate +
                            80 * res$pilot$cpu$family) / 3600, 2)),
    if (!is.null(res$recomputed)) {
      sprintf(paste("- Metrics and decision recomputed %s at commit %s from",
                    "the stored replicates (the block's CORRECTIONS after the",
                    "full run). Decision before the recompute: branch %d."),
              res$recomputed$date, substr(res$recomputed$commit, 1, 8),
              res$recomputed$decision_before$branch)
    },
    sprintf("- Decision rule output: %s (branch %d).", res$decision$option,
            res$decision$branch))
}

# ---- the lower fits only ----------------------------------------------------------

# On X's lower fits (where X differs from E), each fit's root mean square
# error over the 7 free angles, and over the 4 weights, for E and for X:
# the mean over those fits, and the share of fits on which X's error is the
# smaller. The AC4 tables pool every accepted fit; this table isolates the
# fits where the estimators differ.
lower_fits_table <- function() {
  rms <- function(e, idx) sqrt(mean(e[idx]^2))
  ia <- grep("^angle_", names(res$cells[[1]]$reps[[which(!vapply(
    res$cells[[1]]$reps, `[[`, logical(1), "excluded"))[1]]]$recs$E$err))
  ib <- grep("^beta", names(res$cells[[1]]$reps[[which(!vapply(
    res$cells[[1]]$reps, `[[`, logical(1), "excluded"))[1]]]$recs$E$err))
  rows <- list()
  for (k in cells) {
    acc <- Filter(function(r) !r$excluded, res$cells[[k]]$reps)
    for (x in c("W", "H")) {
      lw <- Filter(function(r) r$replaced[[x]], acc)
      if (!length(lw)) next
      ea <- vapply(lw, function(r) rms(r$recs$E$err, ia), numeric(1))
      xa <- vapply(lw, function(r) rms(r$recs[[x]]$err, ia), numeric(1))
      eb <- vapply(lw, function(r) rms(r$recs$E$err, ib), numeric(1))
      xb <- vapply(lw, function(r) rms(r$recs[[x]]$err, ib), numeric(1))
      rows[[length(rows) + 1]] <- data.frame(
        cell = cell_label(k), estimator = x, `lower fits` = length(lw),
        `angle error E (deg)` = fmt(mean(ea), 2),
        `angle error X (deg)` = fmt(mean(xa), 2),
        `X closer on angles` = fmt(mean(xa < ea)),
        `weight error E` = fmt(mean(eb), 4),
        `weight error X` = fmt(mean(xb), 4),
        `X closer on weights` = fmt(mean(xb < eb)),
        check.names = FALSE)
    }
  }
  md_table(do.call(rbind, rows))
}

# ---- AC2: shares and saturation -------------------------------------------------

ac2_table <- function() {
  df <- do.call(rbind, lapply(cells, function(k) {
    m <- res$metrics[[k]]
    s <- m$saturation
    data.frame(cell = cell_label(k), accepted = m$n_accepted,
               excluded = m$n_excluded, errored = m$n_error,
               `H_K lower` = lb(m$share_lower_HK, k),
               `H lower` = lb(m$share_lower_H, k),
               `S1 share` = fmt(s$s1_share), S2 = sprintf("%d of %d", s$s2_count, s$s2_n),
               `family hit rate` = fmt(s$family_hit_rate_median, 4),
               `family reordered` = fmt(s$family_order_changed),
               `family more Heywood` = fmt(s$family_heywood_more),
               saturation = if (s$pass) "pass" else "fail",
               check.names = FALSE)
  }))
  md_table(df)
}

# counts the block reports per cell beside the shares
counts_table <- function() {
  df <- do.call(rbind, lapply(cells, function(k) {
    m <- res$metrics[[k]]
    data.frame(cell = cell_label(k),
               `failed starts W` = m$n_failed_starts[["W"]],
               `failed starts H` = m$n_failed_starts[["H"]],
               `finish errors W` = m$n_finish_error[["W"]],
               `finish errors H` = m$n_finish_error[["H"]],
               `gradient failures W` = m$grad_fail[["W"]],
               `gradient failures H` = m$grad_fail[["H"]],
               check.names = FALSE)
  }))
  md_table(df)
}

# ---- AC3: W and H against E -----------------------------------------------------

ac3_table <- function(x) {
  df <- do.call(rbind, lapply(cells, function(k) {
    s <- res$metrics[[k]]$search[[x]]
    cpu <- res$pilot$cpu[res$pilot$cpu$cell == k, ]
    # only the total share can only rise under a deeper search; its split by
    # order can move either way, so only the total carries ">="
    out <- data.frame(cell = cell_label(k),
                      lower = if (x == "H") lb(s$share_lower, k) else
                        fmt(s$share_lower),
                      `same order` = fmt(s$share_lower_same_order),
                      reordered = fmt(s$share_lower_reordered),
                      `gap median` = fmt(s$gap_median, 2),
                      `gap median LCB` = fmt(s$gap_median_lcb, 2),
                      `gap max` = fmt(s$gap_max, 2),
                      `more Heywood` = fmt(s$heywood_more),
                      `order changed` = fmt(s$order_changed),
                      check.names = FALSE)
    if (x == "W") {
      out$recall <- fmt(s$recall)
      out$`missed (H)` <- fmt(s$share_missed_H)
      out$`missed (H or family)` <- fmt(s$share_missed_subsample)
      out$`CPU W (s)` <- fmt(cpu$W, 2)
      out$`CPU E (s)` <- fmt(cpu$E, 3)
    }
    out
  }))
  md_table(df)
}

# ---- AC4: estimate quality --------------------------------------------------------

ac4_family_table <- function() {
  df <- do.call(rbind, lapply(cells, function(k) {
    q <- res$metrics[[k]]$quality
    do.call(rbind, lapply(c("E", "W", "H"), function(x) {
      data.frame(cell = cell_label(k), estimator = x,
                 `RMSE angle (deg)` = fmt(q[[x]]$rmse_angle, 2),
                 `RMSE zeta` = fmt(q[[x]]$rmse_zeta, 4),
                 `RMSE beta` = fmt(q[[x]]$rmse_beta, 4),
                 `cov dev` = fmt(q[[x]]$cov_dev, 4),
                 `SE defined` = fmt(q[[x]]$se_def),
                 `T exceed` = fmt(q[[x]]$t_exceed),
                 check.names = FALSE)
    }))
  }))
  md_table(df)
}

ac4_parameter_table <- function(k) {
  q <- res$metrics[[k]]$quality
  per <- lapply(q, `[[`, "per_parameter")
  df <- data.frame(parameter = per$E$parameter)
  for (x in c("E", "W", "H")) {
    p <- per[[x]]
    df[[paste(x, "bias")]] <- ifelse(startsWith(p$parameter, "angle_"),
                                     fmt(p$bias, 2), fmt(p$bias, 4))
    df[[paste(x, "RMSE")]] <- ifelse(startsWith(p$parameter, "angle_"),
                                     fmt(p$rmse, 2), fmt(p$rmse, 4))
    df[[paste(x, "SE def")]] <- fmt(p$se_defined)
    df[[paste(x, "Wald cov")]] <- fmt(p$wald_coverage)
  }
  md_table(df)
}

# ---- favors() and the decision inputs ----------------------------------------------

# The per-cell inputs the decision rule read. nw() and im() restate
# no_worse() and improves() of devel/m170-engine-estimate.R for display; the
# decision itself is the one the full run stored (res$decision).

decision_inputs <- function() {
  df <- do.call(rbind, lapply(cells, function(k) {
    m <- res$metrics[[k]]
    q <- m$quality
    nw <- function(x) {
      e <- q$E
      a <- q[[x]]
      isTRUE(all(a$rmse_angle <= e$rmse_angle, a$rmse_zeta <= e$rmse_zeta,
                 a$rmse_beta <= e$rmse_beta, a$cov_dev <= e$cov_dev + 0.01,
                 a$se_def >= e$se_def - 0.02, a$t_dev <= e$t_dev + 0.01))
    }
    im <- function(x) {
      e <- q$E
      a <- q[[x]]
      isTRUE(any(a$rmse_angle <= 0.95 * e$rmse_angle,
                 a$rmse_zeta <= 0.95 * e$rmse_zeta,
                 a$rmse_beta <= 0.95 * e$rmse_beta))
    }
    data.frame(cell = cell_label(k),
               sat = if (m$saturation$pass) "pass" else "fail",
               `no_worse W` = nw("W"), `improves W` = im("W"),
               `no_worse H` = nw("H"), `improves H` = im("H"),
               `share_lower W` = fmt(m$search$W$share_lower),
               `gap LCB W` = fmt(m$search$W$gap_median_lcb, 2),
               `CPU W ok` = isTRUE(res$pilot$cpu$W[res$pilot$cpu$cell == k] <= 10),
               check.names = FALSE)
  }))
  md_table(df)
}

# ---- AC5: real and published fits --------------------------------------------------

ac5_lines <- function() {
  out <- c()
  for (nm in names(res$real)) {
    r <- res$real[[nm]]
    out <- c(out, sprintf("#### %s (N = %d, m = %d)", nm, r$N, r$m), "",
             sprintf("E: accepted %s, T = %s, df = %d. Gaps: W %s, H %s T units.",
                     r$accepted, fmt(r$E$T), r$E$df, fmt(r$gap[["W"]]),
                     fmt(r$gap[["H"]])), "")
    rows <- list(E = r$E)
    for (x in c("W", "H")) if (!is.null(r[[x]])) rows[[x]] <- r[[x]]
    df <- do.call(rbind, lapply(names(rows), function(x) {
      e <- rows[[x]]
      # an angle on the 0/360 pole reads 360, as the engine reports it (D-003)
      th <- e$theta %% 360
      th[th < 1e-9 | 360 - th < 1e-9] <- 360
      data.frame(estimator = x, T = fmt(e$T), df = e$df,
                 angles = paste(fmt(th, 1), collapse = ", "),
                 zeta = paste(fmt(e$zeta, 3), collapse = ", "),
                 beta = paste(fmt(e$beta, 3), collapse = ", "),
                 order = paste(e$order, collapse = " "),
                 `order changed` = if (x == "E") "-" else e$order_changed,
                 check.names = FALSE)
    }))
    out <- c(out, md_table(df), "")
  }
  out
}

# ---- write --------------------------------------------------------------------------

gen <- c(
  "<!-- generated:begin (devel/m170-engine-estimate-summary.R) -->", "",
  "### Run", "", run_facts(), "",
  "### Shares of lower fits and the saturation verdict (AC2)", "",
  H_NOTE, "", ac2_table(), "",
  "### Failed starts, finish errors and gradient failures", "",
  counts_table(), "",
  "### W against E (AC3)", "", ac3_table("W"), "",
  "### H against E (AC3)", "", H_NOTE, "", ac3_table("H"), "",
  "### Estimate quality by family (AC4)", "", H_NOTE, "",
  ac4_family_table(), "",
  "### Estimate quality on the lower fits only", "", H_NOTE, "",
  paste("X is W or H. Errors are each fit's root mean square error over the",
        "7 free angles or the 4 weights, averaged over X's lower fits."), "",
  lower_fits_table(), "",
  "### Decision-rule inputs", "", decision_inputs(), "",
  "### Estimate quality by parameter (AC4)", "", H_NOTE, "",
  unlist(lapply(cells, function(k) {
    c(sprintf("#### %s", cell_label(k)), "", ac4_parameter_table(k), "")
  })),
  "### Real and published fits (AC5)", "", ac5_lines(),
  "<!-- generated:end -->"
)

txt <- if (file.exists(record)) readLines(record) else character(0)
b <- grep("^<!-- generated:begin", txt)
e <- grep("^<!-- generated:end -->", txt)
if (length(b) == 1 && length(e) == 1 && e > b) {
  txt <- c(txt[seq_len(b - 1)], gen, txt[-seq_len(e)])
} else {
  txt <- c(txt, gen)
}
writeLines(txt, record)
cat("wrote the generated part of", record, "\n")
