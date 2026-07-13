# M16 T1: RB seed — characterize the CURRENT certification rule
# round(a_lci, digits) > 0 across amplitude metrics x ladder.
suppressMessages(devtools::load_all(quiet = TRUE))

REPS <- 500
fits <- list(
  COR_healthy  = ssm_analyze(jz2017, scales = PANO(), measures = "ASPD", boots = 500),
  COR_nearzero = ssm_analyze(jz2017, scales = PANO(), measures = "OCPD", boots = 500),
  RAW_means    = ssm_analyze(jz2017, scales = PANO(), boots = 500)
)

seed <- list()
for (nm in names(fits)) {
  set.seed(2026)
  acc <- suppressWarnings(ssm_ci_accuracy(fits[[nm]], reps = REPS))
  g <- acc$guardrail
  cov <- acc$coverage
  amp <- cov[cov$Parameter == "a", ]
  res <- fits[[nm]]$results
  seed[[nm]] <- list(
    a_est = res$a_est, a_lci = res$a_lci, a_uci = res$a_uci,
    ci_width = res$a_uci - res$a_lci,
    lci_over_width = res$a_lci / (res$a_uci - res$a_lci),   # candidate scale-free stat
    threshold = g$Threshold[1],
    guardrail = g[, c("Condition", "Cert_rate", "Cert_lci", "Cert_uci",
                      "Benchmark", "Threshold")],
    amp_cov = amp[, c("Condition", "Coverage", "Median_width", "Structural")]
  )
}

saveRDS(list(reps = REPS, seed = seed), "devel/m16-cert-rule-seed.rds")

# Console tabulation
cat("\n=========== M16 CERT-RULE SEED (reps =", REPS, ") ===========\n")
for (nm in names(seed)) {
  s <- seed[[nm]]
  cat(sprintf("\n### %s\n", nm))
  cat(sprintf("as-estimated: a_est=%.4f  a_lci=%.4f  a_uci=%.4f  width=%.4f  a_lci/width=%.3f\n",
              s$a_est, s$a_lci, s$a_uci, s$ci_width, s$lci_over_width))
  cat(sprintf("fixed threshold (0.5*10^-3) = %.1e  ->  a_lci/threshold = %.0f\n",
              s$threshold, s$a_lci / s$threshold))
  cat("guardrail (current rule) across ladder:\n")
  print(s$guardrail, row.names = FALSE, digits = 4)
  cat("amplitude coverage across ladder:\n")
  print(s$amp_cov, row.names = FALSE, digits = 4)
}
cat("\nSaved devel/m16-cert-rule-seed.rds\n")
