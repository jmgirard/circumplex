# M16 T5: verify the scale-free certification rule (D-007) against AC4's
# two-part acceptance gate, using ssm_ci_accuracy() -- which now measures the
# NEW rule automatically (single-definition doctrine; it calls ssm_certified()).
#
# Gate 1 (hard): false-certification at the c = 0 ladder rung <= 0.05 (point)
#                in every configuration.
# Gate 2 (caution): the diagnostic's stored Wilson-LCI Caution must NOT fire
#                   at c = 0 in any configuration.
# Oracle cross-check (2nd type): the closed-form isotropic-null Rayleigh tail
#   false-cert ~ exp(-t*^2 / 2), t* ~ z(1 + 2k) -- an independent deterministic
#   check on the simulation-coverage measurement (validation doctrine).
suppressMessages(devtools::load_all(quiet = TRUE))

REPS <- 1000
k    <- eval(formals(circumplex:::ssm_certified)$k)  # 0.35, single-sourced

set.seed(11)
jz100 <- jz2017[sample(nrow(jz2017), 100), ]

configs <- list(
  COR_healthy  = function() ssm_analyze(jz2017, scales = PANO(), measures = "ASPD", boots = 500),
  COR_nearzero = function() ssm_analyze(jz2017, scales = PANO(), measures = "OCPD", boots = 500),
  RAW_means    = function() ssm_analyze(jz2017, scales = PANO(), boots = 500),
  RAW_smalln   = function() ssm_analyze(jz100,  scales = PANO(), boots = 500),
  COR_smalln   = function() ssm_analyze(jz100,  scales = PANO(), measures = "ASPD", boots = 500)
)

rows <- list()
for (nm in names(configs)) {
  fit <- suppressWarnings(configs[[nm]]())
  set.seed(2026)
  acc <- suppressWarnings(ssm_ci_accuracy(fit, reps = REPS))
  g   <- acc$guardrail
  g0  <- g[g$Condition == 0, ]
  # power curve: certification rate at each c > 0 rung (as-estimated = 1)
  pc  <- g[g$Condition > 0, c("Condition", "Cert_rate")]
  rows[[nm]] <- list(
    fc0      = g0$Cert_rate,               # false-certification at c = 0
    fc0_lci  = g0$Cert_lci,
    caution  = isTRUE(g0$Caution),
    benchmark = g0$Benchmark,
    power    = setNames(pc$Cert_rate, paste0("c", pc$Condition))
  )
}

# Closed-form Rayleigh-tail oracle (isotropic null)
z       <- qnorm(0.975)
t_star  <- z * (1 + 2 * k)                 # ~ 3.332 at k = 0.35, 95% CIs
oracle  <- exp(-t_star^2 / 2)              # ~ 0.0039

saveRDS(list(reps = REPS, k = k, oracle = oracle, t_star = t_star, rows = rows),
        "devel/m16-cert-rule-verify.rds")

cat("\n===== M16 T5 VERIFICATION (reps =", REPS, ", k =", k, ") =====\n")
cat(sprintf("Closed-form Rayleigh oracle: t* = %.3f  false-cert ~ exp(-t*^2/2) = %.4f\n",
            t_star, oracle))
gate1_all <- TRUE; gate2_all <- TRUE
for (nm in names(rows)) {
  r <- rows[[nm]]
  g1 <- r$fc0 <= 0.05
  g2 <- !r$caution
  gate1_all <- gate1_all && g1
  gate2_all <- gate2_all && g2
  cat(sprintf("\n%-13s false-cert@c0 = %.3f (LCI %.3f, benchmark %.3f)  Caution=%s\n",
              nm, r$fc0, r$fc0_lci, r$benchmark, r$caution))
  cat(sprintf("  gate1 (<=0.05): %s   gate2 (Caution off): %s\n",
              if (g1) "PASS" else "FAIL", if (g2) "PASS" else "FAIL"))
  cat("  power curve (Cert_rate by c): ",
      paste(sprintf("%s=%.2f", names(r$power), r$power), collapse = "  "), "\n")
}
cat(sprintf("\n==== OVERALL: gate1 %s | gate2 %s ====\n",
            if (gate1_all) "PASS" else "FAIL", if (gate2_all) "PASS" else "FAIL"))
cat("Saved devel/m16-cert-rule-verify.rds\n")
