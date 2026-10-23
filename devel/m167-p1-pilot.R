# M167 T12: the timing pilot of P1's stop rule (M167-D7). P1 stays in the
# verdict only if its median CPU time over these 10 fits is at most 6
# seconds per fit, measured on an otherwise idle core. The fits were fixed
# before the run: for each truth, the first accepted replicate of the
# oracle's raw-data cells at N = 250, 500 and 1000 and of its
# correlation-matrix cells at N = 2000 and 20000, drawn with the oracle's own
# seeds and P1 seed (devel/m167-interval-oracle.R).
#
# Usage: Rscript devel/m167-p1-pilot.R   (a few minutes; one process)
# Writes devel/m167-p1-pilot.rds.

devtools::load_all(".", quiet = TRUE)
source(file.path("devel", "m167-interval-helpers.R"))

angles <- octants()
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
arad <- as.numeric(as_radian(as_degree(angles)))
betas <- list(boundary = c(.45, .35, .15, .05), interior = c(.35, .30, .20, .15))
NS_RAW <- c(250, 500, 1000)
NS_COR <- c(2000, 5000, 20000)
SEED_M4 <- 20260706
SEED_NEW <- 20261021

cells <- expand.grid(N = c(250, 500, 1000, 2000, 20000),
                     truth = names(betas), stringsAsFactors = FALSE)
rows <- list()
for (j in seq_len(nrow(cells))) {
  tn <- cells$truth[j]
  N <- cells$N[j]
  raw <- N %in% NS_RAW
  ti <- match(tn, names(betas))
  truth_fit <- cpm_fit(cormat = cpm_implied_cor(arad, rep(0.75, 8), betas[[tn]]),
                       scales = scales, angles = angles, n = 10000, m = 3)
  for (i in 1:20) {
    rep_seed <- (if (raw) SEED_M4 else SEED_NEW) + 1e6 * ti +
      1e3 * match(N, if (raw) NS_RAW else NS_COR) + i
    set.seed(rep_seed)
    R <- if (raw) {
      stats::cor(cpm_simulate(truth_fit, N))
    } else {
      stats::cov2cor(stats::rWishart(1, N - 1, truth_fit$matrices$Phat)[, , 1])
    }
    eng <- try(suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                           variant = "A")), silent = TRUE)
    if (inherits(eng, "try-error") || !isTRUE(eng$accepted)) next
    t0 <- proc.time()
    pr <- suppressWarnings(m167_profile_ci(eng, R, N, seed = rep_seed + 5e8))
    dt <- proc.time() - t0
    rows[[j]] <- data.frame(truth = tn, N = N, replicate = i,
                            cpu_s = dt[["user.self"]] + dt[["sys.self"]],
                            elapsed_s = dt[["elapsed"]], runs = pr$diag$runs,
                            branches = pr$diag$n_branches,
                            unverified = sum(pr$diag$components$unverified),
                            heywood = isTRUE(eng$heywood))
    cat(sprintf("%-8s N=%5d rep %2d  cpu %.2f s  runs %d\n", tn, N, i,
                rows[[j]]$cpu_s, pr$diag$runs))
    break
  }
}
pilot <- do.call(rbind, rows)
med <- stats::median(pilot$cpu_s)
cat(sprintf("\nmedian CPU per fit: %.2f s (rule: at most 6) -> %s\n", med,
            if (med <= 6) "PASS" else "FAIL"))
saveRDS(list(pilot = pilot, median_cpu_s = med, pass = med <= 6,
             load = system("uptime", intern = TRUE),
             commit = system("git rev-parse HEAD", intern = TRUE),
             git_status = system("git status --porcelain", intern = TRUE),
             date = Sys.Date()),
        file.path("devel", "m167-p1-pilot.rds"))
