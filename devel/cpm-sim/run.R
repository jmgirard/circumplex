# CPM CI simulation study -- driver (plan sec. 7, sec. 10.5).
#
# Builds the config table, runs cells through a portable parallel backend
# (PSOCK on Windows -- the sec. 0.2 target box -- else fork/mclapply) created ONCE
# per stage (review S11), checkpoints each cell to its own RDS (resumable,
# sec. 7.2), summarizes at cluster level, and honors the pre-registered
# stage-2/3 selection rules AS CODE (sec. 3.4) -- admissions are computed from
# the prior stage's cache and turned into runnable derived cells. Per-replicate
# local seeding makes every result worker-count- and schedule-independent
# (sec. 7.1).
#
# ============================ NOT RUN =======================================
# Sourcing this file only DEFINES functions. It launches nothing unless invoked
# as a script with CPM_SIM_GO set:  CPM_SIM_GO=1 Rscript devel/cpm-sim/run.R
# No factorial cell, smoke, or benchmark has been executed (review fixes applied
# 2026-07-09; still un-run).
# ============================================================================

# ---- module loading ---------------------------------------------------------
sim_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f)))
  if (!is.null(getOption("cpm_sim_dir"))) return(getOption("cpm_sim_dir"))
  file.path(getwd(), "devel", "cpm-sim")
}

source_modules <- function(dir = sim_dir()) {
  for (m in c("common.R", "config.R", "intervals.R", "kernel.R", "summarize.R"))
    sys.source(file.path(dir, m), envir = globalenv())
}

# ---- per-stage MC parameters (sec. 6.1) -------------------------------------
stage_params <- function(stage, smoke = FALSE) {
  if (smoke) return(list(reps = 25L, boots = 200L, jack_g = 50L,
                         studentized = FALSE, levels = LEVELS))
  switch(stage,
    "1" = list(reps = 2000L, boots = 0L,    jack_g = 0L,   studentized = FALSE,
               levels = LEVELS),
    "2" = list(reps = 1000L, boots = 1000L, jack_g = 100L, studentized = FALSE,
               levels = LEVELS),
    "3" = list(reps = 500L,  boots = 1000L, jack_g = 100L, studentized = FALSE,
               levels = LEVELS),
    stop("unknown stage ", stage))
}

# ---- parallel backend (created once per stage; sec. 7.1, review S11) --------
make_backend <- function(cores) {
  if (.Platform$OS.type != "windows") return(NULL)          # fork/mclapply
  cl <- parallel::makeCluster(cores, type = "PSOCK")
  dir <- sim_dir(); pkg <- getOption("cpm_sim_pkg", ".")
  parallel::clusterCall(cl, function(d, p) {
    options(cpm_sim_pkg = p, cpm_sim_dir = d)
    for (m in c("common.R", "config.R", "intervals.R", "kernel.R", "summarize.R"))
      sys.source(file.path(d, m), envir = globalenv())
  }, dir, pkg)
  cl
}
close_backend <- function(backend) if (!is.null(backend)) parallel::stopCluster(backend)

run_parallel <- function(reps, fn, cores, backend) {
  if (!is.null(backend)) parallel::parLapplyLB(backend, seq_len(reps), fn)
  else parallel::mclapply(seq_len(reps), fn, mc.cores = cores,
                          mc.preschedule = FALSE)
}

# ---- stable per-cell index (drives replicate seeds; resumable) --------------
# Config cells keep their table position; derived (admitted) cells get indices
# in a persisted registry so seeds are stable across resumes and stages.
cell_index_of <- function(id, master_ids, registry_file) {
  m <- match(id, master_ids)
  if (!is.na(m)) return(m)
  reg <- if (file.exists(registry_file)) readRDS(registry_file) else character(0)
  if (!(id %in% reg)) { reg <- c(reg, id); saveRDS(reg, registry_file) }
  length(master_ids) + match(id, reg)
}

# ---- run + checkpoint one cell ----------------------------------------------
run_cell <- function(cell, params, cores, backend, cache_dir,
                    save_records = TRUE, force = FALSE) {
  f <- file.path(cache_dir, cell$stage, paste0(cell$id, ".rds"))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(f) && !force) return(readRDS(f))          # resume (sec. 7.2)

  fn <- function(i) tryCatch(fit_and_score(cell, i, params),
                             error = function(e) NULL)
  out <- run_parallel(params$reps, fn, cores, backend)
  # fork-level failures can return try-error objects, not NULL (review S8)
  err <- vapply(out, function(x) is.null(x) || inherits(x, "try-error"),
                logical(1))
  records <- out[!err]
  summ <- summarize_cell(cell, records, n_error = sum(err))
  payload <- list(summary = summ, cell_meta = cell[c("id", "stage", "arm",
                  "spec_note", "N", "boundary_status", "well_defined")],
                  n_error = sum(err), reps = params$reps, boots = params$boots,
                  base_seed = BASE_SEED, date = Sys.Date(),
                  sessionInfo = utils::sessionInfo(),
                  la_library = tryCatch(La_library(), error = function(e) NA))
  # Per-fit records are written per cell by default (the plan's storage posture,
  # sec. 7.1; review M4); disable with CPM_SIM_NO_RECORDS for a summary-only run.
  if (save_records) payload$records <- records             # large, uncommitted
  saveRDS(payload, f)
  payload
}

# ---- pre-registered selection rules (sec. 3.4) ------------------------------
# Stage-2 selection scalar for a stage-1 cell: max DOWNWARD deviation from
# nominal of the cluster coverage estimate across the angle + zeta families.
selection_scalar <- function(summary, families = c("theta", "zeta"),
                            level = PRIMARY_LEVEL) {
  cp <- summary$cov_primary
  if (is.null(cp)) return(NA_real_)
  sub <- cp[cp$family %in% families & cp$method == "wald", ]  # stage 1 = Wald
  if (!nrow(sub)) return(NA_real_)
  max(pmax(level - sub$est, 0), na.rm = TRUE)
}

# Per-factor-axis admission (sec. 3.4): admit the worst-ranked stage-1 cell at
# each factor-axis level, in overall rank order, dedup, until the cap binds --
# so every factor gets bootstrap evidence where it looks worst (review M6).
# Excludes cells already represented in the stage-2 core and analytic-only-N
# cells the bootstrap budget cannot carry.
stage2_admissions <- function(stage1_summaries, config, cap = 12L) {
  cells_by_id <- setNames(config$cells, vapply(config$cells, `[[`, "", "id"))
  is_core_equiv <- function(id) {
    c <- cells_by_id[[id]]
    isTRUE(c$arm == "core" && c$angle_set %in% c("p8_equal", "p8_perturbed") &&
             all(c$zeta0 == 0.75) && c$variant_fit == "A" &&
             c$m_fit == c$m0 && c$N %in% N_FIELD)
  }
  ids <- names(stage1_summaries)
  cand <- ids[vapply(ids, function(id) !is.null(cells_by_id[[id]]) &&
                       cells_by_id[[id]]$N %in% N_FIELD && !is_core_equiv(id),
                     logical(1))]
  sc <- setNames(vapply(cand, function(id)
    selection_scalar(stage1_summaries[[id]]), 0), cand)
  sc <- sc[is.finite(sc)]
  if (!length(sc)) return(character(0))
  axes <- c("angle_set", "N", "arm", "spec_note", "boundary_status")
  per_axis <- unique(unlist(lapply(axes, function(ax) {
    lv <- vapply(names(sc), function(id)
      as.character(cells_by_id[[id]][[ax]]), "")
    unname(tapply(names(sc), lv, function(g) g[which.max(sc[g])]))
  })))
  Nof <- vapply(per_axis, function(id) cells_by_id[[id]]$N, 0)
  head(per_axis[order(-sc[per_axis], Nof)], cap)                # worst, small N
}

# Stage-3 studentized arm: <= 8 cells ranked by the selection scalar on stage-2
# PERCENTILE beta coverage (worst first; sec. 3.4b/4.4).
studentized_cells <- function(stage2_summaries, cap = 8L) {
  ids <- names(stage2_summaries)
  worst <- vapply(ids, function(id) {
    cp <- stage2_summaries[[id]]$cov_primary
    row <- cp[cp$family == "beta" & cp$method == "percentile", ]
    if (!nrow(row)) return(NA_real_)
    max(PRIMARY_LEVEL - row$est, 0)
  }, 0)
  Nof <- vapply(ids, function(id) stage2_summaries[[id]]$N, 0)
  ok <- is.finite(worst)
  head(ids[ok][order(-worst[ok], Nof[ok])], cap)
}

# Turn a base config cell into a runnable derived bootstrap cell.
derive_cell <- function(base, new_id, stage, extra_flags = list()) {
  base$id <- new_id; base$stage <- stage; base$bootstrap <- TRUE
  base$flags <- utils::modifyList(base$flags, extra_flags)
  base
}

# Load a stage's cached per-cell summaries into a named list id -> summary.
load_stage_summaries <- function(stage, cache_dir) {
  d <- file.path(cache_dir, stage)
  if (!dir.exists(d)) return(list())
  files <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
  out <- lapply(files, function(f) readRDS(f)$summary)
  setNames(out, vapply(out, `[[`, "", "id"))
}

# ---- stage-0 throughput benchmark (sec. 3.4/7.2) ----------------------------
benchmark_stage0 <- function(config, cores, backend) {
  cell <- Filter(function(c) c$stage == "2" && isTRUE(c$bootstrap),
                 config$cells)[[1]]
  cell$cell_index <- 0L
  pb <- stage_params("2")
  n_bench <- max(cores, 16L)
  t0 <- Sys.time()
  bench <- run_parallel(n_bench, function(i) tryCatch(
    fit_and_score(cell, i, list(boots = pb$boots, jack_g = pb$jack_g,
                                studentized = FALSE, levels = PRIMARY_LEVEL)),
    error = function(e) NULL), cores, backend)
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  n_ok <- sum(!vapply(bench, function(x) is.null(x) || inherits(x, "try-error"),
                      logical(1)))
  n_fits_each <- 1L + pb$boots + pb$jack_g
  agg <- n_ok * n_fits_each / dt
  cat(sprintf("stage-0 benchmark: %d ok fits x %d = %d engine fits in %.1fs => ~%.0f fits/s\n",
              n_ok, n_fits_each, n_ok * n_fits_each, dt, agg))
  cat(sprintf("  3-5 day target needs ~>=350 fits/s; this box: %s\n",
              if (agg >= 350) "AFFORDABLE as written" else "apply sec. 7.2 knobs"))
  invisible(agg)
}

# ---- stage runner (takes an explicit resolved cell list) --------------------
run_stage <- function(stage, cells, cores, backend, cache_dir, master_ids,
                     smoke = FALSE, cell_filter = NULL, save_records = TRUE,
                     force = FALSE) {
  params <- stage_params(stage, smoke)
  registry <- file.path(cache_dir, "cell-index-registry.rds")
  if (!is.null(cell_filter))
    cells <- Filter(function(c) c$id %in% cell_filter, cells)
  results <- list()
  for (cell in cells) {
    cell$cell_index <- cell_index_of(cell$id, master_ids, registry)
    if (isTRUE(cell$flags$force_bootstrap)) cell$bootstrap <- TRUE
    p <- params
    if (isTRUE(cell$flags$studentized)) p$studentized <- TRUE
    if (isTRUE(cell$flags$boots2000)) p$boots <- 2000L
    if (!is.null(cell$flags$reps)) p$reps <- min(cell$flags$reps, SEED_MAX_I)
    cat(sprintf("[%s] stage %s %s (N=%d, reps=%d, boots=%d) ...\n",
                format(Sys.time(), "%H:%M:%S"), stage, cell$id, cell$N,
                p$reps, if (cell$bootstrap) p$boots else 0L))
    results[[cell$id]] <- run_cell(cell, p, cores, backend, cache_dir,
                                   save_records, force)
  }
  results
}

# ---- assemble the runnable cell list for a stage ----------------------------
# Stage 1: all config stage-1 cells. Stage 2: config stage-2 core (incl. the
# B-sensitivity cell) + admitted derived cells from stage-1 cache. Stage 3:
# config stage-3 fixed/validation cells + studentized derived from stage-2.
stage_cells <- function(stage, config, cache_dir) {
  by_stage <- Filter(function(c) c$stage == stage, config$cells)
  cells_by_id <- setNames(config$cells, vapply(config$cells, `[[`, "", "id"))
  if (stage == "1") return(by_stage)
  if (stage == "2") {
    s1 <- load_stage_summaries("1", cache_dir)
    adm <- if (length(s1)) stage2_admissions(s1, config) else character(0)
    derived <- lapply(adm, function(id)
      derive_cell(cells_by_id[[id]], paste0("s2adm_", id), "2"))
    return(c(by_stage, derived))
  }
  if (stage == "3") {
    s2 <- load_stage_summaries("2", cache_dir)
    stud <- if (length(s2)) studentized_cells(s2) else character(0)
    # studentized derived cells come from the stage-2 base configs
    derived <- lapply(stud, function(id) {
      base <- cells_by_id[[sub("^s2adm_", "", id)]]
      if (is.null(base)) return(NULL)
      derive_cell(base, paste0("s3stud_", id), "3", list(studentized = TRUE))
    })
    derived <- Filter(Negate(is.null), derived)
    return(c(by_stage, derived))
  }
  by_stage
}

# ---- main -------------------------------------------------------------------
main <- function() {
  options(cpm_sim_pkg = normalizePath(Sys.getenv("CPM_SIM_PKG", "."),
                                      mustWork = FALSE))         # hygiene
  source_modules()
  cores <- as.integer(Sys.getenv("CPM_SIM_CORES",
                                 as.character(max(1L, parallel::detectCores() - 1L))))
  smoke <- nzchar(Sys.getenv("CPM_SIM_SMOKE"))
  cache_dir <- Sys.getenv("CPM_SIM_CACHE", file.path(sim_dir(), "cache"))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  save_records <- !nzchar(Sys.getenv("CPM_SIM_NO_RECORDS"))     # default TRUE (M4)
  cell_filter <- if (nzchar(Sys.getenv("CPM_SIM_CELLS")))
    strsplit(Sys.getenv("CPM_SIM_CELLS"), ",")[[1]] else NULL

  cat("building config table ...\n")
  config <- build_config_table(include_oof = !smoke,
                               cache_file = file.path(cache_dir, "config-table.rds"))
  master_ids <- vapply(config$cells, `[[`, "", "id")

  mode <- Sys.getenv("CPM_SIM_MODE", "benchmark")   # benchmark|1|2|3|all
  backend <- make_backend(cores)
  on.exit(close_backend(backend))

  if (mode == "benchmark") { benchmark_stage0(config, cores, backend); return(invisible()) }

  stages <- if (mode == "all") c("1", "2", "3") else strsplit(mode, ",")[[1]]
  for (st in stages) {
    cat(sprintf("\n==== stage %s ====\n", st))
    cells <- stage_cells(st, config, cache_dir)
    run_stage(st, cells, cores, backend, cache_dir, master_ids, smoke = smoke,
              cell_filter = cell_filter, save_records = save_records)
  }
  cat("done.\n")
}

# Guard: only run when invoked as a script AND explicitly enabled.
if (identical(environment(), globalenv()) && !interactive() &&
    nzchar(Sys.getenv("CPM_SIM_GO"))) {
  main()
}
