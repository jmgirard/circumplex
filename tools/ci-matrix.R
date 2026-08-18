# ci-matrix.R — emit the R-CMD-check job matrix for one workflow run.
#
# Usage:
#   Rscript tools/ci-matrix.R push
#   Rscript tools/ci-matrix.R pull_request <changed-files.txt>
#
# stdout: one JSON array of config objects, consumed by the workflow's
# matrix job via fromJSON(). Any problem stops with an error so the
# workflow run fails; there is no fallback to a smaller matrix (cairn M93).
# Base R only — this runs before any dependency install.

# The os/R-version config literals. Each literal appears here and nowhere
# else (not in .github/workflows/R-CMD-check.yaml); the three matrices
# below are composed from these entries.
CONFIGS <- list(
  macos_release   = c(os = "macos-latest",   r = "release"),
  windows_release = c(os = "windows-latest", r = "release"),
  ubuntu_devel    = c(os = "ubuntu-latest",  r = "devel", `http-user-agent` = "release"),
  ubuntu_release  = c(os = "ubuntu-latest",  r = "release"),
  ubuntu_oldrel   = c(os = "ubuntu-latest",  r = "oldrel-1")
)

MATRICES <- list(
  # push to the default branch: the full five-config matrix.
  push      = c("macos_release", "windows_release", "ubuntu_devel",
                "ubuntu_release", "ubuntu_oldrel"),
  # pull request touching the escalation set: the release platforms.
  escalated = c("windows_release", "macos_release", "ubuntu_release"),
  # any other pull request: the single fast job (cairn M51's economy).
  plain     = "ubuntu_release"
)

# PR-escalation path set. Entries ending in "/**" match any file under that
# directory; other entries match exactly.
ESCALATION_SET <- c(
  "R/**", "src/**", "tests/**", "vignettes/**", "data/**", "inst/**",
  "DESCRIPTION", "NAMESPACE", ".github/workflows/R-CMD-check.yaml",
  "tools/check-ci-deps.R", "tools/ci-matrix.R"
)

escalates <- function(files) {
  stems <- sub("/\\*\\*$", "/", grep("/\\*\\*$", ESCALATION_SET, value = TRUE))
  exact <- grep("/\\*\\*$", ESCALATION_SET, value = TRUE, invert = TRUE)
  hit_dir <- vapply(files, function(f) any(startsWith(f, stems)), logical(1))
  any(hit_dir) || any(files %in% exact)
}

as_json <- function(keys) {
  obj <- vapply(CONFIGS[keys], function(cfg) {
    fields <- sprintf('"%s":"%s"', names(cfg), unname(cfg))
    paste0("{", paste(fields, collapse = ","), "}")
  }, character(1))
  paste0("[", paste(obj, collapse = ","), "]")
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop("usage: ci-matrix.R <event> [<changed-files-file>]")
event <- args[[1L]]

keys <- if (event == "push") {
  MATRICES$push
} else if (event == "pull_request") {
  if (length(args) < 2L) stop("pull_request requires a changed-files file")
  path <- args[[2L]]
  if (!file.exists(path)) stop("changed-files file not found: ", path)
  files <- readLines(path, warn = FALSE)
  files <- files[nzchar(files)]
  # An empty list usually means the diff computation failed upstream
  # (paths-ignore already filters PRs with no relevant files; a PR whose
  # head equals its base is the benign case): fail, never assume plain.
  if (length(files) == 0L) stop("changed-files list is empty")
  # The pulls/N/files API silently truncates at 3000 files, so a list this
  # long may be missing the one member that escalates: refuse to classify.
  if (length(files) >= 3000L) stop("changed-files list at the API cap")
  if (escalates(files)) MATRICES$escalated else MATRICES$plain
} else {
  # Deliberately fail-closed: adding a trigger to the workflow (merge_group,
  # workflow_dispatch, ...) requires giving it a matrix here first.
  stop("unknown event: ", event)
}

cat(as_json(keys), "\n", sep = "")
