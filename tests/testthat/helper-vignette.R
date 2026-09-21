# Where a vignette's source can be read from, in each build the suite runs in.
#
# devtools::test() reads the source tree. R CMD check runs the tests from
# <pkg>.Rcheck/tests, whose "../.." holds the INSTALLED package and not the
# sources, so there the vignette is reachable only through inst/doc, which
# R CMD build populates from vignettes/. A guard that knows only the
# source-tree path therefore skips in the gate that ships -- the M7 lesson,
# and the reason this helper exists.
#
# Returns the first readable path, or "" when neither exists: some builds
# install the package without vignettes (covr does), and a caller must skip
# there rather than fail.
vignette_source <- function(file) {
  candidates <- c(
    testthat::test_path("..", "..", "vignettes", file),
    system.file("doc", file, package = "circumplex")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit) == 0L) "" else hit[[1]]
}

# The code of one named chunk of a vignettes/<name>.Rmd.orig source, as a
# character vector of lines, so a test can evaluate what the page echoes.
# Chunk labels live only in the .orig (the rendered .Rmd carries plain ```r
# fences), and the .orig is .Rbuildignore'd, so a caller skips when the file
# is absent. A label must open exactly one chunk.
vignette_chunk <- function(file, label) {
  path <- testthat::test_path("..", "..", "vignettes", file)
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  open <- which(grepl(paste0("^```\\{r ", label, "(,|\\})"), lines))
  if (length(open) != 1L) {
    stop("chunk `", label, "` opens ", length(open), " times in ", file)
  }
  close <- which(lines == "```" & seq_along(lines) > open)[1]
  lines[seq(open + 1L, close - 1L)]
}
