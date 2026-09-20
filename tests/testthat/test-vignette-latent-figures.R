# The "Advanced Circumplex Visualization" vignette's section on the latent
# circumplex draws three figures whose angles come from three different
# sources: a cpm_fit() estimate, the theoretical angles, and an SSM computed on
# the theoretical angles. The prose has to say which is which, or a reader
# takes a measure vector for a latent location. These four phrases carry that
# distinction, so each must stay in the shipped page verbatim.
#
# The rendered .Rmd is read (vignette_source() in helper-vignette.R), not the
# .Rmd.orig: the shipped page is what the reader sees. A build installed
# without vignettes (covr) skips.

latent_figure_phrases <- c(
  "the ticks are the angles `cpm_fit()` estimated, and the labelled spokes are the theoretical angles",
  "the spoke and the tick for the same scale come from different models, and the vectors come from a third: the SSM computed on the theoretical angles",
  "the points are observed correlations and the line is the fitted correlation function",
  "a vector's length is the measure's amplitude, not its correlation with the latent circumplex",
  # The ellipse subsection: what the ellipse is, how it differs from the
  # wedge, and what excluding the origin does and does not mean.
  "a joint region on the Cartesian coordinates under a normal approximation to the draws",
  "the wedge is the marginal circular-quantile interval on amplitude and displacement",
  "the two need not coincide",
  "an ellipse that excludes the origin is a Wald test of zero amplitude at that level, and only under that approximation",
  "the ellipse is centred on the plotted point estimate, and its covariance comes from the draws"
)

test_that("the latent-circumplex section states where each figure's angles come from", {
  path <- vignette_source("advanced-visualization.Rmd")
  skip_if(!nzchar(path), "vignette source unavailable in this build")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # Prose wraps across source lines, so the page is matched as one string with
  # runs of whitespace collapsed to one space.
  text <- gsub("\\s+", " ", paste(lines, collapse = " "))
  for (phrase in latent_figure_phrases) {
    expect_true(grepl(phrase, text, fixed = TRUE), info = phrase)
  }
})

test_that("the latent-circumplex section hides no chunk", {
  # Chunk options live only in the .Rmd.orig source; the rendered .Rmd carries
  # plain ```r fences. The source is .Rbuildignore'd, so R CMD check skips.
  path <- testthat::test_path("..", "..", "vignettes", "advanced-visualization.Rmd.orig")
  skip_if(!file.exists(path), "vignette .Rmd.orig source unavailable in this build")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  heads <- which(grepl("^## ", lines))
  start <- heads[grepl("^## [0-9]+\\. The latent circumplex from a CPM fit$", lines[heads])]
  expect_length(start, 1L)
  # A renamed heading has already failed above; stop before min(integer(0)).
  if (length(start) != 1L) return(invisible(NULL))
  end <- min(heads[heads > start]) - 1L
  section <- lines[seq(start, end)]
  fences <- section[grepl("^```\\{r", section)]
  # The section has to hold chunks at all, or the assertion below is vacuous.
  expect_gte(length(fences), 4L)
  expect_false(any(grepl("echo\\s*=\\s*FALSE|include\\s*=\\s*FALSE", fences)),
               info = paste(fences, collapse = "\n"))
})
