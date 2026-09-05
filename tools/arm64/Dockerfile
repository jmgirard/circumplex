# Reproduces CRAN's linux-arm64 additional-check flavor:
#   aarch64-unknown-linux-gnu, R 4.6.x, OpenBLAS reference LAPACK.
# Runs natively on Apple Silicon -- no emulation.
#
# Pinned by digest, not by tag: a harness that fails to build on the day of a
# resubmission is worse than one lagging CRAN by a month. See README.md for
# what this covers, what it does not, and how to refresh the pin.
FROM r-base@sha256:41d5564375009abf74a63987fd7fb9b44c90b1580b310be10ef973abe92496c3

# Deliberately narrow. r-base is Debian unstable and its index drifts out of
# sync with the image; asking for a large set (libcurl4-openssl-dev and
# friends) makes apt refuse the whole transaction. libuv1-dev is here because
# `fs` fails to configure without it, taking testthat and rmarkdown with it.
RUN apt-get update -qq && apt-get install -y --no-install-recommends libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

ENV MAKEFLAGS="-j8"

# One install per layer, each verified immediately: install.packages() only
# WARNS on a failed package, so an unverified layer bakes in a silent hole.
RUN Rscript -e 'options(Ncpus = 8L); p <- c("Rcpp", "RcppArmadillo", "rlang", "boot", "ggplot2", "htmlTable"); install.packages(p, repos = "https://cloud.r-project.org"); miss <- setdiff(p, rownames(installed.packages())); if (length(miss)) stop("failed: ", paste(miss, collapse = ", ")); stopifnot(packageVersion("ggplot2") >= "4.0.0")'

# The test runner, plus the Suggests the suite actually reaches for. brms,
# OpenMx and glmmTMB are deliberately out: hours to build, and their tests
# skip when absent. vdiffr wants system font headers and is likewise out --
# its snapshot tests self-skip.
RUN Rscript -e 'options(Ncpus = 8L); p <- c("testthat", "knitr", "rmarkdown", "lavaan", "psych", "ggrepel", "RColorBrewer"); install.packages(p, repos = "https://cloud.r-project.org"); miss <- setdiff(p, rownames(installed.packages())); if (length(miss)) stop("failed: ", paste(miss, collapse = ", "))'

RUN Rscript -e 'cat("R:", R.version.string, "\nplatform:", R.version$platform, "\nLAPACK:", La_library(), "\n")'

WORKDIR /pkg
