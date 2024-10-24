#' Save a ggplot with sensible defaults
#'
#' See \code{ggplot2} for details.
#'
#' @name ggsave
#' @rdname ggsave
#' @keywords internal
#' @export
#' @importFrom ggplot2 ggsave
NULL

# Compute differences between two sets of SSM parameters -----------------------
param_diff <- function(p1, p2) {
  stopifnot(is.numeric(p1))
  stopifnot(is.numeric(p2))
  pd <- p1 - p2
  pd[[5]] <- angle_dist(as_radian(p1[[5]]), as_radian(p2[[5]]))
  pd
}

# Reshape parameters from wide to long format ----------------------------------
reshape_params <- function(v, suffix) {
  # Convert vector to matrix
  out <- matrix(v, ncol = 6, byrow = TRUE)
  # Add column names
  colnames(out) <- paste0(c("e_", "x_", "y_", "a_", "d_", "fit_"), suffix)
  # Convert to data frame
  as.data.frame(out)
}

# Calculate angular distance ---------------------------------------------------
angle_dist <- function(x, y) {
  ((x - y + pi) %% (2 * pi)) - pi
}

# Convert degrees to ggplot's radian format ------------------------------------
ggrad <- function(v) {
  v <- as.numeric(v)
  (v - 90) * (-pi / 180)
}

# Convert percent number to a formatted string ---------------------------------
str_percent <- function(x, digits = 2) {
  paste0(floor(x * 10^(digits + 2)) / (10^digits), "%")
}

# Determine good max amplitude value for circle plot ---------------------------
pretty_max <- function(v) {
  amax <- max(v, na.rm = TRUE)
  options <- c(
    -5.00, -4.00, -3.00, -2.50, -2.00,
    -1.50, -1.25, -1.00, -0.75, -0.50,
    -0.25, -0.20, -0.15, -0.10, -0.05,
    0,
    0.05, 0.10, 0.15, 0.20, 0.25,
    0.50, 0.75, 1.00, 1.25, 1.50,
    2.00, 2.50, 3.00, 4.00, 5.00
  )
  if (amax < 0 ) {
    scalar <- 0.5
  } else {
    scalar <- 1.5
  }
  match <- options > amax * scalar
  if (sum(match) >= 1) {
    out <- options[match][[1]]
  } else {
    out <- amax * scalar
  }
  out
}

# Determine good min amplitude value for circle plot ---------------------------
pretty_min <- function(v) {
  amin <- min(v, na.rm = TRUE)
  options <- c(
    -5.00, -4.00, -3.00, -2.50, -2.00,
    -1.50, -1.25, -1.00, -0.75, -0.50,
    -0.25, -0.20, -0.15, -0.10, -0.05,
    0,
    0.05, 0.10, 0.15, 0.20, 0.25,
    0.50, 0.75, 1.00, 1.25, 1.50,
    2.00, 2.50, 3.00, 4.00, 5.00
  )
  if (amin < 0) {
    scalar <- 1.5
  } else {
    scalar <- 0.5
  }
  match <- options < amin * scalar
  if (sum(match) >= 1) {
    candidates <- options[match]
    out <- candidates[length(candidates)]
  } else {
    out <- amin * scalar
  }
  out
}


rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  min_to <- to[1]
  max_to <- to[2]

  min_from <- from[1]
  max_from <- from[2]
  
  (x - min_from) / (max_from - min_from) * (max_to - min_to) + min_to
}
