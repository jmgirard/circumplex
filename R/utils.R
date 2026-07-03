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
  # Displacement is angular, so its contrast is the signed angular distance
  # (second minus first), not a plain subtraction. Works for a single length-6
  # parameter vector (bootstrap replicate) or an R x 6 matrix of draws (Monte
  # Carlo), so both engines share one contrast convention.
  d <- which(ssm_param_names() == "d")
  if (is.matrix(p1)) {
    pd[, d] <- angle_dist(as_radian(p1[, d]), as_radian(p2[, d]))
  } else {
    pd[[d]] <- angle_dist(as_radian(p1[[d]]), as_radian(p2[[d]]))
  }
  pd
}

# Canonical SSM parameter names -----------------------------------------------
# The fixed order in which group_parameters()/ssm_parameters_cpp() (src/) emit
# each group's parameters: elevation, x, y, amplitude, displacement, fit. Single
# source of truth so the bootstrap assembly can locate parameters (notably
# displacement) by name rather than by positional arithmetic over six-blocks.
ssm_param_names <- function() {
  c("e", "x", "y", "a", "d", "fit")
}

# Reshape parameters from wide to long format ----------------------------------
reshape_params <- function(v, suffix) {
  pnames <- ssm_param_names()
  # One row per group; one column per parameter, named parameter_suffix
  out <- matrix(v, ncol = length(pnames), byrow = TRUE)
  colnames(out) <- paste(pnames, suffix, sep = "_")
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
  
  # What is the largest value?
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
  # If negative, decrease scalar (how much buffer space to add)
  if (amax < 0 ) {
    scalar <- 0.5
  } else {
    scalar <- 1.5
  }
  # Which options are good candidates?
  match <- options > amax * scalar
  # Are there any candidates?
  if (sum(match) >= 1) {
    # Take the smallest candidate 
    out <- options[match][[1]]
  } else {
    # Multiply the max and scalar
    out <- amax * scalar
  }
  out
}

# Determine good min amplitude value for circle plot ---------------------------
pretty_min <- function(v) {
  
  # What is the smallest value?
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
  # If negative, increase scalar (how much buffer space to add)
  if (amin < 0) {
    scalar <- 1.5
  } else {
    scalar <- 0.5
  }
  # Which options are candidates?
  match <- options < amin * scalar
  # Are there any candidates?
  if (sum(match) >= 1) {
    # Take the largest candidate
    candidates <- options[match]
    out <- candidates[length(candidates)]
  } else {
    # Multiply the min and scalar
    out <- amin * scalar
  }
  out
}


# Rescale numeric vector to specified min and max -------------------------
rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  min_to <- to[1]
  max_to <- to[2]

  min_from <- from[1]
  max_from <- from[2]
  
  (x - min_from) / (max_from - min_from) * (max_to - min_to) + min_to
}

# Assertions --------------------------------------------------------------

is_count <- function(x) {
  all(
    is.numeric(x),
    ceiling(x) == floor(x),
    x >= 0
  )
}

is_char <- function(x, n = NULL) {
  if (is.null(n)) {
    is.character(x)
  } else {
    stopifnot(is_count(n))
    is.character(x) && length(x) == n
  }
}

is_null_or_char <- function(x, n = NULL) {
  is.null(x) || is_char(x, n = n)
}

is_var <- function(x, n = NULL) {
  if (is.null(n)) {
    is.character(x) || is.numeric(x)
  } else {
    stopifnot(is_count(n))
    (is.character(x) || is.numeric(x)) && length(x) == n
  }
}

is_null_or_var <- function(x, n = NULL) {
  is.null(x) || is_var(x, n)
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1
}

is_num <- function(x, n = NULL) {
  if (is.null(n)) {
    is.numeric(x)
  } else {
    stopifnot(is_count(n))
    is.numeric(x) && length(x) == n
  }
}

is_null_or_num <- function(x, n = NULL) {
  is.null(x) || is_num(x, n)
}
