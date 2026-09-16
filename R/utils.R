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
  # The C++ ssm_parameters_cpp()/group_parameters() emit one value per entry in
  # ssm_param_names(); a length that is not a whole multiple means the C++
  # parameter count and ssm_param_names() have drifted out of sync (which would
  # otherwise misalign every column). The contract is pinned in test-RcppExports.R.
  stopifnot(length(v) %% length(pnames) == 0)
  # One row per group; one column per parameter, named parameter_suffix
  out <- matrix(v, ncol = length(pnames), byrow = TRUE)
  colnames(out) <- paste(pnames, suffix, sep = "_")
  as.data.frame(out)
}

# Calculate angular distance ---------------------------------------------------
# Shortest signed rotation from y to x on the principal branch (-pi, pi], per
# the contrast convention (second minus first, reported in (-180, 180]). The
# plain wrap ((x - y + pi) %% 2pi) - pi has range [-pi, pi), so an exact
# half-turn lands on -pi; the convention requires +pi. Remapping the exact -pi
# atom is safe because the wrap yields exactly -pi only when x - y is exactly an
# odd multiple of pi, which no genuine near-boundary (non-half-turn) contrast
# ever produces -- so nothing legitimate is flipped. This catches the float-exact
# half-turn (e.g. raw sign-flipped atan2 displacements, which are bit-exact
# +/-pi). A true half-turn that upstream wrapping leaves 1-2 ulp off the atom is
# not remapped and simply reports just inside the branch (e.g. -179.9999...deg,
# which never rounds to -180) -- also correct. NA is preserved.
angle_dist <- function(x, y) {
  d <- ((x - y + pi) %% (2 * pi)) - pi
  d[!is.na(d) & d == -pi] <- pi
  d
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

# A single non-negative whole number: the scalar sibling of is_count(). Unlike
# is_count() (a vectorized non-negative-integer test used only as the internal
# `n=` guard in is_char/is_var/is_num), this bakes in length-1 the way is_flag()
# does, so user-facing count arguments (reps, boots, ncpus, digits, sample n)
# validate with one predicate instead of a hand-bolted `length(x) == 1`. `min`
# sets the floor: 1L for a positive count, 0L where zero is allowed (digits).
# Returns FALSE (never NA) for NA, length != 1, non-numeric, or non-integer.
is_scalar_count <- function(x, min = 1L) {
  is.numeric(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    ceiling(x) == floor(x) &&
    x >= min
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

# The reader-facing phrase for a stored reference-kind token. One mapping for
# both surfaces that report a kind -- norms() and norm_standardize()'s
# disclosure -- so the two cannot drift into describing the same sample
# differently. An unrecognized token returns NA rather than a plausible-looking
# phrase: the controlled vocabulary is pinned by the test suite, so reaching
# this branch means the data went somewhere the vocabulary does not cover, and
# printing a guess there would hide it.
# Display columns, not characters or bytes. The printed cautions carry "²",
# "ζ" and "ℹ", whose column count is what a console line budget is
# spent in, so every width comparison below goes through this one function.
disp_width <- function(x) {
  nchar(x, type = "width")
}

# Wrap prose to the reader's console width and return the finished lines.
#
# The caller cats what this returns, so one wrapping rule serves every caution
# and note the package prints, and the rule can be tested without capturing
# console output. Width defaults to getOption("width"), which is what a reader
# sets to tell R how wide their console is.
#
# `prefix` is placed before the first line and `continuation` before every
# later line. Both are counted AGAINST the width, never added on top of it, so
# an indented or labeled block stays inside the budget. A fixed-width label
# field is expressed as a `prefix` of that width with a `continuation` of the
# same width in spaces.
#
# With `atomic = FALSE`, `x` is prose and a break may fall between any two
# words. With `atomic = TRUE`, each element of `x` is a unit that must not
# split: a break falls only between elements. The bootstrap marker note uses
# the atomic form, because a marker label is taught as a unit and reads as one
# name only while it stays on one line.
#
# A single word wider than the room left on a line is placed on its own line
# rather than split, so that line can exceed the width. Breaking a word would
# change the words the reader sees, which matters more than the column.
wrap_prose <- function(x, prefix = "", continuation = prefix,
                       width = getOption("width"), atomic = FALSE) {
  stopifnot(
    is_char(x),
    is_char(prefix, n = 1),
    is_char(continuation, n = 1),
    is_flag(atomic)
  )
  if (!is_scalar_count(width)) width <- 80L

  pieces <- if (atomic) {
    x
  } else {
    unlist(strsplit(paste(x, collapse = " "), "[ \t\r\n]+"))
  }
  pieces <- pieces[nzchar(pieces)]
  if (length(pieces) == 0) return(character(0))

  bodies <- character(0)
  current <- pieces[[1]]
  for (piece in pieces[-1]) {
    lead <- if (length(bodies) == 0) prefix else continuation
    room <- max(width - disp_width(lead), 1)
    candidate <- paste(current, piece)
    if (disp_width(candidate) <= room) {
      current <- candidate
    } else {
      bodies <- c(bodies, current)
      current <- piece
    }
  }
  bodies <- c(bodies, current)

  leads <- c(prefix, rep(continuation, length(bodies) - 1))
  paste0(leads[seq_along(bodies)], bodies)
}

# The same wrapping, printed. Every caution site that has nothing to add
# between the lines uses this, so the newline handling is written once.
cat_prose <- function(x, prefix = "", continuation = prefix,
                      width = getOption("width"), atomic = FALSE) {
  lines <- wrap_prose(
    x,
    prefix = prefix,
    continuation = continuation,
    width = width,
    atomic = atomic
  )
  if (length(lines) > 0) cat(lines, sep = "\n")
  if (length(lines) > 0) cat("\n")
  invisible(lines)
}

norm_kind_phrase <- function(kind) {
  phrases <- c(
    standardization = "standardization sample",
    published = "identified published source",
    unsourced = "no identified source"
  )
  unname(phrases[match(as.character(kind), names(phrases))])
}
