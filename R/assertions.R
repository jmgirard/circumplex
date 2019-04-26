# Check if an enquo argument was provided and is not null ----------------------
is_enquo <- function(x) {
  xq <- rlang::enquo(x)
  !rlang::quo_is_missing(xq) && !rlang::quo_is_null(xq)
}

assertthat::on_failure(is_enquo) <- function(call, env) {
  s <- deparse(call$x)
  paste0(
    "The '", substr(s, 3, nchar(s) - 3),
    "' argument must be provided and not NULL."
  )
}

# Check if a normal argument was provided and is not null ----------------------
is_provided <- function(x) {
  !(base::missing(x) || is.null(x))
}

assertthat::on_failure(is_provided) <- function(call, env) {
  paste0("The '", deparse(call$x), "' argument must be provided and not NULL.")
}


# Check if a numeric vector or single dimension data frame was provided --------
is_numvec <- function(x) {
  if (is.data.frame(x)) {
    all(sapply(x, is.numeric)) & (nrow(x) == 1 | ncol(x) == 1)
  } else if (is.matrix(x)) {
    is.numeric(x) & (nrow(x) == 1 | ncol(x) == 1)
  } else {
    is.numeric(x) & is.vector(suppressWarnings(as.numeric(x)))
  }
}

assertthat::on_failure(is_numvec) <- function(call, env) {
  paste0("The '", deparse(call$x), "' argument must be a numeric vector or a single-dimension data frame with only numeric variables.")
}