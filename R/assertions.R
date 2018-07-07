# Check if an argument was provided and is not null ----------------------------
is_provided <- function(x) {
  xq <- rlang::enquo(x)
  !rlang::quo_is_missing(xq) && !rlang::quo_is_null(xq)
}

assertthat::on_failure(is_provided) <- function(call, env) {
  paste0("The '", deparse(call$x), "' parameter must be provided and not NULL.")
}