# Check if an enquo argument was provided and is not null ----------------------
is_enquo <- function(x) {
  xq <- rlang::enquo(x)
  !rlang::quo_is_missing(xq) && !rlang::quo_is_null(xq)
}

assertthat::on_failure(is_enquo) <- function(call, env) {
  s <- deparse(call$x)
  paste0("The '", substr(s, 3, nchar(s) - 3), 
    "' argument must be provided and not NULL.")
}

# Check if a normal argument was provided and is not null ----------------------
is_provided <- function(x) {
  !(base::missing(x) || is.null(x))
}

assertthat::on_failure(is_provided) <- function(call, env) {
  paste0("The '", deparse(call$x), "' argument must be provided and not NULL.")
}