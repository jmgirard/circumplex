# Check if a valid argument was provided ---------------------------------------
is_provided <- function(x) {
  if (missing(x)) {
    # If missing, then FALSE
    FALSE
  } else if (rlang::is_null(x)) {
    # If null, then FALSE
    FALSE
  } else if (rlang::is_quosure(x)) {
    if (rlang::quo_is_missing(x)) {
      # If quosure but missing, then FALSE
      FALSE
    } else if (rlang::quo_is_null(x)) {
      # If quosure but null, then FALSE
      FALSE
    } else {
      # If quosure but not missing or null, then TRUE
      TRUE
    }
  } else {
    # If not missing, null, or quosure, then TRUE
    TRUE
  }
}

assertthat::on_failure(is_provided) <- function(call, env) {
  paste0("The '", deparse(call$x), "' parameter must be provided and not NULL.")
}

# Check if filename is a string with correct extension
is_html_fn <- function(x) {
  s <- is.string(x)
  l <- stringr::str_sub(x, -5, -1) == ".html"
  m <- stringr::str_sub(x, -4, -1) == ".htm"
  s && (l || m)
}

assertthat::on_failure(is_html_fn) <- function(call, env) {
  paste0("The 'filename' parameter must be a string that ends with '.html'")
}