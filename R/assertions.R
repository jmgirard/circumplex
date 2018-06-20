# Check if argument is provided to function
is_provided <- function(x) {
  base::missing(x) == FALSE
}

assertthat::on_failure(is_provided) <- function(call, env) {
  paste0("The '", deparse(call$x), "' parameter must be provided.")
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