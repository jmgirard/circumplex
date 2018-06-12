is_provided <- function(x) {
  base::missing(x) == FALSE
}

assertthat::on_failure(is_provided) <- function(call, env) {
  paste0("The '", deparse(call$x), "' parameter must be provided.")
}