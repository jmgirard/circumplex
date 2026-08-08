# Shared helpers for the shipped-norms test files.

# The shipped instruments are enumerated by the same procedure instruments()
# uses -- data() plus a class filter -- rather than a hand-list, so a newly
# added instrument is caught by the tests that sweep this list instead of
# silently going unswept. (instruments() itself prints and returns NULL, so
# its return value cannot be used here.)
shipped_instruments <- function() {
  nms <- utils::data(package = "circumplex")$results[, "Item"]
  sort(Filter(function(nm) {
    e <- new.env()
    utils::data(list = nm, package = "circumplex", envir = e)
    inherits(get(nm, envir = e), "circumplex_instrument")
  }, nms))
}

# Fetch a shipped instrument object by name without attaching it.
shipped_instrument <- function(nm) {
  e <- new.env()
  utils::data(list = nm, package = "circumplex", envir = e)
  get(nm, envir = e)
}
