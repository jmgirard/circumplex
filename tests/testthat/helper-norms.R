# Shared helpers for the shipped-norms test files.

# The shipped instruments are enumerated by the package's own sweep -- data()
# plus a class filter -- rather than a hand-list, so a newly added instrument
# is caught by the tests that sweep this list instead of silently going
# unswept. These are thin wrappers rather than copies (M79): the sweep had been
# written out separately here and in instruments(), and a third copy in
# data-raw/audit-norms.R would have given the audit a roster that could drift
# from the one the tests use. Wrappers, not aliases, because the test-facing
# names are what the sweeping test files read.
shipped_instruments <- function() {
  circumplex:::instrument_names()
}

# Fetch a shipped instrument object by name without attaching it.
shipped_instrument <- function(nm) {
  circumplex:::instrument_object(nm)
}

# The probe is deliberately shaped from the instrument itself rather than
# hand-written, so the sweeps over it cover every shipped instrument whatever
# its scale count or naming.
disclosure_probe <- function(obj) {
  probe <- as.data.frame(matrix(2, nrow = 2, ncol = nrow(obj$Scales)))
  names(probe) <- obj$Scales$Abbrev
  probe
}
