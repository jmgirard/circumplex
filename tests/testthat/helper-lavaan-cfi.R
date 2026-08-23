# Corroborating CFI against lavaan's own implementation.
#
# `lav_fit_cfi()` is UNEXPORTED, so neither its existence nor its argument
# names are a contract. Two spellings are known to exist: the current one
# (`x2`, `df`, `x2_null`, `df_null`; lavaan 0.7.2) and an older one (`X2`,
# `df`, `X2.null`, `df.null`), and the tests were written against the older
# set. When lavaan renamed them the calls started erroring, the surrounding
# tryCatch swallowed it, and all three comparisons skipped without anyone
# noticing -- corroboration that silently stopped corroborating (M107; the
# same failure the M68 round-2 lesson names, one rename later).
#
# So the CALL is what is probed, both spellings tried, and the helper reports
# only what it actually got. It never assumes: a spelling that errors, warns,
# or returns a non-finite value is treated as not available and the next one
# is tried. NULL means neither worked, and the caller skips -- the assertions
# at each call site already pin the behaviour without lavaan's help, and this
# is a second opinion against the reference implementation, not the check.
#
# `f` is an argument so a test can hand in a stand-in accepting only one
# spelling; that is how the fallback arm is exercised on a lavaan that has
# already renamed past it (see test-lavaan-cfi-helper.R).

lav_cfi_ref <- function(x2, df, x2_null, df_null, f = lav_fit_cfi_fn()) {
  if (!is.function(f)) return(NULL)
  probe <- function(thunk) {
    out <- tryCatch(thunk(), error = function(e) NULL, warning = function(w) NULL)
    if (is.null(out) || length(out) != 1L || !is.finite(out)) NULL else unname(out)
  }
  got <- probe(function() {
    f(x2 = x2, df = df, x2_null = x2_null, df_null = df_null)
  })
  if (is.null(got)) {
    got <- probe(function() {
      f(X2 = x2, df = df, X2.null = x2_null, df.null = df_null)
    })
  }
  got
}

lav_fit_cfi_fn <- function() {
  if (!requireNamespace("lavaan", quietly = TRUE)) return(NULL)
  get0("lav_fit_cfi", envir = asNamespace("lavaan"), ifnotfound = NULL)
}
