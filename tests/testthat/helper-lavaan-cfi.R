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
# only what it actually got. It never assumes: a spelling that errors, or
# returns anything that is not a single finite number, is treated as not
# available and the next one is tried; a spelling that merely WARNS while
# returning a usable number is used, its warning muffled.
# NULL means neither worked, and the caller skips -- the assertions
# at each call site already pin the behaviour without lavaan's help, and this
# is a second opinion against the reference implementation, not the check.
#
# `f` is an argument so a test can hand in a stand-in accepting only one
# spelling; that is how the fallback arm is exercised on a lavaan that has
# already renamed past it (see test-lavaan-cfi-helper.R).

lav_cfi_ref <- function(x2, df, x2_null, df_null, f = lav_fit_cfi_fn()) {
  if (!is.function(f)) return(NULL)
  probe <- function(thunk) {
    # The SHAPE checks run inside the tryCatch too. Outside it, a length-one
    # list or any other type is.finite() has no method for turns an
    # unavailable spelling into an error in the caller -- reddening the suite
    # for a reason that is not about this package, which is exactly what the
    # skip design exists to prevent (M107 review).
    #
    # A warning is DEMOTED, not treated as unavailability: withCallingHandlers
    # muffles it and keeps the value. A deprecation warning on a function
    # still returning the right number must not silently kill the
    # corroboration -- the M107 failure with a likelier trigger (M107 review).
    tryCatch(
      withCallingHandlers(
        {
          out <- thunk()
          if (length(out) != 1L || !is.numeric(out) || !is.finite(out)) {
            NULL
          } else {
            unname(out)
          }
        },
        warning = function(w) invokeRestart("muffleWarning")
      ),
      error = function(e) NULL
    )
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
