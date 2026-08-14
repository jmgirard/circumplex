# The abort spellings the audit script must not acquire (M82, RR17 rev 2 BC6).
#
# M81's walk collects `stop()` and `stopifnot()` calls and says so: everything
# else is outside its promise, including the spellings that would raise just as
# hard. That bound is honest but it is not stable -- nothing stopped the script
# from growing an `rlang::abort()`, a `do.call("stop", ...)` or a `fail <- stop`
# alias tomorrow, and any of the three is an abort site the registry would never
# learn about while every count still balanced.
#
# This file closes those three doors from the other side. It does not widen
# what the walk enumerates; it denies the script the shapes the walk cannot see.
# The denied set is a closed list of literals in helper-norms-audit-script.R
# (DENIED_ABORT_HEADS, DENIED_INDIRECT_HEADS) -- a denylist's content IS its
# promise, so it is read off the source rather than derived. Names resolved at
# run time defeat any syntactic rule and stay outside the promise.
#
# DEVELOPMENT-ONLY: data-raw/ is not installed, so these skip against the
# installed package, as the sibling audit test files do.

test_that("the audit script uses no denied abort spelling (M82)", {
  norms_audit_script_path()  # skips against the installed package
  expect_identical(norms_audit_denied_calls(), character(0))
})

# The boundary as a PARTITION, not as examples: every shape below is asserted in
# its own direction, denied and accepted alike, so "and nothing else" is under
# test rather than assumed (the M79 lesson -- an emergent accepted set gives up
# one new member per review round).
#
# Fixtures are SOURCE TEXT parsed by the sweep's own parse call, never quoted
# expressions. `keep.source = FALSE` drops comments, so the comment case is only
# a fixture at all if the fixture starts life as text (M82 plan gate).

denied_shapes <- list(
  "rlang::abort head"        = 'rlang::abort("x")',
  "bare abort head"          = 'abort("x")',
  "cli::cli_abort head"      = 'cli::cli_abort("x")',
  "bare cli_abort head"      = 'cli_abort("x")',
  "parenthesised head"       = '(rlang::abort)("x")',
  "do.call string"           = 'do.call("stop", list("x"))',
  "do.call symbol"           = 'do.call(stop, list("x"))',
  "do.call what ="           = 'do.call(args = list("x"), what = "stop")',
  "do.call denied spelling"  = 'do.call("cli_abort", list("x"))',
  "alias by <-"              = 'fail <- stop',
  "alias by <<-"             = 'fail <<- stop',
  "alias to a denied head"   = 'fail <- rlang::abort',
  "alias to base::stop"      = 'fail <- base::stop',
  "assign()"                 = 'assign("fail", stop)',
  "higher-order"             = 'lapply(msgs, stop)',
  "function default"         = 'f(g = stopifnot)'
)

accepted_shapes <- list(
  "stop() head"              = 'stop("x")',
  "stopifnot() head"         = 'stopifnot(is.numeric(x))',
  "base::stopifnot() head"   = 'base::stopifnot(is.numeric(x))',
  "comment naming a head"    = '# see rlang::abort and cli_abort',
  "string outside do.call"   = 'f("stop")',
  "string named what = "     = 'f(what = "rlang::abort")',
  "do.call of something else"= 'do.call("paste0", list("x"))',
  "a variable so named"      = 'aborted <- TRUE',
  "a string in a message"    = 'stop("do not use rlang::abort here")'
)

test_that("every denied shape is caught, and named by its rule (M82)", {
  for (nm in names(denied_shapes)) {
    hits <- norms_audit_denied_calls(
      norms_audit_parse_text(denied_shapes[[nm]])
    )
    expect_true(length(hits) > 0L, info = nm)
    # The finding names the offending call, not just its rule: a sweep that
    # reports "something is wrong" sends the reader back to the whole file.
    expect_true(all(grepl("^\\((i|ii|iii)\\) .", hits)), info = nm)
  }
})

test_that("every accepted shape is left alone (M82)", {
  for (nm in names(accepted_shapes)) {
    expect_identical(
      norms_audit_denied_calls(norms_audit_parse_text(accepted_shapes[[nm]])),
      character(0),
      info = nm
    )
  }
})

test_that("a comment is dropped by the parse, so its fixture is empty (M82)", {
  # Stated rather than assumed, because it is the reason the fixtures above are
  # text: if this ever became false the comment case would start carrying
  # weight, and if it were already false the case would be carrying none.
  exprs <- norms_audit_parse_text("# see rlang::abort and cli_abort")
  expect_length(exprs, 0L)
})
