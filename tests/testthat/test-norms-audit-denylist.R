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

# Each denied shape carries the RULE it must be caught by, not merely the fact
# that something caught it. Without that, a shape can be held up by the wrong
# rule and the intended one deleted with the suite still green: measured
# 2026-08-14, `(rlang::abort)("x")` is reported by (i) AND by (iii), so a
# format-only assertion covers paren normalisation not at all, and
# `do.call(stop, list("x"))` is reported by (ii) AND (iii), leaving rule (ii)'s
# symbol arm held up by nothing (M82 review, F4/F5).
denied_shapes <- list(
  "rlang::abort head"        = list('rlang::abort("x")', "i"),
  "bare abort head"          = list('abort("x")', "i"),
  "cli::cli_abort head"      = list('cli::cli_abort("x")', "i"),
  "bare cli_abort head"      = list('cli_abort("x")', "i"),
  "parenthesised head"       = list('(rlang::abort)("x")', "i"),
  "do.call string"           = list('do.call("stop", list("x"))', "ii"),
  "do.call symbol"           = list('do.call(stop, list("x"))', "ii"),
  "do.call what ="           = list('do.call(args = list("x"), what = "stop")', "ii"),
  "do.call denied spelling"  = list('do.call("cli_abort", list("x"))', "ii"),
  "alias by <-"              = list('fail <- stop', "iii"),
  "alias by <<-"             = list('fail <<- stop', "iii"),
  "alias to a denied head"   = list('fail <- rlang::abort', "iii"),
  "alias to base::stop"      = list('fail <- base::stop', "iii"),
  "assign()"                 = list('assign("fail", stop)', "iii"),
  "higher-order"             = list('lapply(msgs, stop)', "iii"),
  "function default"         = list('f(g = stopifnot)', "iii"),
  # The `for` exemption is the INDEX slot only: a denied name in the sequence
  # slot is an ordinary rule (iii) hit and stays one (M83).
  "for sequence"             = list('for (i in abort) 1', "iii"),
  # Assignment is NOT exempt. `abort <- rlang::abort` is the aliasing the rule
  # exists to catch, and separating it from `abort <- 1` needs the assigned
  # value inspected, which no syntactic rule here does (M83 plan gate).
  "assignment to a denied name" = list('abort <- 1', "iii")
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
  "a string in a message"    = 'stop("do not use rlang::abort here")',
  # Field and slot names are not values: `$` and `@` take their third operand
  # as a NAME, so no denied function is reachable through one, and flagging it
  # would redden the sweep over an ordinary variable the script may well grow
  # (M83). The same holds when the field access is an assignment target.
  "a field so named"         = 'opts$abort',
  "a slot so named"          = 'x@abort',
  "a field assigned into"    = 'df$stop <- 1',
  # `for`'s index is a binding site, not a value: the loop variable named
  # `abort` is being written, never called.
  "a loop index so named"    = 'for (abort in x) f(1)'
)

test_that("every denied shape is caught, by the rule it is meant for (M82)", {
  for (nm in names(denied_shapes)) {
    code <- denied_shapes[[nm]][[1L]]
    rule <- denied_shapes[[nm]][[2L]]
    hits <- norms_audit_denied_calls(norms_audit_parse_text(code))
    expect_true(length(hits) > 0L, info = nm)
    # The intended rule fired -- not merely some rule. This is what makes
    # `unwrap_parens()` and rule (ii)'s symbol arm load-bearing.
    expect_true(any(startsWith(hits, paste0("(", rule, ") "))), info = nm)
    # And the finding names the offending call, not just its rule: a sweep that
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
