# Reading data-raw/audit-norms.R as a PARSE TREE (M81).
#
# Two earlier attempts to enumerate the audit script's abort sites each fell
# one scope short of the file. Counting `stop(` occurrences in the deparsed
# bodies of the functions a defs-only `sys.source()` leaves behind sees only
# what that source loaded: the script's trailing run block, which
# `norms_audit_defs_only = TRUE` skips, is invisible, so an abort landing there
# is unregistered and the count still balances. Grepping the file's TEXT has
# the opposite defect -- a doc comment or a string literal mentioning a name
# satisfies the grep as readily as the call does.
#
# The parse tree has neither hole. It covers every top-level expression of the
# file, run block included, and it contains calls rather than characters, so no
# comment and no string literal can stand in for one.
#
# What it covers is `stop()` and `stopifnot()` calls, and that is the whole of
# what anything here promises -- not every way this script can fail. Other
# abort spellings (rlang::abort, cli::cli_abort, do.call-dispatched or aliased
# heads), calls assembled at runtime, process exits, warnings promoted under
# options(warn = 2), and non-call failures such as a subscript error or a
# coercion are all outside it, and no procedure here enumerates them. Inside
# that domain the walk is fail-closed: an argument shape whose message the
# keying rules cannot predict raises rather than being passed over, because a
# site silently skipped is the false coverage this whole mechanism replaced.
#
# The consumers are not listed here. A list of them is an enumeration with no
# owner, so it goes stale silently: the list this comment used to carry named
# two files while four used the helper, M82's signature change was green in
# every file it mentioned and broke six tests in the two it did not (measured
# 2026-08-14), and M87 then deleted three of the consumers it named. Derive them
# instead -- `git grep -l norms_audit_ -- tests` -- and read nothing here as a
# guarantee about who calls what.
#
# No list follows here on purpose: the sentence above says lists go stale, and
# writing one anyway is how this comment was wrong before.

norms_audit_script_path <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  script
}

# The one parse call every walk here goes through. Fixtures written to a file
# and parsed by this function travel the shipped path rather than a lookalike,
# which matters for anything a parse can DROP: `keep.source = FALSE` discards
# comments, so a comment fixture built as a quoted expression is not a fixture
# at all -- it is an empty expression list, and asserting the sweep ignores it
# would pass against any implementation whatsoever (M82 plan gate).
norms_audit_parse <- function(path) parse(file = path, keep.source = FALSE)

norms_audit_script_exprs <- function() {
  norms_audit_parse(norms_audit_script_path())
}

# Every call in the tree whose deparsed head is one of `heads`, in source order;
# `heads = NULL` collects every call.
#
# `x[[i]]` on a call's empty-symbol element (the blank in `d[i, , drop = FALSE]`)
# binds a MISSING variable, and touching it errors -- so each child is tested
# with missing() before it is walked, rather than filtered out afterwards.
norms_audit_calls <- function(heads, exprs = norms_audit_script_exprs()) {
  found <- list()
  walk <- function(x) {
    if (is.call(x)) {
      head <- paste(deparse(x[[1L]]), collapse = "")
      if (is.null(heads) || head %in% heads) found[[length(found) + 1L]] <<- x
    }
    if (is.recursive(x)) {
      for (i in seq_along(x)) {
        child <- x[[i]]
        if (!missing(child)) walk(child)
      }
    }
    invisible(NULL)
  }
  for (i in seq_along(exprs)) walk(exprs[[i]])
  found
}

ABORT_HEADS <- c("stop", "stopifnot", "base::stop", "base::stopifnot")

# The unnamed arguments of a call: `stop()`'s message pieces, `stopifnot()`'s
# conditions. Named ones (`call. = FALSE`) are not part of either.
call_positional_args <- function(cl) {
  args <- as.list(cl)[-1L]
  nms <- names(args)
  if (is.null(nms)) return(args)
  args[!nzchar(nms)]
}

# Argument names that are NOT part of the message or the conditions.
#
# For `stop()` that is `call.` and `domain`; R concatenates every OTHER named
# argument into the runtime message while the template above drops it, so
# `stop("boom ", tail = "TAIL")` would key `"boom "` and raise `"boom TAIL"`.
#
# For `stopifnot()` it is its own formals less `...`, read from `formals()`
# rather than written out: the set is version-dependent (this R spells the
# third one `exprObject`, RR17 rev 2 spelled it `exprs.env`), and a literal
# list would silently stop covering a name R renamed. Conditions passed
# through `exprs =` live in an expression object this walk does not descend,
# so such a call is refused rather than under-counted.
STOPIFNOT_RESERVED <- setdiff(names(formals(stopifnot)), "...")
STOP_NON_MESSAGE_NAMES <- c("call.", "domain")

deparse_call <- function(cl) squish(paste(deparse(cl), collapse = " "))

deparse_flat <- function(x) paste(deparse(x), collapse = "")

# Fail closed on an argument shape the keying rules cannot express.
#
# The alternative is to skip it, and skipping is what returned this milestone:
# an unenumerable shape that raises nothing here is an abort site the registry
# never learns about, which is the exact failure the walk replaced. An error
# naming the call deparsed reddens every test that walks the script, so the
# shape has to be handled before the suite can go green again.
refuse_unenumerable <- function(cl, what) {
  stop("abort site this walk cannot enumerate (", what, "): ",
       deparse_call(cl), call. = FALSE)
}

# A `stop()` site's MESSAGE TEMPLATE: every literal fragment in order, each
# non-literal argument rendered `{}`.
#
# Not the first fragment alone: that is "source note " at six distinct sites of
# this script (measured 2026-08-09), so a fixture provoking any one of the six
# would satisfy an assertion written for another -- the false coverage this
# milestone exists to remove, one level in. The template is unique for every
# site here except the deliberate `source note not found: {}` pair.
norms_audit_stop_key <- function(cl) {
  nms <- names(as.list(cl)[-1L])
  if (!is.null(nms)) {
    carried <- setdiff(nms[nzchar(nms)], STOP_NON_MESSAGE_NAMES)
    if (length(carried)) {
      refuse_unenumerable(
        cl, paste0("stop() argument named ", paste(carried, collapse = ", "),
                   ", which R concatenates into the message")
      )
    }
  }
  paste(vapply(call_positional_args(cl), function(a) {
    if (is.character(a) && length(a) == 1L) a else "{}"
  }, character(1)), collapse = "")
}

squish <- function(x) trimws(gsub("[[:space:]]+", " ", x))

# Every CONDITION of a `stopifnot()` site, as (kind, key) pairs in order.
#
# The two forms key differently because they FAIL differently, and the kind is
# what tells the matcher which is which (AC7). A positional condition has no
# message of its own, so R deparses it into one and, where that deparse runs to
# more than one line, keeps the first line and marks it; the key is the whole
# deparsed text, matched as a stem. A NAMED condition's name IS the
# runtime message, verbatim and untruncated, so its key is the name and the
# match is string equality.
#
# Named conditions are why this milestone came back from review: the walk read
# positional arguments only, so `stopifnot("msg" = cond)` contributed no key at
# all -- a guard that genuinely fires, registered nowhere, with every count
# still balancing (M81 review, F1).
norms_audit_stopifnot_conditions <- function(cl) {
  args <- as.list(cl)[-1L]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  out <- list()
  for (i in seq_along(args)) {
    if (!nzchar(nms[[i]])) {
      out[[length(out) + 1L]] <- list(
        kind = "stopifnot",
        key = squish(paste(deparse(args[[i]]), collapse = " "))
      )
    } else if (nms[[i]] %in% STOPIFNOT_RESERVED) {
      refuse_unenumerable(
        cl, paste0("stopifnot() formal ", nms[[i]],
                   ", whose conditions are not arguments of this call")
      )
    } else {
      out[[length(out) + 1L]] <- list(kind = "stopifnot_named", key = nms[[i]])
    }
  }
  out
}

# The binding name carried by run-block sites, which sit under no binding.
NORMS_AUDIT_RUN_BINDING <- "<run>"

# The top-level binding a whole expression defines, or `"<run>"` if it defines
# none. `f <- function() ...` binds `f`; the trailing `if (...) { ... }` block
# binds nothing and its sites are run-block sites.
norms_audit_top_level_binding <- function(expr) {
  if (is.call(expr) && length(expr) >= 3L) {
    head <- deparse_flat(expr[[1L]])
    if (head %in% c("<-", "=", "<<-")) {
      lhs <- expr[[2L]]
      if (is.name(lhs)) return(as.character(lhs))
      if (is.character(lhs) && length(lhs) == 1L) return(lhs)
    }
  }
  NORMS_AUDIT_RUN_BINDING
}

# Every `stop()`/`stopifnot()` site the walk collects, as (kind, binding, key)
# in source order -- the header above bounds what that does and does not cover.
# One entry per `stop()` call and one per `stopifnot()` CONDITION, since each
# condition fails on its own and gets its own fixture.
#
# The identity carried a fourth part until M88: a source-order ordinal within
# each (kind, binding, key) group, there to keep two otherwise identical sites
# -- the same guard written twice inside one function -- separately
# identifiable. Measured 2026-08-15, all 33 shipped sites were ordinal 1 and no
# triple was duplicated, so the field distinguished nothing and was tested by
# nothing. What it nominally bought is kept instead as a REFUSAL, asserted in
# test-norms-audit-manifest.R: both sides of the manifest comparison must be
# duplicate-free, so a twin reddens the suite rather than being numbered
# silently and never asserted (D-043).
#
# The walk is per top-level expression rather than over the whole tree at once,
# because the enclosing binding is a property of the top-level expression a site
# sits inside and is lost by a flat walk (M82).
norms_audit_abort_sites <- function(exprs = norms_audit_script_exprs()) {
  out <- list()
  for (i in seq_along(exprs)) {
    binding <- norms_audit_top_level_binding(exprs[[i]])
    for (cl in norms_audit_calls(ABORT_HEADS, exprs[i])) {
      head <- paste(deparse(cl[[1L]]), collapse = "")
      if (head %in% c("stopifnot", "base::stopifnot")) {
        for (cond in norms_audit_stopifnot_conditions(cl)) {
          out[[length(out) + 1L]] <- c(cond, list(binding = binding))
        }
      } else {
        out[[length(out) + 1L]] <- list(
          kind = "stop", key = norms_audit_stop_key(cl), binding = binding
        )
      }
    }
  }
  out
}

# Does this call resolve `nm` out of a package namespace?
#
# Every shape that reaches the package's own binding counts -- `pkg:::nm`,
# `pkg::nm`, and a `get()`/`getExportedValue()` naming it as a literal -- so
# that a behaviour-preserving switch between them does not redden the caller.
# What does NOT count is a comment mentioning the name, or a bare string
# literal sitting anywhere else in the file: those are what the text grep this
# replaces could not tell apart from the call (M81 AC4).
norms_audit_resolves_name <- function(nm, exprs = norms_audit_script_exprs()) {
  getters <- c("get", "base::get", "getExportedValue", "base::getExportedValue")
  hits <- Filter(function(cl) {
    head <- paste(deparse(cl[[1L]]), collapse = "")
    if (head %in% c("::", ":::")) {
      return(length(cl) >= 3L && identical(cl[[3L]], as.name(nm)))
    }
    if (head %in% getters) {
      args <- as.list(cl)[-1L]
      return(any(vapply(args, function(a) {
        is.character(a) && length(a) == 1L && identical(a, nm)
      }, logical(1))))
    }
    FALSE
  }, norms_audit_calls(NULL, exprs))
  length(hits) > 0L
}

regex_escape <- function(x) gsub("([][{}()^$.|*+?\\\\])", "\\\\\\1", x)

# The pattern a `stop()` site's own message must match: its literal fragments,
# in order, with `.*` where an argument was interpolated.
norms_audit_key_regex <- function(key) {
  parts <- strsplit(key, "{}", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(".")
  paste(vapply(parts, regex_escape, character(1)), collapse = ".*")
}

# What `stopifnot()` printed, less its verdict, and WHETHER R truncated it.
#
# R deparses the failing condition into the message; where that deparse runs to
# more than one line it keeps the FIRST LINE and appends " ....". So the key is
# matched as a prefix of the condition rather than whole -- pinning the width
# would pin R's internals instead of this script's guards.
#
# The marker is returned rather than discarded because it is the only signal
# distinguishing "R cut this short" from "this is the whole condition", and the
# two want different treatment: a short stem is honest in the first case and
# degenerate in the second. Discarding it is what made the matcher reject its
# own site's genuine message (M83). Both readers get a list, so a caller that
# wants the text alone says so.
# The marker is recognised only WITH R's verdict behind it, because R emits the
# two together -- `paste(ch[1L], "....")`, then " is not TRUE". Testing for a
# trailing `....` alone made any message ending that way read as truncated, and
# a truncated reading removes the floor: `is.d....` was accepted against key
# `is.data.frame(batch)` (measured 2026-08-14). A fixture failing BEFORE its
# guard, with an unrelated message ending in `....`, would then be reported as
# coverage for a site never reached -- what this file exists to prevent.
#
# One verdict, not two. R spells the verdict "are not all TRUE" where the failing
# condition is a vector, and this constant carried that alternative until M98 --
# unreachable from every site the script raises, all three of its positional
# `stopifnot()` conditions being scalar, and so exercised by no test (M88 review,
# F4). It is deleted rather than tested, because its absence fails CLOSED: a
# vectorized condition added to `data-raw/audit-norms.R` raises a plural verdict
# this pattern no longer strips, the stem keeps the verdict text, `startsWith()`
# on the key fails, and `audit_key_matches()` refuses the site's own genuine
# message rather than accepting a stranger's (measured 2026-08-20; the message is
# `c(TRUE, FALSE) are not all TRUE` and the refusal is in M98's work log). What
# that costs is a confusing red for whoever adds the first vectorized guard --
# the fix is to restore the alternative here, which this comment is the record
# for.
#
# The parentheses stay, holding one alternative, so that restoring the second
# one is an edit INSIDE them. Both readers below interpolate this value into a
# larger pattern anchored with `$`, and an ungrouped `a|b` would bind the
# alternation across the whole pattern rather than the verdict: measured, a
# plain (untruncated) plural message then matches the TRUNCATION detector,
# which waives the stem floor -- the 2026-08-14 incident recorded above,
# reintroduced by the obvious spelling of the fix this comment recommends
# (M98 review, F2).
NORMS_AUDIT_VERDICT <- "(is not TRUE)"

norms_audit_stopifnot_stem <- function(msg) {
  truncated <- grepl(
    paste0("[[:space:]]\\.\\.\\.\\.[[:space:]]+", NORMS_AUDIT_VERDICT,
           "[[:space:]]*$"),
    msg
  )
  msg <- sub(paste0("[[:space:]]*", NORMS_AUDIT_VERDICT, "[[:space:]]*$"), "", msg)
  if (truncated) msg <- sub("[[:space:]]*\\.\\.\\.\\.[[:space:]]*$", "", msg)
  list(stem = squish(msg), truncated = truncated)
}

# Run `expr` with R's messages pinned to the C locale.
#
# ONE home for the pin, because more than one surface reads these messages: the
# per-site assertion below and the cross-discrimination matrix, which captures
# `conditionMessage()` itself. `stopifnot()`'s positional message is generated
# and TRANSLATED by R, not written by the script, so a translated session fails
# a correct guard: measured under `LANGUAGE=fr`, `stopifnot(is.data.frame(batch))`
# raises "is.data.frame(batch) n'est pas TRUE", the English-only strip removes
# nothing and the match fails (RR17 rev 2 BC9, 2026-08-09). Under testthat 3e
# this is belt-and-braces -- `test_that()` sets LANGUAGE=C itself (measured
# 2026-08-13) -- and it is what covers a call made outside one.
norms_audit_with_c_messages <- function(expr) {
  old <- Sys.getenv(c("LANGUAGE", "LC_MESSAGES"), unset = NA)
  Sys.setenv(LANGUAGE = "C", LC_MESSAGES = "C")
  on.exit({
    set <- old[!is.na(old)]
    if (length(set)) do.call(Sys.setenv, as.list(set))
    unset <- names(old)[is.na(old)]
    if (length(unset)) Sys.unsetenv(unset)
  }, add = TRUE)
  force(expr)
}

# Discriminating-power floors. Both sit inside the bands RR17 rev 2 BC9 fixes
# ([10, 20] and [20, 45]).
#
# The stem floor applies to UNTRUNCATED messages only (M83). Where R truncated,
# the stem is its own first deparsed line and no floor is meaningful: the line
# break is R's choice and can fall anywhere, so comparing it against a floor
# derived from the key rejected correct sites.
#
# Headroom over the shipped sites is asserted for STOP_KEY_FLOOR only, in
# test-norms-audit-manifest.R; STEM_FLOOR's headroom is asserted nowhere since
# M87 retired the markers-file apparatus that used to carry it. Do not read
# either constant as fenced by a test it is not.
NORMS_AUDIT_STOP_KEY_FLOOR <- 15L
NORMS_AUDIT_STEM_FLOOR <- 40L

# A `stop()` key's literal characters -- what a message must actually carry.
# The `{}` placeholders stand for interpolated arguments and match anything, so
# they are not discrimination and do not count toward the floor.
norms_audit_key_literals <- function(key) gsub("{}", "", key, fixed = TRUE)
