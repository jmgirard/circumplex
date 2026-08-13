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
# Used by test-norms-audit-markers.R (the abort registry) and by
# test-norms-audit-roster.R (the single-sourcing assertion).

norms_audit_script_path <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  script
}

norms_audit_script_exprs <- function() {
  parse(file = norms_audit_script_path(), keep.source = FALSE)
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
# message of its own, so R deparses it into one and truncates it; the key is
# that deparsed text, matched as a stem. A NAMED condition's name IS the
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

# Every abort site the script contains, as (kind, key) pairs in source order.
# One entry per `stop()` call and one per `stopifnot()` CONDITION, since each
# condition fails on its own and gets its own fixture under AC2/AC3.
norms_audit_abort_sites <- function(exprs = norms_audit_script_exprs()) {
  out <- list()
  for (cl in norms_audit_calls(ABORT_HEADS, exprs)) {
    head <- paste(deparse(cl[[1L]]), collapse = "")
    if (head %in% c("stopifnot", "base::stopifnot")) {
      for (cond in norms_audit_stopifnot_conditions(cl)) {
        out[[length(out) + 1L]] <- cond
      }
    } else {
      out[[length(out) + 1L]] <- list(kind = "stop", key = norms_audit_stop_key(cl))
    }
  }
  out
}

# Comparable form for the set-equality assertion: kind and key together, so a
# site cannot match a registry entry of the other kind.
norms_audit_site_ids <- function(sites) {
  sort(vapply(sites, function(s) paste0(s$kind, "\t", s$key), character(1)))
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

# What `stopifnot()` printed, less its verdict and its truncation marker.
#
# R deparses the failing condition into the message and TRUNCATES it with
# " ...." past a width R chooses, so the key is matched as a prefix of the
# condition rather than whole -- pinning the width would pin R's internals
# instead of this script's guards. Discriminating all the same: neither of
# validate_batch()'s two conditions is a prefix of the other.
norms_audit_stopifnot_stem <- function(msg) {
  msg <- sub("[[:space:]]*(is not TRUE|are not all TRUE)[[:space:]]*$", "", msg)
  msg <- sub("[[:space:]]*\\.\\.\\.\\.[[:space:]]*$", "", msg)
  squish(msg)
}

# Assert that `thunk` aborts, and aborts at the site `key` names -- never that
# some error occurred. A fixture can reach the wrong guard, or fail before it
# reaches any, and a bare expect_error() reports both as coverage.
expect_abort_at_site <- function(thunk, kind, key, info = key) {
  err <- tryCatch({
    thunk()
    NULL
  }, error = identity)
  expect_true(inherits(err, "error"), info = paste("no error raised:", info))
  if (!inherits(err, "error")) return(invisible(NULL))
  msg <- conditionMessage(err)
  if (identical(kind, "stopifnot")) {
    stem <- norms_audit_stopifnot_stem(msg)
    expect_true(
      nzchar(stem) && startsWith(squish(key), stem),
      info = paste0(info, " -- got: ", msg)
    )
  } else {
    expect_match(msg, norms_audit_key_regex(key), info = info)
  }
  invisible(NULL)
}
