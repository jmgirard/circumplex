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
# Consumers, as of M82: test-norms-audit-markers.R (the abort registry),
# test-norms-audit-roster.R (the single-sourcing assertion),
# test-norms-audit-denylist.R (the denied-spelling sweep), and
# test-norms-audit-batch.R and test-norms-audit-compare.R, which between them
# hold 17 `expect_abort_at_site()` calls.
#
# That list is an enumeration with no owner, so it goes stale silently: it named
# two files while four used the helper, and M82's signature change was green in
# every file this comment mentioned and broke six tests in the two it did not
# (measured 2026-08-14). Re-derive it before trusting it --
# `grep -rn expect_abort_at_site tests/testthat` -- rather than reading it as a
# guarantee.

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

# Parse a fixture written as SOURCE TEXT, through the same call as the script.
norms_audit_parse_text <- function(lines) {
  path <- tempfile("m82-fixture-", fileext = ".R")
  writeLines(lines, path)
  norms_audit_parse(path)
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

# The abort spellings the M81 walk does not collect, and which the script must
# therefore not acquire. Closed, and written here as string literals so the
# denied set is readable off the source rather than emerging from a procedure:
# this is a denylist and its whole content is the promise (M79's lesson).
#
# Nothing here claims to enumerate every way to raise a condition -- a name
# resolved at run time defeats any syntactic list, and that stays outside the
# promise. What it does is close the three doors an aliased abort would walk
# through today unseen.
DENIED_ABORT_HEADS <- c("rlang::abort", "abort", "cli::cli_abort", "cli_abort")

# Heads whose appearance AWAY from a call head is denied. Both sets, not just
# the four above (M82 plan gate, widening RR17 BC6): `fail <- stop` and
# `fail <- abort` are the same defect, and a rule covering only one of them
# leaves the other invisible for no saving.
DENIED_INDIRECT_HEADS <- c(ABORT_HEADS, DENIED_ABORT_HEADS)

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

# `(f)(x)` is a call whose head is a call to `(`. Strip those wrappers so a
# head is compared by what it names, not by how it was written.
unwrap_parens <- function(x) {
  while (is.call(x) && identical(x[[1L]], quote(`(`)) && length(x) == 2L) {
    x <- x[[2L]]
  }
  x
}

# Does this element NAME a denied head -- as a bare symbol (`stop`) or as a
# namespaced call (`base::stop`, `rlang::abort`)? A character literal does not:
# `f("stop")` passes a string, and only `do.call` turns a string into a call,
# which rule (ii) covers on its own.
names_denied_head <- function(x) {
  if (is.name(x)) return(as.character(x) %in% DENIED_INDIRECT_HEADS)
  is.call(x) && deparse_flat(x) %in% DENIED_INDIRECT_HEADS
}

# Every denied appearance of an abort spelling in the script, as "(rule) call".
#
# Three rules, and the rule number travels with the finding so a failure names
# which door was walked through:
#   (i)   a call to one of DENIED_ABORT_HEADS -- an abort the M81 walk, which
#         collects `stop`/`stopifnot` heads only, cannot see at all;
#   (ii)  `do.call` dispatching one of either set by name, string or symbol;
#   (iii) a denied head appearing anywhere but a call's head position, which is
#         aliasing (`fail <- stop`), assignment (`assign("fail", stop)`) and
#         higher-order use (`lapply(msgs, stop)`) in one rule rather than three
#         -- an enumeration of shapes is what the M79 review beat twice.
#
# Position 1 of a call is its head and is exempt from (iii). Three further
# exemptions, and they share one reason: the slot holds a NAME being written or
# selected, never a value that could be called, so no aliased abort is reachable
# through it and flagging it reddens the sweep over ordinary code.
#
#   `::`/`:::` -- the whole call, whose operands are namespace parts rather than
#     arguments: without it the walk reaches the `stopifnot` symbol inside
#     `base::stopifnot(x)`'s own head and reports an ordinary shipped call as an
#     alias (measured 2026-08-14: `base::stopifnot(is.numeric(x))` was flagged
#     "(iii) base::stopifnot"). `fail <- base::stop` is unaffected -- there the
#     `::` call is a CHILD of `<-`, which is where rule (iii) reads.
#   `$`/`@` -- operand 3, the field or slot name. `opts$abort`, `x@abort` and
#     `df$stop <- 1` were each flagged before this (measured 2026-08-14).
#   `for` -- operand 2, the loop variable. `for (abort in x) f(1)` was flagged.
#
# What is NOT exempt, deliberately: the `for` SEQUENCE (`for (i in abort)`), and
# assignment of any kind. `abort <- rlang::abort` is the aliasing rule (iii)
# exists to catch, and telling it from `abort <- 1` needs the assigned value
# inspected -- which no rule here does (M83 plan gate).
NON_VALUE_OPERANDS <- list("$" = 3L, "@" = 3L, "for" = 2L)
norms_audit_denied_calls <- function(exprs = norms_audit_script_exprs()) {
  out <- character(0)
  for (cl in norms_audit_calls(NULL, exprs)) {
    head <- deparse_flat(unwrap_parens(cl[[1L]]))
    if (head %in% DENIED_ABORT_HEADS) {
      out <- c(out, paste0("(i) ", deparse_call(cl)))
    }
    if (head %in% c("do.call", "base::do.call")) {
      args <- as.list(cl)[-1L]
      nms <- names(args)
      if (is.null(nms)) nms <- rep("", length(args))
      what <- if ("what" %in% nms) {
        args[[match("what", nms)]]
      } else {
        pos <- args[!nzchar(nms)]
        if (length(pos)) pos[[1L]] else NULL
      }
      denied <- !is.null(what) &&
        ((is.character(what) && length(what) == 1L &&
            what %in% DENIED_INDIRECT_HEADS) || names_denied_head(what))
      if (denied) out <- c(out, paste0("(ii) ", deparse_call(cl)))
    }
    if (!head %in% c("::", ":::")) {
      exempt <- NON_VALUE_OPERANDS[[head]]
      for (i in setdiff(seq_along(cl)[-1L], exempt)) {
        child <- cl[[i]]
        if (missing(child)) next
        if (names_denied_head(child)) {
          out <- c(out, paste0("(iii) ", deparse_call(cl)))
          break
        }
      }
    }
  }
  out
}

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

# Source-order ordinals, assigned WITHIN each (kind, binding, key) group.
#
# The ordinal exists to keep two sites that are otherwise identical -- the same
# guard written twice inside one function -- separately identifiable, and it
# does nothing else: a site with no twin is ordinal 1, so adding a twin later
# renumbers nothing that already exists.
norms_audit_assign_ordinals <- function(sites) {
  counts <- list()
  for (i in seq_along(sites)) {
    s <- sites[[i]]
    k <- paste(s$kind, s$binding, s$key, sep = "\t")
    n <- if (is.null(counts[[k]])) 1L else counts[[k]] + 1L
    counts[[k]] <- n
    sites[[i]]$ordinal <- n
  }
  sites
}

# Every `stop()`/`stopifnot()` site the walk collects, as
# (kind, binding, key, ordinal) in source order -- the header above bounds what
# that does and does not cover. One entry per `stop()` call and one per
# `stopifnot()` CONDITION, since each condition fails on its own and gets its
# own fixture under AC2/AC3.
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
  norms_audit_assign_ordinals(out)
}

# Build the registry, refusing two entries that claim the same site.
#
# A registry entry DECLARES its ordinal rather than receiving a derived one, and
# that is what makes this check reachable: derive the ordinal per duplicate
# group, as the collected side does, and a doubly-registered entry silently
# becomes ordinal 2 -- a distinct identity, matching nothing, and no error
# anywhere (M82 plan gate, criteria audit). Declared, the two collide and the
# build says so.
norms_audit_build_registry <- function(entries) {
  ids <- vapply(entries, function(e) {
    paste(e$kind, e$binding, e$key, e$ordinal, sep = "\t")
  }, character(1))
  dup <- unique(ids[duplicated(ids)])
  if (length(dup)) {
    stop("registry declares the same abort site twice: ",
         paste(gsub("\t", " | ", dup), collapse = "; "), call. = FALSE)
  }
  # Matchers are built HERE, so a key too weak to discriminate stops the build
  # rather than waiting for a run that happens to exercise that site (AC4).
  lapply(entries, function(e) {
    e$matcher <- norms_audit_matcher(e$kind, e$key)
    e
  })
}

# The entries whose (kind, key) is shared with an entry under ANOTHER binding.
#
# DERIVED from the registry, never declared beside it. A hand-kept list of
# "the shared pairs" is a proxy for the thing it names: a later shared pair
# would be added to the registry and not to the list, and the stack assertions
# that discriminate such a pair would silently stop covering it -- the shape
# the M79 review beat twice, and the reason AC3 requires one structure rather
# than two agreeing ones (M82 plan gate, criteria audit).
norms_audit_shared_key_sites <- function(entries) {
  key <- vapply(entries, function(e) paste(e$kind, e$key, sep = "\t"),
                character(1))
  binding <- vapply(entries, function(e) e$binding, character(1))
  shared <- vapply(seq_along(entries), function(i) {
    any(key == key[[i]] & binding != binding[[i]])
  }, logical(1))
  entries[shared]
}

# The cross-discrimination matrix, computed ONCE for whichever registry it is
# handed: the shipped one and AC1's fixture registry go through this, so the two
# cannot drift into disagreeing about what "accepts" means (M83).
#
# Each fixture is raised ONCE and its message reused across the row and column,
# because the message is a property of the site: provoking per cell would make
# the run quadratic in fixture evaluations for nothing. A fixture that raised
# NOTHING contributes NA, which the caller checks -- a vacuous row and column
# would otherwise read as a clean matrix.
norms_audit_acceptance_matrix <- function(entries, env) {
  msgs <- vapply(entries, function(s) {
    norms_audit_with_c_messages(
      tryCatch({
        s$fixture(env)
        NA_character_
      }, error = conditionMessage)
    )
  }, character(1))
  n <- length(entries)
  accepts <- matrix(FALSE, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) accepts[i, j] <- entries[[i]]$matcher(msgs[[j]])
  }
  list(msgs = msgs, accepts = accepts)
}

# The off-diagonal cells a correct matcher set is ALLOWED to accept: one per
# ordered pair of entries the shared-key helper returned that carry the same
# (kind, key).
#
# Derived through `norms_audit_shared_key_sites()` rather than beside it. The
# matrix used to re-derive the pair set as `outer(key, key, "==")`, which is a
# second derivation of one thing: it ignores the helper's `binding != binding`
# conjunct and agrees with it only because no same-binding twin is shipped
# (M82 review, F8). Membership now comes from the helper alone; only the pairing
# among its members is computed here.
#
# `shared_fn` is an argument so AC5's mutants can be run without editing source.
norms_audit_expected_offdiag <- function(entries,
                                         shared_fn = norms_audit_shared_key_sites) {
  n <- length(entries)
  id <- vapply(entries, function(e) {
    paste(e$kind, e$binding, e$key, e$ordinal, sep = "\t")
  }, character(1))
  shared <- shared_fn(entries)
  pairable <- id %in% vapply(shared, function(e) {
    paste(e$kind, e$binding, e$key, e$ordinal, sep = "\t")
  }, character(1))
  kk <- vapply(entries, function(e) paste(e$kind, e$key, sep = "\t"), character(1))
  kk[!pairable] <- NA_character_
  same <- outer(kk, kk, function(a, b) !is.na(a) & !is.na(b) & a == b)
  same & !diag(TRUE, n)
}

# The frame stack as it stood WHEN the abort was signalled.
#
# A calling handler, because an exiting one (tryCatch) unwinds the stack before
# its handler runs and would leave nothing to look at. The tryCatch here sits
# OUTSIDE the calling handler, so it catches the condition only after the
# capture has happened -- there is no exiting handler between the capture and
# the abort, which is the whole point.
norms_audit_capture_abort_frames <- function(thunk) {
  frames <- list()
  tryCatch(
    withCallingHandlers(thunk(), error = function(e) {
      frames <<- lapply(seq_len(sys.nframe()), sys.function)
    }),
    error = function(e) invisible(NULL)
  )
  frames
}

# The name of the INNERMOST captured frame whose function is a binding of the
# sourced script environment, or NA if none is.
#
# Innermost, so the assertion does not degrade if one of a shared-key pair ever
# calls the other: the site that actually raised is the inner one either way.
# NA rather than an error, so a vacuous capture reads as a FAILURE at the call
# site rather than as a passed assertion about nothing.
norms_audit_innermost_script_binding <- function(frames, env) {
  nms <- ls(env, all.names = TRUE)
  for (i in rev(seq_along(frames))) {
    f <- frames[[i]]
    if (!is.function(f)) next
    for (nm in nms) {
      g <- env[[nm]]
      if (is.function(g) && identical(g, f)) return(nm)
    }
  }
  NA_character_
}

# The expectation failures `expr` raises, rather than letting them fail here.
# Used where what an assertion REFUSES is the thing under test.
norms_audit_expectation_failures <- function(expr) {
  out <- character()
  withCallingHandlers(expr, expectation_failure = function(cnd) {
    out <<- c(out, conditionMessage(cnd))
    invokeRestart("continue_test")
  })
  out
}

# Comparable form for the set-equality assertion: the full identity, so a site
# cannot match a registry entry of another kind, another function, or another
# occurrence of the same guard. M81 compared kind and key alone, which two
# sites sharing a key satisfy in either pairing (M82, RR17 rev 2 BC7).
norms_audit_site_ids <- function(sites) {
  sort(vapply(sites, function(s) {
    paste(s$kind, s$binding, s$key, s$ordinal, sep = "\t")
  }, character(1)))
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
#
# The C locale is pinned here rather than per test, because HERE is where the
# message is both raised and read: `stopifnot()`'s positional message is
# generated by R, not by the script, and R translates it, so a translated
# session would fail these assertions on a correct guard. Every message
# assertion in this milestone goes through this function, so pinning it once
# covers them all and cannot be forgotten by a test added later. Under
# testthat 3e this is belt-and-braces -- test_that() already sets LANGUAGE=C
# (measured 2026-08-13) -- and it is what covers a call made outside one.
SITE_KINDS <- c("stop", "stopifnot", "stopifnot_named")

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
# ([10, 20] and [20, 45]) and keep the headroom it states: the shortest shipped
# `stop` key carries 23 literal characters, and R's truncation leaves 66 of the
# script's longest shipped condition (both re-measured 2026-08-14).
NORMS_AUDIT_STOP_KEY_FLOOR <- 15L
NORMS_AUDIT_STEM_FLOOR <- 40L

# A `stop()` key's literal characters -- what a message must actually carry.
# The `{}` placeholders stand for interpolated arguments and match anything, so
# they are not discrimination and do not count toward the floor.
norms_audit_key_literals <- function(key) gsub("{}", "", key, fixed = TRUE)

# The matcher for one site: a predicate on a raised message.
#
# ONE procedure, and it runs at registry-BUILD time (AC4). The floors live here
# rather than in the assertion because a floor checked at assertion time is only
# checked for sites some test happens to exercise, while a key too weak to
# discriminate is a defect of the REGISTRY -- it should stop the build, not wait
# for a run. `expect_abort_at_site()` therefore consumes what this returns and
# adds no floor of its own.
#
# Fail-closed on an unknown kind, for the same reason the walk is: the `stop`
# branch is the loosest of the three, so an unrecognised kind falling through to
# it would silently get the weakest check. Not hypothetical -- a stale dispatch
# did exactly that during M81, accepting a key's own superstring (M81 work log,
# 2026-08-13). Refusing at build time moves that failure earlier still.
norms_audit_matcher <- function(kind, key) {
  if (!(length(kind) == 1L && !is.na(kind) && kind %in% SITE_KINDS)) {
    stop("unknown abort site kind: ", paste(deparse(kind), collapse = ""),
         " (expected one of ", paste(SITE_KINDS, collapse = ", "), ")",
         call. = FALSE)
  }
  if (identical(kind, "stop")) {
    n <- nchar(norms_audit_key_literals(key))
    if (n < NORMS_AUDIT_STOP_KEY_FLOOR) {
      stop("stop() key carries ", n, " literal characters, under the floor of ",
           NORMS_AUDIT_STOP_KEY_FLOOR, ": ", key, call. = FALSE)
    }
    rx <- norms_audit_key_regex(key)
    fn <- function(msg) grepl(rx, msg)
  } else if (identical(kind, "stopifnot_named")) {
    # Full equality, no stem and no regex. A named condition's message is its
    # name verbatim -- R appends no verdict and truncates nothing -- so there
    # is nothing here for a looser matcher to buy, and a stem would let a
    # longer message satisfy a shorter key. No floor: the key IS the message.
    fn <- function(msg) identical(msg, key)
  } else {
    # A positional condition's message is R's deparse of it, truncated at a
    # width R chooses, so the key is matched as a prefix. The floor is what
    # stops a degenerate prefix standing in for the whole: without it the
    # shipped check accepts a ONE-character stem (measured 2026-08-14).
    floor <- min(nchar(squish(key)), NORMS_AUDIT_STEM_FLOOR)
    fn <- function(msg) {
      stem <- norms_audit_stopifnot_stem(msg)
      nchar(stem) >= floor && startsWith(squish(key), stem)
    }
  }
  structure(fn, kind = kind, key = key, class = c("norms_audit_matcher", "function"))
}

# Assert that `thunk` aborts, and aborts at the site `matcher` was built for --
# never that some error occurred. A fixture can reach the wrong guard, or fail
# before it reaches any, and a bare expect_error() reports both as coverage.
#
# The argument is checked rather than trusted. A stale call passing something
# else reached `matcher(msg)` and died there -- "attempt to apply non-function",
# or, for a character argument, "could not find function \"matcher\"" (measured
# 2026-08-14) -- naming neither this argument nor the site under test, which is
# the shape that hid M82's own call-site breakage. A plain function is refused
# for a second reason: it is callable, so it would be USED, and its verdict
# would stand in for one built from a declared (kind, key).
expect_abort_at_site <- function(thunk, matcher, info = attr(matcher, "key")) {
  if (!inherits(matcher, "norms_audit_matcher")) {
    stop("`matcher` must be a norms_audit_matcher, not ",
         paste(deparse(class(matcher)), collapse = ""),
         "; build one with norms_audit_matcher(kind, key)", call. = FALSE)
  }
  norms_audit_with_c_messages({
    err <- tryCatch({
      thunk()
      NULL
    }, error = identity)
    expect_true(inherits(err, "error"), info = paste("no error raised:", info))
    if (!inherits(err, "error")) return(invisible(NULL))
    msg <- conditionMessage(err)
    expect_true(matcher(msg), info = paste0(info, " -- got: ", msg))
  })
  invisible(NULL)
}
