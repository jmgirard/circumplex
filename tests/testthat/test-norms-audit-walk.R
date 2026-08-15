# The abort-site walk's own helpers, tested directly (M88).
#
# M87 retired the registry, matchers, matrix and denylist that used to exercise
# these functions, and kept the manifest check. The helpers the manifest path
# still goes through came through that retirement with no unit tests at all:
# four of their branches are reachable from no site the audit script ships, so
# the manifest test cannot exercise them even indirectly. This file covers those
# branches, and nothing here sweeps for anything -- what M87 gave up stays given
# up (D-042).
#
# DEVELOPMENT-ONLY on one point only: the helpers are pure and need no script,
# so unlike its sibling audit files this one does not skip against the installed
# package. Nothing below reads data-raw/.

# ---- AC1: the truncation marker, and what it does to the floor ---------------

# R truncates a `stopifnot()` message at its first deparsed line and marks the
# cut with " ...." before its verdict. The marker is the only signal separating
# "R cut this short" from "this is the whole condition", and the two want
# opposite treatment: a truncated stem is honest at any length, an untruncated
# one must clear the floor. Reading a trailing "...." as truncation WITHOUT the
# verdict behind it removes the floor from any message ending that way, and a
# fixture failing before its guard is then reported as coverage for a site never
# reached (helper-norms-audit-script.R:300-306).
#
# The accepted truncated shape is captured from a live stopifnot() rather than
# typed: a hand-typed stem is one the author already believes matches, and R's
# actual line break is what the matcher has to survive.
live_truncated <- function(cond, env = list(batch = 1)) {
  err <- tryCatch(eval(bquote(stopifnot(.(cond))), list2env(env)),
                  error = identity)
  stopifnot(inherits(err, "error"))
  conditionMessage(err)
}

test_that("the stopifnot stem accepts and rejects as a partition (M88)", {
  long <- quote(is.data.frame(batch) && is.numeric(batch) &&
                  is.character(batch) && is.list(batch) && nrow(batch) > 3)
  long_key <- squish(paste(deparse(long), collapse = " "))
  truncated <- live_truncated(long)

  # The live message really is the truncated shape, not a short one that
  # happened to fit -- otherwise the accept case below proves nothing about
  # truncation (M60: a probe that cannot reach the regime it names).
  expect_true(norms_audit_stopifnot_stem(truncated)$truncated)
  expect_true(grepl("....", truncated, fixed = TRUE))

  short_key <- "is.data.frame(batch)"
  cases <- list(
    list(key = long_key,  msg = truncated,
         want = TRUE,  what = "live truncated message, keyed on the whole condition"),
    list(key = long_key,  msg = sub("[[:space:]]*is not TRUE$", "", truncated),
         want = FALSE, what = "same text with no verdict behind the marker"),
    list(key = short_key, msg = "is.data.frame(batch) is not TRUE",
         want = TRUE,  what = "untruncated whole condition"),
    list(key = short_key, msg = "is.data is not TRUE",
         want = FALSE, what = "below-floor stem, verdict, no marker"),
    list(key = short_key, msg = "is.data ....",
         want = FALSE, what = "below-floor stem, marker, no verdict")
  )

  for (case in cases) {
    expect_identical(
      audit_key_matches("stopifnot", case$key, case$msg), case$want,
      info = case$what
    )
  }

  # Asserted as a partition rather than as examples: every shape above is
  # declared in both directions, so a matcher that accepted everything and one
  # that accepted nothing each fail (M79).
  got <- vapply(cases, function(c) audit_key_matches("stopifnot", c$key, c$msg),
                logical(1))
  expect_identical(got, vapply(cases, function(c) c$want, logical(1)))
  expect_true(any(got))
  expect_false(all(got))
})

# ---- AC2: the matcher's kind dispatch ---------------------------------------

test_that("an unrecognised abort kind is refused, not judged loosely (M88)", {
  err <- tryCatch(audit_key_matches("stopifnot_nmaed", "k", "k"),
                  error = identity)
  expect_true(inherits(err, "error"))

  # Named, so a stale kind fails by what it was rather than by falling through
  # to the positional branch -- the loosest of the three, which is what a
  # fall-through would silently hand it (helper-norms-audit-manifest.R:185-192).
  expect_match(conditionMessage(err), "unknown abort site kind", fixed = TRUE)
  expect_match(conditionMessage(err), "stopifnot_nmaed", fixed = TRUE)

  for (kind in c("stop", "stopifnot", "stopifnot_named")) {
    expect_no_error(audit_key_matches(kind, "a key", "a key"))
  }
})

test_that("a named stopifnot condition matches by equality only (M88)", {
  key <- "batch must name each instrument once"

  expect_true(audit_key_matches("stopifnot_named", key, key))
  expect_false(audit_key_matches("stopifnot_named", key, paste0(key, " really")))
  expect_false(audit_key_matches("stopifnot_named", key, paste0("really ", key)))
  expect_false(audit_key_matches("stopifnot_named", key, substr(key, 1L, 20L)))

  # No shipped site raises a named condition, so this branch is reachable from
  # nothing the manifest contains -- which is why it needs a test of its own.
  expect_false("stopifnot_named" %in% NORMS_AUDIT_MANIFEST$kind)
})

# ---- AC3: the two fail-closed refusals --------------------------------------

test_that("a stopifnot formal carrying conditions is refused by name (M88)", {
  # Iterated as the running R defines the set, never as a literal list: the
  # spelling is version-dependent (this R has exprObject where RR17 rev 2 had
  # exprs.env), and a written-out list would silently stop covering a rename.
  #
  # Anchored non-vacuous, because the loop and the branch it tests read the same
  # constant: with STOPIFNOT_RESERVED emptied, a bare iteration runs zero times
  # and passes green over the very mutation it should catch (M65/M78).
  expect_gt(length(STOPIFNOT_RESERVED), 0L)
  expect_true("exprs" %in% STOPIFNOT_RESERVED)

  for (nm in STOPIFNOT_RESERVED) {
    cl <- as.call(list(quote(stopifnot), quote(x > 0)))
    names(cl) <- c("", nm)
    err <- tryCatch(norms_audit_stopifnot_conditions(cl), error = identity)
    expect_true(inherits(err, "error"), info = nm)
    expect_match(conditionMessage(err), "cannot enumerate", fixed = TRUE)
    expect_match(conditionMessage(err), nm, fixed = TRUE)
  }
})

test_that("stopifnot conditions key by position and by name (M88)", {
  got <- norms_audit_stopifnot_conditions(
    quote(stopifnot(is.data.frame(batch), "batch is empty" = nrow(batch) > 0))
  )

  expect_identical(length(got), 2L)
  expect_identical(got[[1L]]$kind, "stopifnot")
  expect_identical(got[[1L]]$key, "is.data.frame(batch)")
  expect_identical(got[[2L]]$kind, "stopifnot_named")
  expect_identical(got[[2L]]$key, "batch is empty")
})

test_that("a stop() argument R folds into the message is refused (M88)", {
  # R concatenates every named argument other than call./domain into the
  # runtime message while the key template drops it, so such a site would key
  # one string and raise another (helper-norms-audit-script.R:91-104).
  one <- tryCatch(norms_audit_stop_key(quote(stop("boom ", tail = "TAIL"))),
                  error = identity)
  expect_true(inherits(one, "error"))
  expect_match(conditionMessage(one), "cannot enumerate", fixed = TRUE)
  expect_match(conditionMessage(one), "tail", fixed = TRUE)

  # More than one carried name exercises the paste(collapse = ", ") rendering,
  # which no single-name probe reaches.
  many <- tryCatch(
    norms_audit_stop_key(quote(stop("boom ", tail = "T", extra = "E"))),
    error = identity
  )
  expect_true(inherits(many, "error"))
  expect_match(conditionMessage(many), "tail, extra", fixed = TRUE)

  # The negatives: no names at all, and the two names that are legitimately not
  # part of the message.
  expect_identical(norms_audit_stop_key(quote(stop("plain ", x))), "plain {}")
  expect_identical(
    norms_audit_stop_key(quote(stop("plain ", x, call. = FALSE))), "plain {}"
  )
  expect_identical(
    norms_audit_stop_key(quote(stop("plain ", x, domain = NA))), "plain {}"
  )
  expect_identical(
    norms_audit_stop_key(quote(stop("plain ", x, call. = FALSE, domain = NA))),
    "plain {}"
  )
})
