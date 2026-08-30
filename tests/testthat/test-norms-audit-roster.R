# AUDIT_BATCH is bound to the shipped roster (M79).
#
# The batch is a hand-written table, and until M79 nothing tied it to `data/`.
# The audit walks the batch and the notes the batch names, so an instrument or
# sample the batch omits was never reached: measured 2026-08-08 at cef9d36f,
# dropping `isc` cost all 17 of its audited values while the ledger fell from
# 194 rows to 177, the coverage report from 15 to 13, and the gap count stayed
# at 0 with no row anywhere naming the instrument or its note.
#
# DEVELOPMENT-ONLY: data-raw/ and cairn/ are not installed, so these skip
# against the installed package, as the sibling audit test files do.

roster_defs <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  env
}

roster_notes <- function() {
  dir <- testthat::test_path("..", "..", "cairn", "references")
  skip_if_not(dir.exists(dir), "cairn/ not present (installed package)")
  dir
}

# The shipped roster, written out (M86).
#
# ORIGIN: read from `data/*.rda` with `load()` directly, 2026-08-15 -- not from
# `roster_from_objects()`, and not through the package namespace. That
# independence is the whole point of the literal. The assertion this replaced
# compared `audit_norms(slice, dir)` against
# `audit_norms(slice, dir, roster = shipped_roster())`, and the default IS
# `shipped_roster()`, so both sides came from one nullary call and no defect of
# the builder could make it fail (M84 review F6; the M72/M76/M78 shared-origin
# family). Reading the `.rda` files rather than asking the package is the M78
# lesson: a data-raw script verifies the ARTIFACT, and no install can shadow it.
#
# It is a fence on `data/` as well as on the builder: adding or removing a
# shipped sample reddens this until the list is updated by hand, which is the
# intended cost -- shipped norms changing unnoticed is the hazard the whole
# audit exists for.
SHIPPED_ROSTER_PAIRS <- c(
  "cais", "1",    "csie", "1",    "csig", "1",
  "csip", "1",    "csiv", "1",    "iei", "1",     "iei", "2",
  "igicr", "1",   "igicr", "2",   "igicr", "3",   "iip32", "1",
  "iip32", "2",   "iip32", "3",   "iip64", "1",   "iip64", "2",
  "iip64", "3",   "iipsc", "1",   "iipsc", "2",   "iis32", "1",
  "iis64", "1",   "iitc", "1",    "ipipipc", "1", "isc", "1"
)

shipped_roster_literal <- function() {
  m <- matrix(SHIPPED_ROSTER_PAIRS, ncol = 2L, byrow = TRUE)
  data.frame(instrument = m[, 1L], sample = m[, 2L],
             stringsAsFactors = FALSE)
}

test_that("dropping any batch row is visible in the run (M79)", {
  env <- roster_defs()
  dir <- roster_notes()
  batch <- env$AUDIT_BATCH

  # Abort-or-gap, not gap alone. Five of the 23 rows are the `scales = TRUE`
  # entry of a multi-sample instrument (iei, igicr, iip32, iip64, iipsc;
  # measured 2026-08-30 -- six of 24 until M112 withdrew the cais adult
  # sample, which left cais single-sample), and removing one leaves that
  # instrument with no `scales` entry, which validate_batch() refuses before
  # any coverage count exists -- so demanding a gap from all 24 would be
  # demanding one from a run that never returns. What must hold of every row
  # is that its removal is NOTICED, by either route.
  #
  # Measured 2026-08-08. Before the roster sweep: 6 abort, 8 report a gap, and
  # 10 -- the 9 single-sample rows and iipsc sample 1 -- are silent. After:
  # 6 abort, 18 report a gap, 0 silent. The silent 10 are what this fences.
  #
  # Every run now carries one standing gap that no dropped row causes:
  # sodano2006.md still tables the cais adult sample the package withdrew in
  # M112, and no batch pass claims it. Counting raw gaps would therefore make
  # this fence vacuous -- every drop would look noticed. It is subtracted, and
  # its presence asserted first, so the subtraction cannot quietly become a
  # subtraction of nothing.
  standing <- function(cov) {
    cov$side == "note-sample-not-audited" & cov$instrument == "cais" &
      cov$sample == "2"
  }
  base <- env$audit_norms(batch, dir = dir)$coverage
  expect_identical(sum(standing(base) & !base$exempt), 1L)

  silent <- character(0)
  for (i in seq_len(nrow(batch))) {
    res <- tryCatch(env$audit_norms(batch[-i, , drop = FALSE], dir = dir),
                    error = function(e) NULL)
    if (is.null(res)) next
    cov <- res$coverage
    if (sum(!cov$exempt & !standing(cov)) == 0L) {
      silent <- c(silent, paste(batch$instrument[[i]], batch$sample[[i]]))
    }
  }
  expect_identical(silent, character(0))
})

test_that("a dropped instrument is named, not just counted (M79)", {
  env <- roster_defs()
  dir <- roster_notes()
  batch <- env$AUDIT_BATCH
  # isc is single-sample and its note is read by nothing else, so dropping it
  # removes the instrument from the run entirely -- the shape no note-side
  # sweep can see. Assert the row names the instrument and the sample, not
  # merely that the count moved: a gap count alone cannot say what is missing.
  res <- env$audit_norms(batch[batch$instrument != "isc", , drop = FALSE],
                         dir = dir)
  gaps <- res$coverage[!res$coverage$exempt, , drop = FALSE]
  hit <- gaps[gaps$side == "shipped-sample-not-audited", , drop = FALSE]
  expect_identical(hit$instrument, "isc")
  # The sample rides in `sample` since M80; it was pasted into `scale` before.
  expect_identical(hit$sample, "1")
})

test_that("the batch covers the shipped roster exactly (M79)", {
  env <- roster_defs()
  # The check a gap count cannot make. `sum(!exempt) == 0` in
  # test-norms-provenance.R says nothing is uncovered; it cannot say the batch
  # and the roster are the same set, and it names nothing when they are not.
  # Shipping a new instrument should fail HERE, by name.
  roster <- env$shipped_roster()
  batch <- env$AUDIT_BATCH
  expect_setequal(
    paste(roster$instrument, roster$sample),
    paste(batch$instrument, batch$sample)
  )
  # And that the comparison ranged over something: 15 shipped instruments,
  # 23 (instrument, sample) pairs -- 24 when measured 2026-08-08 at cef9d36f,
  # one fewer since M112 withdrew the cais adult sample. A roster that came
  # back empty would satisfy expect_setequal against an empty batch.
  expect_identical(nrow(roster), 23L)
  expect_identical(length(unique(roster$instrument)), 15L)
})

test_that("an instrument shipping no norms is not a roster gap (M79)", {
  env <- roster_defs()
  # Every shipped instrument carries norms today, so this is future-proofing:
  # an instrument with nothing to audit must not be reported as unaudited, or
  # the gap count stops meaning what the roster sweep exists to make it mean.
  # NULL[[1]] is NULL rather than an error in R, which is what makes the
  # one-line guard sufficient -- pinned here so a rewrite cannot quietly rely
  # on the other behaviour.
  none <- list(Norms = NULL, Scales = data.frame(Abbrev = "PA", Angle = 90))
  expect_identical(nrow(env$roster_from_objects(list(fz = none))), 0L)

  empty <- list(Norms = list(
    data.frame(Sample = numeric(0), Scale = character(0), M = numeric(0))
  ))
  expect_identical(nrow(env$roster_from_objects(list(fz = empty))), 0L)
})

test_that("the roster is the package's own enumeration, not a copy (M79)", {
  # Comparing shipped_roster()'s instruments against shipped_instruments() is
  # a tautology -- both bottom out in circumplex:::instrument_names(), so the
  # assertion holds even if that function returns nothing (M79 review, F11).
  # The claim is about the SOURCE, so the source is what is checked: the audit
  # script must reach the package's enumeration and must not carry one of its
  # own. A third copy written into data-raw/ reddens the second assertion.
  #
  # PRESENCE is asserted over the parse tree, not the file's text (M81). The
  # text grep this replaces matched the doc comment above shipped_roster() and
  # the string literal the call itself passes, so deleting the CALL left it
  # green -- the false coverage the abort registry in the sibling file was
  # rebuilt to remove. What must exist is a call resolving the name from the
  # namespace, today `get("instrument_names", envir = ns)()`; a comment
  # mentioning it and a bare literal now satisfy nothing. Every namespace-
  # resolving shape counts, so switching to `circumplex:::instrument_names()`
  # would not redden this -- the assertion is about reaching the package's
  # enumeration, not about which accessor reaches it.
  expect_true(norms_audit_resolves_name("instrument_names"))

  # ABSENCE stays a text assertion, and deliberately: a second enumeration is
  # a defect wherever it is written, a live call and a commented-out draft
  # alike, so the broader instrument is the right one here.
  # The sweep is `utils::data(package = "circumplex")` plus a class filter.
  # data-raw/ may not run one: that is the second copy this test exists to stop.
  script <- norms_audit_script_path()
  src <- readLines(script, warn = FALSE)
  expect_false(any(grepl("data(package", src, fixed = TRUE)))

  env <- roster_defs()
  # Non-vacuity, so an empty enumeration cannot satisfy the above by silence.
  roster <- env$shipped_roster()
  expect_gt(length(unique(roster$instrument)), 1L)
  expect_true(all(vapply(
    unique(roster$instrument),
    function(nm) inherits(shipped_instrument(nm), "circumplex_instrument"),
    logical(1)
  )))
})

test_that("injecting one object does not shrink the audited world (M79)", {
  env <- roster_defs()
  dir <- roster_notes()
  # The return-2 hole: `objects` overrides one instrument's VALUES, and the
  # roster used to be derived from it, so a value override silently narrowed
  # the world. Auditing a one-instrument slice of the real batch reported a
  # CLEAN run over every other shipped sample -- the exact "clean run over
  # data it never read" the Goal forbids. The two are now separate arguments.
  batch <- env$AUDIT_BATCH
  inst <- batch$instrument[[1L]]
  slice <- batch[batch$instrument == inst, , drop = FALSE]

  gaps <- function(res) {
    g <- res$coverage[!res$coverage$exempt, , drop = FALSE]
    g[g$side == "shipped-sample-not-audited", , drop = FALSE]
  }
  bare <- gaps(env$audit_norms(slice, dir = dir))
  # Injecting that instrument's own real object changes no value and so must
  # change no gap: same count, same pairs. Before the fix this was 0 rows.
  injected <- gaps(env$audit_norms(
    slice, dir = dir,
    objects = stats::setNames(list(shipped_instrument(inst)), inst)
  ))
  expect_gt(nrow(bare), 0L)
  expect_identical(nrow(injected), nrow(bare))
  expect_setequal(paste(injected$instrument, injected$sample),
                  paste(bare$instrument, bare$sample))
  # And the omitted instruments are named, not merely counted.
  expect_false(inst %in% injected$instrument)
  expect_gt(length(unique(injected$instrument)), 1L)
})

# The roster ARGUMENT's own boundary (M84).
#
# Everything above tests what the roster sweep catches. These test the roster
# itself: it is the only thing standing between an unaudited sample and a clean
# count, and until M84 nothing checked it, so a fixture -- or a caller writing
# `Instrument`/`Sample` -- could hand the audit a roster that makes every
# uncovered sample invisible. Measured 2026-08-14 with validate_roster() bound
# to a no-op: the csie slice below reported 0 non-exempt gaps against a
# capitalised-column roster and 0 against an empty one, where the shipped
# roster reports 22 (23 at that measurement; M112 withdrew one sample).

test_that("audit_norms() refuses a roster it cannot audit against (M84)", {
  env <- roster_defs()
  dir <- roster_notes()
  batch <- env$AUDIT_BATCH
  slice <- batch[batch$instrument == batch$instrument[[1L]], , drop = FALSE]

  # Each fixture is well-formed in every respect but the one it is named for,
  # and the batch it rides with is a real slice, so validate_batch() cannot
  # abort in validate_roster()'s place and the message can only be this shape's.
  expect_error(
    env$audit_norms(slice, dir = dir,
                    roster = list(instrument = "fx", sample = "1")),
    "is.data.frame(roster)", fixed = TRUE
  )
  # One message per missing column (M86). Until then both omissions raised one
  # `all(c("instrument", "sample") %in% names(roster)) is not TRUE`, which names
  # both columns and so names neither: a roster misspelling ONE of them read
  # exactly like a roster misspelling the other, and the assertion that stood
  # here survived weakening the condition to a single column.
  expect_error(
    env$audit_norms(slice, dir = dir,
                    roster = data.frame(Instrument = "fx", sample = "1",
                                        stringsAsFactors = FALSE)),
    "`roster` has no `instrument` column", fixed = TRUE
  )
  expect_error(
    env$audit_norms(slice, dir = dir,
                    roster = data.frame(instrument = "fx", Sample = "1",
                                        stringsAsFactors = FALSE)),
    "`roster` has no `sample` column", fixed = TRUE
  )
  expect_error(
    env$audit_norms(slice, dir = dir,
                    roster = data.frame(instrument = character(0),
                                        sample = character(0),
                                        stringsAsFactors = FALSE)),
    "names no (instrument, sample) pair to cover", fixed = TRUE
  )
})

test_that("a roster touching `data/` must cover all of it (M86)", {
  env <- roster_defs()
  dir <- roster_notes()
  batch <- env$AUDIT_BATCH
  slice <- batch[batch$instrument == batch$instrument[[1L]], , drop = FALSE]
  # Measured 2026-08-15 before this guard: the roster below audits the csie
  # slice with 0 non-exempt shipped-sample gaps where the shipped roster
  # reports 22 -- 23 at that measurement, one fewer since M112 withdrew the
  # cais adult sample -- a clean run over every sample nothing read. It is already a
  # superset of the slice's own batch pairs, and csie ships exactly one sample
  # so it is also complete for the only instrument it names: neither weaker
  # rule sees it, which is why the rule is all-or-nothing.
  narrow <- data.frame(instrument = "csie", sample = "1",
                       stringsAsFactors = FALSE)
  expect_error(env$audit_norms(slice, dir = dir, roster = narrow),
               "omits 22 shipped (instrument, sample) pair(s)", fixed = TRUE)
  # The exemption is asked for at the call site, so no spelling of an
  # instrument can buy it. Through M86's first pass it was INFERRED, by testing
  # the roster's instruments against circumplex:::instrument_names(); each
  # spelling below missed that list and was taken for a fixture's own world,
  # and each then audited the csie slice at 1 non-exempt shipped-sample gap
  # where the shipped roster reports 22 (all three measured 2026-08-15 at 23,
  # one fewer since M112 withdrew the cais adult sample).
  # They are regression fixtures for a rule that no longer reads the column:
  # what makes them refused is that they omit shipped pairs, which is why the
  # near-miss family needs no enumerating.
  for (spelling in list("CSIE", "csie ", NA_character_)) {
    evasion <- data.frame(instrument = spelling, sample = "1",
                          stringsAsFactors = FALSE)
    expect_error(env$audit_norms(slice, dir = dir, roster = evasion),
                 "omits 23 shipped (instrument, sample) pair(s)", fixed = TRUE)
  }
  # The four cells of (exemption asked | not asked) x (real | fake instrument).
  # Not asked + fake is the one that changed: a roster over instruments that do
  # not ship used to pass unasked, and every coverage and marker fixture in
  # this suite leaned on that. It is refused now, and each of those fixtures
  # says `fixture_world = TRUE` instead.
  fake <- data.frame(instrument = c("fx", "fy"), sample = c("1", "1"),
                     stringsAsFactors = FALSE)
  expect_error(env$validate_roster(fake),
               "omits 23 shipped (instrument, sample) pair(s)", fixed = TRUE)
  expect_identical(env$validate_roster(fake, fixture_world = TRUE), TRUE)
  # Asked + real is the declared lie the exemption deliberately does not
  # police: a caller who says out loud that this run is not about `data/` is
  # taken at their word, and the point of the amendment is that saying it is
  # the only way through. Pinned so the hole stays a stated one.
  expect_identical(env$validate_roster(narrow, fixture_world = TRUE), TRUE)
  # The full shipped roster passes, defaulted or stated, exemption or none.
  expect_identical(env$validate_roster(shipped_roster_literal()), TRUE)
})

test_that("the default roster is resolved before it is validated (M84)", {
  env <- roster_defs()
  dir <- roster_notes()
  batch <- env$AUDIT_BATCH
  slice <- batch[batch$instrument == batch$instrument[[1L]], , drop = FALSE]
  # Passing nothing must not be refused for passing nothing: `roster = NULL` is
  # the default, and a validator run before the default resolved would refuse
  # every ordinary call with "is.data.frame(roster) is not TRUE".
  expect_no_error(env$audit_norms(slice, dir = dir))
  expect_no_error(env$audit_norms(slice, dir = dir, roster = NULL))
})

test_that("the batch is validated before the default roster is built (M86)", {
  env <- roster_defs()
  dir <- roster_notes()
  batch <- env$AUDIT_BATCH
  slice <- batch[batch$instrument == batch$instrument[[1L]], , drop = FALSE]
  slice$divisor <- "x"
  # The two orders are indistinguishable while `shipped_roster()` succeeds,
  # which it always does under the suite -- so the probe has to make building
  # the default roster fail. `sys.source()` makes `env` the enclosure of
  # `audit_norms`, so rebinding the name here is what the call resolves.
  # Against the pre-M86 order this reports the stub, not the batch.
  env$shipped_roster <- function() {
    stop("STUB: the default roster was built", call. = FALSE)
  }
  expect_error(env$audit_norms(slice, dir = dir),
               "AUDIT_BATCH$divisor must be numeric", fixed = TRUE)
})

# The BUILDER's boundary (M84).
#
# `shipped_roster()` no longer takes an object list, so these shapes are
# reachable only through `roster_from_objects()`, which fixtures call by name.
# Measured 2026-08-14 against the pre-M84 builder: the non-frame table raised
# R's "invalid argument type", the Sample-less table and the all-NA table both
# raised "arguments imply differing number of rows: 1, 0" -- one message for
# two different faults, naming neither instrument -- the one-NA table returned
# a single row with the NA sample silently gone, and an unnamed object list
# returned a zero-row roster.

norms_object <- function(norms) list(Norms = list(norms))

test_that("the builder refuses a norms table it cannot roster (M84)", {
  env <- roster_defs()
  # Each fixture is well-formed but for the shape it is named for, so the
  # message can only be that shape's, and each message must name `fx`.
  expect_error(
    env$roster_from_objects(list(fx = list(Norms = list(list(Sample = 1))))),
    "norms table for fx is not a data frame but a list", fixed = TRUE
  )
  expect_error(
    env$roster_from_objects(list(fx = norms_object(
      data.frame(Scale = "PA", M = 1, stringsAsFactors = FALSE)))),
    "norms table for fx has no `Sample` column", fixed = TRUE
  )
})

test_that("a `Norms` field the builder cannot index is refused (M86)", {
  env <- roster_defs()
  # `objects[[nm]]$Norms[[1]]` was reached before anything about the entry was
  # known, so R's own message was the whole report and it named neither the
  # instrument nor the fault. Measured 2026-08-15 before these guards: a
  # non-list entry raised "$ operator is invalid for atomic vectors" and an
  # empty `Norms` list raised "subscript out of bounds".
  expect_error(env$roster_from_objects(list(fx = 1:3)),
               "instrument object for fx is not a list but a integer",
               fixed = TRUE)
  expect_error(env$roster_from_objects(list(fx = list(Norms = list()))),
               "`Norms` for fx must be a non-empty list to hold a norms table; it is a list of length 0",
               fixed = TRUE)
  # An atomic `Norms` reaches the same guard rather than R's message. Through
  # M85 it fell to the `is.data.frame()` refusal below instead, because
  # `(1:3)[[1]]` is 1 -- correct by luck, and only for atomics of length >= 1.
  expect_error(env$roster_from_objects(list(fx = list(Norms = 1:3))),
               "`Norms` for fx must be a non-empty list to hold a norms table; it is a integer of length 3",
               fixed = TRUE)
  # The skip these guards must NOT swallow: `Norms = NULL` is an instrument
  # with nothing to audit, not a malformed one, and it stays a skip rather than
  # a refusal (pinned above at the M79 case, restated here against the guards
  # that now stand between it and the loop body).
  none <- list(Norms = NULL, Scales = data.frame(Abbrev = "PA", Angle = 90))
  expect_identical(nrow(env$roster_from_objects(list(fz = none))), 0L)
})

test_that("an instrument named with no object behind it is refused (M86)", {
  env <- roster_defs()
  # The third shape, distinct from both above it. `Norms = NULL` is an
  # instrument that ships nothing to audit and stays a skip; a NULL ENTRY is an
  # instrument the caller named and then supplied nothing for, so the name is a
  # claim with no object behind it and the roster loses every sample it would
  # have carried. Before M86 the loop skipped it in silence -- `NULL$Norms` is
  # NULL, so it took the no-norms branch -- and through M86's first pass it
  # reached the is.list() guard and reported "is not a list but a NULL", a type
  # complaint for what is an empty slot.
  expect_error(env$roster_from_objects(list(fx = NULL)),
               "`objects` names fx but carries NULL for it, so the instrument would be rostered with no samples at all",
               fixed = TRUE)
  # The skip it must not swallow, restated against this guard: a named entry
  # that IS a list and whose `Norms` is NULL is still an instrument with
  # nothing to audit, and is still skipped.
  none <- list(Norms = NULL, Scales = data.frame(Abbrev = "PA", Angle = 90))
  expect_identical(nrow(env$roster_from_objects(list(fz = none))), 0L)
})

test_that("a failed shipped-roster build is not blamed on `roster` (M86)", {
  env <- roster_defs()
  # The completeness check reads every shipped norms table, so it can fail for
  # reasons the caller's roster had nothing to do with. Unattributed, the
  # BUILDER's message answered for an argument that was well-formed -- the same
  # message-precedence inversion moving validate_batch() ahead of the default
  # roster removed, reappearing inside this guard. The stub is what separates
  # the two: `sys.source()` makes `env` the enclosure of validate_roster(), so
  # rebinding `shipped_roster` there is what the check resolves.
  local <- new.env(parent = parent.env(env))
  for (nm in ls(env)) assign(nm, get(nm, envir = env), envir = local)
  environment(local$validate_roster) <- local
  local$shipped_roster <- function() stop("STUB: builder failed")
  fine <- data.frame(instrument = "fx", sample = "1", stringsAsFactors = FALSE)
  expect_error(local$validate_roster(fine),
               "`roster` cannot be checked for completeness: the shipped roster could not be built",
               fixed = TRUE)
  # The builder's own words survive inside it -- attribution adds a subject,
  # it does not swallow the cause.
  expect_error(local$validate_roster(fine), "STUB: builder failed", fixed = TRUE)
  # And the exemption never reaches the builder at all, so a fixture is not
  # held hostage to `data/` parsing cleanly.
  expect_identical(local$validate_roster(fine, fixture_world = TRUE), TRUE)
})

test_that("a missing Sample is refused, not dropped by sort() (M84)", {
  env <- roster_defs()
  # One NA beside a real sample: the silent case. sort() dropped the NA, so the
  # builder returned one row and nothing anywhere named the missing sample.
  expect_error(
    env$roster_from_objects(list(fx = norms_object(
      data.frame(Sample = c(1, NA), Scale = "PA", M = 1,
                 stringsAsFactors = FALSE)))),
    "norms table for fx leaves `Sample` missing in 1 of 2 rows", fixed = TRUE
  )
  # All NA: the case that DID abort before, with the same message the
  # Sample-less table raises, so the two faults were indistinguishable.
  expect_error(
    env$roster_from_objects(list(fx = norms_object(
      data.frame(Sample = c(NA, NA), Scale = "PA", M = 1,
                 stringsAsFactors = FALSE)))),
    "norms table for fx leaves `Sample` missing in 2 of 2 rows", fixed = TRUE
  )
})

test_that("an unnamed object list rosters nothing and is refused (M84)", {
  env <- roster_defs()
  # A roster over an unnamed list covers no pair at all, which reports every
  # unaudited shipped sample as covered -- the failure the sweep exists to
  # stop, reached through the builder rather than through the argument.
  expect_error(
    env$roster_from_objects(list(norms_object(
      data.frame(Sample = 1, Scale = "PA", M = 1, stringsAsFactors = FALSE)))),
    "must be named for the instrument it carries", fixed = TRUE
  )
  # An NA name is the same fault wearing a name, and it reached the same
  # zero-row roster: `nzchar(NA_character_)` is TRUE, so the guard's first
  # spelling let it through and `objects[[NA_character_]]` returned NULL, which
  # the no-norms skip swallowed (measured 2026-08-14 at the M84 review, F1,
  # scored 87). `setNames(list(obj), lookup)` with a lookup that missed is the
  # route in, and the file's own fixtures build object lists that way.
  na_named <- list(norms_object(
    data.frame(Sample = 1, Scale = "PA", M = 1, stringsAsFactors = FALSE)))
  names(na_named) <- NA_character_
  expect_error(env$roster_from_objects(na_named),
               "must be named for the instrument it carries", fixed = TRUE)
  # An empty list is not that shape: it has nothing to name and rosters nothing
  # by construction, which is what the shipped path returns when no instrument
  # carries norms.
  expect_identical(nrow(env$roster_from_objects(list())), 0L)
})

test_that("a name carried twice by `objects` is refused (M86)", {
  env <- roster_defs()
  # `for (nm in nms)` walks names, and `objects[[nm]]` resolves each to the
  # FIRST entry carrying it -- so a repeated name rosters that entry's samples
  # twice and the second entry's samples not at all. Measured 2026-08-15 before
  # the guard: the call below returned two rows, both reading `fx 1`, with
  # sample 2 nowhere. `validate_batch()` has refused exactly this shape for the
  # batch since M72, so the sibling asymmetry was the whole of the defect.
  dup <- list(
    fx = norms_object(data.frame(Sample = 1, Scale = "PA", M = 1,
                                 stringsAsFactors = FALSE)),
    fx = norms_object(data.frame(Sample = 2, Scale = "PA", M = 1,
                                 stringsAsFactors = FALSE))
  )
  expect_error(env$roster_from_objects(dup),
               "`objects` carries the name fx more than once", fixed = TRUE)
  # A name repeated three times names itself once, not three times.
  tri <- list(fx = dup$fx, fy = dup$fx, fx = dup$fx, fx = dup$fx)
  expect_error(env$roster_from_objects(tri),
               "`objects` carries the name fx more than once", fixed = TRUE)
})

test_that("shipped_roster() cannot be re-fused to an object list (M84)", {
  env <- roster_defs()
  # The pre-T18 fusion, spelt `roster = shipped_roster(objects)`, reproduced
  # M79's return-2 defect with no guard: it derived the roster from the same
  # list that overrode the VALUES, so a value override narrowed the world.
  # Cutting the parameter makes the call unspellable rather than merely wrong.
  expect_length(formals(env$shipped_roster), 0L)
  # And the derivation is not duplicated back into it: the shipped path goes
  # through the same builder the fixtures use, so a refusal added to one is a
  # refusal on both.
  expect_true("roster_from_objects" %in% all.names(body(env$shipped_roster)))
})

test_that("the builder's roster is the shipped data, pair for pair (M86)", {
  env <- roster_defs()
  # Compared UNCOERCED: the builder's own column types are under test, so a
  # `sample` returned numeric must redden here rather than be coerced away.
  # `shipped_roster()` is the whole derivation -- the namespace enumeration,
  # the object list, and `roster_from_objects()`'s sort/unique/as.character --
  # measured against a list read from the `.rda` files by another route.
  expect_identical(env$shipped_roster(), shipped_roster_literal())
})

test_that("passing the shipped roster explicitly changes no gap (M84, M86)", {
  env <- roster_defs()
  dir <- roster_notes()
  # The M79 regression, restated against the surviving spelling. Auditing a
  # one-instrument slice must report the same gaps whether the roster is
  # defaulted or passed: the roster says what must be covered, and stating it
  # explicitly is not a licence to shrink it. The explicit side is now the
  # LITERAL above rather than a second `shipped_roster()` call, so the two
  # sides no longer share an origin (M86).
  batch <- env$AUDIT_BATCH
  slice <- batch[batch$instrument == batch$instrument[[1L]], , drop = FALSE]
  gaps <- function(res) {
    g <- res$coverage[!res$coverage$exempt, , drop = FALSE]
    g[g$side == "shipped-sample-not-audited", , drop = FALSE]
  }
  bare <- gaps(env$audit_norms(slice, dir = dir))
  passed <- gaps(env$audit_norms(slice, dir = dir,
                                 roster = shipped_roster_literal()))
  expect_gt(nrow(bare), 0L)
  expect_identical(nrow(passed), nrow(bare))
  expect_setequal(paste(passed$instrument, passed$sample),
                  paste(bare$instrument, bare$sample))
})
