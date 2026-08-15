# M86: Name every roster shape the norms-audit builder cannot honestly audit

**Status:** done (2026-08-15, PR #114 https://github.com/jmgirard/circumplex/pull/114)

**Goal:** Every roster and object list `audit_norms()` cannot honestly audit
against is refused by a message naming the instrument, column, or pair at fault.

**Outcome:** `validate_roster()` splits its combined `%in% names(roster)` test into
per-column refusals and gains `fixture_world =`, refusing any roster short of the
shipped pair set unless the caller declares a fixture world — asked for, never
inferred from instrument names. `roster_from_objects()` refuses a repeated name, a
non-list entry, a NULL entry and an empty `Norms`, each naming the instrument, with
M79's `Norms = NULL` skip intact. `audit_norms()` validates `batch` before the
default roster resolves; a 24-pair `data/*.rda` literal replaces a self-comparing
assertion; 33 abort sites registered, 14 on the roster path.

**Decisions:** none milestone-local; gate choices are in the work log — refuse on
the shipped superset rather than per-instrument completeness, a hand-authored
literal over asserted counts, and a call-site exemption narrowing a recalled list.

**Review:** Two rounds. Round 1 actioned four (F2 88, F3 80, F4 85, F5 85) and
returned AC6 for a gated criterion amendment; round 2 actioned none, with three
sub-threshold fixes taken as branch-added false prose. No lesson retired.
