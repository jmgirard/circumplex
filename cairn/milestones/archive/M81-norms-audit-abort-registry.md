# M81: Enumerate the norms-audit abort registry from the script's parse tree

**Status:** done (2026-08-13, PR #108 https://github.com/jmgirard/circumplex/pull/108)

**Goal:** Make the norms-audit registry count over the `stop()`/`stopifnot()`
calls in the script's parse tree, run block included, and nothing else.

**Outcome:** `tests/testthat/helper-norms-audit-script.R` (new) walks the parse
tree of `data-raw/audit-norms.R`, replacing a sourced count blind to the run
block and a text grep that matched comments. `stop()` keys on its message
template; positional `stopifnot()` conditions on deparsed text; NAMED ones on
the name -- the runtime message -- as kind `stopifnot_named`, matched by exact
equality. Unkeyable shapes are refused, not skipped: reserved `stopifnot()`
formals, `stop()` args past `call.`/`domain`, unknown kinds. `SCRIPT_ABORTS` is
one entry per site with a fixture; set equality bidirectional. Tests only.

**Decisions:** parse over source-or-grep (a sourced domain is only what the test
loaded). Split RR17 rev 2 -- BC1-BC5 here, BC6-BC11 to a successor milestone
carried as a ROADMAP candidate. `Driving RR` left `—`: RR17's two
`## Binding criteria` sections are both unparseable, so none binds verbatim.

**Review:** returned once on AC1 (named-`stopifnot` hole), which drove the
RB17/RR17 escalation. Round 2: three lenses, 17 findings scored, one actioned --
F4 at 82, an unknown `kind` fell through to the loosest matcher -- fixed at the
gate; 16 logged. AC7's C-locale pin: unfalsifiable, testthat 3e sets LANGUAGE=C.
