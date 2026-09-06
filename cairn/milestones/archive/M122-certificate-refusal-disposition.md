# M122: Make the certificate suite exhaustive over the routes the shipped pricing can take
**Status:** done (2026-09-05, PR #155 https://github.com/jmgirard/circumplex/pull/155)
**Goal:** At counterexample B the shipped pricing may price or may refuse on the platform's
LU roundoff; assert both routes exhaustively rather than freeze one machine's outcome.
**Outcome:** `cert_true_error()` admits a refusal where a committed `rcond` band straddles
eps — read from a per-case `cert_admission` table, never a case name — and asserts its
identity (`"unidentified"` from both `v` and `u`). The counterexample-B block branches on the
recorded disposition: refusing route asserts the sentinel and the `"uncertified"` predicate,
priced route keeps its brackets. A double-double-versus-exact assertion runs on both, so the
refusing platform keeps a live oracle check; `dd_ulp()` measures against `ulp(hi)`.
Dispositions are four pinned constants (detail separate); `cert_record()` refuses anything
else, the table prints every run, and the detector needs an ANCHOR priced — not merely a
case — plus `cxb` in {priced, refused}. `test-axes-certificate-refusal.R` got the same
repair; a committed-bytes mismatch fails instead of skipping. `cert_bracket()` selects its
floor branch by an `at_floor` argument; `cert_rel()` refuses a zero denominator, and
`tools/arm64/testfile.sh` sweeps stale build products and runs with the namespace in scope.
No exported behavior changed; macOS prices B, linux-arm64 refuses it, both green.
**Decisions:** D-055 (recorded at plan time).
**Review:** Three-lens fan-out; blame-history clean, prior-review one, diff-bug twelve →
eleven consolidated. Five fixed on the branch: `dd_ulp()`'s relative-to-ulp conversion (the
`< 0.5` bound really demanded 0.391–0.461 ulp), DESIGN.md still listing both fixed
fragilities as open, a false comment claiming the `cert_bracket` probe discriminates its
repair (it cannot — the new failure set is a strict subset of the old below the floor),
"third CRAN rejection" against D-055's "second", and an unasserted unit-diagonal dependency
in the refusal-suite probe. Two → candidate row; three rejected as too small; one no-action.
