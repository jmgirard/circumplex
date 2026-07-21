# M45: Source notes for the RANDALL structure test pair

**Status:** done (2026-07-20, PR #71 https://github.com/jmgirard/circumplex/pull/71)

**Goal:** Author `cairn/references/` pages for the two sources the package relies
on for its RANDALL structure test, discharging the standing ROADMAP candidate.

**Outcome:** `hubert1987.md` banks the normalized order index `(A−D)/(A+D+T)`
(Eq. 3/5), the exact p-value, and the `(M+1)/(N+1)` Monte-Carlo form;
`tracey1997.md` banks the RANDALL Correspondence Index and the RIASEC example.
Both reconcile against `structure_randall()`/`structure_randall_test()`/
`randall_predictions()`, cross-checked live on Tracey Table 1 (72 predictions;
code A=69/D=2/T=1 → .9167 vs CI .93). `INDEX.md` gains both entries and retires
the RANDALL owes-a-page ledger; `test-fit_structure_api.R` miscount fixed. Docs
only apart from that comment; `check(--no-manual)` 0/0/0.

**Decisions:** M45-D1 — the strict-`>` scores ties as disagreements, so the code
index departs from Tracey's CI by T/N on tied inputs (.92 vs .93 on Table 1);
left unchanged (out of scope; ties measure-zero for continuous r). D-023
(forward-source prospects) was recorded at plan time.

**Review:** three lenses + scorer; F1=90/F2=88/F3=83, all fixed. F1/F2 were
M40-D2 Extraction over-claims (rendered ranges excluded p.175/p.166 carrying the
exact p-value and `p=.0167`); re-rendered both, which surfaced a Tracey printed
slip (`.0167 (1/720)`; =1/60). F3 fixed a "switches to sampled" wording error.
