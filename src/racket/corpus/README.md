# typed-peg corpus evaluation

`run-corpus.rkt` runs the type-inference algorithm on a curated corpus of 18
grammars written in the `typed-peg-stack` concrete syntax and reports, for each,
whether it is **accepted** (well-typed, cannot loop) or **rejected** (the
constraints are unsatisfiable, so the grammar can loop). It runs both solver
back-ends side by side and checks they agree.

Run it with:

```
raco pkg install --auto --name typed-peg-stack   # once, from ../ (the racket dir)
racket corpus/run-corpus.rkt
```

Result (Z3 4.15.4 and Rosette): 12 accepted, 6 rejected, 0 errors, and the two
back-ends agree on all 18 grammars.

## Two solver back-ends

- **Z3 (original).** `typing/constraint-solver.rkt` serialises the constraints
  to an SMT-LIB script, shells out to the external `z3` executable via `system`,
  and parses Z3's textual model with a hand-written lexer/parser
  (`typing/solver/model-*.rkt`). This last step is brittle and breaks on Z3
  versions newer than the 4.8.14 the parser was written for (the heartbeat
  grammar, for example, triggers a "Model parser error" under Z3 4.15).

- **Rosette (new).** `typing/rosette-solver.rkt` expresses the same constraints
  as Rosette symbolic values and lets Rosette drive the solver through its API.
  There is no external process and no solver output to parse; Rosette bundles its
  own solver, so a separate Z3 install is not required. A type is a symbolic
  boolean (nullability) plus one symbolic boolean per non-terminal (head-set
  membership); the type operations mirror the SMT `define-fun`s. The accept/reject
  decision matches the Z3 back-end on the whole corpus.

  To make Rosette the solver used by the reader, replace the `infer`-based gate in
  `reader.rkt` with `(rosette-solve (gen-constr grammar))` and treat `'unsat` as
  the ill-typed case (the inferred types are only used for that gate, not by the
  generated parser/pretty-printer).

## Prototype fixes made during this evaluation

- `typing/constraint-gen.rkt`: matched the nonexistent `pdropall`; corrected to
  `ppopall` (every `dropall` grammar previously failed to typecheck).
- `grammar.rkt`: the char-class recursion returned the raw token string instead
  of a `pchr`, and the `?` operator used the constructor `peps` instead of an
  instance `(peps)`. Both are fixed.
