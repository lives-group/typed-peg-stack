# Pest Control: a formal model of the Pest parser generator

This repository accompanies the paper on a type system for the Pest parsing
expression grammar calculus with an implicit stack. It contains:

- `src/pest-sn/` : the Agda formalization. A machine-checked **strong
  normalization** (parsing-totality) proof for well-typed grammars, covering the
  core PEG operators, the six stack operators, and exact and interval repetition.
  It has no postulates, holes, `TERMINATING` pragmas or `trustMe`.
- `src/racket/` : the `typed-peg` prototype (`#lang typed-peg-stack`). It infers
  PEG types and rejects grammars that can loop. It ships two corpora (`corpus/`)
  and two solver back-ends: Rosette, which is the default and bundles its own
  solver, and an external Z3 process, kept as an independent cross-check.
- `papers/cola/` : the paper itself, written as a literate Agda document
  (`paper.lagda.tex`) whose code is checked against `src/pest-sn`.
- `src/lean/` : the earlier Lean development from the conference version, whose
  fuel-based soundness result is superseded by `src/pest-sn/`. Kept for
  reference; it is not needed to reproduce any result reported in the paper.

The environment is described by a Nix flake, which pins every tool version. The
three sections below reproduce the results. Everything also runs on a host that
has Agda with agda-stdlib and Racket installed, using the same `make` targets
directly.

## 1) Starting the environment

You need Nix with flakes enabled. Enter the development shell:

```
nix develop
```

This provides Agda with the standard library, Racket, GNU make and, optionally,
Z3, Python (for re-harvesting the grammars) and a TeX installation (for the
paper). The first run downloads the toolchain from the binary cache; afterwards
it is instantaneous. Inside the shell, `make help` lists every target and
`make all` reproduces everything.

To reproduce a single target without entering the shell interactively:

```
nix develop -c make verify
nix develop -c make experiments
```

If you only care about the proof, a smaller shell with just Agda is available as
`nix develop .#agda`.

## 2) Verifying the Agda formalization

The quickest check does not even need the shell. The flake's default package
typechecks the development inside the Nix sandbox and fails the build if it does
not go through:

```
nix build
```

For the same check plus the constructivity audit, use the `make` target:

```
nix develop -c make verify
```

This runs `agda src/Pest/MainTheorem.agda` inside `src/pest-sn` (which imports
the whole development and thus typechecks it), and then greps the sources to
confirm there are no `postulate`, `{! !}`, `TERMINATING` or `trustMe`. A
successful run ends with:

```
OK: no postulates, holes, TERMINATING pragmas or trustMe in src/pest-sn
```

The main theorem lives in `src/pest-sn/src/Pest/MainTheorem.agda`:

```agda
terminates       : WellTyped G → HasType Γ e τ → ∀ s st → ∃ λ r → Parses G e s st r
start-terminates : WellTyped G → ∀ s → ∃ λ r → Parses G (Grammar.start G) s [] r
```

that is, every well-typed grammar yields a parse result on every input and every
initial stack.

## 3) Reproducing the grammar experiments

The prototype is exercised on two corpora. The `experiments` target installs the
`typed-peg-stack` Racket collection and Rosette (idempotent, into the user's
Racket package scope) and runs both:

```
nix develop -c make experiments
```

You can also run them individually.

- `make corpus` runs a curated corpus of 18 grammars that includes intentionally
  looping grammars. It validates that the type system **rejects** the looping
  patterns. Expected result: 12 accepted, 6 rejected, matching the expectation
  recorded for every grammar.

- `make real-corpus` runs a corpus of **real** Pest grammars harvested from
  projects listed in `pest-parser/awesome-pest` (the pest project's own JSON,
  TOML, SQL, HTTP, calculator and indentation grammars, and grammars from
  `caith`, `elastic-rs`, `qubit`, `rouler`, `py_literal`, `pta-parser`,
  `ashpaper` and `melody`). Provenance (repository, path, commit) is recorded in
  `src/racket/corpus/real/manifest.tsv`. Expected result: of 32 grammar files, 25
  are analyzable and all 25 are accepted, none is rejected, and 7 are
  not-expressible (fragments of multi-file grammars, or constructs outside the
  prototype's fragment).

Both runners report the verdict of the Rosette back-end. When a `z3` executable
is on `PATH` they additionally print a Z3 column and the agreement between the
two back-ends (18/18 on the curated corpus; on the real corpus Rosette decides
five grammars on which the legacy Z3-text parser fails, and agrees with Z3 on
every grammar both can decide). When there is no `z3`, the cross-check column is
omitted and the run says so; the reported results are unaffected.

The translated grammars (`corpus/real/*.tps`) are committed, so `make real-corpus`
runs offline. To reproduce the harvesting from scratch (which needs network
access), run:

```
nix develop -c make harvest
```

This clones the source repositories, reconstructs the multi-file grammars, and
regenerates the translations and the manifest using
`src/racket/corpus/pest2tps.py`. The translation is sound for the type system:
since the analysis depends only on nullability and head sets, every
single-character matcher is abstracted to one terminal, while the structure that
drives the analysis (non-terminal references, sequence, choice, repetition,
predicates and the stack operators) is preserved.

## Optional: building the paper

`paper.lagda.tex` is a literate Agda document. Building the PDF runs
`agda --latex` (which also typechecks the code quoted in the paper against
`src/pest-sn`) and then LuaLaTeX. The devShell provides both:

```
nix develop -c make paper
```

produces `papers/cola/paper.pdf`.

## On the SMT solver

The type inference reduces well-formedness to a constraint-satisfaction problem.
The prototype solves it through **Rosette**, a solver-aided Racket language,
which drives a solver through its API and returns a model as Racket values.
Rosette bundles its own solver, so **no separately installed Z3 is required** to
use the language, run the examples, or reproduce either corpus.

The original back-end (`src/racket/typing/constraint-solver.rkt`) serialises the
constraints to an SMT-LIB script, shells out to an external `z3` executable, and
parses Z3's textual model with a hand-written parser, which is sensitive to the
model syntax of a particular Z3 version. It is retained as an independent
cross-check of the Rosette results, and it now raises a clear error when no `z3`
is on `PATH` rather than returning an empty answer that would read as an
inconclusive verdict.
