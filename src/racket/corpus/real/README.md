# Real Pest grammar corpus

This directory holds `.pest` grammars **harvested from real projects** that use
the Pest parser generator, together with the tool-syntax translations that the
inference algorithm is run on. It exists to answer the empirical question of how
restrictive the type system is on grammars written in practice.

## Provenance

The grammars come from projects listed in
[pest-parser/awesome-pest](https://github.com/pest-parser/awesome-pest):
`pest` itself (its example, test and reference grammars: JSON, TOML, SQL, HTTP,
calc, lists, ...), plus `caith`, `elastic-rs`, `qubit`, `rouler`, `py_literal`,
`pta-parser`, `ashpaper` and `melody`. `manifest.tsv` records, for every grammar,
its source repository, the path of the original `.pest` file, the commit it was
taken from, and whether it translated. The `pta_full_*` entries are the complete
pta grammars reconstructed by concatenating the shared `base.pest` with each
domain fragment, as pest's multi-file derive does.

## Translation

The `.pest` files use the full Pest surface syntax, of which the prototype
implements a subset. They are translated to the tool syntax by `../pest2tps.py`.
The translation is sound for the type system because the analysis depends only on
nullability and head-sets: every single-character matcher (string/char literal,
character range, `ASCII_*` built-in, `ANY`) has the type `<false, empty>`, so all
of them are abstracted to the single terminal `.`; zero-width anchors (`SOI`,
`EOI`) become `epsilon`; the stack operators are mapped to their tool
counterparts; and the structure that actually drives the analysis (non-terminal
references, sequence, choice, repetition, predicates, stack operations) is
preserved. Dropped or unsupported: implicit whitespace insertion, atomicity and
silence modifiers, node tags, `PUSH` slices, and the Pratt-parser rules. Grammars
that are fragments of a multi-file grammar (undefined references) or that use a
construct outside this subset are reported as **not-expressible**, not silently
dropped.

## Result (Z3 4.15.4 and Rosette)

Run with `racket corpus/run-real-corpus.rkt`. Of 32 grammar files, 25 were
analyzable and **all 25 were accepted** as well-formed; **none was rejected**. The
remaining 7 are not-expressible (undefined references in multi-file or
Pratt-parser grammars). The type system is therefore not overly restrictive on
real grammars; the separate `../run-corpus.rkt` corpus of intentionally looping
grammars confirms it does reject the four looping patterns it is designed to
exclude. On the 5 grammars whose Z3 model output the original textual parser could
not handle, the Rosette solver still returned a verdict, and it agrees with Z3 on
every grammar both can decide.
