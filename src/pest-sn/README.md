# pest-sn

A machine-checked **strong-normalization** (parsing-totality) proof, in Agda, for
the well-typed fragment of the **Pest** parsing-expression-grammar calculus with
an implicit parsing stack.

This development extends the core-PEG proof in `peg-sn`
(`~/Documents/projects/agda/peg-sn`) — the logical-relations (Tait-style)
argument with the lexicographic `(input length, head-set cardinality)` measure —
to cover the operators that Pest adds on top of standard PEGs.

## Main result

`Pest.MainTheorem`:

```agda
terminates       : WellTyped G → HasType Γ e τ → ∀ s st → ∃ λ r → Parses G e s st r
start-terminates : WellTyped G → ∀ s → ∃ λ r → Parses G (Grammar.start G) s [] r
```

Every well-typed Pest grammar yields a parse result on every input word and every
initial stack; parsing never diverges. "Strong normalization" is read here as
totality of the (relational, big-step) parser, exactly as in `peg-sn`.

The proof is **fully constructive**: no `postulate`, no `{-# TERMINATING #-}`, no
`{! !}` holes, no `trustMe`, no `--rewriting`.

## What is proved (status)

**Milestone A — core PEG + stack operators (COMPLETE, typechecks).**
The calculus of `Pest.Syntax` covers the core PEG constructors (`empty`, `term`,
`var`, `seq`, `choice`, `star`, `not`) plus the six Pest stack operators:

| operator  | behaviour                                             | type            |
|-----------|-------------------------------------------------------|-----------------|
| `push e`  | parse `e`, push the consumed prefix onto the stack    | inherits `e`    |
| `pop`     | match the top-of-stack word, then pop it              | `⟨true, ∅⟩`     |
| `peek`    | match the top-of-stack word, keep the stack           | `⟨true, ∅⟩`     |
| `drop`    | pop the top word without consuming input              | `⟨true, ∅⟩`     |
| `peekall` | match the concatenation of the whole stack            | `⟨true, ∅⟩`     |
| `dropall` | empty the stack without consuming input               | `⟨true, ∅⟩`     |

Key observation making the extension clean: every stack operator is **nullable**
except `push` (which inherits its body's type), so none of them carries a progress
obligation, and none introduces unbounded recursion. The stack is threaded through
the semantics and the logical relation but does **not** enter the termination
measure — the `(length, rank)` measure of the core proof still governs.

**Milestone B — exact / indexed repetition (COMPLETE, typechecks).**
The exact repetition operator `rep i e` (the paper's `e^i`) and the index
sublanguage live in `Pest.Index` (`Index`, `Value`, `norm : Index → Stack →
Maybe Value`, `norm-static`) with literals, `∞`, the two stack-top queries
`topNat`/`topLen` (the paper's `top.tonat`/`top.length`), and `plus`/`times`.
Repetition semantics uses a mutually-defined `RepN G e k` relation (e run
exactly k times). Termination of `e^i` follows by induction on the finite
repetition count `k` obtained from `norm i st`; when the index does not
normalise to a number — a non-numeric top for `topNat`, an empty stack, or an
infinite index — the parser fails gracefully (`repFailNone`/`repFailInf`), so
totality is preserved. `rep` is typed as `⟨true, first τ⟩` (conservatively
nullable), which discharges its progress obligation and keeps the `(length,
rank)` measure intact; the index dimension is handled by a standalone `repN`
helper so the lexicographic recursion in `Pest.Fixpoint` only ever calls the
structurally-smaller body.

**Milestone C — interval repetition (COMPLETE, typechecks).**
The interval operator `bound i j e` (the paper's `e[i,j]`) with the greedy
semantics of the Lean model: it tries the largest count in `[n1,n2]` first and
backs off, modelled by a gap-indexed relation `BoundR G e lo d` whose
termination is induction on the gap `d`. `bound` is restricted to
statically-finite bounds; the unbounded interval `e[i,∞]` is definable as the
composition `(rep i e)` followed by `(star e)`, where the non-nullable
requirement of `star` supplies the missing side condition. As with `rep`, an
undefined or infinite index fails gracefully and the type is conservatively
nullable.

With Milestone C the development covers the **entire Pest operator set**.

## Module layout

- `Pest.Base`        — words and stacks
- `Pest.Index`       — the repetition-index sublanguage and its evaluation (`norm`, `norm-static`)
- `Pest.Syntax`      — parsing expressions and grammars
- `Pest.Semantics`   — mutual relational big-step semantics (`Parses`/`RepN`, stack-threaded), decidable prefix matching
- `Pest.Typing`      — nullable + head-set types, typing judgment, `WellTyped`
- `Pest.Rel`         — the logical relation `𝓡`, `firstMono`/`rank`, closure lemmas (one per operator)
- `Pest.Fundamental` — every well-typed expression is in `𝓡`
- `Pest.Fixpoint`    — the lexicographic `lex-key`; every rule body is in `𝓡`
- `Pest.MainTheorem` — the strong-normalization theorem

## Building

Requires Agda 2.7.x and the standard library (2.2), registered globally (same as
`peg-sn`). Then:

```
agda src/Pest/MainTheorem.agda
```
