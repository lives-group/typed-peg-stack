module Pest.MainTheorem where

open import Data.List using ([])
open import Data.Nat using (ℕ)
open import Data.Product using (∃; _,_; proj₁; proj₂)

open import Pest.Syntax
open import Pest.Semantics
open import Pest.Typing
open import Pest.Rel
open import Pest.Fixpoint
open import Pest.Fundamental

private variable n : ℕ

------------------------------------------------------------------------
-- Strong-normalization theorem for well-typed Pest (stack) grammars
--
-- Combining:
--   fixpoint    : ∀ i → 𝓡 G Γ (Γ i) (rules G i)   (Pest.Fixpoint)
--   fundamental : 𝓡-environment → HasType → 𝓡        (Pest.Fundamental)
--
-- "Strong normalization" here means totality of parsing: every well-typed
-- expression, on every input word and every stack, yields a parse result.

-- Any well-typed expression in a well-typed grammar terminates on every
-- input and every initial stack.
terminates : ∀ {G : Grammar n}
           → (wt : WellTyped G)
           → ∀ {e : PExp n} {τ : Ty n}
           → HasType (WellTyped.Γ wt) e τ
           → ∀ (s : Word) (st : Stack) → ∃ λ r → Parses G e s st r
terminates {G = G} wt ht s st =
  let open WellTyped wt
      env = fixpoint hacy hfix
  in 𝓡.terminates (fundamental env ht) s st

-- Parsing the start symbol of a well-typed grammar always terminates
-- (run from the empty stack, as the Pest interpreter does).
start-terminates : ∀ {G : Grammar n}
                 → WellTyped G
                 → ∀ (s : Word) → ∃ λ r → Parses G (Grammar.start G) s [] r
start-terminates {G = G} wt s =
  let open WellTyped wt
      τ , ht = hstart
  in terminates wt ht s []
