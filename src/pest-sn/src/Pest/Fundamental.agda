module Pest.Fundamental where

open import Data.Bool using (false)
open import Data.List using ([])
open import Data.Nat using (ℕ)
open import Data.Product using (_,_)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_)

open import Pest.Syntax
open import Pest.Semantics
open import Pest.Typing
open import Pest.Rel

private variable n : ℕ

------------------------------------------------------------------------
-- Fundamental lemma
--
-- Under a grammar environment where every rule is in 𝓡, every well-typed
-- expression is in 𝓡.  Structural induction on the HasType derivation,
-- dispatching to the R-* closure lemmas.

fundamental : ∀ {G : Grammar n} {Γ : Ctx n}
            → (∀ i → 𝓡 G Γ (Γ i) (Grammar.rules G i))
            → ∀ {e : PExp n} {τ : Ty n}
            → HasType Γ e τ
            → 𝓡 G Γ τ e

fundamental _ htEmpty = R-empty
fundamental _ (htTerm {c = c}) = R-term c

fundamental {G = G} {Γ = Γ} hΓ (htVar {i = i}) = record
  { terminates = λ s st →
      let r , h = 𝓡.terminates (hΓ i) s st
      in r , var h
  ; progress = prog
  }
  where
  prog : nullable (Γ i) ≡ false
       → ∀ s st p s' st' → Parses G (var i) s st (ok p s' st') → p ≢ []
  prog hnn s st p s' st' (var h) = 𝓡.progress (hΓ i) hnn s st p s' st' h

fundamental hΓ (htSeq ht₁ ht₂) = R-seq (fundamental hΓ ht₁) (fundamental hΓ ht₂)
fundamental hΓ (htChoice ht₁ ht₂) = R-choice (fundamental hΓ ht₁) (fundamental hΓ ht₂)
fundamental hΓ (htStar ht hnn) = R-star (fundamental hΓ ht) hnn
fundamental hΓ (htNot ht) = R-not (fundamental hΓ ht)
fundamental hΓ (htPush ht) = R-push (fundamental hΓ ht)
fundamental _ htPop = R-pop
fundamental _ htPeek = R-peek
fundamental _ htDrop = R-drop
fundamental _ htPeekall = R-peekall
fundamental _ htDropall = R-dropall
fundamental hΓ (htRep {i = i} ht _) = R-rep (fundamental hΓ ht) i
fundamental hΓ (htBound {i = i} {j = j} ht _ _) = R-bound (fundamental hΓ ht) i j
