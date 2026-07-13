module Pest.Typing where

open import Data.Bool using (Bool; true; false; _∧_; _∨_; if_then_else_)
open import Data.Fin using (Fin)
open import Data.Fin.Subset using (Subset; _∈_; _∉_; _⊆_; _∪_; ⊥; ⁅_⁆)
open import Data.Nat using (ℕ)
open import Data.Product using (∃; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_)
open import Relation.Nullary using (¬_)

open import Pest.Syntax
open import Pest.Index using (Index; Value; infinite; norm-static)

-- The type of a parsing expression over a grammar with n nonterminals.
-- nullable: true iff the expression may succeed on empty input.
-- head-set: the set of nonterminals reachable at the leftmost (head) position.
record Ty (n : ℕ) : Set where
  constructor mkTy
  field
    nullable : Bool
    head-set : Subset n

open Ty public

-- A typing context assigns a type to each nonterminal.
Ctx : ℕ → Set
Ctx n = Fin n → Ty n

-- Type operators

-- Sequence e₁ · e₂.
_⊗_ : ∀ {n} → Ty n → Ty n → Ty n
τ₁ ⊗ τ₂ = mkTy
  (nullable τ₁ ∧ nullable τ₂)
  (head-set τ₁ ∪ (if nullable τ₁ then head-set τ₂ else ⊥))

-- Ordered choice e₁ / e₂.
_⊕_ : ∀ {n} → Ty n → Ty n → Ty n
τ₁ ⊕ τ₂ = mkTy
  (nullable τ₁ ∨ nullable τ₂)
  (head-set τ₁ ∪ head-set τ₂)

-- Kleene star e*.
starTy : ∀ {n} → Ty n → Ty n
starTy τ = mkTy true (head-set τ)

-- Not-predicate !e.
neg : ∀ {n} → Ty n → Ty n
neg τ = mkTy true (head-set τ)

-- Exact / indexed repetition e^i.  Conservatively typed as nullable (the
-- head set is the body's): this is sound for well-formedness and means the
-- repetition carries no progress obligation, since its finite iteration
-- count always terminates regardless of whether the body is nullable.
repTy : ∀ {n} → Ty n → Ty n
repTy τ = mkTy true (head-set τ)

-- Interval repetition e[i,j], conservatively typed as nullable (like repTy).
boundTy : ∀ {n} → Ty n → Ty n
boundTy τ = mkTy true (head-set τ)

-- Typing judgment.  HasType Γ e τ: under context Γ, expression e has type τ.
data HasType {n : ℕ} (Γ : Ctx n) : PExp n → Ty n → Set where

  htEmpty  : HasType Γ empty (mkTy true ⊥)

  htTerm   : ∀ {c} → HasType Γ (term c) (mkTy false ⊥)

  htVar    : ∀ {i} → HasType Γ (var i) (mkTy (nullable (Γ i)) (⁅ i ⁆ ∪ head-set (Γ i)))

  htSeq    : ∀ {e₁ e₂ τ₁ τ₂}
           → HasType Γ e₁ τ₁
           → HasType Γ e₂ τ₂
           → HasType Γ (seq e₁ e₂) (τ₁ ⊗ τ₂)

  htChoice : ∀ {e₁ e₂ τ₁ τ₂}
           → HasType Γ e₁ τ₁
           → HasType Γ e₂ τ₂
           → HasType Γ (choice e₁ e₂) (τ₁ ⊕ τ₂)

  htStar   : ∀ {e τ}
           → HasType Γ e τ
           → nullable τ ≡ false
           → HasType Γ (star e) (starTy τ)

  htNot    : ∀ {e τ}
           → HasType Γ e τ
           → HasType Γ (not e) (neg τ)

  -- Stack operators.
  --
  -- push e inherits the type of its body: it consumes exactly what e consumes
  -- and only manipulates the stack, which does not appear in the type.
  htPush   : ∀ {e τ}
           → HasType Γ e τ
           → HasType Γ (push e) τ

  -- The remaining stack operators are nullable with empty head set: they never
  -- expand a nonterminal at the head and may succeed without consuming input
  -- (the matched word can be empty).  Hence they carry no progress obligation.
  htPop     : HasType Γ pop     (mkTy true ⊥)
  htPeek    : HasType Γ peek    (mkTy true ⊥)
  htDrop    : HasType Γ drop    (mkTy true ⊥)
  htPeekall : HasType Γ peekall (mkTy true ⊥)
  htDropall : HasType Γ dropall (mkTy true ⊥)

  -- Exact repetition e^i is well-formed when the index is statically finite
  -- (the paper's norm*(i) ≠ ∞ constraint), ruling out unbounded repetition of
  -- a nullable body.
  htRep     : ∀ {i e τ}
            → HasType Γ e τ
            → ¬ (norm-static i ≡ infinite)
            → HasType Γ (rep i e) (repTy τ)

  -- Interval repetition e[i,j] is well-formed when both bounds are statically
  -- finite.  (The unbounded form e[i,∞] is written as (rep i e) followed by
  -- (star e), where the non-nullable requirement of star applies.)
  htBound   : ∀ {i j e τ}
            → HasType Γ e τ
            → ¬ (norm-static i ≡ infinite)
            → ¬ (norm-static j ≡ infinite)
            → HasType Γ (bound i j e) (boundTy τ)

-- A grammar G is well-typed if there exists a context Γ such that:
--   (1) fixpoint:   every rule body is typed by Γ under Γ;
--   (2) acyclicity: no nonterminal i appears in its own head set.
record WellTyped {n : ℕ} (G : Grammar n) : Set where
  field
    Γ      : Ctx n
    hfix   : ∀ (i : Fin n) → HasType Γ (Grammar.rules G i) (Γ i)
    hacy   : ∀ (i : Fin n) → i ∉ head-set (Γ i)
    hstart : ∃ λ τ → HasType Γ (Grammar.start G) τ
