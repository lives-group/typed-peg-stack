open import Data.Char
open import Data.List
open import Data.Product

open import Relation.Binary.PropositionalEquality
open import Relation.Nullary.Decidable.Core
open import Relation.Nullary.Negation.Core

module Prefix where


  Prefix : List Char → List Char → Set
  Prefix xs ys = ∃ (λ zs → ys ≡ xs ++ zs)

  Prefix-nil-r : ∀ {x xs} → ¬ Prefix (x ∷ xs) []
  Prefix-nil-r (zs , ())

  prefix? : (xs ys : List Char) → Dec (Prefix xs ys)
  prefix? [] ys = yes (ys , refl)
  prefix? (x ∷ xs) [] = no {!Prefix-nil-r!}
  prefix? (x ∷ xs) (x₁ ∷ ys) = {!!}
