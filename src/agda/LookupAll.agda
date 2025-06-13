open import Data.Fin
open import Data.Nat
open import Data.Product
open import Data.Vec
open import Data.Vec.Relation.Unary.All


module LookupAll where

  lookupAll : ∀ {A : Set}{P : A → Set}{n}{xs : Vec A n} → Fin n → All P xs → ∃ (λ x → P x)
  lookupAll zero (px ∷ v) = _ , px
  lookupAll (suc i) (_ ∷ v) = lookupAll i v
