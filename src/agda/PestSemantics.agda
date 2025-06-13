open import Data.Bool hiding (_≟_; _<?_)
open import Data.Char hiding (_<?_)
open import Data.Empty
open import Data.List
open import Data.List.NonEmpty.Base renaming (_∷_ to _∷n_) hiding ([_]; concat; length)
open import Data.List.Properties
open import Data.List.Relation.Binary.Prefix.Heterogeneous hiding (_++_)
open import Data.List.Relation.Binary.Prefix.Heterogeneous.Properties
open import Data.Maybe
open import Data.Nat hiding (_≟_)
open import Data.Nat.Show
open import Data.Product
open import Data.String using (fromList⁺ )
open import Data.Unit hiding (_≟_)

open import Function

open import Relation.Binary.PropositionalEquality hiding ([_])
open import Relation.Nullary.Decidable.Core
open import Relation.Nullary.Negation.Core

module PestSemantics (n : ℕ) where

  open import PestSyntax n

  open import LookupAll

  Fuel : Set
  Fuel = ℕ

  String : Set
  String = List Char

  Stack : Set
  Stack = List String

  prefix-lemma : ∀ {xs ys : List Char} → Prefix _≡_ xs ys → ∃ (λ zs → ys ≡ xs ++ zs)
  prefix-lemma {ys = ys}[] = ys , refl
  prefix-lemma (x ∷ p) with prefix-lemma p
  ... | zs , eq rewrite x | eq = _ , refl

  app-lemma : ∀ {A : Set}{s xs ys zs qs : List A} → s ≡ xs ++ ys → ys ≡ zs ++ qs → s ≡ (xs ++ zs) ++ qs
  app-lemma {_}{_}{xs}{ys}{zs}{qs} eq eq1 rewrite eq | eq1 | ++-assoc xs zs qs = refl

  data Result : String → Set where
    Ok : ∀ {s} pref suf (stk : Stack) → s ≡ pref ++ suf → Result s
    Error : ∀ (s : String)(stk : Stack) → Result s
    OutOfGas : ∀ (s : String)(stk : Stack) → Result s

  norm : Idx → Stack → Maybe Val
  norm ∞ stk = just ∞
  norm to-nat [] = nothing
  norm to-nat ([] ∷ stk) = nothing
  norm to-nat ((x ∷ x₁) ∷ stk) with readMaybe 10 (fromList⁺ (x ∷n x₁))
  ...| just n = just ($ n)
  ...| nothing = nothing
  norm lengthx [] = nothing
  norm lengthx (x ∷ stk) = just ($ (length x))
  norm ($ x) stk = just ($ x)
  norm (idx +i idx₁) stk with norm idx stk | norm idx₁ stk
  ... | just ∞ | just r1 = just ∞
  ... | just ($ x) | just ∞ = just ∞
  ... | just ($ x) | just ($ x₁) = just ($ (x + x₁))
  ...| nothing | _ = nothing
  ...| _       | nothing = nothing
  norm (idx *i idx₁) stk with norm idx stk | norm idx₁ stk
  ... | just ∞ | just r1 = just ∞
  ... | just ($ x) | just ∞ = just ∞
  ... | just ($ x) | just ($ x₁) = just ($ (x * x₁))
  ...| nothing | _ = nothing
  ...| _       | nothing = nothing

  module ParseImpl {sig : Ctx} (rls : Rules sig) where

    parseExp : ∀ {t} → Fuel → Exp sig t → (s : String) → Stack → Result s
    parseExp zero e s stk = OutOfGas s stk
    parseExp (suc fuel) ϵ s stk = Ok [] s stk refl
    parseExp (suc fuel) (# _) [] stk = Error [] stk
    parseExp (suc fuel) (# c) (c1 ∷ s) stk with c ≟ c1
    ... | yes p rewrite p = Ok [ c1 ] s stk refl
    ... | no ¬p = Error (c1 ∷ s) stk
    parseExp (suc fuel) (var v x) s stk with lookupAll v (Rules.exprs rls)
    ... | px = parseExp fuel (proj₂ px) s stk
    parseExp (suc fuel) (e ∙ e₁) s stk with parseExp fuel e s stk
    parseExp (suc fuel) (e ∙ e₁) s stk | Ok pref suf stk₁ x with parseExp fuel e₁ suf stk₁
    ... | Ok pref₁ suf₁ stk₂ x₁  rewrite x | x₁ = Ok (pref ++ pref₁) suf₁ stk₂ (sym (++-assoc pref pref₁ suf₁) )
    ... | Error .suf stk₂ = Error s stk₁
    ... | OutOfGas .suf stk₂ = Error s stk₁
    parseExp (suc fuel) (e ∙ e₁) s stk | Error .s stk₁ = Error s stk
    parseExp (suc fuel) (e ∙ e₁) s stk | OutOfGas .s stk₁ = OutOfGas s stk
    parseExp (suc fuel) (e / e₁) s stk with parseExp fuel e s stk
    parseExp (suc fuel) (e / e₁) s stk | Ok pref suf stk₁ x = Ok pref suf stk₁ x
    parseExp (suc fuel) (e / e₁) s stk | Error .s stk₁ = parseExp fuel e₁ s stk
    parseExp (suc fuel) (e / e₁) s stk | OutOfGas .s stk₁ = Error s stk
    parseExp (suc fuel) (! e) s stk with parseExp fuel e s stk
    ... | Ok pref suf stk₁ x = Error s stk
    ... | Error .s stk₁ = Ok [] s stk refl
    ... | OutOfGas .s stk₁ = OutOfGas s stk
    parseExp (suc fuel) (e ⋆) s stk with parseExp fuel e s stk
    parseExp (suc fuel) (e ⋆) s stk | Ok pref suf stk₁ x with parseExp fuel (e ⋆) suf stk₁
    ... | Ok pref₁ suf₁ stk₂ x₁ rewrite x | x₁ = Ok (pref ++ pref₁) suf₁ stk₂ (sym (++-assoc pref pref₁ suf₁))
    ... | Error .suf stk₂ = Ok pref suf stk₂ x
    ... | OutOfGas .suf stk₂ = OutOfGas s stk
    parseExp (suc fuel) (e ⋆) s stk | Error .s stk₁ = Ok [] s stk₁ refl
    parseExp (suc fuel) (e ⋆) s stk | OutOfGas .s stk₁ = OutOfGas s stk₁
    parseExp (suc fuel) (rep {i = i} x e) s stk with norm i stk
    ... | just ∞ = Error s stk
    ... | just ($ m) = case m of λ {
                         zero     → Ok [] s stk refl
                       ; (suc m') → case parseExp fuel e s stk of λ {
                                      (Ok pref suf stk' x) →
                                          case parseExp fuel (rep {i = $ m'} refl e) suf stk' of λ{
                                             (Ok pref₁ suf₁ stk₁ x₁) → Ok (pref ++ pref₁) suf₁ stk₁ (app-lemma x x₁)
                                          ;  (Error .suf stk₁) → Error s stk
                                          ;  (OutOfGas .suf stk₁) → OutOfGas s stk
                                          }
                                    ; (Error .s stk) → Error s stk
                                    ; (OutOfGas .s stk) → OutOfGas s stk
                                    }
                       }
    ... | nothing = Error s stk
    parseExp (suc fuel) (bound {t = mk hs false} {n = n₁} {v = ∞} x x₁ e x₂) s stk = parseExp fuel ((rep {i = $ n₁} refl e) ∙ (e ⋆)) s stk
    parseExp (suc fuel) (bound {t = mk hs true} {n = n₁} {v = ∞} x x₁ e x₂) s stk = ⊥-elim (x₂ (refl , tt))
    parseExp (suc fuel) (bound {n = n₁}{v = $ x₃} x x₁ e x₂) s stk with n₁ <? x₃
    ...| yes p = case parseExp fuel (rep {i = $ x₃} refl e) s stk of λ {
                   (Ok pref suf stk x) → Ok pref suf stk x
                 ; (Error .s stk) → parseExp fuel (bound {i = $ n₁}{j = $ (x₃ ∸ 1)} refl refl e λ ()) s stk
                 ; (OutOfGas .s stk) → OutOfGas s stk
                 }
    ...| no q = Error s stk
    parseExp (suc fuel) (push e) s stk with parseExp fuel e s stk
    ... | Ok pref suf stk₁ x = Ok pref suf (pref ∷ stk₁) x
    ... | Error .s stk₁ = Error s stk
    ... | OutOfGas .s stk₁ = OutOfGas s stk₁
    parseExp (suc fuel) pop s [] = Error s []
    parseExp (suc fuel) pop s (x ∷ stk) with prefix? _≟_ x s
    ... | yes p = case prefix-lemma p of λ { (zs , eq) → Ok x zs stk eq }
    ... | no q = Error s stk
    parseExp (suc fuel) peek s [] = Error s []
    parseExp (suc fuel) peek s (x ∷ stk) with prefix? _≟_ x s
    ... | yes p = case prefix-lemma p of λ {(zs , eq) → Ok x zs (x ∷ stk) eq}
    ... | no q = Error s stk
    parseExp (suc fuel) drop s [] = Error s []
    parseExp (suc fuel) drop s (x ∷ stk) = Ok [] s stk refl
    parseExp (suc fuel) peekall s [] = Error s []
    parseExp (suc fuel) peekall s (x ∷ stk) with prefix? _≟_ (concat (x ∷ stk)) s
    ... | yes p = case prefix-lemma p of λ { (zs , eq) → Ok (concat (x ∷ stk)) zs [] eq }
    ... | no q = Error s stk
    parseExp (suc fuel) popall s [] = Error s []
    parseExp (suc fuel) popall s (x ∷ stk) = Ok [] s [] refl


  parse : Fuel → Grammar → (s : String) → Result s
  parse fuel g s = parseExp fuel (Grammar.start g) s []
      where
        open ParseImpl (Grammar.rules g)
