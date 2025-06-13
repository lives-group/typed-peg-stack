open import Data.Bool
open import Data.Char
open import Data.Fin hiding (#_ ; _+_)
open import Data.List hiding (drop)
open import Data.List.Membership.Propositional
open import Data.Product
open import Data.Nat hiding (_/_ ; _^_)
open import Data.Vec renaming (_++_ to _++v_ ; lookup to lookupV) hiding (drop)
open import Data.Vec.Relation.Unary.All

open import Relation.Binary.PropositionalEquality
open import Relation.Nullary.Negation

module PestSyntax (n : ℕ) where

  -- definition of head-set

  HeadSet : Set
  HeadSet = List (Fin n)

  -- definition of types

  record Type : Set where
    constructor mk
    field
      head-set : HeadSet
      nullable : Bool

  open Type

  -- operators for combining types

  _⇒_ : Bool → HeadSet → HeadSet
  true ⇒ hs = hs
  false ⇒ _ = []

  _⊗_ : Type → Type → Type
  (mk hs n) ⊗ (mk hs' n') = mk (hs ++ n ⇒ hs') (n ∧ n')

  _⊕_ : Type → Type → Type
  (mk hs n) ⊕ (mk hs' n') = mk (hs ++ hs') (n ∨ n')

  -- defining context for intrinsically typed syntax

  Ctx : Set
  Ctx = Vec Type n

  -- valid variables

  validVar : Ctx → Fin n → Set
  validVar sig v = ¬ (v ∈ head-set (lookupV sig v))

  -- index syntax

  data Idx : Set where
    ∞ to-nat lengthx : Idx
    $_ : ℕ → Idx
    _+i_ _*i_ : Idx → Idx → Idx

  -- normalization of indexes (static)

  data Val : Set where
    ∞ : Val
    $_ : ℕ → Val

  norm* : Idx → Val
  norm* ∞ = ∞
  norm* to-nat = $ 0
  norm* lengthx = $ 0
  norm* ($ x) = $ x
  norm* (idx +i idx₁) with norm* idx | norm* idx₁
  ... | ∞ | v2 = ∞
  ... | $ x | ∞ = ∞
  ... | $ x | $ x₁ = $ (x + x₁)
  norm* (idx *i idx₁) with norm* idx | norm* idx₁
  ... | ∞ | v2 = ∞
  ... | $ x | ∞ = ∞
  ... | $ x | $ x₁ = $ (x * x₁)

  -- more operations on types

  _^_ : Type → ℕ → Type
  mk hs b ^ zero = mk hs true
  t       ^ suc m = t

  lim : Type → ℕ → Val → Type
  lim (mk hs b)  zero v = mk hs true
  lim t          (suc n) _ = t

  -- syntax of pest parser expressions

  data Exp (sig : Ctx) : Type → Set where
    ϵ : Exp sig (mk [] true)
    #_ : Char → Exp sig (mk [] false)
    var : ∀ v → validVar sig v → Exp sig (lookupV sig v)
    _∙_ : ∀ {t t'} → Exp sig t → Exp sig t' → Exp sig (t ⊗ t')
    _/_ : ∀ {t t'} → Exp sig t → Exp sig t' → Exp sig (t ⊕ t')
    !_ : ∀ {b hs} → Exp sig (mk hs b) → Exp sig (mk hs true)
    _⋆ : ∀ {hs} → Exp sig (mk hs false) → Exp sig (mk hs true)
    -- new repetition operators
    rep : ∀ {t i n} → norm* i ≡ $ n → Exp sig t → Exp sig (t ^ n)
    bound : ∀ {t i j n v} → norm* i ≡ $ n →
                            norm* j ≡ v →
                            Exp sig t →
                            ¬ (v ≡ ∞ × T (nullable t)) →
                            Exp sig (lim t n v)
    -- stack operators
    push : ∀ {t} → Exp sig t → Exp sig t
    pop : Exp sig (mk [] true)
    peek : Exp sig (mk [] true)
    drop : Exp sig (mk [] true)
    peekall : Exp sig (mk [] true)
    popall : Exp sig (mk [] true)

  -- definition of grammars

  record Rules (sig : Ctx) : Set where
    constructor MkRules
    field
      exprs : All (λ t → Exp sig t) sig

  record Grammar : Set where
    constructor MkGrammar
    field
      sig : Ctx
      type : Type
      start : Exp sig type
      rules : Rules sig
