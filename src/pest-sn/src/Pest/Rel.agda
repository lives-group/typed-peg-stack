module Pest.Rel where

open import Data.Bool
  using (Bool; true; false; _∧_; _∨_; if_then_else_)
open import Data.Char
  using (Char)
open import Data.Char.Properties
  using () renaming (_≟_ to _≟ᶜ_)
open import Data.Empty
  using (⊥-elim)
open import Data.Fin
  using (Fin)
open import Data.Fin.Subset
  using (Subset; _∈_; _∉_; _⊆_; _⊂_; _∪_; ⊥; ⁅_⁆; ∣_∣)
open import Data.Fin.Subset.Properties
  using ( x∈⁅x⁆; x∈⁅y⁆⇒x≡y; ∉⊥
        ; x∈p∪q⁻; x∈p∪q⁺; p⊆p∪q; q⊆p∪q
        ; p⊂q⇒∣p∣<∣q∣; p⊆q⇒∣p∣≤∣q∣; ⊆-trans)
open import Data.List
  using (List; []; _∷_; _++_; length; concat)
open import Data.List.Properties
  using (++-assoc; ++-conicalˡ; ++-conicalʳ; length-++)
open import Data.Maybe
  using (just; nothing)
open import Data.Nat
  using (ℕ; _<_; _≤_; _≤?_; _+_; _∸_; s≤s; z≤n; zero; suc)
open import Data.Nat.Induction
  using (Acc; acc; <-wellFounded)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; <-≤-trans; m≤n+m; ≰⇒>)
open import Data.Product
  using (∃; _,_; _×_)
open import Data.Sum
  using (_⊎_; inj₁; inj₂)
open import Relation.Binary.PropositionalEquality
  using (_≡_; _≢_; refl; sym; trans; cong; subst)
open import Relation.Nullary
  using (¬_; yes; no)

open import Pest.Syntax
open import Pest.Semantics
open import Pest.Typing
open import Pest.Index using (Index; Value; finite; infinite; norm)

private variable n : ℕ

------------------------------------------------------------------------
-- First-set monotonicity and rank

FirstMono : Ctx n → Set
FirstMono {n} Γ = ∀ (i j : Fin n) → j ∈ head-set (Γ i) → head-set (Γ j) ⊆ head-set (Γ i)

rank : Ctx n → Fin n → ℕ
rank Γ i = ∣ head-set (Γ i) ∣

firstMono : {G : Grammar n} {Γ : Ctx n}
          → (∀ i → HasType Γ (Grammar.rules G i) (Γ i))
          → (∀ i → i ∉ head-set (Γ i))
          → FirstMono Γ
firstMono {n} {G} {Γ} hfix hacy i j hj =
  key-B ∣ head-set (Γ i) ∣ (<-wellFounded _) (hfix i) ≤-refl j hj
  where
  Oracle : ℕ → Set
  Oracle m = ∀ {e : PExp n} {τ : Ty n}
           → HasType Γ e τ → ∣ head-set τ ∣ < m
           → ∀ k → k ∈ head-set τ → head-set (Γ k) ⊆ head-set τ

  key-A : ∀ m → Oracle m → ∀ {e : PExp n} {τ : Ty n}
        → HasType Γ e τ → ∣ head-set τ ∣ ≤ m
        → ∀ k → k ∈ head-set τ → head-set (Γ k) ⊆ head-set τ

  key-A _ _ htEmpty _ k hk = ⊥-elim (∉⊥ hk)
  key-A _ _ htTerm _ k hk = ⊥-elim (∉⊥ hk)

  key-A _ or (htVar {i = vi}) hle k hk
    with x∈p∪q⁻ ⁅ vi ⁆ (head-set (Γ vi)) hk
  ... | inj₁ hkvi =
    subst (λ x → head-set (Γ x) ⊆ ⁅ vi ⁆ ∪ head-set (Γ vi))
          (sym (x∈⁅y⁆⇒x≡y vi hkvi))
          (q⊆p∪q _ _)
  ... | inj₂ hkvi =
    let proper : head-set (Γ vi) ⊂ ⁅ vi ⁆ ∪ head-set (Γ vi)
        proper = q⊆p∪q _ _ , vi , x∈p∪q⁺ (inj₁ (x∈⁅x⁆ vi)) , hacy vi
        sub = or (hfix vi) (<-≤-trans (p⊂q⇒∣p∣<∣q∣ proper) hle) k hkvi
    in ⊆-trans sub (q⊆p∪q _ _)

  key-A m or (htSeq {τ₁ = τ₁} {τ₂ = τ₂} ht₁ ht₂) hle k hk
    with x∈p∪q⁻ (head-set τ₁) (if nullable τ₁ then head-set τ₂ else ⊥) hk
  ... | inj₁ hk₁ =
    let cp = if nullable τ₁ then head-set τ₂ else ⊥
    in ⊆-trans (key-A m or ht₁ (≤-trans (p⊆q⇒∣p∣≤∣q∣ (p⊆p∪q {p = head-set τ₁} cp)) hle) k hk₁)
               (p⊆p∪q {p = head-set τ₁} cp)
  ... | inj₂ hk₂ with nullable τ₁
  ... | false = ⊥-elim (∉⊥ hk₂)
  ... | true =
    ⊆-trans (key-A m or ht₂ (≤-trans (p⊆q⇒∣p∣≤∣q∣ (q⊆p∪q (head-set τ₁) (head-set τ₂))) hle) k hk₂)
            (q⊆p∪q (head-set τ₁) (head-set τ₂))

  key-A m or (htChoice {τ₁ = τ₁} {τ₂ = τ₂} ht₁ ht₂) hle k hk
    with x∈p∪q⁻ (head-set τ₁) (head-set τ₂) hk
  ... | inj₁ hk₁ =
    ⊆-trans (key-A m or ht₁ (≤-trans (p⊆q⇒∣p∣≤∣q∣ (p⊆p∪q {p = head-set τ₁} (head-set τ₂))) hle) k hk₁)
            (p⊆p∪q {p = head-set τ₁} (head-set τ₂))
  ... | inj₂ hk₂ =
    ⊆-trans (key-A m or ht₂ (≤-trans (p⊆q⇒∣p∣≤∣q∣ (q⊆p∪q (head-set τ₁) (head-set τ₂))) hle) k hk₂)
            (q⊆p∪q (head-set τ₁) (head-set τ₂))

  key-A m or (htStar ht _) hle k hk = key-A m or ht hle k hk
  key-A m or (htNot ht) hle k hk = key-A m or ht hle k hk
  -- push, rep and bound share their body's head set.
  key-A m or (htPush ht) hle k hk = key-A m or ht hle k hk
  key-A m or (htRep ht _) hle k hk = key-A m or ht hle k hk
  key-A m or (htBound ht _ _) hle k hk = key-A m or ht hle k hk
  -- the remaining stack operators have empty head set.
  key-A _ _ htPop _ k hk = ⊥-elim (∉⊥ hk)
  key-A _ _ htPeek _ k hk = ⊥-elim (∉⊥ hk)
  key-A _ _ htDrop _ k hk = ⊥-elim (∉⊥ hk)
  key-A _ _ htPeekall _ k hk = ⊥-elim (∉⊥ hk)
  key-A _ _ htDropall _ k hk = ⊥-elim (∉⊥ hk)

  key-B : ∀ m → Acc _<_ m → ∀ {e : PExp n} {τ : Ty n}
        → HasType Γ e τ → ∣ head-set τ ∣ ≤ m
        → ∀ k → k ∈ head-set τ → head-set (Γ k) ⊆ head-set τ
  key-B m (acc rs) ht hle k hk =
    key-A m (λ ht' hlt → key-B _ (rs hlt) ht' ≤-refl) ht hle k hk

------------------------------------------------------------------------
-- Consequences of FirstMono

head-set-ssubset : (Γ : Ctx n)
              → (∀ i → i ∉ head-set (Γ i))
              → FirstMono Γ
              → ∀ {i j : Fin n} → j ∈ head-set (Γ i) → head-set (Γ j) ⊂ head-set (Γ i)
head-set-ssubset Γ hacy hmono {i} {j} hij =
  hmono i j hij , j , hij , hacy j

rankDecreases : (Γ : Ctx n)
              → (∀ i → i ∉ head-set (Γ i))
              → FirstMono Γ
              → ∀ {i j : Fin n} → j ∈ head-set (Γ i) → rank Γ j < rank Γ i
rankDecreases Γ hacy hmono hij =
  p⊂q⇒∣p∣<∣q∣ (head-set-ssubset Γ hacy hmono hij)

------------------------------------------------------------------------
-- The logical relation (now over an input word AND a stack)

record 𝓡 {n : ℕ} (G : Grammar n) (Γ : Ctx n) (τ : Ty n) (e : PExp n) : Set where
  field
    terminates : ∀ (s : Word) (st : Stack) → ∃ λ r → Parses G e s st r
    progress   : nullable τ ≡ false
               → ∀ s st p s' st' → Parses G e s st (ok p s' st') → p ≢ []

------------------------------------------------------------------------
-- Auxiliary: successful parse preserves the input split

parses-append : ∀ {G : Grammar n} {e : PExp n} {s p s' : Word} {st st' : Stack}
              → Parses G e s st (ok p s' st') → s ≡ p ++ s'
repN-append   : ∀ {G : Grammar n} {e : PExp n} {k} {s p s' : Word} {st st' : Stack}
              → RepN G e k s st (ok p s' st') → s ≡ p ++ s'
boundR-append : ∀ {G : Grammar n} {e : PExp n} {lo d} {s p s' : Word} {st st' : Stack}
              → BoundR G e lo d s st (ok p s' st') → s ≡ p ++ s'

parses-append empty = refl
parses-append termOk = refl
parses-append (var h) = parses-append h
parses-append (seqOk {p₁ = p₁} h₁ h₂) =
  trans (parses-append h₁)
        (trans (cong (p₁ ++_) (parses-append h₂))
               (sym (++-assoc p₁ _ _)))
parses-append (choiceOk h) = parses-append h
parses-append (choiceFail _ h) = parses-append h
parses-append (starBase _) = refl
parses-append (starStep {p₁ = p₁} h₁ h₂) =
  trans (parses-append h₁)
        (trans (cong (p₁ ++_) (parses-append h₂))
               (sym (++-assoc p₁ _ _)))
parses-append (notOk _) = refl
parses-append (pushOk h) = parses-append h
parses-append popOk = refl
parses-append peekOk = refl
parses-append dropOk = refl
parses-append dropallOk = refl
parses-append peekallOk = refl
parses-append (repOk _ h) = repN-append h
parses-append (boundOk _ _ _ h) = boundR-append h

repN-append repN0 = refl
repN-append (repNstep {p₁ = p₁} h₁ h₂) =
  trans (parses-append h₁)
        (trans (cong (p₁ ++_) (repN-append h₂))
               (sym (++-assoc p₁ _ _)))

boundR-append (bZero h) = repN-append h
boundR-append (bHit h) = repN-append h
boundR-append (bBack _ h) = boundR-append h

------------------------------------------------------------------------
-- Closure lemmas: each operator preserves 𝓡

R-empty : ∀ {G : Grammar n} {Γ : Ctx n} → 𝓡 G Γ (mkTy true ⊥) (empty)
R-empty = record
  { terminates = λ s st → ok [] s st , empty
  ; progress = λ ()
  }

R-term : ∀ {G : Grammar n} {Γ : Ctx n} (c : Char) → 𝓡 G Γ (mkTy false ⊥) (term c)
R-term {G = G} c = record
  { terminates = go
  ; progress = prog
  }
  where
  go : ∀ s st → ∃ λ r → Parses G (term c) s st r
  go [] st = fail , termFailEof
  go (d ∷ s) st with c ≟ᶜ d
  ... | yes refl = ok (c ∷ []) s st , termOk
  ... | no c≠d = fail , termFailChar c≠d

  prog : false ≡ false
       → ∀ s st p s' st' → Parses G (term c) s st (ok p s' st') → p ≢ []
  prog _ _ _ _ _ _ termOk ()

R-not : ∀ {G : Grammar n} {Γ : Ctx n} {τ : Ty n} {e : PExp n}
      → 𝓡 G Γ τ e → 𝓡 G Γ (neg τ) (not e)
R-not {G = G} {e = e} h = record
  { terminates = go
  ; progress = λ ()
  }
  where
  go : ∀ s st → ∃ λ r → Parses G (not e) s st r
  go s st with 𝓡.terminates h s st
  ... | fail , hfail = ok [] s st , notOk hfail
  ... | ok _ _ _ , hok = fail , notFail hok

R-choice : ∀ {G : Grammar n} {Γ : Ctx n} {τ₁ τ₂ : Ty n} {e₁ e₂ : PExp n}
         → 𝓡 G Γ τ₁ e₁ → 𝓡 G Γ τ₂ e₂
         → 𝓡 G Γ (τ₁ ⊕ τ₂) (choice e₁ e₂)
R-choice {G = G} {τ₁ = τ₁} {τ₂ = τ₂} {e₁ = e₁} {e₂ = e₂} h₁ h₂ = record
  { terminates = go
  ; progress = prog
  }
  where
  term₁ = 𝓡.terminates h₁
  term₂ = 𝓡.terminates h₂
  prog₁ = 𝓡.progress h₁
  prog₂ = 𝓡.progress h₂

  go : ∀ s st → ∃ λ r → Parses G (choice e₁ e₂) s st r
  go s st with term₁ s st
  ... | ok p s' st' , hok₁ = ok p s' st' , choiceOk hok₁
  ... | fail , hfail₁ with term₂ s st
  ... | r , hr₂ = r , choiceFail hfail₁ hr₂

  prog : nullable (τ₁ ⊕ τ₂) ≡ false
       → ∀ s st p s' st' → Parses G (choice e₁ e₂) s st (ok p s' st') → p ≢ []
  prog hnn s st p s' st' (choiceOk h) with nullable τ₁ in n₁-eq | hnn
  ... | false | _ = prog₁ n₁-eq s st p s' st' h
  ... | true | ()
  prog hnn s st p s' st' (choiceFail _ h) with nullable τ₁ | nullable τ₂ in n₂-eq | hnn
  ... | _ | false | _ = prog₂ n₂-eq s st p s' st' h
  ... | false | true | ()
  ... | true | true | ()

R-seq : ∀ {G : Grammar n} {Γ : Ctx n} {τ₁ τ₂ : Ty n} {e₁ e₂ : PExp n}
      → 𝓡 G Γ τ₁ e₁ → 𝓡 G Γ τ₂ e₂
      → 𝓡 G Γ (τ₁ ⊗ τ₂) (seq e₁ e₂)
R-seq {G = G} {τ₁ = τ₁} {τ₂ = τ₂} {e₁ = e₁} {e₂ = e₂} h₁ h₂ = record
  { terminates = go
  ; progress = prog
  }
  where
  term₁ = 𝓡.terminates h₁
  term₂ = 𝓡.terminates h₂
  prog₁ = 𝓡.progress h₁
  prog₂ = 𝓡.progress h₂

  go : ∀ s st → ∃ λ r → Parses G (seq e₁ e₂) s st r
  go s st with term₁ s st
  ... | fail , hfail₁ = fail , seqFail₁ hfail₁
  ... | ok p₁ s' st' , hok₁ with term₂ s' st'
  ... | fail , hfail₂ = fail , seqFail₂ hok₁ hfail₂
  ... | ok p₂ s'' st'' , hok₂ = ok (p₁ ++ p₂) s'' st'' , seqOk hok₁ hok₂

  prog : nullable (τ₁ ⊗ τ₂) ≡ false
       → ∀ s st p s'' st'' → Parses G (seq e₁ e₂) s st (ok p s'' st'') → p ≢ []
  prog hnn s st _ s'' st'' (seqOk {s' = sq} {st' = stq} {p₁ = p₁} {p₂ = p₂} h₁' h₂')
      with nullable τ₁ in n₁-eq | hnn
  ... | false | _ = λ heq → prog₁ n₁-eq s st p₁ sq stq h₁' (++-conicalˡ p₁ p₂ heq)
  ... | true | hnn₂ = λ heq → prog₂ hnn₂ sq stq p₂ s'' st'' h₂' (++-conicalʳ p₁ p₂ heq)

R-star : ∀ {G : Grammar n} {Γ : Ctx n} {τ : Ty n} {e : PExp n}
       → 𝓡 G Γ τ e → nullable τ ≡ false
       → 𝓡 G Γ (starTy τ) (star e)
R-star {G = G} {τ = τ} {e = e} h hnn = record
  { terminates = λ s st → let p , s' , st' , hp = go (length s) (<-wellFounded _) s st ≤-refl
                          in ok p s' st' , hp
  ; progress = λ ()
  }
  where
  step = 𝓡.terminates h
  prog = 𝓡.progress h

  length-lt : ∀ {p : Word} → p ≢ [] → ∀ {s s' : Word}
            → s ≡ p ++ s' → length s' < length s
  length-lt {[]} hp _ = ⊥-elim (hp refl)
  length-lt {x ∷ xs} _ {_} {s'} eq rewrite eq =
    s≤s (subst (length s' ≤_) (sym (length-++ xs))
               (m≤n+m (length s') (length xs)))

  go : ∀ len → Acc _<_ len → ∀ s st → length s ≤ len
     → ∃ λ p → ∃ λ s' → ∃ λ st' → Parses G (star e) s st (ok p s' st')
  go len (acc rs) s st hlen with step s st
  ... | fail , hfail = [] , s , st , starBase hfail
  ... | ok p s' st' , hok =
    let hpne = prog hnn s st p s' st' hok
        heq = parses-append hok
        hlt = <-≤-trans (length-lt hpne heq) hlen
        p' , s'' , st'' , hs = go (length s') (rs hlt) s' st' ≤-refl
    in p ++ p' , s'' , st'' , starStep hok hs

------------------------------------------------------------------------
-- Closure lemmas for the stack operators

R-push : ∀ {G : Grammar n} {Γ : Ctx n} {τ : Ty n} {e : PExp n}
       → 𝓡 G Γ τ e → 𝓡 G Γ τ (push e)
R-push {G = G} {τ = τ} {e = e} h = record
  { terminates = go
  ; progress = prog
  }
  where
  go : ∀ s st → ∃ λ r → Parses G (push e) s st r
  go s st with 𝓡.terminates h s st
  ... | ok p s' st' , hok = ok p s' (p ∷ st') , pushOk hok
  ... | fail , hf = fail , pushFail hf

  prog : nullable τ ≡ false
       → ∀ s st p s' st' → Parses G (push e) s st (ok p s' st') → p ≢ []
  prog hnn s st p s' _ (pushOk {st' = st₀} h') = 𝓡.progress h hnn s st p s' st₀ h'

R-pop : ∀ {G : Grammar n} {Γ : Ctx n} → 𝓡 G Γ (mkTy true ⊥) pop
R-pop {G = G} = record
  { terminates = go
  ; progress = λ ()
  }
  where
  go : ∀ s st → ∃ λ r → Parses G pop s st r
  go s [] = fail , popFailEmpty
  go s (w ∷ st1) with isPrefix? w s
  ... | yes (s'' , eq) rewrite eq = ok w s'' st1 , popOk
  ... | no ¬p = fail , popFailMiss ¬p

R-peek : ∀ {G : Grammar n} {Γ : Ctx n} → 𝓡 G Γ (mkTy true ⊥) peek
R-peek {G = G} = record
  { terminates = go
  ; progress = λ ()
  }
  where
  go : ∀ s st → ∃ λ r → Parses G peek s st r
  go s [] = fail , peekFailEmpty
  go s (w ∷ st1) with isPrefix? w s
  ... | yes (s'' , eq) rewrite eq = ok w s'' (w ∷ st1) , peekOk
  ... | no ¬p = fail , peekFailMiss ¬p

R-drop : ∀ {G : Grammar n} {Γ : Ctx n} → 𝓡 G Γ (mkTy true ⊥) drop
R-drop {G = G} = record
  { terminates = go
  ; progress = λ ()
  }
  where
  go : ∀ s st → ∃ λ r → Parses G drop s st r
  go s [] = fail , dropFailEmpty
  go s (w ∷ st1) = ok [] s st1 , dropOk

R-dropall : ∀ {G : Grammar n} {Γ : Ctx n} → 𝓡 G Γ (mkTy true ⊥) dropall
R-dropall {G = G} = record
  { terminates = λ s st → ok [] s [] , dropallOk
  ; progress = λ ()
  }

R-peekall : ∀ {G : Grammar n} {Γ : Ctx n} → 𝓡 G Γ (mkTy true ⊥) peekall
R-peekall {G = G} = record
  { terminates = go
  ; progress = λ ()
  }
  where
  go : ∀ s st → ∃ λ r → Parses G peekall s st r
  go s st with isPrefix? (concat st) s
  ... | yes (s'' , eq) rewrite eq = ok (concat st) s'' st , peekallOk
  ... | no ¬p = fail , peekallFailMiss ¬p

-- Exact repetition e^k terminates whenever the body does: induction on the
-- (finite) count k, threading input and stack through the iterations.
repN∞ : ∀ {G : Grammar n} {e : PExp n}
      → (∀ s st → ∃ λ r → Parses G e s st r)
      → ∀ k s st → ∃ λ r → RepN G e k s st r
repN∞ bp zero s st = ok [] s st , repN0
repN∞ {G = G} bp (suc k) s st with bp s st
... | fail , h = fail , repNfail1 h
... | ok p₁ s' st' , h with repN∞ bp k s' st'
... | ok p₂ s'' st'' , h₂ = ok (p₁ ++ p₂) s'' st'' , repNstep h h₂
... | fail , h₂ = fail , repNfail2 h h₂

-- rep i e: evaluate the index; a finite count runs the body that many times,
-- an undefined or infinite index fails.  Always nullable, so progress is
-- vacuous.
R-rep : ∀ {G : Grammar n} {Γ : Ctx n} {τ : Ty n} {e : PExp n}
      → 𝓡 G Γ τ e → (i : Index) → 𝓡 G Γ (repTy τ) (rep i e)
R-rep {G = G} {e = e} h i = record
  { terminates = go
  ; progress = λ ()
  }
  where
  go : ∀ s st → ∃ λ r → Parses G (rep i e) s st r
  go s st with norm i st in eq
  ... | just (finite k) = let r , hr = repN∞ (𝓡.terminates h) k s st
                          in r , repOk eq hr
  ... | just infinite = fail , repFailInf eq
  ... | nothing = fail , repFailNone eq

-- Greedy interval repetition e[lo,lo+d] terminates whenever the body does:
-- induction on the gap d, trying the largest count first via repN∞.
boundR-total : ∀ {G : Grammar n} {e : PExp n}
             → (∀ s st → ∃ λ r → Parses G e s st r)
             → ∀ lo d s st → ∃ λ r → BoundR G e lo d s st r
boundR-total bp lo zero s st = let r , hr = repN∞ bp lo s st in r , bZero hr
boundR-total bp lo (suc d) s st with repN∞ bp (lo + suc d) s st
... | ok p s' st' , h = ok p s' st' , bHit h
... | fail , h with boundR-total bp lo d s st
... | r , rest = r , bBack h rest

-- bound i j e: evaluate both indexes; a finite interval [n1,n2] with n1 ≤ n2
-- runs the greedy interval repetition, n1 > n2 fails, and an undefined or
-- infinite index fails.  Always nullable, so progress is vacuous.
R-bound : ∀ {G : Grammar n} {Γ : Ctx n} {τ : Ty n} {e : PExp n}
        → 𝓡 G Γ τ e → (i j : Index) → 𝓡 G Γ (boundTy τ) (bound i j e)
R-bound {G = G} {e = e} h i j = record
  { terminates = go
  ; progress = λ ()
  }
  where
  go : ∀ s st → ∃ λ r → Parses G (bound i j e) s st r
  go s st with norm i st in eqi
  ... | nothing = fail , boundNoneI eqi
  ... | just infinite = fail , boundInfI eqi
  ... | just (finite n1) with norm j st in eqj
  ... | nothing = fail , boundNoneJ eqi eqj
  ... | just infinite = fail , boundInfJ eqi eqj
  ... | just (finite n2) with n1 ≤? n2
  ... | yes n1≤n2 = let r , hr = boundR-total (𝓡.terminates h) n1 (n2 ∸ n1) s st
                        in r , boundOk eqi eqj n1≤n2 hr
  ... | no n1≰n2 = fail , boundGt eqi eqj (≰⇒> n1≰n2)
