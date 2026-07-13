module Pest.Fixpoint where

open import Data.Bool using (Bool; true; false; _∧_; _∨_; if_then_else_)
open import Data.Char using (Char)
open import Data.Char.Properties using () renaming (_≟_ to _≟ᶜ_)
open import Data.Empty using (⊥-elim)
open import Data.Fin using (Fin)
open import Data.Fin.Subset using (Subset; _∈_; _∉_; _⊆_; _∪_; ⊥; ⁅_⁆; ∣_∣)
open import Data.Fin.Subset.Properties using (x∈⁅x⁆; x∈p∪q⁺; q⊆p∪q; p⊆p∪q; p⊂q⇒∣p∣<∣q∣; p⊆q⇒∣p∣≤∣q∣)
open import Data.List using (List; []; _∷_; _++_; length; concat)
open import Data.List.Properties using (++-conicalˡ; ++-conicalʳ; length-++)
open import Data.Maybe using (just; nothing)
open import Data.Nat using (ℕ; _<_; _≤_; _≤?_; _+_; _∸_; s≤s; zero; suc)
open import Data.Nat.Induction using (Acc; acc; <-wellFounded)
open import Data.Nat.Properties using (≤-refl; ≤-trans; <-≤-trans; m≤n+m; n≤1+n; ≰⇒>)
open import Data.Product using (∃; _,_; _×_; proj₁; proj₂)
open import Data.Sum using (inj₁; inj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; sym; trans; cong; subst)
open import Relation.Nullary using (yes; no)

open import Pest.Syntax
open import Pest.Semantics
open import Pest.Typing
open import Pest.Rel
open import Pest.Index using (Index; finite; infinite; norm)

private variable n : ℕ

-- length s' ≤ length (p ++ s'), used to bound suffixes across rep iterations.
suffix-len : ∀ (p : Word) {s'} → length s' ≤ length (p ++ s')
suffix-len [] = ≤-refl
suffix-len (_ ∷ p) = ≤-trans (suffix-len p) (n≤1+n _)

-- Exact repetition e^k terminates when the body terminates on every suffix
-- bounded by len.  Structural induction on the count k; each iteration parses
-- one copy of the body and threads the (strictly-or-equally shorter) suffix.
repN≤ : ∀ {G : Grammar n} {e : PExp n} {len : ℕ}
      → (∀ s' st' → length s' ≤ len → ∃ λ r → Parses G e s' st' r)
      → ∀ k s st → length s ≤ len → ∃ λ r → RepN G e k s st r
repN≤ bp zero s st hlen = ok [] s st , repN0
repN≤ bp (suc k) s st hlen with bp s st hlen
... | fail , h = fail , repNfail1 h
... | ok p₁ s' st' , h with
      repN≤ bp k s' st'
            (≤-trans (subst (length s' ≤_) (sym (cong length (parses-append h)))
                            (suffix-len p₁))
                     hlen)
... | ok p₂ s'' st'' , h₂ = ok (p₁ ++ p₂) s'' st'' , repNstep h h₂
... | fail , h₂ = fail , repNfail2 h h₂

-- Greedy interval repetition e[lo,lo+d] terminates when the body does on every
-- suffix bounded by len.  Structural induction on the gap d.
boundR≤ : ∀ {G : Grammar n} {e : PExp n} {len : ℕ}
        → (∀ s' st' → length s' ≤ len → ∃ λ r → Parses G e s' st' r)
        → ∀ lo d s st → length s ≤ len → ∃ λ r → BoundR G e lo d s st r
boundR≤ bp lo zero s st hlen = let r , hr = repN≤ bp lo s st hlen in r , bZero hr
boundR≤ bp lo (suc d) s st hlen with repN≤ bp (lo + suc d) s st hlen
... | ok p s' st' , h = ok p s' st' , bHit h
... | fail , h with boundR≤ bp lo d s st hlen
... | r , rest = r , bBack h rest

-- Rank strictly decreases when following a head-dependency edge.
var-rank-lt : ∀ {Γ : Ctx n} (i : Fin n)
            → i ∉ head-set (Γ i)
            → ∣ head-set (Γ i) ∣ < ∣ ⁅ i ⁆ ∪ head-set (Γ i) ∣
var-rank-lt {Γ = Γ} i h∉ =
  p⊂q⇒∣p∣<∣q∣
    (q⊆p∪q ⁅ i ⁆ (head-set (Γ i)) , i , x∈p∪q⁺ (inj₁ (x∈⁅x⁆ i)) , h∉)

-- Double well-founded induction on (input length, head-set cardinality).
-- The stack is threaded but does not enter the termination measure: stack
-- operators are non-recursive (or, for push, structurally smaller), so the
-- (length, rank) measure of the core proof still governs.

lex-key : ∀ {G : Grammar n} {Γ : Ctx n}
        → (∀ i → i ∉ head-set (Γ i))
        → (∀ i → HasType Γ (Grammar.rules G i) (Γ i))
        → ∀ len → Acc _<_ len
        → ∀ fc  → Acc _<_ fc
        → ∀ (s : Word) (st : Stack) {e : PExp n} {τ : Ty n}
        → HasType Γ e τ
        → length s ≤ len → ∣ head-set τ ∣ ≤ fc
        → (∃ λ r → Parses G e s st r)
          × (nullable τ ≡ false → ∀ p s' st' → Parses G e s st (ok p s' st') → p ≢ [])
lex-key {G = G} {Γ = Γ} hacy hfix = go
  where

  suffix-≤ : ∀ (p : Word) {s'} → length s' ≤ length (p ++ s')
  suffix-≤ [] = ≤-refl
  suffix-≤ (_ ∷ p) = ≤-trans (suffix-≤ p) (n≤1+n _)

  length-lt : ∀ {p : Word} → p ≢ [] → ∀ {s s' : Word}
            → s ≡ p ++ s' → length s' < length s
  length-lt {[]} hp _ = ⊥-elim (hp refl)
  length-lt {_ ∷ xs} _ {_} {s'} eq rewrite eq =
    s≤s (subst (length s' ≤_) (sym (length-++ xs)) (m≤n+m (length s') (length xs)))

  -- Progress only.  Termination on (Acc _<_ fc, HasType).
  go-prog : ∀ fc → Acc _<_ fc
          → ∀ (s : Word) (st : Stack) {e} {τ}
          → HasType Γ e τ → ∣ head-set τ ∣ ≤ fc
          → nullable τ ≡ false → ∀ p s' st' → Parses G e s st (ok p s' st') → p ≢ []
  go-prog _ _ _ _ htEmpty _ ()
  go-prog _ _ _ _ (htTerm {c = c}) _ _ _ _ _ termOk ()
  go-prog fc (acc rsFc) s st (htVar {i = i}) hfc hnn p s' st' (var h) =
      go-prog ∣ head-set (Γ i) ∣
              (rsFc (<-≤-trans (var-rank-lt {Γ = Γ} i (hacy i)) hfc))
              s st (hfix i) ≤-refl hnn p s' st' h
  go-prog fc fAcc s st (htSeq {τ₁ = τ₁} {τ₂ = τ₂} ht₁ ht₂) hfc hnn _ s'' st''
      (seqOk {s' = s_q} {st' = st_q} {p₁ = q₁} {p₂ = q₂} hq₁ hq₂)
      with nullable τ₁ in n₁-eq | hnn
  ... | false | _ = λ heq →
          go-prog fc fAcc s st ht₁
                  (≤-trans (p⊆q⇒∣p∣≤∣q∣ (p⊆p∪q {p = head-set τ₁} _)) hfc)
                  n₁-eq q₁ s_q st_q hq₁ (++-conicalˡ q₁ q₂ heq)
  ... | true | hnn₂ = λ heq →
          go-prog fc fAcc s_q st_q ht₂
                  (≤-trans (p⊆q⇒∣p∣≤∣q∣ (q⊆p∪q (head-set τ₁) (head-set τ₂))) hfc)
                  hnn₂ q₂ s'' st'' hq₂ (++-conicalʳ q₁ q₂ heq)
  go-prog fc fAcc s st (htChoice {τ₁ = τ₁} {τ₂ = τ₂} ht₁ ht₂) hfc hnn _ s' st'
      (choiceOk hq₁)
      with nullable τ₁ in n₁-eq | hnn
  ... | false | _ =
          go-prog fc fAcc s st ht₁
                  (≤-trans (p⊆q⇒∣p∣≤∣q∣ (p⊆p∪q {p = head-set τ₁} (head-set τ₂))) hfc)
                  n₁-eq _ s' st' hq₁
  ... | true | ()
  go-prog fc fAcc s st (htChoice {τ₁ = τ₁} {τ₂ = τ₂} ht₁ ht₂) hfc hnn _ s'' st''
      (choiceFail _ hq₂)
      with nullable τ₁ | nullable τ₂ in n₂-eq | hnn
  ... | _ | false | _ =
          go-prog fc fAcc s st ht₂
                  (≤-trans (p⊆q⇒∣p∣≤∣q∣ (q⊆p∪q (head-set τ₁) (head-set τ₂))) hfc)
                  n₂-eq _ s'' st'' hq₂
  ... | false | true | ()
  ... | true | true | ()
  go-prog _ _ _ _ (htStar _ _) _ ()
  go-prog _ _ _ _ (htNot _) _ ()
  -- push inherits the body's type; progress reduces to the body.
  go-prog fc fAcc s st (htPush ht) hfc hnn p s' _ (pushOk {st' = st₀} h) =
      go-prog fc fAcc s st ht hfc hnn p s' st₀ h
  -- the other stack operators are nullable, so progress is vacuous.
  go-prog _ _ _ _ htPop _ ()
  go-prog _ _ _ _ htPeek _ ()
  go-prog _ _ _ _ htDrop _ ()
  go-prog _ _ _ _ htPeekall _ ()
  go-prog _ _ _ _ htDropall _ ()
  -- rep and bound are (conservatively) nullable, so progress is vacuous.
  go-prog _ _ _ _ (htRep _ _) _ ()
  go-prog _ _ _ _ (htBound _ _ _) _ ()

  mutual

    go-term : ∀ len → Acc _<_ len → ∀ fc → Acc _<_ fc
            → ∀ (s : Word) (st : Stack) {e} {τ}
            → HasType Γ e τ
            → length s ≤ len → ∣ head-set τ ∣ ≤ fc
            → ∃ λ r → Parses G e s st r
    go-term _ _ _ _ s st htEmpty _ _ = ok [] s st , empty
    go-term _ _ _ _ [] st (htTerm {c = c}) _ _ =
        fail , termFailEof
    go-term _ _ _ _ (d ∷ s') st (htTerm {c = c}) _ _ with c ≟ᶜ d
    ... | yes refl = ok (c ∷ []) s' st , termOk
    ... | no c≠d = fail , termFailChar c≠d
    go-term len (acc rsLen) fc (acc rsFc) s st (htVar {i = i}) hlen hfc =
        let r , h = go-term len (acc rsLen) ∣ head-set (Γ i) ∣
                            (rsFc (<-≤-trans (var-rank-lt {Γ = Γ} i (hacy i)) hfc))
                            s st (hfix i) hlen ≤-refl
        in r , var h
    go-term len lAcc fc fAcc s st
        (htSeq {e₁ = e₁} {e₂ = e₂} {τ₁ = τ₁} {τ₂ = τ₂} ht₁ ht₂) hlen hfc
        with nullable τ₁ in n₁-eq
    ... | false = seq-false-term len lAcc fc fAcc s st ht₁ ht₂ hlen
                      (≤-trans (p⊆q⇒∣p∣≤∣q∣ (p⊆p∪q {p = head-set τ₁} _)) hfc)
                      n₁-eq
    ... | true = seq-true-term len lAcc fc fAcc s st ht₁ ht₂ hlen
                      (≤-trans (p⊆q⇒∣p∣≤∣q∣ (p⊆p∪q {p = head-set τ₁} (head-set τ₂))) hfc)
                      (≤-trans (p⊆q⇒∣p∣≤∣q∣ (q⊆p∪q (head-set τ₁) (head-set τ₂))) hfc)
    go-term len lAcc fc fAcc s st
        (htChoice {e₁ = e₁} {e₂ = e₂} {τ₁ = τ₁} {τ₂ = τ₂} ht₁ ht₂) hlen hfc
        with go-term len lAcc fc fAcc s st ht₁ hlen
                     (≤-trans (p⊆q⇒∣p∣≤∣q∣ (p⊆p∪q {p = head-set τ₁} (head-set τ₂))) hfc)
    ... | ok p₁ s' st' , h₁ = ok p₁ s' st' , choiceOk h₁
    ... | fail , h₁
        with go-term len lAcc fc fAcc s st ht₂ hlen
                     (≤-trans (p⊆q⇒∣p∣≤∣q∣ (q⊆p∪q (head-set τ₁) (head-set τ₂))) hfc)
    ... | r₂ , h₂ = r₂ , choiceFail h₁ h₂
    go-term len (acc rsLen) fc fAcc s st (htStar {τ = τ} ht hnn) hlen hfc
        with go-term len (acc rsLen) fc fAcc s st ht hlen hfc
    ... | fail , h = ok [] s st , starBase h
    ... | ok p s' st' , h
        with go-term (length s')
                     (rsLen (<-≤-trans
                        (length-lt (go-prog fc fAcc s st ht hfc hnn p s' st' h)
                                   (parses-append h))
                        hlen))
                     fc fAcc s' st' (htStar ht hnn) ≤-refl hfc
    ... | fail , ()
    ... | ok p'' s'' st'' , hstar = ok (p ++ p'') s'' st'' , starStep h hstar
    go-term len lAcc fc fAcc s st (htNot ht) hlen hfc
        with go-term len lAcc fc fAcc s st ht hlen hfc
    ... | fail , h = ok [] s st , notOk h
    ... | ok _ _ _ , h = fail , notFail h
    -- push: recurse structurally on the body, then push the consumed prefix.
    go-term len lAcc fc fAcc s st (htPush ht) hlen hfc
        with go-term len lAcc fc fAcc s st ht hlen hfc
    ... | ok p s' st' , h = ok p s' (p ∷ st') , pushOk h
    ... | fail , h = fail , pushFail h
    -- the remaining stack operators produce a result directly.
    go-term _ _ _ _ s [] htPop _ _ = fail , popFailEmpty
    go-term _ _ _ _ s (w ∷ st1) htPop _ _ with isPrefix? w s
    ... | yes (s'' , eq) rewrite eq = ok w s'' st1 , popOk
    ... | no ¬p = fail , popFailMiss ¬p
    go-term _ _ _ _ s [] htPeek _ _ = fail , peekFailEmpty
    go-term _ _ _ _ s (w ∷ st1) htPeek _ _ with isPrefix? w s
    ... | yes (s'' , eq) rewrite eq = ok w s'' (w ∷ st1) , peekOk
    ... | no ¬p = fail , peekFailMiss ¬p
    go-term _ _ _ _ s [] htDrop _ _ = fail , dropFailEmpty
    go-term _ _ _ _ s (w ∷ st1) htDrop _ _ = ok [] s st1 , dropOk
    go-term _ _ _ _ s st htDropall _ _ = ok [] s [] , dropallOk
    go-term _ _ _ _ s st htPeekall _ _ with isPrefix? (concat st) s
    ... | yes (s'' , eq) rewrite eq = ok (concat st) s'' st , peekallOk
    ... | no ¬p = fail , peekallFailMiss ¬p
    -- rep i e: iterate the body norm(i,st) times.  The body is parsed via a
    -- structurally-smaller go-term call (ht < htRep ht), so the measure holds.
    go-term len lAcc fc fAcc s st (htRep {i = i} ht _) hlen hfc with norm i st in eq
    ... | just (finite k) =
            let r , hr = repN≤ (λ s' st' hlen' → go-term len lAcc fc fAcc s' st' ht hlen' hfc)
                               k s st hlen
            in r , repOk eq hr
    ... | just infinite = fail , repFailInf eq
    ... | nothing = fail , repFailNone eq
    -- bound i j e: greedy finite interval, iterated via the structurally-smaller
    -- body just like rep.
    go-term len lAcc fc fAcc s st (htBound {i = i} {j = j} ht _ _) hlen hfc
        with norm i st in eqi
    ... | nothing = fail , boundNoneI eqi
    ... | just infinite = fail , boundInfI eqi
    ... | just (finite n1) with norm j st in eqj
    ... | nothing = fail , boundNoneJ eqi eqj
    ... | just infinite = fail , boundInfJ eqi eqj
    ... | just (finite n2) with n1 ≤? n2
    ... | yes n1≤n2 =
                let r , hr = boundR≤ (λ s' st' hlen' → go-term len lAcc fc fAcc s' st' ht hlen' hfc)
                                     n1 (n2 ∸ n1) s st hlen
                in r , boundOk eqi eqj n1≤n2 hr
    ... | no n1≰n2 = fail , boundGt eqi eqj (≰⇒> n1≰n2)

    seq-false-term : ∀ len → Acc _<_ len → ∀ fc → Acc _<_ fc
                   → ∀ (s : Word) (st : Stack) {e₁ e₂} {τ₁ τ₂}
                   → HasType Γ e₁ τ₁ → HasType Γ e₂ τ₂
                   → length s ≤ len
                   → ∣ head-set τ₁ ∣ ≤ fc
                   → nullable τ₁ ≡ false
                   → ∃ λ r → Parses G (seq e₁ e₂) s st r
    seq-false-term len (acc rsLen) fc fAcc s st ht₁ ht₂ hlen hfc₁ n₁
        with go-term len (acc rsLen) fc fAcc s st ht₁ hlen hfc₁
    ... | fail , h₁ = fail , seqFail₁ h₁
    ... | ok p₁ s' st' , h₁
        with go-term (length s')
                     (rsLen (<-≤-trans
                        (length-lt
                           (go-prog fc fAcc s st ht₁ hfc₁ n₁ p₁ s' st' h₁)
                           (parses-append h₁))
                        hlen))
                     _ (<-wellFounded _) s' st' ht₂ ≤-refl ≤-refl
    ... | fail , h₂ = fail , seqFail₂ h₁ h₂
    ... | ok p₂ s'' st'' , h₂ = ok (p₁ ++ p₂) s'' st'' , seqOk h₁ h₂

    seq-true-term : ∀ len → Acc _<_ len → ∀ fc → Acc _<_ fc
                  → ∀ (s : Word) (st : Stack) {e₁ e₂} {τ₁ τ₂}
                  → HasType Γ e₁ τ₁ → HasType Γ e₂ τ₂
                  → length s ≤ len
                  → ∣ head-set τ₁ ∣ ≤ fc
                  → ∣ head-set τ₂ ∣ ≤ fc
                  → ∃ λ r → Parses G (seq e₁ e₂) s st r
    seq-true-term len lAcc fc fAcc s st ht₁ ht₂ hlen hfc₁ hfc₂
        with go-term len lAcc fc fAcc s st ht₁ hlen hfc₁
    ... | fail , h₁ = fail , seqFail₁ h₁
    ... | ok p₁ s' st' , h₁
        with go-term len lAcc fc fAcc s' st' ht₂
                     (≤-trans (subst (length s' ≤_)
                                     (sym (cong length (parses-append h₁)))
                                     (suffix-≤ p₁))
                              hlen)
                     hfc₂
    ... | fail , h₂ = fail , seqFail₂ h₁ h₂
    ... | ok p₂ s'' st'' , h₂ = ok (p₁ ++ p₂) s'' st'' , seqOk h₁ h₂

  go : ∀ len → Acc _<_ len → ∀ fc → Acc _<_ fc
     → ∀ (s : Word) (st : Stack) {e} {τ}
     → HasType Γ e τ
     → length s ≤ len → ∣ head-set τ ∣ ≤ fc
     → (∃ λ r → Parses G e s st r)
       × (nullable τ ≡ false → ∀ p s' st' → Parses G e s st (ok p s' st') → p ≢ [])
  go len lAcc fc fAcc s st ht hlen hfc =
      go-term len lAcc fc fAcc s st ht hlen hfc ,
      go-prog fc fAcc s st ht hfc

------------------------------------------------------------------------
-- Fixpoint: every rule body is in 𝓡 under the context that typed it.

fixpoint : ∀ {G : Grammar n} {Γ : Ctx n}
         → (∀ i → i ∉ head-set (Γ i))
         → (∀ i → HasType Γ (Grammar.rules G i) (Γ i))
         → ∀ i → 𝓡 G Γ (Γ i) (Grammar.rules G i)
fixpoint {G = G} {Γ = Γ} hacy hfix i = record
  { terminates = λ s st →
      proj₁ (lex-key hacy hfix (length s) (<-wellFounded _)
                     ∣ head-set (Γ i) ∣ (<-wellFounded _)
                     s st (hfix i) ≤-refl ≤-refl)
  ; progress = λ hnn s st p s' st' h →
      proj₂ (lex-key hacy hfix (length s) (<-wellFounded _)
                     ∣ head-set (Γ i) ∣ (<-wellFounded _)
                     s st (hfix i) ≤-refl ≤-refl)
            hnn p s' st' h
  }
