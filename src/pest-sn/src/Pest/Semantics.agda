module Pest.Semantics where

open import Data.Char using (Char)
open import Data.Char.Properties using () renaming (_≟_ to _≟ᶜ_)
open import Data.List using (List; []; _∷_; _++_; concat)
open import Data.List.Properties using (∷-injective)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Nat using (ℕ; suc; _+_; _≤_; _<_; _∸_)
open import Data.Product using (∃; _,_; _×_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; cong; subst)
open import Relation.Nullary using (¬_; Dec; yes; no)

open import Pest.Syntax
open import Pest.Index using (Index; Value; finite; infinite; norm)

-- The outcome of a parse attempt.
--
-- ok p s' st' means: prefix p was consumed, s' remains, and the stack after
-- the successful parse is st'.  The input s always satisfies s ≡ p ++ s'.
-- fail carries no stack: on failure a PEG backtracks.
data ParseResult : Set where
  ok   : Word → Word → Stack → ParseResult
  fail : ParseResult

-- w is a prefix of s, witnessed by the remaining suffix.
IsPrefix : Word → Word → Set
IsPrefix w s = ∃ λ s'' → s ≡ w ++ s''

-- Prefix matching is decidable; the stack operators use this to match or fail.
isPrefix? : (w s : Word) → Dec (IsPrefix w s)
isPrefix? [] s = yes (s , refl)
isPrefix? (c ∷ w) [] = no λ { (_ , ()) }
isPrefix? (c ∷ w) (d ∷ s) with c ≟ᶜ d
... | no c≢d = no λ { (s'' , eq) → c≢d (sym (proj₁ (∷-injective eq))) }
... | yes refl with isPrefix? w s
... | yes (s'' , refl) = yes (s'' , refl)
... | no ¬p = no λ { (s'' , eq) → ¬p (s'' , proj₂ (∷-injective eq)) }

------------------------------------------------------------------------
-- Relational big-step semantics.  Parses G e s st r reads: expression e on
-- input s with stack st yields result r.  RepN G e k s st r captures the
-- exact repetition e^k (e run sequentially k times); the two are mutually
-- defined because rep's result is built from RepN and RepN runs the body.

data Parses {n : ℕ} (G : Grammar n) : PExp n → Word → Stack → ParseResult → Set
data RepN   {n : ℕ} (G : Grammar n) (e : PExp n) : ℕ → Word → Stack → ParseResult → Set
data BoundR {n : ℕ} (G : Grammar n) (e : PExp n) : ℕ → ℕ → Word → Stack → ParseResult → Set

data Parses {n} G where

  -- ε always succeeds consuming nothing; the stack is unchanged.
  empty : ∀ {s st}
        → Parses G empty s st (ok [] s st)

  -- Terminal c succeeds when the input starts with c.
  termOk : ∀ {c s st}
         → Parses G (term c) (c ∷ s) st (ok (c ∷ []) s st)

  termFailEof : ∀ {c st}
              → Parses G (term c) [] st fail

  termFailChar : ∀ {c d s st}
               → ¬ c ≡ d
               → Parses G (term c) (d ∷ s) st fail

  -- Nonterminal: expand rule i and parse.
  var : ∀ {i s st r}
      → Parses G (Grammar.rules G i) s st r
      → Parses G (var i) s st r

  seqOk : ∀ {e₁ e₂ s s' s'' st st' st'' p₁ p₂}
        → Parses G e₁ s  st  (ok p₁ s'  st')
        → Parses G e₂ s' st' (ok p₂ s'' st'')
        → Parses G (seq e₁ e₂) s st (ok (p₁ ++ p₂) s'' st'')

  seqFail₁ : ∀ {e₁ e₂ s st}
           → Parses G e₁ s st fail
           → Parses G (seq e₁ e₂) s st fail

  seqFail₂ : ∀ {e₁ e₂ s s' st st' p₁}
           → Parses G e₁ s  st  (ok p₁ s' st')
           → Parses G e₂ s' st' fail
           → Parses G (seq e₁ e₂) s st fail

  choiceOk : ∀ {e₁ e₂ s s' st st' p}
           → Parses G e₁ s st (ok p s' st')
           → Parses G (choice e₁ e₂) s st (ok p s' st')

  choiceFail : ∀ {e₁ e₂ s st r}
             → Parses G e₁ s st fail
             → Parses G e₂ s st r
             → Parses G (choice e₁ e₂) s st r

  starBase : ∀ {e s st}
           → Parses G e s st fail
           → Parses G (star e) s st (ok [] s st)

  starStep : ∀ {e s s' s'' st st' st'' p₁ p₂}
           → Parses G e s st (ok p₁ s' st')
           → Parses G (star e) s' st' (ok p₂ s'' st'')
           → Parses G (star e) s st (ok (p₁ ++ p₂) s'' st'')

  notOk : ∀ {e s st}
        → Parses G e s st fail
        → Parses G (not e) s st (ok [] s st)

  notFail : ∀ {e s s' st st' p}
          → Parses G e s st (ok p s' st')
          → Parses G (not e) s st fail

  -- push e: parse e, then push the consumed prefix onto the resulting stack.
  pushOk : ∀ {e s s' st st' p}
         → Parses G e s st (ok p s' st')
         → Parses G (push e) s st (ok p s' (p ∷ st'))

  pushFail : ∀ {e s st}
           → Parses G e s st fail
           → Parses G (push e) s st fail

  -- pop: match the top-of-stack word against the input, then pop it.
  popOk : ∀ {w st s''}
        → Parses G pop (w ++ s'') (w ∷ st) (ok w s'' st)

  popFailEmpty : ∀ {s}
               → Parses G pop s [] fail

  popFailMiss : ∀ {w st s}
              → ¬ IsPrefix w s
              → Parses G pop s (w ∷ st) fail

  -- peek: like pop, but the stack is kept.
  peekOk : ∀ {w st s''}
         → Parses G peek (w ++ s'') (w ∷ st) (ok w s'' (w ∷ st))

  peekFailEmpty : ∀ {s}
                → Parses G peek s [] fail

  peekFailMiss : ∀ {w st s}
               → ¬ IsPrefix w s
               → Parses G peek s (w ∷ st) fail

  -- drop: pop the top word without consuming input.
  dropOk : ∀ {w st s}
         → Parses G drop s (w ∷ st) (ok [] s st)

  dropFailEmpty : ∀ {s}
                → Parses G drop s [] fail

  -- dropall: empty the stack without consuming input.
  dropallOk : ∀ {s st}
            → Parses G dropall s st (ok [] s [])

  -- peekall: match the concatenation of the whole stack against the input.
  peekallOk : ∀ {st s''}
            → Parses G peekall (concat st ++ s'') st (ok (concat st) s'' st)

  peekallFailMiss : ∀ {st s}
                  → ¬ IsPrefix (concat st) s
                  → Parses G peekall s st fail

  -- rep i e: evaluate the index against the current stack.  A finite count k
  -- runs e exactly k times; a non-numeric top (nothing) or an infinite index
  -- makes the parser fail gracefully.  For a well-typed rep the index is
  -- statically finite, so the infinite case never arises.
  repOk : ∀ {i e k s st r}
        → norm i st ≡ just (finite k)
        → RepN G e k s st r
        → Parses G (rep i e) s st r

  repFailInf : ∀ {i e s st}
             → norm i st ≡ just infinite
             → Parses G (rep i e) s st fail

  repFailNone : ∀ {i e s st}
              → norm i st ≡ nothing
              → Parses G (rep i e) s st fail

  -- bound i j e (the paper's e[i,j]): greedily repeat e between n1 and n2
  -- times, where n1 = norm i st and n2 = norm j st.  For a well-typed bound
  -- both indexes are statically finite; an undefined or infinite index fails.
  boundOk : ∀ {i j e n1 n2 s st r}
          → norm i st ≡ just (finite n1)
          → norm j st ≡ just (finite n2)
          → n1 ≤ n2
          → BoundR G e n1 (n2 ∸ n1) s st r
          → Parses G (bound i j e) s st r

  boundGt : ∀ {i j e n1 n2 s st}
          → norm i st ≡ just (finite n1)
          → norm j st ≡ just (finite n2)
          → n2 < n1
          → Parses G (bound i j e) s st fail

  boundNoneI : ∀ {i j e s st}
             → norm i st ≡ nothing
             → Parses G (bound i j e) s st fail

  boundInfI : ∀ {i j e s st}
            → norm i st ≡ just infinite
            → Parses G (bound i j e) s st fail

  boundNoneJ : ∀ {i j e n1 s st}
             → norm i st ≡ just (finite n1)
             → norm j st ≡ nothing
             → Parses G (bound i j e) s st fail

  boundInfJ : ∀ {i j e n1 s st}
            → norm i st ≡ just (finite n1)
            → norm j st ≡ just infinite
            → Parses G (bound i j e) s st fail

data RepN {n} G e where

  -- Zero repetitions: succeed consuming nothing.
  repN0 : ∀ {s st}
        → RepN G e 0 s st (ok [] s st)

  -- The first copy fails ⟹ the whole repetition fails.
  repNfail1 : ∀ {k s st}
            → Parses G e s st fail
            → RepN G e (suc k) s st fail

  -- The first copy succeeds and the remaining k copies succeed.
  repNstep : ∀ {k s s' s'' st st' st'' p₁ p₂}
           → Parses G e s st (ok p₁ s' st')
           → RepN G e k s' st' (ok p₂ s'' st'')
           → RepN G e (suc k) s st (ok (p₁ ++ p₂) s'' st'')

  -- The first copy succeeds but a later copy fails.
  repNfail2 : ∀ {k s s' st st' p₁}
            → Parses G e s st (ok p₁ s' st')
            → RepN G e k s' st' fail
            → RepN G e (suc k) s st fail

-- Greedy interval repetition.  BoundR G e lo d s st r represents e repeated
-- between lo and lo+d times, trying the largest count first and backing off:
--   bZero — the bottom of the interval: exactly lo copies (whatever RepN gives);
--   bHit  — lo+suc d copies succeed, so that (maximal) result is taken;
--   bBack — lo+suc d copies fail, so back off to the interval [lo, lo+d].
data BoundR {n} G e where

  bZero : ∀ {lo s st r}
        → RepN G e lo s st r
        → BoundR G e lo 0 s st r

  bHit  : ∀ {lo d s st p s' st'}
        → RepN G e (lo + suc d) s st (ok p s' st')
        → BoundR G e lo (suc d) s st (ok p s' st')

  bBack : ∀ {lo d s st r}
        → RepN G e (lo + suc d) s st fail
        → BoundR G e lo d s st r
        → BoundR G e lo (suc d) s st r
