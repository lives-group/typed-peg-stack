module Pest.Index where

open import Data.Bool using (Bool; true; false; _∧_; if_then_else_)
open import Data.Char using (Char)
import Data.Char as Char
open import Data.List using (List; []; _∷_; length)
open import Data.Maybe using (Maybe; just; nothing; map)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _≤ᵇ_)

open import Pest.Base

-- Repetition indexes evaluate to either a natural number or ∞ (infinite).
data Value : Set where
  finite   : ℕ → Value
  infinite : Value

-- The index sublanguage: literals, ∞, the two stack-top queries
-- (top.tonat, top.length in the paper), and addition/multiplication.
data Index : Set where
  lit    : ℕ → Index
  inf    : Index
  topNat : Index          -- read the top-of-stack word as a decimal number
  topLen : Index          -- length of the top-of-stack word
  plus   : Index → Index → Index
  times  : Index → Index → Index

------------------------------------------------------------------------
-- Reading a decimal word into a number (empty word / non-digit ⟹ nothing).
-- The exact decoding is irrelevant to strong normalization; norm only needs
-- to be a total function.

digitOf : Char → Maybe ℕ
digitOf c = let k = Char.toℕ c in
            if (48 ≤ᵇ k) ∧ (k ≤ᵇ 57) then just (k ∸ 48) else nothing

toℕ-go : ℕ → Word → Maybe ℕ
toℕ-go acc [] = just acc
toℕ-go acc (c ∷ cs) with digitOf c
... | just d = toℕ-go (acc * 10 + d) cs
... | nothing = nothing

wordToℕ : Word → Maybe ℕ
wordToℕ [] = nothing
wordToℕ (c ∷ cs) = toℕ-go 0 (c ∷ cs)

------------------------------------------------------------------------
-- Dynamic normalisation: evaluate an index against the current stack.
-- Returns nothing when the top query is undefined (empty stack, or a
-- non-numeric top for topNat).

norm : Index → Stack → Maybe Value
norm (lit n) _ = just (finite n)
norm inf _ = just infinite
norm topNat [] = nothing
norm topNat (w ∷ _) = map finite (wordToℕ w)
norm topLen [] = nothing
norm topLen (w ∷ _) = just (finite (length w))
norm (plus i j) st with norm i st | norm j st
... | just (finite a) | just (finite b) = just (finite (a + b))
... | just infinite | _ = just infinite
... | _ | just infinite = just infinite
... | _ | _ = nothing
norm (times i j) st with norm i st | norm j st
... | just (finite a) | just (finite b) = just (finite (a * b))
... | just infinite | _ = just infinite
... | _ | just infinite = just infinite
... | _ | _ = nothing

------------------------------------------------------------------------
-- Static over-approximation: the stack-top queries are treated as the
-- finite placeholder 0, so norm-static i = infinite iff i syntactically
-- reaches inf.  This is the well-formedness check used by htRep.

norm-static : Index → Value
norm-static (lit n) = finite n
norm-static inf = infinite
norm-static topNat = finite 0
norm-static topLen = finite 0
norm-static (plus i j) with norm-static i | norm-static j
... | finite a | finite b = finite (a + b)
... | infinite | _ = infinite
... | _ | infinite = infinite
norm-static (times i j) with norm-static i | norm-static j
... | finite a | finite b = finite (a * b)
... | infinite | _ = infinite
... | _ | infinite = infinite
