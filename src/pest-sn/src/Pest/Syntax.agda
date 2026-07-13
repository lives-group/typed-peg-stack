module Pest.Syntax where

open import Data.Char using (Char)
open import Data.Fin using (Fin)
open import Data.Nat using (ℕ)

open import Pest.Base public -- re-export Word and Stack
open import Pest.Index using (Index)

-- Parsing expressions of the Pest calculus, parameterised by n, the number of
-- nonterminals.  Nonterminals are represented as Fin n indices into the rule
-- table.
--
-- Beyond the core PEG constructors this development adds the six Pest stack
-- operators (Milestone A) and the exact/indexed repetition operator `rep`
-- (Milestone B).
data PExp (n : ℕ) : Set where
  empty   : PExp n
  term    : Char → PExp n
  var     : Fin n → PExp n
  seq     : PExp n → PExp n → PExp n
  choice  : PExp n → PExp n → PExp n
  star    : PExp n → PExp n
  not     : PExp n → PExp n
  -- Pest stack operators.
  push    : PExp n → PExp n
  pop     : PExp n
  peek    : PExp n
  drop    : PExp n
  peekall : PExp n
  dropall : PExp n
  -- Exact / indexed repetition e^i: repeat e exactly (norm i stack) times.
  rep     : Index → PExp n → PExp n
  -- Interval repetition e[i,j]: greedily repeat e between (norm i) and
  -- (norm j) times.  The unbounded interval e[i,∞] is expressible as the
  -- composition (rep i e) followed by (star e), so bound is restricted to
  -- statically-finite bounds.
  bound   : Index → Index → PExp n → PExp n

-- A PEG grammar with n nonterminals.
record Grammar (n : ℕ) : Set where
  field
    rules : Fin n → PExp n
    start : PExp n
