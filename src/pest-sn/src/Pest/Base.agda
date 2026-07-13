module Pest.Base where

open import Data.Char using (Char)
open import Data.List using (List)

-- A word (string) is a list of characters.
Word : Set
Word = List Char

-- The implicit parsing stack is a list of words (most-recently pushed first).
Stack : Set
Stack = List Word
