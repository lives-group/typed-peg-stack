import Mathlib.Data.Nat.Basic
import Pest.Utils

-- basic definitions for types

abbrev headset (n : ℕ) := List (fin n)

structure type (n : ℕ) : Type where
  nullable : Bool
  headset : headset n

abbrev ctx (n : ℕ) := vec (type n) n

-- operations for types

def implies {n}
            (b : Bool)
            (h : headset n) : headset n :=
  if b then h else []

def type.prod {n}(t1 t2 : type n) : type n :=
  type.mk (t1.nullable && t2.nullable)
          (t1.headset ++ implies t1.nullable t2.headset)

def type.sum {n}(t1 t2 : type n) : type n :=
  type.mk (t1.nullable || t2.nullable)
          (t1.headset ++ t2.headset)

def type.tyrep {n}(m : ℕ)(t : type n) : type n :=
  type.mk (Nat.beq m 0 && t.nullable)
          t.headset
