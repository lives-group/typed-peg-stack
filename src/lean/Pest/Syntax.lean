import Mathlib.Data.Nat.Basic
import Pest.Type
import Pest.Utils
import Pest.Index

def validvar {n}(sig : ctx n)(v : fin n) : Prop :=
  let t : type n := vec.lookup sig v
  match t with
  | type.mk _ h => v ∉ h

inductive pexp {n}(sig : ctx n) : type n -> Type where
| epsilon : pexp sig (type.mk true [])
| chr : Char → pexp sig (type.mk false [])
| var : (v : fin n) → validvar sig v → pexp sig (vec.lookup sig v)
| cat : ∀ {t1 t2}, pexp sig t1 →
                  pexp sig t2 →
                  pexp sig (type.prod t1 t2)
| choice : ∀ {t1 t2}, pexp sig t1 →
                     pexp sig t2 →
                     pexp sig (type.sum t1 t2)
| not : ∀ {b s}, pexp sig (type.mk b s) →
                 pexp sig (type.mk true s)
| star : ∀ {s}, pexp sig (type.mk false s) →
                pexp sig (type.mk true s)
| push : ∀ {t}, pexp sig t → pexp sig t
| pop : pexp sig (type.mk true [])
| peek : pexp sig (type.mk true [])
| drop : pexp sig (type.mk true [])
| peekall : pexp sig (type.mk true [])
| dropall : pexp sig (type.mk true [])
| rep : ∀ {n t} i, norm_static i = Value.nat n →
                   pexp sig t →
                   pexp sig (type.tyrep n t)
| bound : ∀ {n v} t i1 i2, norm_static i1 = Value.nat n →
                           norm_static i2 = v →
                           pexp sig t →
                           ¬ (v = .infty ∧ t.nullable = true) →
                           pexp sig (type.mk (Nat.beq n 0 && t.nullable) t.headset)

structure rules {n}(sig : ctx n) : Type where
  exprs : vec.all (λ t => pexp sig t) sig

structure grammar : Type where
  n : ℕ
  sig : ctx n
  pegs : rules sig
  startnullable : Bool
  start : pexp sig (type.mk startnullable [])
