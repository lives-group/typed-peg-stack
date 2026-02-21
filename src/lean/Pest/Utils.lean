import Mathlib.Data.Nat.Basic


abbrev word := List Char
abbrev stack := List word

-- finite sets, inductively

inductive fin : ℕ -> Type where
| zero : ∀ {n}, fin (n + 1)
| succ : ∀ {n}, fin n → fin (n + 1)
deriving Repr

-- vectors inductively

inductive vec (A : Type) : ℕ → Type where
| nil : vec A 0
| cons : ∀ {n}, A → vec A n → vec A (n + 1)
deriving Repr

-- vector lookup

def vec.lookup {n}
               {A : Type}
               (v : vec A n)
               (i : fin n) : A :=
  match i, v with
  | fin.zero, vec.cons x _ => x
  | fin.succ i1, vec.cons _ v1 =>
    vec.lookup v1 i1

inductive vec.all {A : Type}
                  (P : A → Type)
                  : ∀ {n}, vec A n → Type where
| nil : vec.all P nil
| cons : ∀ {n}{x : A}{v : vec A n},
           P x →
           vec.all P v →
           vec.all P (vec.cons x v)

def all.lookup {A : Type}
               {P : A → Type}
               {n}
               {v : vec A n}
               (xs : vec.all P v)
               (i : fin n) : P (vec.lookup v i) :=
  match i, xs with
  | fin.zero, vec.all.cons x _ => x
  | fin.succ i1, vec.all.cons _ v1 =>
    all.lookup v1 i1

