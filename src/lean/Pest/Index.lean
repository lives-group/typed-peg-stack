import Mathlib.Data.Nat.Basic
import Pest.Utils
import Aesop

-- definition of the syntax of Indexes.

inductive Index : Type where
| nat : ℕ → Index
| infty : Index
| tonat : Index
| length : Index
| add : Index → Index → Index
| mult : Index → Index → Index

-- Values

inductive Value : Type where
| nat : ℕ → Value
| infty : Value

def norm (i : Index)(s : stack) : Option Value :=
  match i, s with
  | .nat n, _ => .some (.nat n)
  | .infty, _ => .some .infty
  | .tonat, [] => .none
  | .tonat, (s :: _) =>
    match (String.ofList s).toNat? with
    | .none => .none
    | .some i => .some (.nat i)
  | .length, [] => .none
  | .length, (s :: _) => .some (.nat s.length)
  | .add i1 i2, stk =>
    match norm i1 stk, norm i2 stk with
    | .some (.nat n1), .some (.nat n2) =>
        .some (.nat (n1 + n2))
    | .some .infty, _ => .some .infty
    | _ , .some .infty => .some .infty
    | _, _ => .none
  | .mult i1 i2, stk =>
    match norm i1 stk, norm i2 stk with
    | .some (.nat n1), .some (.nat n2) =>
        .some (.nat (n1 * n2))
    | .some .infty, _ => .some .infty
    | _ , .some .infty => .some .infty
    | _, _ => .none

def norm_static (i : Index) : Value :=
  match i with
  | .nat n => .nat n
  | .infty => .infty
  | .tonat => .nat 0
  | .length => .nat 0
  | .add i1 i2 =>
    match norm_static i1, norm_static i2 with
    | .nat n1, .nat n2 => .nat (n1 + n2)
    | .infty , _ => .infty
    | _, .infty => .infty
  | .mult i1 i2 =>
    match norm_static i1, norm_static i2 with
    | .nat n1, .nat n2 => .nat (n1 * n2)
    | .infty , _ => .infty
    | _, .infty => .infty

lemma norm_infy : ∀ i s, norm i s = .some .infty →
                         norm_static i = .infty := by
  intros i
  induction i <;> intros s H <;> simp [norm, norm_static] at *
  ·
    rcases s <;> simp [norm] at *
    rename_i h tl
    revert H
    split <;> intros H <;> aesop
  ·
    rcases s <;> simp [norm] at *
  ·
    rename_i i1 i2 IH1 IH2
    revert H
    split <;> intros H <;> aesop
  ·
    rename_i i1 i2 IH1 IH2
    revert H
    split <;> intros H <;> aesop

lemma norm_const : ∀ i s n, norm i s = .some (.nat n) →
                            ∃ m, norm_static i = .nat m := by
    intros i
    induction i <;>
    intros s n H1 <;>
    simp [norm, norm_static] at *
    <;> grind


