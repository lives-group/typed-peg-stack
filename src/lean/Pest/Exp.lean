import Mathlib.Data.Nat.Basic

-- type syntax

inductive Ty : Type where
| NAT | BOOL

-- expression syntax

inductive Exp : Ty → Type where
| True : Exp .BOOL
| False : Exp .BOOL
| Zero : Exp .NAT
| Succ : Exp .NAT → Exp .NAT
| Pred : Exp .NAT → Exp .NAT
| IsZero : Exp .NAT → Exp .BOOL
| If : ∀ {t}, Exp .BOOL → Exp t → Exp t → Exp t

inductive Val : Ty → Type where
| True : Val .BOOL
| False : Val .BOOL
| Zero : Val .NAT
| Succ : Val .NAT → Val .NAT

@[simp]
def vsucc (v : Val .NAT) : Option (Val .NAT) :=
  .some (.Succ v)

@[simp]
def viszero (v : Val .NAT) : Option (Val .BOOL) :=
  match v with
  | .Zero => .some .True
  | _     => .some .False

@[simp]
def vpred (v : Val .NAT) : Option (Val .NAT) :=
  match v with
  | .Zero => .some .Zero
  | .Succ v1 => .some v1

-- definitional intepreter

@[simp]
def interp {t}(fuel : ℕ)(e : Exp t) : Option (Val t) :=
  match fuel with
  | 0 => .none
  | fuel1 + 1 =>
    match e with
    | .True => .some Val.True
    | .False => .some .False
    | .Zero => .some .Zero
    | .Succ e1 => do
        let v ← interp fuel1 e1
        vsucc v
    | .Pred e1 => do
        let v ← interp fuel1 e1
        vpred v
    | .IsZero e1 => do
        let v ← interp fuel1 e1
        viszero  v
    | .If e1 e2 e3 => do
        let v ← interp fuel1 e1
        match v with
        | .True => interp fuel1 e2
        | .False => interp fuel1 e3

theorem soundness
  : ∀ fuel t (e : Exp t) r,
      interp fuel e = r →
      r = .none ∨ ∃ (v : Val t), r = .some v := by
    intros fuel
    induction fuel <;>
    intros t e r H <;>
    simp at *      <;>
    try grind

