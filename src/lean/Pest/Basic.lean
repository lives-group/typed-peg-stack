import Mathlib.Data.List.Basic
import Aesop 

-- list membership predicate 

inductive In {A : Type}(x : A) : List A → Prop where 
| here : ∀ {xs}, In x (x :: xs)
| there : ∀ {y ys}, In x ys → In x (y :: ys) 

def InDec {A : Type}
          [DecidableEq A]
          (x : A) 
          (xs : List A) : Decidable (In x xs) := 
    match xs with 
    | [] => by 
      apply Decidable.isFalse  
      intros contra 
      rcases contra 
    | y :: ys =>
      match decEq x y with 
      | Decidable.isTrue H1 => by 
        simp [H1]
        apply Decidable.isTrue 
        constructor 
      | Decidable.isFalse H2 =>
        match InDec x ys with 
        | Decidable.isTrue H3 => by 
          apply Decidable.isTrue 
          constructor ; assumption 
        | Decidable.isFalse H4 => by 
          apply Decidable.isFalse 
          intros contra 
          rcases contra <;> aesop

