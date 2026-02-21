import Mathlib.Data.Nat.Basic
import Pest.Syntax
import Pest.Type
import Pest.Utils


-- definition of the parser.

inductive Result : Type where
| Ok : word → word → stack → Result
| ParserError : Result

def parse {n}
          {sig : ctx n}
          {t : type n}
          (fuel : ℕ)
          (rs : rules sig)
          (e : pexp sig t)
          (s : word)(st : stack) : Option Result :=
  match fuel with
  | 0 => .none
  | fuel1 + 1 =>
    match e with
    | pexp.epsilon => pure (.Ok [] s st)
    | pexp.chr c =>
      match s with
      | [] => pure .ParserError
      | (c1 :: s1) =>
        match decEq c c1 with
        | .isTrue rfl => pure (.Ok [c] s1 st)
        | .isFalse _ => pure .ParserError
    | pexp.var v _ =>
        match all.lookup rs.exprs v with
        | e1 => parse fuel1 rs e1 s st
    | pexp.cat e1 e2 =>
        match parse fuel1 rs e1 s st with
        | .some (.Ok pre1 suf1 st1) =>
          match parse fuel1 rs e2 suf1 st1 with
          | .some (.Ok pre2 suf2 st2) =>
            pure (.Ok (pre1 ++ pre2) suf2 st2)
          | .some .ParserError => pure .ParserError
          | .none => .none
        | .some .ParserError => pure .ParserError
        | .none => .none
    | pexp.choice e1 e2 =>
        match parse fuel1 rs e1 s st with
        | .some (.Ok pre1 suf1 st1) => .some (.Ok pre1 suf1 st1)
        | .some .ParserError => parse fuel1 rs e2 s st
        | .none => .none
    | pexp.not e1 =>
      match parse fuel1 rs e1 s st with
      | .some (.Ok _ _ _) => pure .ParserError
      | .some .ParserError => pure (.Ok [] s st)
      | .none => .none
    | pexp.star e1 =>
      match parse fuel1 rs e1 s st with
      | .some .ParserError => pure (.Ok [] s st)
      | .some (.Ok pref1 suf1 st1 ) =>
        match parse fuel1 rs (pexp.star e1) suf1 st1 with
        | .some (.Ok pref2 suf2 st2) =>
          pure (.Ok (pref1 ++ pref2) suf2 st2)
        | .some .ParserError =>
          pure (.Ok pref1 suf1 st1)
        | .none => .none
      | .none => .none
    -- stack operators
    | pexp.push e1 =>
        match parse fuel1 rs e1 s st with
        | .some (.Ok pref1 suf1 st1) =>
          pure (.Ok pref1 suf1 (pref1 :: st1))
        | .some .ParserError => pure .ParserError
        | .none => .none
    | pexp.pop =>
      match st with
      | [] => pure .ParserError
      | (w :: st1) =>
        let n  := List.length w
        let w1 := List.take n s
        if w = w1 then pure (.Ok w  (List.drop n s) st1)
        else pure .ParserError
    | pexp.peek =>
      match st with
      | [] => pure .ParserError
      | (w :: st1) =>
        let n := List.length w
        let w1 := List.take n s
        if w = w1 then pure (.Ok w (List.drop n s) (w :: st1))
        else pure .ParserError
    | pexp.peekall =>
      let w := List.flatten st
      let n := List.length w
      let w1 := List.take n s
      if w = w1 then pure (.Ok w  (List.drop n s) st)
      else pure .ParserError
    | pexp.drop =>
      match st with
      | [] => pure .ParserError
      | (_ :: st1) => pure (.Ok [] s st1)
    | pexp.dropall  => pure (.Ok [] s [])
    -- exact repetition
    | pexp.rep i eq e =>
      match Hv : norm i st with
      | .some (Value.nat 0) => pure (.Ok [] s st)
      | .some (Value.nat (m + 1)) =>
        match parse fuel1 rs e s st with
        | .some (.Ok pref1 suf1 st1) =>
          let i1 := Index.nat m
          let eq1 : norm_static i1 = Value.nat m := by rfl
          match parse fuel1 rs (pexp.rep i1 eq1 e) suf1 st1 with
          | .some (.Ok pref2 suf2 st2) =>
            pure (.Ok (pref1 ++ pref2) suf2 st2)
          | .some .ParserError => pure .ParserError
          | .none => .none
        | .some .ParserError =>
          pure .ParserError
        | .none => .none
      | .some Value.infty => by
        -- contradiction case.
        have contra : norm_static i = .infty := by
          apply norm_infy
          exact Hv
        aesop
      | .none => .none
    | pexp.bound t i1 i2 eq1 eq2 e neq =>
      match Hv1 : norm i1 st, Hv2 : norm i2 st with
      | .some (Value.nat n1), .some (Value.nat n2) =>
        match Ord.compare n1 n2 with
        | Ordering.lt =>
          let i3 := Index.nat n2
          match parse fuel1 rs (pexp.rep i3 (by rfl) e) s st with
          | .none => .none
          | .some .ParserError =>
            let i4 := Index.nat n1
            let i5 := Index.nat (n2 - 1)
            parse fuel1 rs (pexp.bound t i4 i5 (by rfl)
                                               (by rfl)
                                               e
                                               (by
                                                 intros H3
                                                 simp [norm_static] at *))
                                               s st
          | res => res
        | Ordering.eq =>
          let i3 := Index.nat n1
          parse fuel1 rs (pexp.rep i3 (by rfl) e) s st
        | Ordering.gt => .some .ParserError
      | .some (Value.nat n1), .some .infty =>
        if Ht : t.nullable then by
          have contra : norm_static i2 = .infty := by
            apply norm_infy
            exact Hv2
          rw [contra] at eq2
          rw [← eq2] at neq
          aesop
        else
          let i3 := Index.nat n1
          let e1 : pexp sig (type.mk false t.headset) := by
            have Hn : t.nullable = false := by aesop
            have Heq : t = type.mk t.nullable t.headset := by
              rcases t
              aesop
            rw [Heq] at e
            rw [Hn] at e
            exact e
          parse fuel1 rs (.cat (.rep i3 (by rfl) e) (.star e1)) s st
      | .some .infty, _ => by
        -- contradiction
        have contra : norm_static i1 = .infty := by
          apply norm_infy
          exact Hv1
        aesop
      | .none, _ => .none
      | _, .none => .none

def run (fuel : ℕ)(g : grammar)(s : word) : Option Result :=
  parse fuel g.pegs g.start s []

