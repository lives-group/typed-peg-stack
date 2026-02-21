import Pest.Type
import Pest.Syntax
import Pest.Semantics
import Pest.Utils

-- lemma: well typed environment lookups are not stuck
-- and preserve types

lemma lookupVarLemma {n}{sig : ctx n}
  : ∀ (rs : vec.all (λ t => pexp sig t) sig)(v : fin n),
      ∃ (e : pexp sig (vec.lookup sig v)),
        all.lookup rs v = e := by
    intros rs v
    exists (all.lookup rs v)

-- soundness theorem

theorem soundness
  : ∀ {fuel}{n}{sig : ctx n}{t}(rs : rules sig)(e : pexp sig t) s st r,
  parse fuel rs e s st = .some r →
  r = Result.ParserError ∨ ∃ pre, ∃ suf, ∃ st, r = .Ok pre suf st := by
  intros fuel n sig t rs e s st r H
  induction fuel <;> rcases e <;> rcases r <;> aesop
