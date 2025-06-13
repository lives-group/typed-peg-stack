#lang racket

(provide (all-defined-out))

; constraint syntax

(struct constr-F
  ()
  #:prefab)

(struct constr-T
  ()
  #:prefab)

(struct constr-and
  (left right)
  #:prefab)

(struct constr-not
  (constr)
  #:prefab)

(struct constr-eq
  (left right)
  #:prefab)


(struct constr-diff
  (left right)
  #:prefab)

(struct constr-ex
  (tyvar constr)
  #:prefab)

(struct constr-def
  (non-term ty constr)
  #:prefab)

; term syntax

(struct term-type
  (type)
  #:prefab)

(struct term-tyvar
  (tyvar)
  #:prefab)

(struct term-plus
  (left right)
  #:prefab)

(struct term-prod
  (left right)
  #:prefab)

(struct term-pow
  (base exp)
  #:prefab)

(struct term-interval
  (base start end)
  #:prefab)

(struct term-not
  (type)
  #:prefab)

(struct term-star
  (type)
  #:prefab)

(struct val-nat
  (val)
  #:prefab)

(struct val-infty
  ()
  #:prefab)