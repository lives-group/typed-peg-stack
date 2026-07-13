#lang racket

;; main type inference driver
;;
;; Inference goes through the Rosette back-end (typing/rosette-solver.rkt), which
;; drives a solver through its API and reads the model back as Racket values.
;; Rosette bundles its own solver, so no separately installed z3 executable is
;; needed to use the language or to run the examples. The Z3 back-end
;; (typing/constraint-solver.rkt) is kept as an independent cross-check and is
;; exercised by the corpus runners whenever a z3 executable is available.

(require "../core.rkt"
         "type.rkt"
         "constraint-gen.rkt"
         "rosette-solver.rkt")

(provide infer)

(define (infer g)
  (rosette-infer (gen-constr g)))
