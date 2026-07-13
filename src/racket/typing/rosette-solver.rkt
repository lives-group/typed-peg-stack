#lang rosette

;; A Rosette-based replacement for the Z3 solver interface.
;;
;; The original `solver` (constraint-solver.rkt) serialises the constraints to
;; an SMT-LIB script, shells out to the `z3` executable, and then parses Z3's
;; textual model with a hand-written lexer/parser (solver/model-*.rkt). That
;; last step is brittle: it depends on the exact model syntax of a particular
;; Z3 version, and it breaks on newer Z3 releases.
;;
;; Here we instead express the very same constraints as Rosette symbolic values
;; and let Rosette drive the solver through its API. There is no external
;; process to launch and no solver output to parse: Rosette returns a solution
;; object that we query directly. Rosette also bundles its own solver, so no
;; separate Z3 installation is required.
;;
;; A type is a pair (null?, head-set). Nullability is a symbolic boolean; the
;; head-set, a subset of the finite set of grammar non-terminals, is a symbolic
;; boolean per non-terminal (its membership bit). The type operations are the
;; Rosette counterparts of the SMT `define-fun`s in solver/script-gen.rkt.

(require (only-in racket/base [make-hash rkt-make-hash])
         "constraint.rkt"
         (only-in "type.rkt" type type-null? type-head-set)
         "../core.rkt"
         (only-in "constraint-solver.rkt"
                  gen-context move-exists group-equalities))

(provide rosette-solve         ; constraint -> (or/c 'sat 'unsat)
         rosette-well-typed?   ; constraint -> boolean
         rosette-infer)        ; constraint -> (cons ctx (or/c 'unsat model))

;; A symbolic type: a nullability bit and a hash from non-terminal name to a
;; membership bit.
(struct styp (null head) #:transparent)

;; Solve the constraints and, on success, also return the context, the symbolic
;; type of every type variable and the solution, so that a caller can read back
;; the inferred types without going through a textual model.
;; -> (values (or/c 'sat 'unsat) ctx vars sol)
(define (rosette-run c)
  (clear-vc!)
  (define ctx  (car (gen-context c)))          ; (listof (nt . tyvar))
  (define nts  (map car ctx))                  ; head-set domain
  (define eqs  (cdr (group-equalities (move-exists (cdr (gen-context c))))))

  ;; A reference to a non-terminal with no rule (an incomplete or multi-file
  ;; grammar) is not something the algorithm can decide; signal it so the caller
  ;; counts the grammar as not-expressible rather than reporting a spurious
  ;; verdict.
  (for ([e (in-list eqs)])
    (match e
      [(constr-eq (pvar v) _)
       (unless (assoc v ctx) (error 'rosette-solve "undefined non-terminal: ~a" v))]
      [_ (void)]))

  ;; lazily create one symbolic type per type variable
  (define vars (rkt-make-hash))
  (define (var n)
    (hash-ref! vars n
               (lambda ()
                 (define-symbolic* nul boolean?)
                 (styp nul
                       (for/hash ([a (in-list nts)])
                         (define-symbolic* m boolean?)
                         (values a m))))))

  ;; head-set operations (hashes nt -> symbolic boolean)
  (define (h-empty)      (for/hash ([a (in-list nts)]) (values a #f)))
  (define (h-single v)   (for/hash ([a (in-list nts)]) (values a (equal? a v))))
  (define (h-union h1 h2)(for/hash ([a (in-list nts)]) (values a (|| (hash-ref h1 a) (hash-ref h2 a)))))
  (define (h-imp b h)    (for/hash ([a (in-list nts)]) (values a (&& b (hash-ref h a)))))
  (define (h-of set)     (for/hash ([a (in-list nts)]) (values a (and (member a set) #t))))
  (define (h= h1 h2)     (apply && (for/list ([a (in-list nts)]) (<=> (hash-ref h1 a) (hash-ref h2 a)))))
  (define (styp= x y)    (&& (<=> (styp-null x) (styp-null y)) (h= (styp-head x) (styp-head y))))

  ;; INat helpers (indices are concrete: a number or the symbol 'infty)
  (define (iZero? i) (and (number? i) (= i 0)))
  (define (iInf?  i) (eq? i 'infty))
  (define (iLt? i j) (cond [(iInf? i) #f] [(iInf? j) #t] [else (< i j)]))

  ;; type operations, mirroring the SMT define-fun's
  (define (t-prod a b) (styp (&& (styp-null a) (styp-null b))
                             (h-union (styp-head a) (h-imp (styp-null a) (styp-head b)))))
  (define (t-sum a b)  (styp (|| (styp-null a) (styp-null b))
                             (h-union (styp-head a) (styp-head b))))
  (define (t-star a)   (styp #t (styp-head a)))
  (define (t-neg a)    (styp #t (styp-head a)))
  (define (t-pow a i)  (styp (|| (iZero? i) (styp-null a)) (styp-head a)))
  (define (t-inter a i j) (styp (|| (iZero? i) (styp-null a)) (styp-head a)))

  ;; translate one grouped constraint into Rosette assertions
  (define (go e)
    (match e
      [(constr-eq (term-tyvar t1) (term-tyvar t2))
       (assert (styp= (var t1) (var t2)))]
      [(constr-eq (term-tyvar t1) (type nt ht))
       (define x (var t1))
       (assert (<=> (styp-null x) (and nt #t)))
       (assert (h= (styp-head x) (h-of ht)))]
      [(constr-eq (term-tyvar t1) (term-star (term-tyvar t2)))
       (assert (styp= (var t1) (t-star (var t2))))
       (assert (not (styp-null (var t2))))]              ; star body must be non-nullable
      [(constr-eq (term-tyvar t1) (term-prod (term-tyvar t2) (term-tyvar t3)))
       (assert (styp= (var t1) (t-prod (var t2) (var t3))))]
      [(constr-eq (term-tyvar t1) (term-plus (term-tyvar t2) (term-tyvar t3)))
       (assert (styp= (var t1) (t-sum (var t2) (var t3))))]
      [(constr-eq (term-tyvar t1) (term-not (term-tyvar t2)))
       (assert (styp= (var t1) (t-neg (var t2))))]
      [(constr-eq (term-tyvar t1) (term-pow (term-tyvar t2) i))
       (define iv (inat i))
       (assert (styp= (var t1) (t-pow (var t2) iv)))]
      [(constr-eq (term-tyvar t1) (term-interval (term-tyvar t2) i j))
       (define iv (inat i)) (define jv (inat j))
       (assert (styp= (var t1) (t-inter (var t2) iv jv)))
       (assert (not (and (iInf? jv) (styp-null (var t2)))))
       (assert (iLt? iv jv))]
      [(constr-eq (pvar v) (term-tyvar t1))               ; non-terminal use
       (define t2 (cdr (assoc v ctx)))
       (define x2 (var (term-tyvar-tyvar t2)))
       (assert (not (hash-ref (styp-head x2) v)))          ; acyclicity: v not in its own head
       (assert (styp= (var t1)
                      (styp (styp-null x2) (h-union (h-single v) (styp-head x2)))))]
      [(constr-diff a b) (assert (diff a b))]))

  ;; concrete index/value inequalities
  (define (inat v) (match v [(val-nat n) n] [(val-infty) 'infty] [_ v]))
  (define (diff a b)
    (define (v x) (match x [(val-nat n) n] [(val-infty) 'infty] [_ x]))
    (not (equal? (v a) (v b))))

  ;; A concrete contradiction detected while asserting (Rosette raises an
  ;; [assert] exception) means the constraints are unsatisfiable, i.e. the
  ;; grammar is ill-typed. Genuine errors are re-raised.
  (with-handlers ([exn:fail? (lambda (e)
                               (if (regexp-match? #rx"assert" (exn-message e))
                                   (values 'unsat ctx vars #f)
                                   (raise e)))])
    (for-each go eqs)
    (define sol (solve #t))
    (if (sat? sol)
        (values 'sat ctx vars sol)
        (values 'unsat ctx vars #f))))

(define (rosette-solve c)
  (define-values (status _ctx _vars _sol) (rosette-run c))
  status)

(define (rosette-well-typed? c)
  (eq? (rosette-solve c) 'sat))

;; Read the inferred types back from the solution, in the same shape the Z3
;; back-end produced by parsing the textual model: a pair of the context and
;; either 'unsat or an association list from the printed name of a type variable
;; ("t3") to a `type` struct (nullability flag and head set).
(define (rosette-infer c)
  (define-values (status ctx vars sol) (rosette-run c))
  (cond
    [(eq? status 'unsat) (cons ctx 'unsat)]
    [else
     (define (concrete b) (eq? #t (evaluate b sol)))
     (cons ctx
           (for/list ([(n st) (in-hash vars)])
             (cons (string-append "t" (number->string n))
                   (type (concrete (styp-null st))
                         (for/list ([(nt bit) (in-hash (styp-head st))]
                                    #:when (concrete bit))
                           nt)))))]))
