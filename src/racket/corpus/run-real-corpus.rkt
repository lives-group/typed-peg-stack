#lang racket
;; Runs the type-inference algorithm over the corpus of REAL Pest grammars in
;; corpus/real/ (harvested from projects listed in pest-parser/awesome-pest and
;; translated to the tool syntax by corpus/real/../../pest2tps.py; see
;; corpus/real/manifest.tsv for the provenance and commit of every grammar).
;;
;; Reports, per grammar, the accept/reject decision from both solver back-ends.

(require typed-peg-stack/grammar
         typed-peg-stack/typing/constraint-gen
         typed-peg-stack/typing/constraint-solver
         typed-peg-stack/typing/rosette-solver)

(define real-dir
  (build-path (current-directory) "corpus" "real"))

;; Rosette is the robust oracle. Any exception (a tool parse error, an
;; undefined-reference, or a Pest feature the translator cannot express) is
;; reported as 'notexpr: the grammar is not expressible in the current prototype,
;; which we count separately rather than silently dropping.
(define (classify-rosette src)
  (with-handlers ([exn:fail? (lambda (e) 'notexpr)])
    (case (rosette-solve (gen-constr (parse (open-input-string src))))
      [(sat) 'accept] [(unsat) 'reject] [else 'notexpr])))

;; The Z3 back-end is an optional cross-check: the reported results come from
;; Rosette, which bundles its own solver. When no z3 executable is installed the
;; column is simply omitted, rather than silently reporting every grammar as
;; inconclusive.
(define z3? (z3-available?))

(define (classify-z3 src)
  (with-handlers ([exn:fail? (lambda (e) 'notexpr)])
    (define res (string-trim (cdr (solver (gen-constr (parse (open-input-string src)))))))
    (cond [(regexp-match? #rx"^unsat" res) 'reject]
          [(regexp-match? #rx"^sat"   res) 'accept]
          [else 'notexpr])))

(define (main)
  (define files (sort (map path->string
                           (filter (lambda (p) (regexp-match? #rx"\\.tps$" (path->string p)))
                                   (directory-list real-dir)))
                      string<?))
  (printf "~a\n" (make-string 84 #\=))
  (printf "~a  ~a~a\n"
          (~a "grammar" #:min-width 58)
          (~a "rosette" #:min-width 8)
          (if z3? "  z3" ""))
  (printf "~a\n" (make-string 84 #\=))
  (define counts (make-hash))
  (define agree 0)
  (for ([f files])
    (define src (file->string (build-path real-dir f)))
    (define ros (classify-rosette src))
    (define z3  (and z3? (classify-z3 src)))
    (when (and z3? (eq? z3 ros)) (set! agree (add1 agree)))
    (hash-update! counts ros add1 0)
    (printf "~a  ~a~a\n"
            (~a (regexp-replace #rx"\\.tps$" f "") #:min-width 58)
            (~a ros #:min-width 8)
            (cond [(not z3?) ""]
                  [(eq? z3 ros) (format "  ~a" z3)]
                  [else (format "  ~a  <-- DIFFER" z3)])))
  (printf "~a\n" (make-string 84 #\=))
  (define A (hash-ref counts 'accept 0))
  (define R (hash-ref counts 'reject 0))
  (define N (hash-ref counts 'notexpr 0))
  (printf "totals over ~a real grammars (rosette): accepted=~a  rejected=~a  not-expressible=~a\n"
          (length files) A R N)
  (printf "of the ~a analyzable grammars: ~a accepted, ~a rejected\n" (+ A R) A R)
  (if z3?
      (printf "z3 vs rosette agreement: ~a / ~a\n" agree (length files))
      (printf "z3 cross-check skipped: no z3 executable on PATH (results above are complete)\n")))

(main)
