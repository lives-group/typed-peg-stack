#lang racket
;; Corpus runner for the typed-peg-stack inference algorithm.
;;
;; For each grammar (given as PEG source text, i.e. the body of a
;; `#lang typed-peg-stack` file) this script runs the type-inference pipeline
;; and reports whether the grammar is ACCEPTED (well-typed, cannot loop) or
;; REJECTED (the constraints are unsatisfiable, so the grammar can loop).
;;
;; The accept/reject decision is read directly from Z3's sat/unsat verdict,
;; which is robust across Z3 versions, rather than from the (version-sensitive)
;; textual model produced on a sat result.
;;
;; Usage:  racket corpus/run-corpus.rkt

(require typed-peg-stack/grammar
         typed-peg-stack/typing/constraint-gen
         typed-peg-stack/typing/constraint-solver
         typed-peg-stack/typing/rosette-solver)

;; The accept/reject decision reported here comes from the Rosette back-end,
;; which bundles its own solver. An externally installed z3 is used, when
;; present, only as an independent cross-check; when it is absent the column is
;; omitted rather than silently turning every verdict into an error.
(define z3? (z3-available?))

;; classify-z3 : string -> symbol   (accept/reject/error, via the Z3 executable)
(define (classify-z3 src)
  (with-handlers ([exn:fail? (lambda (e) 'error)])
    (define c (gen-constr (parse (open-input-string src))))
    (define res (string-trim (cdr (solver c))))
    (cond [(regexp-match? #rx"^unsat" res) 'reject]
          [(regexp-match? #rx"^sat"   res) 'accept]
          [else 'error])))

;; classify-rosette : string -> symbol  (accept/reject/error, via Rosette)
(define (classify-rosette src)
  (with-handlers ([exn:fail? (lambda (e) 'error)])
    (define c (gen-constr (parse (open-input-string src))))
    (case (rosette-solve c) [(sat) 'accept] [(unsat) 'reject] [else 'error])))

(define (classify src) (values (classify-z3 src) ""))

;; The corpus: (name expected . source).  `expected` is what the type system
;; ought to decide, used only to flag surprises.
(define corpus
  (list
   ;; ---- well-typed grammars (should be accepted) ----
   (list "single-terminal" 'accept
         "start: 'a'")
   (list "byte" 'accept
         "bit <-- '0' / '1'; start: bit{8}")
   (list "star-then-terminal" 'accept
         "start: 'a'* ~ 'b'")
   (list "identifier" 'accept
         "letter <-- ['a','b','c','d']; digit <-- ['0','1','2','3','4','5','6','7','8','9']; start: letter ~ (letter / digit)*")
   (list "fixed-width-ipv6" 'accept
         "bit <-- '0' / '1'; byte <-- bit{8}; start: byte{16}")
   (list "interval-repetition" 'accept
         "bit <-- '0' / '1'; start: bit{2,4}")
   (list "heartbeat" 'accept
         "bit <-- '0' / '1'; byte <-- bit{8}; htype <-- byte; length <-- push(byte{2}); challenge <-- byte{top.tonat}; padding <-- byte*; start: htype ~ length ~ challenge ~ padding")
   (list "netstring" 'accept
         "digit <-- ['0','1','2','3','4','5','6','7','8','9']; num <-- push(digit ~ digit*); start: num ~ ':' ~ '.'{top.tonat} ~ ','")
   (list "stack-match" 'accept
         "open <-- push('a'); close <-- pop; start: open ~ 'x' ~ close")
   (list "not-predicate" 'accept
         "start: (!'b' ~ 'a')* ~ 'b'")
   (list "and-predicate" 'accept
         "start: &'a' ~ ['a','b']")
   (list "nested-choice-seq" 'accept
         "start: ('a' ~ 'a' ~ 'b')* ~ 'a' ~ 'a' ~ 'c' / 'a' ~ 'a' ~ 'd'")
   ;; ---- ill-typed grammars (should be rejected: they can loop) ----
   (list "nullable-star" 'reject
         "start: (epsilon)*")
   (list "optional-under-star" 'reject
         "start: ('a'?)*")
   (list "direct-left-recursion" 'reject
         "A <-- A ~ 'a'; start: A")
   (list "indirect-left-recursion" 'reject
         "A <-- B ~ 'a'; B <-- A ~ 'b'; start: A")
   (list "nullable-nonterminal-star" 'reject
         "E <-- epsilon ~ epsilon; start: E*")
   (list "exact-infty" 'reject
         "start: 'a'{infty}")))

(define (main)
  (printf "~a\n" (make-string 76 #\=))
  (printf "~a  ~a  ~a~a\n"
          (~a "grammar" #:min-width 28) (~a "expected" #:min-width 9)
          (~a "rosette" #:min-width 8) (if z3? "  z3" ""))
  (printf "~a\n" (make-string 76 #\=))
  (define counts (make-hash))
  (define agree 0)
  (for ([g corpus])
    (match-define (list name expected src) g)
    (define ros (classify-rosette src))
    (define z3  (and z3? (classify-z3 src)))
    (when (and z3? (eq? z3 ros)) (set! agree (add1 agree)))
    (hash-update! counts ros add1 0)
    (printf "~a  ~a  ~a~a~a\n"
            (~a name #:min-width 28)
            (~a expected #:min-width 9)
            (~a ros #:min-width 8)
            (cond [(not z3?) ""]
                  [else (format "  ~a" (~a z3 #:min-width 8))])
            (cond [(not (eq? ros expected))      "  <-- vs expected"]
                  [(and z3? (not (eq? z3 ros)))  "  <-- z3/rosette DIFFER"]
                  [else ""])))
  (printf "~a\n" (make-string 76 #\=))
  (printf "rosette totals: accepted=~a  rejected=~a  errors=~a  (of ~a grammars)\n"
          (hash-ref counts 'accept 0) (hash-ref counts 'reject 0)
          (hash-ref counts 'error 0) (length corpus))
  (if z3?
      (printf "z3 vs rosette agreement: ~a / ~a\n" agree (length corpus))
      (printf "z3 cross-check skipped: no z3 executable on PATH (results above are complete)\n")))

(main)
