#lang racket

(require "core.rkt"
         "tree.rkt")

(provide (rename-out [parse peg-parse]))

(define (flattent t)
   (match t
      [(tunit)     '()]
      [(tchr c)    (list c)]
      [(tpair f s) (append (flattent f) (flattent s))]
      [(tleft t)   (flattent t)] 
      [(tright t)  (flattent t)]
      [(tlist xs)  (flatten (map flattent xs))]
      [(tpop xs)  (flatten (map flattent xs))]
      [(tpush xs)  (flatten (map flattent xs))]
      [(tdrop)  '()]
      [(tpeek xs)  (flatten (map flattent xs))]
      [(tpeek xs)  (flatten (map flattent xs))]
      [(tpeekall xs)  (flatten (map flattent xs))]
      [(tpopall xs)  (flatten (map flattent xs))]
     )
  )

(define (match-input x s)
      (cond [(null? x) s]
            [(null? s) #f]
            [(char=? (car x) (car s)) (match-input (cdr x) (cdr s))]
            [else #f]
            )
  )
;; definition of the top level parser

(define (parse g s)
  (match g
    [(peg-grammar rs p)
     (let* ([inp (string->list s)]
            [r (run-expr '() rs p inp)])
       (if (null? r)
           (displayln "Could not parse the input string!")
            r))]))

(define (run-eps stk s)
  (list (tunit) s stk))

(define (run-chr stk c s)
  (match s
    ['() '()]
    [(cons c1 s1)
     (if (eq? c c1)
         (list (tchr c) stk s1)
         '())]))

(define (run-any stk s)
  (match s
    ['() '()]
    [(cons c s1) (list (tchr c) stk s1)]))

(define (run-var stk g v s)
  (match (assoc v g)
    [#f (begin
          (printf "Undefined variable: ~a\n~a\n" v g)
          '())]
    [(cons _ e1) (run-expr stk g e1 s)]))

(define (run-cat stk g e1 e2 s)
  (match (run-expr stk g e1 s)
    ['() '()]
    [(list t1 stk1 s1)
     (match (run-expr stk1 g e2 s1)
       ['() '()]
       [(list t2 stk2 s2)
        (list (tpair t1 t2) stk2 s2)])]))

(define (run-choice stk g e1 e2 s)
  (match (run-expr stk g e1 s)
    ['() (match (run-expr stk g e2 s)
           ['() '()]
           [(list t2 stk2 s2)
            (list (tright t2) stk2 s2)])]
    [(list t1 stk1 s1)
     (list (tleft t1) stk1 s1)]))

(define (run-neg stk g e1 s)
  (match (run-expr stk g e1 s)
    ['() (cons (tunit) stk s)]
    [(cons t s1) '()]))

(define (run-star stk g e s)
  (match (run-expr stk g e s)
    ['() (list (tlist '()) stk s)]
    [(list t stk1 s1)
     (match (run-expr stk1 g (pstar e) s1)
       ['() (list (tlist (list t)) stk1 s1)]
       [(list (tlist t2) stk2 s2)
        (list (tlist (cons t t2)) stk2 s2)]
       [(list t stk2 s2) (raise 'invalid-tree)])]))

(define (run-repeat-exact stk n g e s)
  (cond
    [(<= n 0) (list (tlist '()) stk s)]
    [else (match (run-expr stk g e s)
               ['() '()]
               [(list t stk1 s1)
                    (match (run-repeat-exact stk1 (- n 1) g e s1)
                           ['() '()]
                           [(list (tlist t2) stk2 s2)
                            (list (tlist (cons t t2)) stk2 s2)]
               [(list t stk2 s2) (raise 'invalid-tree)])])]))

(define (run-push stk g e s)
      (match (run-expr stk g e s)
             ['() '()]
             [(list t stk1 s1) (list (tpush t) (cons (flattent t) stk1) s1) ]))

(define (run-pop stk g s)
     (if (null? stk)
         '()
          (match (match-input (car stk) s)
                [#f '()]
                [s1 (list (tpop (car stk)) (cdr stk) s1)])))

(define (run-peek stk g s)
     (if (null? stk)
         '()
          (match (match-input (car stk) s)
                [#f '()]
                [s1 (list (tpop (car stk)) stk s1)])))

(define (run-drop stk g s)
     (if (null? stk)
         '()
          (list (tdrop) (cdr stk) s)))

(define (run-repeat-while stk t f g e s)
   (cond [(<= f 0) (list t stk s)]
         [else (match (run-expr stk g e s)
                      ['()                       (list t stk s)]
                      [(list t1 stk1 s1) (run-repeat-while stk1
                                                          (append t (list t1))
                                                          (- f 1)
                                                          g e s)]) ])
  )

(define (run-repeat-interval stk i f g e s)
  (cond
    [(< f i) (list (tlist '()) stk s)]
    [else (match (run-repeat-exact stk i g e s)
              ['() '()]
              [(list t stk1 s1) (run-repeat-while stk1 (list t) f g e s1)])]))

(define (eval stk e)
    (match e
      [(ax-lit n) n]
      [(ax-var 'len) (length (car stk))]
      [(ax-var 'tonat) (let ([k (string->number (list->string (car stk)))])
                             (if (not k) (raise 'numberFormatFail) k))]
      [(ax-op '+ e d) (+ (eval stk e) (eval stk d))]
      [(ax-op '- e d) (max 0 (- (eval stk e) (eval stk d)))]
      [(ax-op '* e d) (* (eval stk e) (eval stk d))]
      )
  )

(define (run-expr stk g e s)
  (match e
    [(peps) (run-eps stk s)]
    [(pchr c) (run-chr stk c s)]
    [(pany) (run-any stk s)]
    [(pvar v) (run-var stk g v s)]
    [(pcat e1 e2) (run-cat stk g e1 e2 s)]
    [(pchoice e1 e2) (run-choice stk g e1 e2 s)]
    [(pneg e1) (run-neg stk g e1 s)]
    [(pstar e1) (run-star stk g e1 s)]
    [(prepeat-exact e1 ae) (run-repeat-exact stk (eval stk ae) g e1 s)]
    [(prepeat-interval e1 ai af) (run-repeat-interval stk (eval stk ai) (eval stk af) g e1 s)]
    [(ppush e1) (run-push stk g e1 s)]
    [(ppeek) (run-peek stk g s)]
    [(ppop)    (run-pop stk g s)]
    [(pdrop)   (run-drop stk g s)]
    ;[(ppopall) (run-expr (cdr stk) g e s)]
    
  ))
