#lang racket

(module reader racket
  (require "../grammar.rkt"
           "../core.rkt")
  (provide (rename-out [peg-read read]
                       [peg-read-syntax read-syntax]))


  (define (peg-read in)
    (syntax->datum
     (peg-read-syntax #f in)))

  (define (peg-read-syntax path port)
    (datum->syntax
     #f
     `(module peg-mod racket
        ,(parse port)))))
