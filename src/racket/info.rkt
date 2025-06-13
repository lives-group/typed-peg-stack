#lang info
(define collection "typed-peg-stack")
(define deps '("base"
               "pprint"
               "rackcheck"
               "parser-tools-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"))
(define scribblings '(("scribblings/typed-peg-stack.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(rodrigo elton guilherme))
(define license '(Apache-2.0 OR MIT))
