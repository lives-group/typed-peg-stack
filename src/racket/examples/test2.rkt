#lang typed-peg-stack/untyped

S <-- 'a' S 'a' / epsilon ;

start: S
