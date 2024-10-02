#lang typed-peg-stack/untyped

A <-- B 'a' / 'a' 'b' ;
B <-- 'a' * A / epsilon ;

start: A
