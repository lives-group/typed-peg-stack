#lang typed-peg-stack/untyped

start: ('a' 'a' 'b')* 'a' 'a' 'c' / 'a' 'a' 'd'
