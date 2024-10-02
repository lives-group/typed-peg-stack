#lang typed-peg-stack/untyped

start: 'a' (! 'b' / 'c')*
