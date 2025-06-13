#lang typed-peg-stack/untyped

start: push('0')~push('2')~('a' / epsilon){top.len + 1} 
