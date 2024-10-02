#lang typed-peg-stack

start: push('0')~push('2')~('a' / epsilon){top.len + 1} 
