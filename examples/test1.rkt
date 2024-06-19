#lang typed-peg-stack/debug/z3-script-only

start: push('0')~push('2')~'a'{top.len} 
