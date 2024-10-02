#lang typed-peg-stack/untyped

start: push('0')~push('2')~(('a'){2 * top.tonat, infty} ){0,infty}
