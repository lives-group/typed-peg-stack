#lang typed-peg-stack/untyped

I <-- '1' epsilon;
L <-- I I ;
W <-- epsilon I;
start: W *
