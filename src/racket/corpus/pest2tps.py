#!/usr/bin/env python3
"""Translate a .pest grammar into the typed-peg-stack concrete syntax.

The typed-peg type system decides well-formedness from just two pieces of static
information: whether an expression is nullable, and which non-terminals can occur
at its head. Every single-character matcher (a string/char literal, a character
range, an ASCII_* built-in, ANY, NEWLINE) has the same type <false, empty>, so we
soundly abstract all of them to the tool's `.` (any) terminal. Zero-width anchors
(SOI, EOI) become `epsilon`. The structure that actually drives the analysis
(non-terminal references, sequence, choice, repetition, predicates, stack ops) is
preserved. This keeps the translation faithful for the purposes of the type
system while making it mechanical rather than hand-curated.
"""
import re, sys

BUILTINS_TERMINAL = {
    "ANY","NEWLINE","ASCII","ASCII_DIGIT","ASCII_NONZERO_DIGIT","ASCII_ALPHA",
    "ASCII_ALPHANUMERIC","ASCII_ALPHA_LOWER","ASCII_ALPHA_UPPER","ASCII_HEX_DIGIT",
    "ASCII_OCT_DIGIT","ASCII_BIN_DIGIT","LETTER","UPPERCASE_LETTER","LOWERCASE_LETTER",
    "NUMBER","DIGIT",
}
BUILTINS_EPS = {"SOI","EOI","soi","eoi"}
BUILTINS_TERMINAL |= {"any","newline"}
STACK = {"PEEK_ALL":"peekall","POP_ALL":"dropall","PUSH_LITERAL":"push",
         "PUSH":"push","POP":"pop","PEEK":"peek","DROP":"drop"}

def strip_comments(s):
    s = re.sub(r"/\*.*?\*/", " ", s, flags=re.S)
    s = re.sub(r"//[^\n]*", " ", s)
    s = re.sub(r"#\w+\s*=\s*", "", s)   # pest 2.x node tags: #name=expr
    return s

def skip_literal(s, i):
    """If s[i] starts a string/char literal, return index past it, else None."""
    q = s[i]
    if q not in "\"'":
        return None
    j = i+1
    while j < len(s):
        if s[j] == "\\": j += 2; continue
        if s[j] == q: return j+1
        j += 1
    return j

def match_braces(s, i):
    """s[i] == '{'; return index just past the matching '}', skipping literals."""
    depth = 0; j = i
    while j < len(s):
        lit = skip_literal(s, j)
        if lit is not None: j = lit; continue
        if s[j] == "{": depth += 1
        elif s[j] == "}":
            depth -= 1
            if depth == 0: return j+1
        j += 1
    return j

def parse_rules(s):
    """Return list of (name, modifier, body-text) at top level, in order."""
    rules = []
    header = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([@_$!]?)\s*\{")
    i = 0
    while i < len(s):
        m = header.match(s, i) if False else header.search(s, i)
        if not m: break
        brace_open = m.end()-1
        end = match_braces(s, brace_open)
        body = s[brace_open+1:end-1]
        rules.append((m.group(1), m.group(2), body))
        i = end
    return rules

def sanitize(name):
    n = name.replace("_", "")
    if not n: n = "r"
    if not n[0].isalpha(): n = "r"+n
    return n

def translate_body(body):
    out = []
    i = 0
    n = len(body)
    def emit(t): out.append(t)
    while i < n:
        c = body[i]
        # range continuation 'a'..'z': the start literal already emitted '.', so
        # skip '..' and the end literal, emitting nothing more.
        if c == "." and body[i:i+2] == "..":
            j = i+2
            while j < len(body) and body[j].isspace(): j += 1
            litj = skip_literal(body, j) if (j < len(body) and body[j] in "\"'") else None
            i = litj if litj is not None else j
            continue
        # literals -> terminal or epsilon
        lit = skip_literal(body, i) if c in "\"'" else None
        if lit is not None:
            inner = body[i:lit][1:-1]
            emit("epsilon" if inner == "" else ".")
            i = lit; continue
        if c.isspace():
            emit(" "); i += 1; continue
        if c == "^" and i+1 < n and body[i+1] in "\"'":
            i += 1; continue  # case-insensitive prefix, treat following literal normally
        if c == "|":
            emit(" / "); i += 1; continue
        if c in "~*?()!&":
            emit(c); i += 1; continue
        if c == "+":
            emit("+"); i += 1; continue
        if c == "{":
            end = match_braces(body, i)
            rep = body[i:end]
            inner = rep[1:-1].strip()
            # normalise repetition bounds
            mm = re.fullmatch(r"\s*(\d*)\s*,\s*(\d*)\s*", inner)
            if mm:
                lo, hi = mm.group(1), mm.group(2)
                if lo == "" : lo = "0"
                if hi == "" :
                    emit("+" if lo not in ("0","") else "*")  # n,  -> + (n>=1) / *
                else:
                    emit("{"+lo+","+hi+"}")
            elif re.fullmatch(r"\s*\d+\s*", inner):
                emit("{"+inner.strip()+"}")
            else:
                raise ValueError("unsupported repetition {%s}" % inner)
            i = end; continue
        # identifier / builtin / stack op
        m = re.match(r"[A-Za-z_][A-Za-z0-9_]*", body[i:])
        if m:
            word = m.group(0); i += m.end()
            if word in BUILTINS_TERMINAL: emit(".")
            elif word in BUILTINS_EPS: emit("epsilon")
            elif word in STACK:
                emit(STACK[word])
                # PUSH takes (e); others are nullary. POP_ALL etc. handled.
            else:
                emit(sanitize(word))
            continue
        # anything else (e.g. '..' stray, '#', etc.) -> skip as terminal
        i += 1
    # collapse spaces
    return re.sub(r"\s+", " ", "".join(out)).strip()

def translate(pest_text):
    rules = parse_rules(strip_comments(pest_text))
    if not rules: raise ValueError("no rules found")
    tps = []
    names = [sanitize(n) for (n,_,_) in rules]
    referenced = set()
    bodies = {}
    for (name, mod, body) in rules:
        t = translate_body(body)
        if t == "": t = "epsilon"
        bodies[sanitize(name)] = t
    # references: any sanitized name occurring in another body
    for nm, t in bodies.items():
        for other in names:
            if other != nm and re.search(r"\b"+re.escape(other)+r"\b", t):
                referenced.add(other)
    # start rule: first unreferenced rule that is not the implicit whitespace/
    # comment rule; else the first rule. (The verdict is robust to this choice,
    # since every rule is type-checked, but a sensible entry reads better.)
    aux = {"whitespace","comment","WHITESPACE","COMMENT"}
    cands = [nm for nm in names if nm not in referenced and nm.lower() not in aux]
    start = cands[0] if cands else names[0]
    lines = []
    for nm in names:
        if nm == start: continue
        lines.append("%s <-- %s;" % (nm, bodies[nm]))
    lines.append("start: %s" % bodies[start])
    return "\n".join(lines), start

if __name__ == "__main__":
    txt = open(sys.argv[1]).read()
    g, start = translate(txt)
    print(g)
