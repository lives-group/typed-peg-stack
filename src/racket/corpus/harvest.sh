#!/usr/bin/env bash
#
# Re-harvest the real Pest grammars and regenerate the translated corpus in
# corpus/real/. This step needs network access (it clones repositories listed in
# pest-parser/awesome-pest). Running the corpus itself does NOT need this script:
# the translated .tps files and their provenance (manifest.tsv) are committed, so
# `racket corpus/run-real-corpus.rkt` works offline.
#
# Usage:  bash harvest.sh
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
OUT="$HERE/real"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# name:owner/repo  for the projects that ship .pest grammars
PROJECTS="
ashpaper:shnewto/ashpaper
caith:Geobert/caith
elastic:cch123/elastic-rs
melody:yoav-lavi/melody
pest:pest-parser/pest
pta:AltaModaTech/pta-parser
pyliteral:jturner314/py_literal
qubit:abhimanyu003/qubit
rouler:jarcane/rouler
"

echo ">> cloning projects into $WORK"
cd "$WORK"
declare -A SLUG
for entry in $PROJECTS; do
  name="${entry%%:*}"; slug="${entry#*:}"; SLUG[$name]="$slug"
  git clone --depth 1 --quiet "https://github.com/$slug" "$name" \
    && echo "   ok   $name ($slug)" || echo "   FAIL $name ($slug)"
done

# reconstruct the complete pta grammars (base + each domain fragment), as pest's
# multi-file derive does
if [ -d pta/pta-parser/src/grammars ]; then
  mkdir -p combined
  cat pta/pta-parser/src/grammars/base.pest pta/pta-parser/src/grammars/generic.pest   > combined/pta_generic_full.pest
  cat pta/pta-parser/src/grammars/base.pest pta/pta-parser/src/grammars/beancount.pest > combined/pta_beancount_full.pest
fi

echo ">> translating grammars into $OUT"
mkdir -p "$OUT"
rm -f "$OUT"/*.tps
MAN="$OUT/manifest.tsv"; : > "$MAN"
printf 'name\trepo\tpath\tcommit\tstatus\n' >> "$MAN"

for f in $(find . -path ./combined -prune -o -name '*.pest' -print | sort); do
  top="$(echo "$f" | cut -d/ -f2)"
  slug="${SLUG[$top]:-unknown}"
  commit="$(git -C "$top" rev-parse --short HEAD 2>/dev/null || echo '?')"
  rel="${f#./$top/}"
  base="$(echo "${top}_${rel}" | sed 's#[/.]#_#g; s#_pest$##')"
  if python3 "$HERE/pest2tps.py" "$f" > "$OUT/$base.tps" 2>/dev/null; then s=ok; else s=skip; rm -f "$OUT/$base.tps"; fi
  printf '%s\t%s\t%s\t%s\t%s\n' "$base" "$slug" "$rel" "$commit" "$s" >> "$MAN"
done

# the two reconstructed complete pta grammars
if [ -d combined ]; then
  commit="$(git -C pta rev-parse --short HEAD 2>/dev/null || echo '?')"
  for c in generic beancount; do
    base="pta_full_$c"
    if python3 "$HERE/pest2tps.py" "combined/pta_${c}_full.pest" > "$OUT/$base.tps" 2>/dev/null; then s=ok; else s=skip; rm -f "$OUT/$base.tps"; fi
    printf '%s\t%s\t%s\t%s\t%s\n' "$base" "AltaModaTech/pta-parser" "grammars/base.pest + grammars/$c.pest" "$commit" "$s" >> "$MAN"
  done
fi

echo ">> done: $(ls "$OUT"/*.tps | wc -l) grammars translated, $(grep -c 'skip$' "$MAN") skipped"
echo "   provenance recorded in $MAN"
