#!/usr/bin/env bash
# Checks that the source distribution ships every module and documentation
# input exactly once, and that the public Haddock index documents every public
# module on its own page. Extra arguments are passed to cabal haddock.
set -euo pipefail

cd "$(dirname "$0")/.."

failures=0
fail() {
  echo "check-package: $*" >&2
  failures=$((failures + 1))
}

cabal_modules() {
  awk '
    /^[^ ]/ { section = $0 }
    /^  [a-z-]+:/ { field = $1 }
    /^  (exposed-modules|other-modules|reexported-modules):/ {
      for (i = 2; i <= NF; i++) print section "\t" $i
      next
    }
    /^    [A-Z]/ && (field == "exposed-modules:" || field == "other-modules:" || field == "reexported-modules:") {
      print section "\t" $NF
    }
  ' seki.cabal
}

cabal_files() {
  awk '
    /^[^ ]/ { field = $1 }
    /^  [^ ]/ && (field == "extra-source-files:" || field == "extra-doc-files:") {
      print $1
    }
  ' seki.cabal
}

tarball=$(cabal sdist | tail -n 1)
tarball=${tarball#Wrote tarball sdist to }
listing=$(tar -tzf "$tarball" | sed 's|^[^/]*/||')

while IFS=$'\t' read -r section module; do
  path="${module//.//}.hs"
  count=$(grep -c -E "(^|/)${path//./\\.}\$" <<<"$listing" || true)
  [ "$count" -eq 1 ] || fail "$section module $module appears $count times in $tarball"
done < <(cabal_modules)

while read -r file; do
  count=$(grep -c -x -F "$file" <<<"$listing" || true)
  [ "$count" -eq 1 ] || fail "$file appears $count times in $tarball"
done < <(cabal_files)

cabal haddock all "$@" >/dev/null
index=$(find dist-newstyle/build -path '*/seki-0.1.0.0/doc/html/seki/index.html' -not -path '*/l/*' -newer "$tarball" -printf '%T@ %p\n' | sort -rn | head -n 1 | cut -d' ' -f2-)
if [ -z "$index" ]; then
  fail "public Haddock index was not regenerated"
else
  while IFS=$'\t' read -r section module; do
    [ "$section" = "library" ] || continue
    page="${module//./-}.html"
    grep -q "href=\"$page\"" "$index" || fail "public Haddock index does not link $module to $page"
    [ -f "$(dirname "$index")/$page" ] || fail "public Haddock page $page is missing"
  done < <(cabal_modules)
fi

if [ "$failures" -ne 0 ]; then
  exit 1
fi
echo "check-package: $tarball ok"
