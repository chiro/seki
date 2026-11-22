#!/usr/bin/env bash
# Checks that the source distribution ships every module and documentation
# input exactly once, that the public Haddock index documents every public
# module on its own page, and that every export of a public module has a
# Haddock comment. Extra arguments are passed to cabal haddock.
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
    /^  [^ ]/ && (field == "extra-source-files:" || field == "extra-doc-files:" || field == "data-files:") {
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
index=$(find dist-newstyle/build \( -path '*/seki-0.1.0.0/doc/html/seki/index.html' -o -path '*/seki-0.1.0.0/opt/doc/html/seki/index.html' \) -not -path '*/l/*' -newer "$tarball" -printf '%T@ %p\n' | sort -rn | head -n 1 | cut -d' ' -f2-)
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

public_dependencies() {
  awk '
    /^[^ ]/ { section = $0 }
    section == "library" && /^  [a-z-]+:/ { field = $1 }
    section == "library" && field == "build-depends:" && /seki:/ {
      gsub(/[ ,]/, "")
      sub(/.*seki:/, "")
      print
    }
  ' seki.cabal
}

# Cabal registers each internal library's Haddock interface one directory
# above the file it writes, so the public library would otherwise read none of
# the comments on the definitions it re-exports.
interfaces=()
while read -r dependency; do
  interface=$(find "$PWD/dist-newstyle/build" \( -path "*/l/$dependency/doc/html/seki/$dependency/$dependency.haddock" -o -path "*/l/$dependency/opt/doc/html/seki/$dependency/$dependency.haddock" \) -newer "$tarball" | head -n 1)
  if [ -z "$interface" ]; then
    fail "Haddock interface of seki:$dependency was not regenerated"
  else
    interfaces+=("--read-interface=$(dirname "$interface"),$interface")
  fi
done < <(public_dependencies)

public=$(cabal_modules | awk -F '\t' '$1 == "library" { print $2 }')
if ! log=$(cabal haddock seki:lib:seki --haddock-options="${interfaces[*]}" "$@" 2>&1); then
  printf '%s\n' "$log" >&2
  fail "cabal haddock seki:lib:seki failed"
fi
coverage=$(awk '/^Running Haddock on library for / { p = 1 } p' <<<"$log")
while read -r module; do
  line=$(grep -F "in '$module'" <<<"$coverage" || true)
  if [ -z "$line" ]; then
    fail "Haddock printed no coverage for public module $module"
    continue
  fi
  read -r documented total < <(sed -E 's/.*\( *([0-9]+) \/ *([0-9]+)\).*/\1 \2/' <<<"$line")
  if [ "$documented" -ne "$total" ]; then
    missing=$(awk -v module="'$module'" '
      $NF == module { p = 1; next }
      p && (/^ +[0-9]+% / || /^[^ ]/) { exit }
      p && /^    [^ ]/ { sub(/^ +/, ""); printf "%s%s", sep, $0; sep = ", " }
    ' <<<"$coverage")
    fail "public module $module documents $documented of $total exports; undocumented: $missing"
  fi
  page="$(dirname "$index")/${module//./-}.html"
  [ -f "$page" ] || continue
  while read -r linked; do
    [ -n "$linked" ] || continue
    if ! grep -q -x -F "$linked" <<<"$public"; then
      fail "public module $module shows the exports of $linked only as a link to that module"
    fi
  done < <(grep -o -E '<p class="src">module <a href="[^"]*">[^<]*</a>' "$page" | sed -E 's/.*">([^<]*)<\/a>$/\1/')
done <<<"$public"

if [ "$failures" -ne 0 ]; then
  exit 1
fi
echo "check-package: $tarball ok"
