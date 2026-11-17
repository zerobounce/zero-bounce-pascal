#!/bin/sh
# The Lazarus package (packages/zerobounce.lpk) is the version of record. The Delphi package
# (ZeroBounce.dpk, ZeroBounce.dproj) must carry the same major.minor.build.
set -eu
lpk_version=$(grep 'Version Major' packages/zerobounce.lpk | sed -E 's/.*Major="([0-9]+)" Minor="([0-9]+)" Build="([0-9]+)".*/\1.\2.\3/')
if [ -z "$lpk_version" ]; then
  echo "Error: Could not read the version from packages/zerobounce.lpk"
  exit 1
fi
fail=0
check() {
  if [ "$2" != "$3" ]; then
    echo "Error: $1 is '$2', packages/zerobounce.lpk is '$3'."
    fail=1
  fi
}
check "{\$LIBVERSION} in packages/ZeroBounce.dpk" \
  "$(sed -n -E "s/^\{\\\$LIBVERSION '([^']*)'\}.*/\1/p" packages/ZeroBounce.dpk)" "$lpk_version"
check "DllVersion in packages/ZeroBounce.dproj" \
  "$(sed -n -E 's/.*<DllVersion>([^<]*)<\/DllVersion>.*/\1/p' packages/ZeroBounce.dproj | sort -u | tr '\n' ' ' | sed 's/ $//')" "$lpk_version"
check "FileVersion in packages/ZeroBounce.dproj" \
  "$(sed -n -E 's/.*FileVersion=([0-9.]+);.*/\1/p' packages/ZeroBounce.dproj | sed -E 's/\.0$//')" "$lpk_version"
check "ProductVersion in packages/ZeroBounce.dproj" \
  "$(sed -n -E 's/.*ProductVersion=([0-9.]+);.*/\1/p' packages/ZeroBounce.dproj | sed -E 's/\.0$//')" "$lpk_version"
# The package files with their comments removed, so a commented-out entry does not count:
# // line comments, { } (not {$ directives) and (* *) blocks in Pascal sources, <!-- --> in XML.
strip_pascal() { perl -0pe 's/\(\*.*?\*\)//gs; s/\{(?!\$).*?\}//gs; s{//[^\n]*}{}g' "$1"; }
strip_xml() { perl -0pe 's/<!--.*?-->//gs' "$1"; }
dpk=$(strip_pascal packages/ZeroBounce.dpk)
dproj=$(strip_xml packages/ZeroBounce.dproj)
lpk=$(strip_xml packages/zerobounce.lpk)
lpkpas=$(strip_pascal packages/zerobounce.pas)
# The units named in the uses clause of the Lazarus package unit.
lpkuses=$(printf '%s\n' "$lpkpas" | awk 'tolower($1)=="uses"{on=1;next} /LazarusPackageIntf/{on=0} on')
# Every unit under src/ must be listed in the Delphi and the Lazarus packages.
for f in src/*.pas; do
  u=$(basename "$f")
  n=$(basename "$f" .pas)
  printf '%s\n' "$dpk" | grep -q "src\\\\$u" || { echo "Error: $u is not in packages/ZeroBounce.dpk"; fail=1; }
  printf '%s\n' "$dproj" | grep -q "src\\\\$u" || { echo "Error: $u is not in packages/ZeroBounce.dproj"; fail=1; }
  printf '%s\n' "$lpk" | grep -q "src/$u\"" || { echo "Error: $u is not in packages/zerobounce.lpk"; fail=1; }
  printf '%s\n' "$lpkuses" | grep -iqw "$n" || { echo "Error: $n is not in the uses clause of packages/zerobounce.pas"; fail=1; }
  printf '%s\n' "$lpkpas" | grep -iq "RegisterUnit('$n'" || { echo "Error: $n is not registered in packages/zerobounce.pas"; fail=1; }
done
# Every include file under src/ must be listed in the Lazarus package.
for f in src/*.inc; do
  u=$(basename "$f")
  printf '%s\n' "$lpk" | grep -q "src/$u\"" || { echo "Error: $u is not in packages/zerobounce.lpk"; fail=1; }
done
[ "$fail" = 0 ] && echo "Package versions agree: $lpk_version"
exit "$fail"
