#!/bin/sh
# The Lazarus package (packages/zerobounce.lpk) is the version of record. The Delphi package
# (ZeroBounce.dpk, ZeroBounce.dproj) must carry the same major.minor.build.
set -eu
lpk=$(grep 'Version Major' packages/zerobounce.lpk | sed -E 's/.*Major="([0-9]+)" Minor="([0-9]+)" Build="([0-9]+)".*/\1.\2.\3/')
if [ -z "$lpk" ]; then
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
  "$(sed -n -E "s/^\{\\\$LIBVERSION '([^']*)'\}.*/\1/p" packages/ZeroBounce.dpk)" "$lpk"
check "DllVersion in packages/ZeroBounce.dproj" \
  "$(sed -n -E 's/.*<DllVersion>([^<]*)<\/DllVersion>.*/\1/p' packages/ZeroBounce.dproj | sort -u | tr '\n' ' ' | sed 's/ $//')" "$lpk"
check "FileVersion in packages/ZeroBounce.dproj" \
  "$(sed -n -E 's/.*FileVersion=([0-9.]+);.*/\1/p' packages/ZeroBounce.dproj | sed -E 's/\.0$//')" "$lpk"
check "ProductVersion in packages/ZeroBounce.dproj" \
  "$(sed -n -E 's/.*ProductVersion=([0-9.]+);.*/\1/p' packages/ZeroBounce.dproj | sed -E 's/\.0$//')" "$lpk"
# Every unit under src/ must be listed in the Delphi package.
for f in src/*.pas; do
  u=$(basename "$f")
  grep -q "src\\\\$u" packages/ZeroBounce.dpk || { echo "Error: $u is not in packages/ZeroBounce.dpk"; fail=1; }
  grep -q "src\\\\$u" packages/ZeroBounce.dproj || { echo "Error: $u is not in packages/ZeroBounce.dproj"; fail=1; }
done
[ "$fail" = 0 ] && echo "Package versions agree: $lpk"
exit "$fail"
