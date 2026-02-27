#!/usr/bin/env sh
if [ -f TestValidateEmail ]; then
  ./TestValidateEmail
  exit $?
fi
if [ -f TestValidateEmail.exe ]; then
  ./TestValidateEmail.exe
  exit $?
fi
echo "Build TestValidateEmail.dpr first (Delphi or FPC), then run this script."
exit 1
