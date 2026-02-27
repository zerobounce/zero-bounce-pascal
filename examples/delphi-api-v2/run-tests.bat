@echo off
if exist TestValidateEmail.exe (
  TestValidateEmail.exe
  exit /b %ERRORLEVEL%
)
echo Build TestValidateEmail.dpr first (e.g. in Delphi), then run this script.
exit /b 1
