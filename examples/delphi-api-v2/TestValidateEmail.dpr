program TestValidateEmail;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ZeroBounceValidate in 'ZeroBounceValidate.pas';

var
  Passed: Integer;
  Failed: Integer;

procedure Test(const Name, Expected, Actual: string; Ok: Boolean);
begin
  if Ok then
  begin
    Inc(Passed);
    WriteLn('  PASS: ', Name);
  end
  else
  begin
    Inc(Failed);
    WriteLn('  FAIL: ', Name, ' (expected: ', Expected, ', got: ', Actual, ')');
  end;
end;

procedure RunUrlEncodeTests;
begin
  WriteLn('UrlEncode tests:');
  Test('urlencode empty', '', UrlEncode('', False), UrlEncode('', False) = '');
  Test('urlencode alphanumeric', 'abc123', UrlEncode('abc123', False), UrlEncode('abc123', False) = 'abc123');
  Test('urlencode email @', '%40', UrlEncode('@', False), UrlEncode('@', False) = '%40');
  Test('urlencode user@example.com', 'user%40example.com', UrlEncode('user@example.com', False),
    UrlEncode('user@example.com', False) = 'user%40example.com');
  Test('urlencode space to %20', '%20', UrlEncode(' ', False), UrlEncode(' ', False) = '%20');
  Test('urlencode space to + when Pluses', '+', UrlEncode(' ', True), UrlEncode(' ', True) = '+');
  Test('urlencode dot', '%2E', UrlEncode('.', False), UrlEncode('.', False) = '%2E');
  Test('urlencode hyphen', '%2D', UrlEncode('-', False), UrlEncode('-', False) = '%2D');
end;

procedure RunIsValidStatusTests;
begin
  WriteLn('IsValidStatus tests:');
  Test('valid -> true', 'true', BoolToStr(IsValidStatus('valid'), True), IsValidStatus('valid') = True);
  Test('Valid -> true', 'true', BoolToStr(IsValidStatus('Valid'), True), IsValidStatus('Valid') = True);
  Test('VALID -> true', 'true', BoolToStr(IsValidStatus('VALID'), True), IsValidStatus('VALID') = True);
  Test('invalid -> false', 'false', BoolToStr(IsValidStatus('invalid'), True), IsValidStatus('invalid') = False);
  Test('Invalid -> false', 'false', BoolToStr(IsValidStatus('Invalid'), True), IsValidStatus('Invalid') = False);
  Test('unknown -> false', 'false', BoolToStr(IsValidStatus('unknown'), True), IsValidStatus('unknown') = False);
  Test('Unknown -> false', 'false', BoolToStr(IsValidStatus('Unknown'), True), IsValidStatus('Unknown') = False);
  Test('catch-all -> true', 'true', BoolToStr(IsValidStatus('catch-all'), True), IsValidStatus('catch-all') = True);
  Test('spamtrap -> true (no invalid/unknown)', 'true', BoolToStr(IsValidStatus('spamtrap'), True), IsValidStatus('spamtrap') = True);
  Test('empty -> true', 'true', BoolToStr(IsValidStatus(''), True), IsValidStatus('') = True);
  Test('valid, sub_status -> true', 'true', BoolToStr(IsValidStatus('valid, some_sub'), True), IsValidStatus('valid, some_sub') = True);
  Test('invalid, sub_status -> false', 'false', BoolToStr(IsValidStatus('invalid, typo'), True), IsValidStatus('invalid, typo') = False);
end;

begin
  Passed := 0;
  Failed := 0;
  WriteLn('ZeroBounce Delphi API v2 - Unit tests');
  WriteLn('');
  RunUrlEncodeTests;
  WriteLn('');
  RunIsValidStatusTests;
  WriteLn('');
  WriteLn('Result: ', Passed, ' passed, ', Failed, ' failed');
  if Failed > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
