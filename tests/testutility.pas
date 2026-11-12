unit TestUtility;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, base64, openssl, fpopenssl, sslbase,
  BaseTest, ZbUtility, ZbGeneric;

type
  TTestUtility = class(TBaseTestCase)
  published
    procedure TestEncodeParamSafeChars;
    procedure TestEncodeParamSpaces;
    procedure TestEncodeParamSpecialChars;
    procedure TestZBInitializeSetsBaseURL;
    procedure TestZBInitializeWithRegionSetsBaseURL;
    procedure TestZBInitializeWithAnExplicitBaseURL;
    procedure TestZBInitializeRejectsABadBaseURL;
    procedure TestRequireHttpsUrlRejectsLoopbackLookalikes;
    procedure TestRequireHttpsUrlAllowsLoopbackWithPortsAndPaths;
    procedure TestZBInitializeRejectsLoopbackLookalikes;
    procedure TestRequestToALoopbackLookalikeIsNeverSent;
    procedure TestMissingCACertFileIsAClearError;
    procedure TestTrustDerCertificatesAddsParsableCertificates;
    procedure TestDeprecatedSetApiKeyChecksTheKey;
    procedure TestAKeyWithAControlCharacterInsideIsRefused;
    procedure TestRedactSecretsHidesTheKeyRawAndEncoded;
    procedure TestAnApiErrorMessageNeverCarriesTheKey;
    procedure TestInitializeIsAllOrNothing;
    procedure TestSuccessFalseAsAStringIsAFailure;
  end;

implementation

procedure TTestUtility.TestEncodeParamSafeChars;
var
  Encoded: String;
begin
  Encoded := EncodeParam('abc123XYZ');
  AssertEquals('Safe alphanumeric unchanged', 'abc123XYZ', Encoded);
end;

procedure TTestUtility.TestEncodeParamSpaces;
var
  Encoded: String;
begin
  Encoded := EncodeParam('hello world');
  AssertTrue('Spaces encoded', (Encoded = 'hello+world') or (Encoded = 'hello%20world'));
end;

procedure TTestUtility.TestEncodeParamSpecialChars;
var
  Encoded: String;
begin
  Encoded := EncodeParam('a@b.c');
  AssertTrue('Encoded non-empty', Length(Encoded) > 0);
  // EncodeParam is used for query params; @ may or may not be encoded depending on FPC/Delphi
  AssertTrue('Contains expected chars', (Pos('a', Encoded) > 0) and (Pos('b', Encoded) > 0) and (Pos('c', Encoded) > 0));
end;

procedure TTestUtility.TestZBInitializeSetsBaseURL;
begin
  ZBInitialize('test_key');
  AssertEquals('Default base URL', 'https://api.zerobounce.net/v2', ZbApiBaseURL);
  AssertEquals('API key set', 'test_key', ZbApiKey);
end;

procedure TTestUtility.TestZBInitializeWithRegionSetsBaseURL;
begin
  ZBInitialize('test_key', ZbApiURLEU);
  AssertEquals('EU base URL', 'https://api-eu.zerobounce.net/v2', ZbApiBaseURL);
  ZBInitialize('test_key', ZbApiURLUSA);
  AssertEquals('USA base URL', 'https://api-us.zerobounce.net/v2', ZbApiBaseURL);
end;

{ The enum names three regions. A stub, or a region it does not name, needs a
  literal URL, and the alternative was assigning the ZbApiBaseURL threadvar by
  hand. The four bulk base URLs are already plain overridable vars. }
procedure TTestUtility.TestZBInitializeWithAnExplicitBaseURL;
begin
  ZBInitialize('test_key', 'https://api-au.zerobounce.net/v2');
  AssertEquals(
    'explicit base URL', 'https://api-au.zerobounce.net/v2', ZbApiBaseURL
  );
  AssertEquals('API key set', 'test_key', ZbApiKey);

  { A trailing slash would build '//getcredits'. }
  ZBInitialize('test_key', 'https://api-au.zerobounce.net/v2/');
  AssertEquals(
    'trailing slash dropped', 'https://api-au.zerobounce.net/v2', ZbApiBaseURL
  );

  { Loopback is allowed, so an offline test can point the Main API at a stub. }
  ZBInitialize('test_key', 'http://127.0.0.1:8080/v2');
  AssertEquals('loopback allowed', 'http://127.0.0.1:8080/v2', ZbApiBaseURL);

  { The enum overload still resolves and still wins where it is used. }
  ZBInitialize('test_key', ZbApiURLEU);
  AssertEquals('EU base URL', 'https://api-eu.zerobounce.net/v2', ZbApiBaseURL);
end;

procedure TTestUtility.TestZBInitializeRejectsABadBaseURL;
var
  Raised: Boolean;
begin
  ZBInitialize('test_key', ZbApiURLDefault);

  Raised := False;
  try
    ZBInitialize('test_key', 'http://api.zerobounce.net/v2');
  except on E: ZbException do
    begin
      Raised := True;
      AssertTrue(E.Message, Pos('https', E.Message) > 0);
    end;
  end;
  AssertTrue('plain http off loopback should be rejected', Raised);

  Raised := False;
  try
    ZBInitialize('test_key', '   ');
  except on E: ZbException do
    begin
      Raised := True;
      AssertTrue(E.Message, Pos('api_base_url', E.Message) > 0);
    end;
  end;
  AssertTrue('a blank base URL should be rejected', Raised);

  { A refused URL leaves the SDK on the one it already had. }
  AssertEquals(
    'base URL unchanged', 'https://api.zerobounce.net/v2', ZbApiBaseURL
  );
end;

function UrlRejected(const Url: String): Boolean;
begin
  Result := False;
  try
    ZbRequireHttpsUrl(Url, 'url');
  except on E: ZbException do
    Result := True;
  end;
end;

{ Plain http is for a local stub only, so the host must be exactly a loopback
  name. A prefix match let all of these through, and every one of them sends
  the request - api_key included - to evil.example or another host. }
procedure TTestUtility.TestRequireHttpsUrlRejectsLoopbackLookalikes;
const
  Bad: array[0..20] of String = (
    'http://localhost.evil.example/v2',
    'http://127.0.0.1@evil.example/v2',
    'http://localhost@evil.example/v2',
    'http://[::1]@evil.example/v2',
    'http://127.0.0.1:8080@evil.example/v2',
    'http://localhost:pw@evil.example/v2',
    'http://127.0.0.1.evil.example/v2',
    'http://127.0.0.1.nip.io/v2',
    'http://[::1].evil.example/v2',
    'http://[::1]evil.example/v2',
    'http://localhostevil.example/v2',
    'http://localhost%2eevil.example/v2',
    'http://localhost\@evil.example/v2',
    'http://evil.example/v2?h=http://localhost',
    'http://127.0.0.1:99999/v2',
    'http://127.0.0.1:80x/v2',
    'http:///v2',
    { FPC's ParseURI cuts the query at the last '?', so this one is user
      'localhost?a=b' at host evil.example there. }
    'http://localhost?a=b@evil.example/x?k=v',
    'http://localhost#@evil.example/v2',
    'http://localhost\.evil.example/v2',
    'ftp://localhost/v2'
  );
var
  Url: String;
begin
  for Url in Bad do
    AssertTrue(Url + ' should be rejected', UrlRejected(Url));
  { Credentials have no place in an https URL either. }
  AssertTrue('https userinfo', UrlRejected('https://user:pass@api.zerobounce.net/v2'));
  AssertTrue('https userinfo, no password', UrlRejected('https://api.zerobounce.net@evil.example/v2'));
end;

procedure TTestUtility.TestRequireHttpsUrlAllowsLoopbackWithPortsAndPaths;
const
  Good: array[0..13] of String = (
    'https://api.zerobounce.net/v2',
    'https://api.zerobounce.net:443/v2/validate?api_key=k&email=a%40example.com',
    'https://api.zerobounce.net/v2/validate?email=a@example.com',
    'https://bulkapi.zerobounce.net/v2/listevaluator/a%20b/?api_key=k',
    'HTTPS://API.ZEROBOUNCE.NET/v2',
    'http://127.0.0.1',
    'http://127.0.0.1/',
    'http://127.0.0.1:8080/v2',
    'http://127.0.0.1:8080/v2/validate?email=a@example.com',
    'http://localhost',
    'http://localhost:3000/v2/',
    'http://LOCALHOST:3000/v2',
    'http://[::1]',
    'http://[::1]:8080/v2?x=1'
  );
var
  Url: String;
begin
  for Url in Good do
    AssertFalse(Url + ' should be allowed', UrlRejected(Url));
end;

procedure TTestUtility.TestZBInitializeRejectsLoopbackLookalikes;
var
  Raised: Boolean;
begin
  ZBInitialize('test_key', ZbApiURLDefault);
  Raised := False;
  try
    ZBInitialize('test_key', 'http://localhost.evil.example/v2');
  except on E: ZbException do
    Raised := True;
  end;
  AssertTrue('a loopback-prefixed host should be rejected', Raised);

  Raised := False;
  try
    ZBInitialize('test_key', 'http://127.0.0.1@evil.example/v2');
  except on E: ZbException do
    begin
      Raised := True;
      AssertTrue(E.Message, Pos('user information', E.Message) > 0);
    end;
  end;
  AssertTrue('userinfo should be rejected', Raised);
  AssertEquals(
    'base URL unchanged', 'https://api.zerobounce.net/v2', ZbApiBaseURL
  );

  ZBInitialize('test_key', 'http://[::1]:8080/v2/');
  AssertEquals('IPv6 loopback with a port', 'http://[::1]:8080/v2', ZbApiBaseURL);
  ZBInitialize('test_key', 'http://localhost:3000/v2');
  AssertEquals('localhost with a port', 'http://localhost:3000/v2', ZbApiBaseURL);
  ZBInitialize('test_key', ZbApiURLDefault);
end;

{ The same rail guards every request, so a base URL assigned by hand cannot
  get round ZBInitialize's check. }
procedure TTestUtility.TestRequestToALoopbackLookalikeIsNeverSent;
var
  Raised: Boolean;
begin
  ZBMockResponse(200, '{"Credits": "7"}');
  ZbResponseMock.UrlCalled := '';
  ZbApiBaseURL := 'http://127.0.0.1@evil.example/v2';
  Raised := False;
  try
    try
      ZbGetCredits;
    except on E: ZbException do
      Raised := True;
    end;
  finally
    ZbApiBaseURL := 'https://api.zerobounce.net/v2';
  end;
  AssertTrue('the request should be refused', Raised);
  AssertEquals('no request should have been made', '', ZbResponseMock.UrlCalled);
end;

{ An explicit bundle that does not exist must say so, not fail later with a
  connection error that never names it. No connection is attempted: the
  handler is chosen before the socket is opened. }
procedure TTestUtility.TestMissingCACertFileIsAClearError;
var
  Raised: Boolean;
  Saved: String;
begin
  ZbResponseMock := cDefaultMock;
  Saved := ZbCACertFile;
  ZbCACertFile := '/nonexistent/zb-test/cacert.pem';
  Raised := False;
  try
    try
      ZBGetRequest('https://127.0.0.1:9/v2/getcredits?api_key=x');
    except on E: Exception do
      begin
        Raised := True;
        AssertTrue(E.Message, Pos('ZbCACertFile', E.Message) > 0);
        AssertTrue(E.Message, Pos('/nonexistent/zb-test/cacert.pem', E.Message) > 0);
      end;
    end;
  finally
    ZbCACertFile := Saved;
  end;
  AssertTrue('a missing CA bundle should be raised', Raised);
end;

{ The OpenSSL half of the Windows system-store path, which is the same code
  on every platform: certificates handed over as DER become trust anchors of
  the connection, and anything OpenSSL cannot parse is skipped. }
procedure TTestUtility.TestDeprecatedSetApiKeyChecksTheKey;
var
    Raised: Boolean;
begin
    { The deprecated setter gets the same check as ZBInitialize: a key with
      surrounding whitespace or control characters is refused, and the key
      already set stays. }
    ZBInitialize('good-key');
    Raised := False;
    try
        {$WARN SYMBOL_DEPRECATED OFF}
        ZBSetApiKey(' dirty'#10);
        {$WARN SYMBOL_DEPRECATED ON}
    except on e: ZbException do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('api_key', e.Message) > 0);
        end;
    end;
    AssertTrue('ZBSetApiKey should refuse a dirty key', Raised);
    AssertEquals('the previous key is kept', 'good-key', ZbApiKey);

    {$WARN SYMBOL_DEPRECATED OFF}
    ZBSetApiKey('clean-key');
    {$WARN SYMBOL_DEPRECATED ON}
    AssertEquals('clean-key', ZbApiKey);
end;

procedure TTestUtility.TestTrustDerCertificatesAddsParsableCertificates;
const
  BUNDLE = '/etc/ssl/certs/ca-certificates.crt';
  BEGIN_MARK = '-----BEGIN CERTIFICATE-----';
  END_MARK = '-----END CERTIFICATE-----';
var
  Pem: TStringList;
  Text, Body: String;
  Ders: TZbDerCertificates;
  StartAt, EndAt, Found: Integer;
  Ctx: TSSLContext;
  Conn: TSSL;
begin
  {$IFNDEF MSWINDOWS}
  AssertEquals('no system store off Windows', 0, Length(ZbWindowsRootCertificates));
  {$ENDIF}
  if not FileExists(BUNDLE) then
    Ignore('no system CA bundle at ' + BUNDLE);
  Pem := TStringList.Create;
  try
    Pem.LoadFromFile(BUNDLE);
    Text := Pem.Text;
  finally
    Pem.Free;
  end;
  Ders := nil;
  Found := 0;
  StartAt := Pos(BEGIN_MARK, Text);
  while (StartAt > 0) and (Found < 2) do
  begin
    EndAt := Pos(END_MARK, Copy(Text, StartAt, MaxInt));
    AssertTrue('unterminated PEM block', EndAt > 0);
    Body := Copy(Text, StartAt + Length(BEGIN_MARK), EndAt - Length(BEGIN_MARK) - 1);
    Body := StringReplace(StringReplace(Body, #13, '', [rfReplaceAll]), #10, '', [rfReplaceAll]);
    SetLength(Ders, Length(Ders) + 1);
    Ders[High(Ders)] := DecodeStringBase64(Body);
    Inc(Found);
    Text := Copy(Text, StartAt + EndAt, MaxInt);
    StartAt := Pos(BEGIN_MARK, Text);
  end;
  AssertEquals('two certificates read from ' + BUNDLE, 2, Found);
  SetLength(Ders, Length(Ders) + 2);
  Ders[High(Ders) - 1] := 'not a certificate';
  Ders[High(Ders)] := '';

  AssertTrue('OpenSSL loads', InitSSLInterface);
  Ctx := TSSLContext.Create(stAny);
  try
    Conn := TSSL.Create(Ctx);
    try
      AssertEquals('nothing to add', 0, ZbTrustDerCertificates(Conn.SSL, nil));
      AssertEquals('the two parsable certificates', 2, ZbTrustDerCertificates(Conn.SSL, Ders));
    finally
      Conn.Free;
    end;
  finally
    Ctx.Free;
  end;
end;

procedure TTestUtility.TestAKeyWithAControlCharacterInsideIsRefused;
const
  BAD: array[0..4] of String = ('ab'#10'cd', 'ab'#9'cd', 'ab'#0'cd', 'ab'#27'cd', 'ab'#127'cd');
var
  I: Integer;
  Raised: Boolean;
begin
  { Trim only looks at the ends; a control character inside the key would go
    out on the wire (and into a header for the Warmup key) unchecked. }
  for I := Low(BAD) to High(BAD) do
  begin
    ZBInitialize('good-key');
    Raised := False;
    try
      ZBInitialize(BAD[I]);
    except on E: ZbException do
      begin
        Raised := True;
        AssertTrue(E.Message, Pos('control characters', E.Message) > 0);
      end;
    end;
    AssertTrue(Format('a key with #%d inside should be refused', [Ord(BAD[I][3])]), Raised);
    AssertEquals('the previous key is kept', 'good-key', ZbApiKey);
  end;
end;

procedure TTestUtility.TestRedactSecretsHidesTheKeyRawAndEncoded;
const
  KEY = 'k3y+with/chars';
begin
  ZBInitialize(KEY);
  AssertEquals('the raw key', 'failed: REDACTED.', RedactSecrets('failed: ' + KEY + '.'));
  { On the query string the key travels url-encoded. }
  AssertTrue('the test key must change when encoded', EncodeParam(KEY) <> KEY);
  AssertEquals('the url-encoded key', 'GET /v2/getcredits?api_key=REDACTED failed',
    RedactSecrets('GET /v2/getcredits?api_key=' + EncodeParam(KEY) + ' failed'));
end;

procedure TTestUtility.TestAnApiErrorMessageNeverCarriesTheKey;
const
  KEY = 'secret-key-123';
var
  Raised: Boolean;
begin
  ZBInitialize(KEY);
  ZBMockResponse(400, '{"error":"api_key ' + KEY + ' is not valid"}');
  Raised := False;
  try
    ZbGetCredits;
  except on E: ZbException do
    begin
      Raised := True;
      AssertEquals(400, E.StatusCode);
      AssertTrue('the key leaked: ' + E.Message, Pos(KEY, E.Message) = 0);
      AssertTrue(E.Message, Pos('REDACTED', E.Message) > 0);
    end;
  end;
  AssertTrue('an HTTP 400 should raise', Raised);
end;

procedure TTestUtility.TestInitializeIsAllOrNothing;
const
  OLD_URL = 'https://api.zerobounce.net/v2';

  procedure ExpectRefused(const Key, Url: String; UseRegion: Boolean);
  var
    Raised: Boolean;
  begin
    ZBInitialize('old-key', OLD_URL);
    Raised := False;
    try
      if UseRegion then
        ZBInitialize(Key, ZbApiURLEU)
      else
        ZBInitialize(Key, Url);
    except on E: ZbException do
      Raised := True;
    end;
    AssertTrue('ZBInitialize(' + Url + ') should raise', Raised);
    { A caller that catches the error must not go on with half of it. }
    AssertEquals('the key after a refused ZBInitialize', 'old-key', ZbApiKey);
    AssertEquals('the base URL after a refused ZBInitialize', OLD_URL, ZbApiBaseURL);
  end;

begin
  { A good key with a bad URL, and a bad key with a good URL. }
  ExpectRefused('new-key', 'http://api.zerobounce.net/v2', False);
  ExpectRefused('bad'#10'key', 'https://api-eu.zerobounce.net/v2', False);
  ExpectRefused(' bad-key', 'https://api-eu.zerobounce.net/v2', False);
  ExpectRefused('bad'#10'key', '(region EU)', True);
end;

procedure TTestUtility.TestSuccessFalseAsAStringIsAFailure;
var
  Raised: Boolean;
begin
  AssertTrue(ZbSuccessMeansFailure('{"success":"false","message":"x"}'));
  AssertTrue(ZbSuccessMeansFailure('{"success": false}'));
  AssertFalse(ZbSuccessMeansFailure('{"success":"true"}'));
  { End to end: a string "false" on an HTTP 200 is an error with the status. }
  ZBMockResponse(200, '{"success":"false","message":"File cannot be found."}');
  Raised := False;
  try
    ZbGetCredits;
  except on E: ZbException do
    begin
      Raised := True;
      AssertEquals(200, E.StatusCode);
      AssertTrue(E.Message, Pos('File cannot be found', E.Message) > 0);
    end;
  end;
  AssertTrue('"success": "false" should raise', Raised);
end;

initialization
  RegisterTest(TTestUtility);
end.
