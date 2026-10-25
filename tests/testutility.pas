unit TestUtility;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  BaseTest, ZbUtility;

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

initialization
  RegisterTest(TTestUtility);
end.
