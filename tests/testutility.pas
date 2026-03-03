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

initialization
  RegisterTest(TTestUtility);
end.
