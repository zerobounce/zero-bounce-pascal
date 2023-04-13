unit TestCredits;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    // ZeroBounce was installed in Lazarus and imported via Package > Package links
    ZbGeneric, ZbUtility, ZbMock;

type

    TTestCredits= class(TTestCase)
    published
             procedure TestHookUp;
    end;

implementation

procedure TTestCredits.TestHookUp;
var
   CreditsAmount: integer;
begin
     ZbSetApiKey('mock api key');
     //TMockClient.ExpectResponse(200, '{"Credits":"50000"}');
     CreditsAmount := ZBGetCredits;
     WriteLn('Credits: ', CreditsAmount);
     AssertEquals('Credits with invalid key', CreditsAmount, -1);
end;



initialization
   RegisterTest(TTestCredits);
end.

