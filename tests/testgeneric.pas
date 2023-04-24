unit TestGeneric;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    // ZeroBounce was installed in Lazarus and imported via Package > Package links
    ZbGeneric, ZbUtility, ZbMock;

type

        TTestGeneric= class(TTestCase)
        published
                procedure TestCredits;
        end;

implementation

procedure TTestGeneric.TestCredits;
var
   CreditsAmount: integer;
begin
     TMockClient.ExpectResponse(200, '{"Credits":"50000"}');
     CreditsAmount := ZBGetCredits;
     WriteLn('Credits: ', CreditsAmount);
     AssertEquals('Credits with invalid key', CreditsAmount, -1);
end;



initialization
        RegisterTest(TTestGeneric);
end.

