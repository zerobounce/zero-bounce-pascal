unit TestValidation;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, fpcunit, testutils, testregistry,
        ZbUtility;

type

        TTestValidation= class(TTestCase)
        protected
                procedure TearDown; override;
        published
                procedure TestHookUp;
        end;

implementation

procedure TTestValidation.TestHookUp;
begin
     // TODO: update
end;


procedure TTestValidation.TearDown;
begin
   if ZbResponseMock.Headers <> nil then
      ZbResponseMock.Headers.Free;
   ZbResponseMock := cDefaultMock;
end;

initialization

        RegisterTest(TTestValidation);
end.

