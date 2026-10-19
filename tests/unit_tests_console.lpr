program unit_tests_console;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Classes, SysUtils, consoletestrunner,
  TestGeneric, TestValidation, TestDeserialization, TestBulk, TestPriority3, TestUtility, TestOffline;

var
  App: TTestRunner;

begin
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'ZeroBounce Pascal SDK tests';
    App.Run;
  finally
    App.Free;
  end;
end.
