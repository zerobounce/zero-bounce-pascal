program unit_tests_console;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, consoletestrunner,
  TestGeneric, TestValidation, TestDeserialization, TestBulk;

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
