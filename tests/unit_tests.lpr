program unit_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestGeneric, TestValidation,
  TestDeserialization, TestBulk;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

