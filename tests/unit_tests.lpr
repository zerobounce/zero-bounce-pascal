program unit_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestGeneric, TestValidation, TestDeserialization;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

