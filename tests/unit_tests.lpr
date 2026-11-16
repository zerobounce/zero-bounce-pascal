program unit_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestGeneric, TestValidation,
  TestDeserialization, TestBulk, TestPriority3, TestUtility, TestOffline,
  TestBulkJobs, TestWarmup, TestBehaviour;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

