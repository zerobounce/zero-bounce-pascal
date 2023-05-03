program ExampleGetCredits;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  zbgeneric in '..\..\src\zbgeneric.pas',
  zbstructures in '..\..\src\zbstructures.pas',
  zbutility in '..\..\src\zbutility.pas';

// ZBSetApiKey method

var
   Amount: Integer;
begin
    ZBSetApiKey('YOUR__API__KEY');
    Amount := ZbGetCredits;
    WriteLn('Credits left: ', Amount);
    ReadLn;
end.

