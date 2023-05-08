program ExampleGetCredits;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ZbGeneric,
  ZbUtility;


var
   Amount: Integer;
begin
    ZBSetApiKey('YOUR__API__KEY');
    Amount := ZbGetCredits;
    WriteLn('Credits left: ', Amount);
    ReadLn;
end.

