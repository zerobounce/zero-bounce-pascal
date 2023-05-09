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
    try
        ZBSetApiKey('YOUR__API__KEY');
        Amount := ZbGetCredits;
        WriteLn('Credits left: ', Amount);
    except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
    end;
    ReadLn;
end.

