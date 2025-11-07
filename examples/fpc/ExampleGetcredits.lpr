program ExampleGetCredits;

uses
    SysUtils,
    ZbGeneric, // ZbGetCredits method
    ZbUtility; // ZBInitialize method

var
   Amount: Integer;
begin
    try
        ZBInitialize('YOUR__API__KEY');
        Amount := ZbGetCredits;
        WriteLn('Credits left: ', Amount);
    except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
    end;
end.
