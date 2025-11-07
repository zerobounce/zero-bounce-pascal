program ExampleActivityData;

uses
    SysUtils,
    ZbGeneric, // ZbActivityData method
    ZbUtility; // ZBInitialize method


const
    VALID_EMAIL = 'valid@example.com';
    INVALID_EMAIL = 'not_known@example.com';
var
   Amount: Integer;
begin
    try
        ZBInitialize('YOUR__API__KEY');

        Amount := ZbActivityData(VALID_EMAIL);
        WriteLn('Activity data of the valid email: ', Amount);

        Amount := ZbActivityData(INVALID_EMAIL);
        WriteLn('Activity data of the invalid email: ', Amount);
    except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
    end;
end.

