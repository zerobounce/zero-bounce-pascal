program ExampleActivityData;

uses
    ZbGeneric, // ZbActivityData method
    ZbUtility; // ZBSetApiKey method


const
    VALID_EMAIL = 'valid@example.com';
    INVALID_EMAIL = 'not_known@example.com';
var
   Amount: Integer;
begin
    ZBSetApiKey('YOUR__API__KEY');

    Amount := ZbActivityData(VALID_EMAIL);
    WriteLn('Activity data of the valid email: ', Amount);

    Amount := ZbActivityData(INVALID_EMAIL);
    WriteLn('Activity data of the invalid email: ', Amount);
end.

