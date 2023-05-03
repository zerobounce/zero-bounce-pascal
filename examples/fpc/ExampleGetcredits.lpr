program ExampleGetCredits;

uses
    ZbGeneric, // ZbGetCredits method
    ZbUtility; // ZBSetApiKey method

var
   Amount: Integer;
begin
    ZBSetApiKey('YOUR__API__KEY');
    Amount := ZbGetCredits;
    WriteLn('Credits left: ', Amount);
end.

