program ExampleValidation;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  zbvalidation in '..\..\src\zbvalidation.pas',
  zbstructures in '..\..\src\zbstructures.pas',
  zbutility in '..\..\src\zbutility.pas';

var
    response: TZbValidationResult;
begin
    ZBSetApiKey('YOUR__API__KEY');
    response := ZbValidateEmail('possible_trap@example.com', '99.110.204.1');

    WriteLn('Validation status: ', response.Status);
    WriteLn('Validation sub status: ', response.SubStatus);
    ReadLn;
end.


