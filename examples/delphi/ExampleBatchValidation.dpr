program ExampleBatchValidation;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  zbvalidation in '..\..\src\zbvalidation.pas',
  zbstructures in '..\..\src\zbstructures.pas',
  zbutility in '..\..\src\zbutility.pas';

var
    param: array[0..1] of TZbEmailAndIp;
    response: TZbBatchValidation;
    Validation: TZbValidationResult;
begin
    param[0].Email := 'invalid@example.com';
    param[0].Ip := '99.110.204.1';
    param[1].Email := 'donotmail@example.com';
    param[1].Ip := '';

    ZBSetApiKey('YOUR__API__KEY');

    response := ZbBatchValidateEmails(param);
    for Validation in response.EmailBatch do
    begin
        WriteLn('Email ', Validation.Address, ' has status ', Validation.Status);
    end;
    ReadLn;
end.

