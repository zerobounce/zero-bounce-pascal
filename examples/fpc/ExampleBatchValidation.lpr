program ExampleBatchValidation;

uses
    ZbValidation, // ZbBatchValidateEmails method, TZbEmailAndIp record
    ZbStructures, // TZbValidationResult record
    ZbUtility; // ZBSetApiKey method

const
   param: array of TZbEmailAndIp = (
        ( Email: 'invalid@example.com'; Ip: '99.110.204.1' ),
        ( Email: 'donotmail@example.com' )
    );
var
    response: TZbBatchValidation;
    Validation: TZbValidationResult;
begin
    ZBSetApiKey('YOUR__API__KEY');

    response := ZbBatchValidateEmails(param);
    for Validation in response.EmailBatch do
    begin
        WriteLn('Email ', Validation.Address, ' has status ', Validation.Status);
    end;
end.

