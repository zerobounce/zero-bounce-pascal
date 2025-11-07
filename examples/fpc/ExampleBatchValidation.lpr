program ExampleBatchValidation;

uses
    SysUtils,
    ZbValidation, // ZbBatchValidateEmails method, TZbEmailAndIp record
    ZbStructures, // TZbValidationResult record
    ZbUtility; // ZBInitialize method

const
   param: array of TZbEmailAndIp = (
        ( Email: 'invalid@example.com'; Ip: '99.110.204.1' ),
        ( Email: 'donotmail@example.com'; Ip: '' )
    );
var
    response: TZbBatchValidation;
    Validation: TZbValidationResult;
begin
    try
        ZBInitialize('YOUR__API__KEY');

        response := ZbBatchValidateEmails(param);
        for Validation in response.EmailBatch do
        begin
            WriteLn('Email ', Validation.Address, ' has status ', Validation.Status);
        end;
    except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
    end;
end.

