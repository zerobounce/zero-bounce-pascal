program ExampleBatchValidation;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ZbValidation,
  ZbStructures,
  ZbUtility;

var
    param: array[0..1] of TZbEmailAndIp;
    response: TZbBatchValidation;
    Validation: TZbValidationResult;
begin
    param[0].Email := 'invalid@example.com';
    param[0].Ip := '99.110.204.1';
    param[1].Email := 'donotmail@example.com';
    param[1].Ip := '';

    try
        ZBSetApiKey('YOUR__API__KEY');

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

