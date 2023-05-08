program ExampleValidation;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ZbValidation,
  ZbStructures,
  ZbUtility;

var
    response: TZbValidationResult;
begin
    try
        ZBSetApiKey('YOUR__API__KEY');
        response := ZbValidateEmail('possible_trap@example.com', '99.110.204.1');

        WriteLn('Validation status: ', response.Status);
        WriteLn('Validation sub status: ', response.SubStatus);
    except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
    end;
    ReadLn;
end.


