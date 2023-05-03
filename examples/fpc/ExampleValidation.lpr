program ExampleValidation;

uses
    ZbValidation, // ZbValidateEmail method
    ZbStructures, // TZbValidationResult record
    ZbUtility; // ZBSetApiKey method


var
    response: TZbValidationResult;
begin
    ZBSetApiKey('YOUR__API__KEY');
    response := ZbValidateEmail('possible_trap@example.com', '99.110.204.1');

    WriteLn('Validation status: ', response.Status);
    WriteLn('Validation sub status: ', response.SubStatus);
end.

