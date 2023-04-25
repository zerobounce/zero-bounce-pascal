unit ZbValidation;

{$mode ObjFPC}{$H+}

interface


uses
    Classes, SysUtils, DateUtils, fpjson,

    ZbStructures, ZbUtility;

type
    TZbEmailAndIp = record
        Email: String;
        Ip: String;
    end;

function ZbValidateEmail(Email: String): TZbValidationResult;
function ZbBatchValidateEmails(Emails: array of String): TZBBatchValidation;
function ZbBatchValidateEmails(Emails: array of TZbEmailAndIp): TZBBatchValidation;
procedure Register;

implementation

    procedure Register;
    begin
    end;

    function ZbValidateEmail(Email: String): TZbValidationResult;
    var
        UrlToAccess: string;
        response: TZbRequestResponse;
        JObject: TJSONObject;
        error: ZbException;
    begin
    end;

    function ZbBatchValidateEmails(Emails: array of String): TZBBatchValidation;
    begin
    end;

    function ZbBatchValidateEmails(Emails: array of TZbEmailAndIp): TZBBatchValidation;
    var
        UrlToAccess: string;
        response: TZbRequestResponse;
        JObject: TJSONObject;
        error: ZbException;
    begin
    end;


end.

