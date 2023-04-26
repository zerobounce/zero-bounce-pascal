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
              
function ZbBatchRequestBodyFromEmails(Emails: array of TZbEmailAndIp): String;

function ZbValidateEmail(Email: String; IpAddress: String): TZbValidationResult;
function ZbValidateEmail(Email: String): TZbValidationResult;
function ZbBatchValidateEmails(Emails: array of TZbEmailAndIp): TZBBatchValidation;
function ZbBatchValidateEmails(Emails: array of String): TZBBatchValidation;

procedure Register;

implementation

    procedure Register;
    begin
    end;

    function ZbBatchRequestBodyFromEmails(Emails: array of TZbEmailAndIp): String;
    var
        EmailAndIp: TZbEmailAndIp;
        BatchContent: String;
        FirstElement: Boolean;
    begin
        FirstElement := True;
        for EmailAndIp in Emails do
        begin
            if FirstElement then
                FirstElement := False
            else
                BatchContent += ',';
            BatchContent += format(
                '{"email_address": "%s", "ip_address": "%s"}',
                [EmailAndIp.Email, EmailAndIp.Ip]
            );
        end;
        Result := format(
            '{"api_key": "%s", "email_batch": [%s]}', [ZbApiKey, BatchContent]
        )
    end;

    function ZbValidateEmail(Email: String; IpAddress: String): TZbValidationResult;
    var
        UrlToAccess: string;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(BASE_URI, ENDPOINT_CREDITS);
        UrlToAccess := Concat(UrlToAccess, '?api_key=', ZbApiKey);
        UrlToAccess := Concat(UrlToAccess, '&email=', Email);
        UrlToAccess := Concat(UrlToAccess, '&ip_address=', IpAddress);
        response := ZBGetRequest(UrlToAccess);

        try
            Result := ZbValidationFromJson(response.Payload);
        except on e: Exception do
            begin
               error := ZbException.FromResponse(e.Message, response);
               error.MarkJsonError;
               raise error;
			end;
		end;
    end;

    function ZbValidateEmail(Email: String): TZbValidationResult;
    begin
        Result := ZbValidateEmail(Email, '');
    end;

    function ZbBatchValidateEmails(Emails: array of TZbEmailAndIp): TZBBatchValidation;
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


end.

