unit ZbValidation;

{$I zboptions.inc}{$H+}

interface


uses
    Classes, SysUtils, DateUtils,
    ZbStructures, ZbUtility;

type
    TZbEmailAndIp = record
        Email: String;
        Ip: String;
    end;

procedure Register;
function ZbBatchRequestBodyFromEmails(Emails: array of TZbEmailAndIp): String;

function ZbValidateEmail(Email: String; IpAddress: String): TZbValidationResult; overload;
function ZbValidateEmail(Email: String): TZbValidationResult; overload;
function ZbBatchValidateEmails(Emails: array of TZbEmailAndIp): TZBBatchValidation; overload;
function ZbBatchValidateEmails(Emails: array of String): TZBBatchValidation; overload;


implementation

    procedure Register;
    begin
    end;

    { Escapes a value for use inside a JSON string literal. }
    function ZbJsonEscape(const Value: String): String;
    var
        I: Integer;
        C: Char;
    begin
        Result := '';
        for I := 1 to Length(Value) do
        begin
            C := Value[I];
            case C of
                '"': Result := Result + '\"';
                '\': Result := Result + '\\';
                #0..#31: Result := Result + '\u' + IntToHex(Ord(C), 4);
            else
                Result := Result + C;
            end;
        end;
    end;

    function ZbBatchRequestBodyFromEmails(Emails: array of TZbEmailAndIp): String;
    var
        EmailAndIp: TZbEmailAndIp;
        BatchContent: String;
        FirstElement: Boolean;
    begin
        BatchContent := '';
        FirstElement := True;
        for EmailAndIp in Emails do
        begin
            if FirstElement then
                FirstElement := False
            else
                BatchContent := BatchContent + ',';

            BatchContent := BatchContent + format(
                '{"email_address": "%s", "ip_address": "%s"}',
                [ZbJsonEscape(EmailAndIp.Email), ZbJsonEscape(EmailAndIp.Ip)]
            );
        end;
        Result := format(
            '{"api_key": "%s", "email_batch": [%s]}', [ZbJsonEscape(ZbApiKey), BatchContent]
        )
    end;

    function ZbValidateEmail(Email: String; IpAddress: String): TZbValidationResult;
    var
        UrlToAccess: string;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(ZbApiBaseURL, ENDPOINT_VALIDATE);
        UrlToAccess := Concat(UrlToAccess, '?api_key=', EncodeParam(ZbApiKey));
        UrlToAccess := Concat(UrlToAccess, '&email=', EncodeParam(Email));
        UrlToAccess := Concat(UrlToAccess, '&ip_address=', EncodeParam(IpAddress));
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
        UrlToAccess: String;
        JsonBody: String;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(ZbApiBaseURL, ENDPOINT_BATCH_VALIDATE);
        JsonBody := ZbBatchRequestBodyFromEmails(Emails);
        response := ZBPostRequest(UrlToAccess, JsonBody);

        try
            Result := ZbBatchValidationFromJson(response.Payload);
        except on e: Exception do
            begin
                error := ZbException.FromResponse(e.Message, response);
               error.MarkJsonError;
               raise error;
            end;
        end;
    end;

    function ZbBatchValidateEmails(Emails: array of String): TZBBatchValidation;
    var
        EmailsAndIps: array of TZbEmailAndIp;
        Index: Integer;
    begin
        EmailsAndIps := [];
        SetLength(EmailsAndIps, Length(Emails));
        for Index := 0 to Length(Emails) - 1 do
            EmailsAndIps[Index].Email := Emails[Index];

        Result := ZbBatchValidateEmails(EmailsAndIps);
    end;

end.

