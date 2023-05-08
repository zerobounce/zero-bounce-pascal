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

function ZbValidateEmail(Email: String; IpAddress: String): TZbValidationResult; {$IFNDEF FPC} overload; {$ENDIF}
function ZbValidateEmail(Email: String): TZbValidationResult; {$IFNDEF FPC} overload; {$ENDIF}
function ZbBatchValidateEmails(Emails: array of TZbEmailAndIp): TZBBatchValidation; {$IFNDEF FPC} overload; {$ENDIF}
function ZbBatchValidateEmails(Emails: array of String): TZBBatchValidation; {$IFNDEF FPC} overload; {$ENDIF}


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
        UrlToAccess := Concat(BASE_URI, ENDPOINT_VALIDATE);
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
        UrlToAccess: String;
        JsonBody: String;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(BASE_URI, ENDPOINT_BATCH_VALIDATE);
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

