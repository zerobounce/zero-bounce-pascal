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

    TZbValidateOptions = record
        Timeout: Integer;
        ActivityData: Integer;
        VerifyPlus: Integer;
    end;

procedure Register;
function ZbDefaultValidateOptions: TZbValidateOptions;
function ZbBatchRequestBodyFromEmails(Emails: array of TZbEmailAndIp): String; overload;
function ZbBatchRequestBodyFromEmails(
    Emails: array of TZbEmailAndIp;
    Options: TZbValidateOptions
): String; overload;

function ZbValidateEmail(Email: String; IpAddress: String): TZbValidationResult; overload;
function ZbValidateEmail(Email: String): TZbValidationResult; overload;
function ZbValidateEmail(
    Email: String;
    IpAddress: String;
    Options: TZbValidateOptions
): TZbValidationResult; overload;
function ZbBatchValidateEmails(Emails: array of TZbEmailAndIp): TZBBatchValidation; overload;
function ZbBatchValidateEmails(Emails: array of String): TZBBatchValidation; overload;
function ZbBatchValidateEmails(
    Emails: array of TZbEmailAndIp;
    Options: TZbValidateOptions
): TZBBatchValidation; overload;


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

    function ZbDefaultValidateOptions: TZbValidateOptions;
    begin
        Result.Timeout := -1;
        Result.ActivityData := -1;
        Result.VerifyPlus := -1;
    end;

    function ZbBatchRequestBodyFromEmails(Emails: array of TZbEmailAndIp): String;
    begin
        Result := ZbBatchRequestBodyFromEmails(Emails, ZbDefaultValidateOptions);
    end;

    function ZbBatchRequestBodyFromEmails(
        Emails: array of TZbEmailAndIp;
        Options: TZbValidateOptions
    ): String;
    var
        EmailAndIp: TZbEmailAndIp;
        BatchContent: String;
        FirstElement: Boolean;
        Extras: String;
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
        Extras := '';
        if Options.Timeout >= 0 then
            Extras := Extras + format(', "timeout": %d', [Options.Timeout]);
        if Options.ActivityData >= 0 then
            if Options.ActivityData = 0 then
                Extras := Extras + ', "activity_data": false'
            else
                Extras := Extras + ', "activity_data": true';
        if Options.VerifyPlus >= 0 then
            if Options.VerifyPlus = 0 then
                Extras := Extras + ', "verify_plus": false'
            else
                Extras := Extras + ', "verify_plus": true';
        Result := format(
            '{"api_key": "%s", "email_batch": [%s]%s}',
            [ZbJsonEscape(ZbApiKey), BatchContent, Extras]
        )
    end;

    function ZbValidateEmail(Email: String; IpAddress: String): TZbValidationResult;
    begin
        Result := ZbValidateEmail(Email, IpAddress, ZbDefaultValidateOptions);
    end;

    function ZbValidateEmail(
        Email: String;
        IpAddress: String;
        Options: TZbValidateOptions
    ): TZbValidationResult;
    var
        UrlToAccess: string;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(ZbApiBaseURL, ENDPOINT_VALIDATE);
        UrlToAccess := Concat(UrlToAccess, '?api_key=', EncodeParam(ZbApiKey));
        UrlToAccess := Concat(UrlToAccess, '&email=', EncodeParam(Email));
        UrlToAccess := Concat(UrlToAccess, '&ip_address=', EncodeParam(IpAddress));
        if (Options.Timeout >= 0) and ((Options.Timeout < 3) or (Options.Timeout > 60)) then
            raise ZbException.Create('Invalid parameter: timeout (must be between 3 and 60 seconds)');
        if Options.Timeout >= 0 then
            UrlToAccess := Concat(UrlToAccess, '&timeout=', EncodeParam(IntToStr(Options.Timeout)));
        if Options.ActivityData >= 0 then
            if Options.ActivityData = 0 then
                UrlToAccess := Concat(UrlToAccess, '&activity_data=false')
            else
                UrlToAccess := Concat(UrlToAccess, '&activity_data=true');
        if Options.VerifyPlus >= 0 then
            if Options.VerifyPlus = 0 then
                UrlToAccess := Concat(UrlToAccess, '&verify_plus=false')
            else
                UrlToAccess := Concat(UrlToAccess, '&verify_plus=true');
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
    begin
        Result := ZbBatchValidateEmails(Emails, ZbDefaultValidateOptions);
    end;

    function ZbBatchValidateEmails(
        Emails: array of TZbEmailAndIp;
        Options: TZbValidateOptions
    ): TZBBatchValidation;
    var
        UrlToAccess: String;
        JsonBody: String;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        if (Options.Timeout >= 0) and ((Options.Timeout < 10) or (Options.Timeout > 120)) then
            raise ZbException.Create('Invalid parameter: timeout (must be between 10 and 120 seconds)');
        UrlToAccess := Concat(ZbApiBaseURL, ENDPOINT_BATCH_VALIDATE);
        JsonBody := ZbBatchRequestBodyFromEmails(Emails, Options);
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

