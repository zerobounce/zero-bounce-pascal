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
        { -1 omit, 0 false, 1 true. Fills DomainWebsiteExists and
          DomainRegistrantCompanyName on the result. }
        DomainInfo: Integer;
        { -1 omit, 0 false, 1 true. Retries a greylisted address in the background. }
        GreylistProcessing: Integer;
        { Where the greylist retry posts its result. Empty to omit. It needs
          GreylistProcessing = 1 and is rejected without it, and when blank,
          before any request is sent. }
        CallbackUrl: String;
        UsePost: Integer;
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
        Result.DomainInfo := -1;
        Result.GreylistProcessing := -1;
        Result.CallbackUrl := '';
        Result.UsePost := -1;
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

            if EmailAndIp.Ip = '' then
                BatchContent := BatchContent + format(
                    '{"email_address": "%s", "ip_address": null}',
                    [ZbJsonEscape(EmailAndIp.Email)]
                )
            else
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
        FormBody: string;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(ZbApiBaseURL, ENDPOINT_VALIDATE);
        UrlToAccess := Concat(UrlToAccess, '?api_key=', EncodeParam(ZbApiKey));
        UrlToAccess := Concat(UrlToAccess, '&email=', EncodeParam(Email));
        UrlToAccess := Concat(UrlToAccess, '&ip_address=', EncodeParam(IpAddress));
        if (Options.Timeout >= 0) and ((Options.Timeout < 3) or (Options.Timeout > 60)) then
            raise ZbException.Create('Invalid parameter: timeout (must be between 3 and 60 seconds)');
        if Options.CallbackUrl <> '' then
        begin
            if Trim(Options.CallbackUrl) = '' then
                raise ZbException.Create('Empty parameter: callback_url');
            { The callback is where the greylist retry posts its result, so it
              is meaningless - and rejected by the API - without the retry. }
            if Options.GreylistProcessing <> 1 then
                raise ZbException.Create(
                    'Invalid parameter: callback_url requires greylist_processing to be true'
                );
        end;
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
        if Options.DomainInfo >= 0 then
            if Options.DomainInfo = 0 then
                UrlToAccess := Concat(UrlToAccess, '&domain_info=false')
            else
                UrlToAccess := Concat(UrlToAccess, '&domain_info=true');
        if Options.GreylistProcessing >= 0 then
            if Options.GreylistProcessing = 0 then
                UrlToAccess := Concat(UrlToAccess, '&greylist_processing=false')
            else
                UrlToAccess := Concat(UrlToAccess, '&greylist_processing=true');
        if Options.CallbackUrl <> '' then
            UrlToAccess := Concat(UrlToAccess, '&callback_url=', EncodeParam(Options.CallbackUrl));
        if Options.UsePost = 1 then
        begin
            FormBody := Concat(
                'api_key=', EncodeParam(ZbApiKey),
                '&email=', EncodeParam(Email),
                '&ip_address=', EncodeParam(IpAddress)
            );
            if Options.Timeout >= 0 then
                FormBody := Concat(FormBody, '&timeout=', EncodeParam(IntToStr(Options.Timeout)));
            if Options.ActivityData >= 0 then
                if Options.ActivityData = 0 then
                    FormBody := Concat(FormBody, '&activity_data=false')
                else
                    FormBody := Concat(FormBody, '&activity_data=true');
            if Options.VerifyPlus >= 0 then
                if Options.VerifyPlus = 0 then
                    FormBody := Concat(FormBody, '&verify_plus=false')
                else
                    FormBody := Concat(FormBody, '&verify_plus=true');
            if Options.DomainInfo >= 0 then
                if Options.DomainInfo = 0 then
                    FormBody := Concat(FormBody, '&domain_info=false')
                else
                    FormBody := Concat(FormBody, '&domain_info=true');
            if Options.GreylistProcessing >= 0 then
                if Options.GreylistProcessing = 0 then
                    FormBody := Concat(FormBody, '&greylist_processing=false')
                else
                    FormBody := Concat(FormBody, '&greylist_processing=true');
            if Options.CallbackUrl <> '' then
                FormBody := Concat(FormBody, '&callback_url=', EncodeParam(Options.CallbackUrl));
            response := ZBFormPostRequest(Concat(ZbApiBaseURL, ENDPOINT_VALIDATE), FormBody);
        end
        else
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
        errorIndex: Integer;
    begin
        if (Options.Timeout >= 0) and ((Options.Timeout < 10) or (Options.Timeout > 120)) then
            raise ZbException.Create('Invalid parameter: timeout (must be between 10 and 120 seconds)');
        UrlToAccess := Concat(ZbApiBaseURL, ENDPOINT_BATCH_VALIDATE);
        JsonBody := ZbBatchRequestBodyFromEmails(Emails, Options);
        response := ZBPostRequest(UrlToAccess, JsonBody);

        try
            Result := ZbBatchValidationFromJson(response.Payload);
            if Length(Result.Errors) > 0 then
                for errorIndex := 0 to High(Result.Errors) do
                    if Result.Errors[errorIndex].EmailAddress = 'all' then
                        raise ZbException.Create(Result.Errors[errorIndex].Error);
        except
            on e: ZbException do
                raise;
            on e: Exception do
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

