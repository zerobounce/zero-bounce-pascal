unit ZbStructures;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, StrUtils, DateUtils, fpjson, jsonparser,
    ZbUtility;

type
    TApiUsage = Record
        Total: Integer;
        StatusValid: Integer;
        StatusInvalid: Integer;
        StatusCatchAll: Integer;
        StatusDoNotMail: Integer;
        StatusSpamtrap: Integer;
        StatusUnknown: Integer;
        SubStatusToxic: Integer;
        SubStatusDisposable: Integer;
        SubStatusRoleBased: Integer;
        SubStatusPossibleTrap: Integer;
        SubStatusGlobalSuppression: Integer;
        SubStatusTimeoutExceeded: Integer;
        SubStatusMailServerTemporaryError: Integer;
        SubStatusMailServerDidNotRespond: Integer;
        SubStatusGreylisted: Integer;
        SubStatusAntispamSystem: Integer;
        SubStatusDoesNotAcceptMail: Integer;
        SubStatusExceptionOccurred: Integer;
        SubStatusFailedSyntaxCheck: Integer;
        SubStatusMailboxNotFound: Integer;
        SubStatusUnroutableIpAddress: Integer;
        SubStatusPossibleTypo: Integer;
        SubStatusNoDnsEntries: Integer;
        SubStatusRoleBasedCatchAll: Integer;
        SubStatusMailboxQuotaExceeded: Integer;
        SubStatusForcibleDisconnect: Integer;
        SubStatusFailedSmtpConnection: Integer;
        SubStatusMxForward: Integer;
        SubStatusAlternate: Integer;
        SubStatusBlocked: Integer;
        SubStatusAllowed: Integer;
        StartDate: TDate;
        EndDate: TDate;
    end;

    TZbValidationResult = record
        Address: String;
        Status: String;
        SubStatus: String;
        FreeEmail: boolean;
        DidYouMean: String;
        Account: String;
        Domain: String;
        DomainAgeDays: String;
        SmtpProvider: String;
        MxRecord: String;
        MxFound: String;
        Firstname: String;
        Lastname: String;
        Gender: String;
        Country: String;
        Region: String;
        City: String;
        Zipcode: String;
        ProcessedAt: TDateTime;
    end;

    TZbBatchError = record
        Error: String;
        EmailAddress: String;
    end;

    TZbBatchValidation = record
        EmailBatchLength: Integer;
        EmailBatch: array of TZbValidationResult;
        ErrorsLength: Integer;
        Errors: array of TZbBatchError;
    end;

    TZbFileFeedback = record
        Success: Boolean;
        Message: String;
        FileName: String;
        FileId: String;
    end;

    TZbFileStatus = record
        Success: Boolean;
        FileId: String;
        FileName: String;
        FileStatus: String;
        ErrorReason: String;
        ReturnUrl: String;
        UploadDate: TDateTime;
        CompletePercentage: Double;
    end;

    TZbBulkResponse = record
        HasContent: Boolean;
        Feedback: TZbFileFeedback;
        Content: String;
    end;

function ZbApiUsageFromJson(JsonContent: string): TApiUsage;
function ZbValidationFromJson(JObject: TJSONObject): TZbValidationResult;
function ZbValidationFromJson(JsonContent: string): TZbValidationResult;
function ZbBatchErrorFromJson(JObject: TJSONObject): TZbBatchError;
function ZbBatchErrorFromJson(JsonContent: string): TZbBatchError;
function ZbBatchValidationFromJson(JsonContent: string): TZBBatchValidation;
function ZbFileFeedbackFromJson(JsonContent: string): TZbFileFeedback;
function ZbFileStatusFromJson(JsonContent: string): TZbFileStatus;
procedure Register;

implementation

    procedure Register;
    begin
    end;

    function StringOrNull(JObject: TJSONObject; JsonKey: String): String;
    var
        JData: TJSONData;
    begin
        Result := '';
        JData := JObject.Find(JsonKey);
        if (JData <> nil) and (not JData.IsNull) then
            Result := JData.AsString;
    end;

    function ZbApiUsageFromJson(JsonContent: string): TApiUsage;
    var
        JObject: TJSONObject;

        function ExtractDate(JsonKey: String): TDate;
        var
            Day, Month, Year: Integer;
        begin
            SScanf(JObject.Find(JsonKey).AsString, '%d/%d/%d', [@Month, @Day, @Year]);
            Result := EncodeDateTime(Year, Month, Day, 0, 0, 0, 0);
        end;

    begin
        JObject := TJSONObject(GetJSON(JsonContent));
        Result.Total :=                               JObject.Find('total').AsInteger;
        Result.StatusValid :=                         JObject.Find('status_valid').AsInteger;
        Result.StatusInvalid :=                       JObject.Find('status_invalid').AsInteger;
        Result.StatusCatchAll :=                      JObject.Find('status_catch_all').AsInteger;
        Result.StatusDoNotMail :=                     JObject.Find('status_do_not_mail').AsInteger;
        Result.StatusSpamtrap :=                      JObject.Find('status_spamtrap').AsInteger;
        Result.StatusUnknown :=                       JObject.Find('status_unknown').AsInteger;
        Result.SubStatusToxic :=                      JObject.Find('sub_status_toxic').AsInteger;
        Result.SubStatusDisposable :=                 JObject.Find('sub_status_disposable').AsInteger;
        Result.SubStatusRoleBased :=                  JObject.Find('sub_status_role_based').AsInteger;
        Result.SubStatusPossibleTrap :=               JObject.Find('sub_status_possible_trap').AsInteger;
        Result.SubStatusGlobalSuppression :=          JObject.Find('sub_status_global_suppression').AsInteger;
        Result.SubStatusTimeoutExceeded :=            JObject.Find('sub_status_timeout_exceeded').AsInteger;
        Result.SubStatusMailServerTemporaryError :=   JObject.Find('sub_status_mail_server_temporary_error').AsInteger;
        Result.SubStatusMailServerDidNotRespond :=    JObject.Find('sub_status_mail_server_did_not_respond').AsInteger;
        Result.SubStatusGreylisted :=                 JObject.Find('sub_status_greylisted').AsInteger;
        Result.SubStatusAntispamSystem :=             JObject.Find('sub_status_antispam_system').AsInteger;
        Result.SubStatusDoesNotAcceptMail :=          JObject.Find('sub_status_does_not_accept_mail').AsInteger;
        Result.SubStatusExceptionOccurred :=          JObject.Find('sub_status_exception_occurred').AsInteger;
        Result.SubStatusFailedSyntaxCheck :=          JObject.Find('sub_status_failed_syntax_check').AsInteger;
        Result.SubStatusMailboxNotFound :=            JObject.Find('sub_status_mailbox_not_found').AsInteger;
        Result.SubStatusUnroutableIpAddress :=        JObject.Find('sub_status_unroutable_ip_address').AsInteger;
        Result.SubStatusPossibleTypo :=               JObject.Find('sub_status_possible_typo').AsInteger;
        Result.SubStatusNoDnsEntries :=               JObject.Find('sub_status_no_dns_entries').AsInteger;
        Result.SubStatusRoleBasedCatchAll :=          JObject.Find('sub_status_role_based_catch_all').AsInteger;
        Result.SubStatusMailboxQuotaExceeded :=       JObject.Find('sub_status_mailbox_quota_exceeded').AsInteger;
        Result.SubStatusForcibleDisconnect :=         JObject.Find('sub_status_forcible_disconnect').AsInteger;
        Result.SubStatusFailedSmtpConnection :=       JObject.Find('sub_status_failed_smtp_connection').AsInteger;
        Result.SubStatusMxForward :=                  JObject.Find('sub_status_mx_forward').AsInteger;
        Result.SubStatusAlternate :=                  JObject.Find('sub_status_alternate').AsInteger;
        Result.SubStatusBlocked :=                    JObject.Find('sub_status_blocked').AsInteger;
        Result.SubStatusAllowed :=                    JObject.Find('sub_status_allowed').AsInteger;

        Result.StartDate := ExtractDate('start_date');
        Result.EndDate := ExtractDate('end_date');
    end;

    function ZbValidationFromJson(JsonContent: string): TZbValidationResult;
    begin
        Result := ZbValidationFromJson(TJSONObject(GetJSON(JsonContent)))
    end;

    function ZbValidationFromJson(JObject: TJSONObject): TZbValidationResult;

        function ExtractDateTime(JsonKey: String): TDateTime;
        var
            Day, Month, Year, Hour, Minute, Second: Integer;
            Milis: Word;
        begin
            // %Y-%m-%d %H:%M:%S.%3f
            SScanf(
                JObject.Find(JsonKey).AsString,
                '%d-%d-%d %d:%d:%d.%d',
                [@Year, @Month, @Day, @Hour, @Minute, @Second, @Milis]
            );
            Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Milis);
        end;
    begin
        Result.Address :=       JObject.Find('address').AsString;
        Result.Status :=        JObject.Find('status').AsString;
        Result.SubStatus :=     JObject.Find('sub_status').AsString;
        Result.FreeEmail :=     JObject.Find('free_email').AsBoolean;

        Result.DidYouMean :=    StringOrNull(JObject, 'did_you_mean');
        Result.Account :=       StringOrNull(JObject, 'account');
        Result.Domain :=        StringOrNull(JObject, 'domain');
        Result.DomainAgeDays := StringOrNull(JObject, 'domain_age_days');
        Result.SmtpProvider :=  StringOrNull(JObject, 'smtp_provider');
        Result.MxRecord :=      StringOrNull(JObject, 'mx_record');
        Result.MxFound :=       StringOrNull(JObject, 'mx_found');
        Result.Firstname :=     StringOrNull(JObject, 'firstname');
        Result.Lastname :=      StringOrNull(JObject, 'lastname');
        Result.Gender :=        StringOrNull(JObject, 'gender');
        Result.Country :=       StringOrNull(JObject, 'country');
        Result.Region :=        StringOrNull(JObject, 'region');
        Result.City :=          StringOrNull(JObject, 'city');
        Result.Zipcode :=       StringOrNull(JObject, 'zipcode');

        Result.ProcessedAt := ExtractDateTime('processed_at');
    end;


    function ZbBatchErrorFromJson(JsonContent: string): TZbBatchError;
    begin
        Result := ZbBatchErrorFromJson(TJSONObject(GetJSON(JsonContent)));
    end;

    function ZbBatchErrorFromJson(JObject: TJSONObject): TZbBatchError;
    begin
        Result.Error := JObject.Find('error').AsString;
        Result.EmailAddress := JObject.Find('email_address').AsString;
    end;

    function ZbBatchValidationFromJson(JsonContent: String): TZBBatchValidation;
    var
        JObject: TJSONObject;
        JArray: TJSONArray;
        IIndex: Integer;
        Found: Boolean;
    begin
        JObject := TJSONObject(GetJSON(JsonContent));

        // parse emails validations
        Found := JObject.Find('email_batch', JArray);
        if not Found then
            raise Exception.Create(
                'Field "email_batch" not found while parsing batch validation response'
            );
        Result.EmailBatchLength := JArray.Count;
        if Result.EmailBatchLength > 0 then
        begin
            SetLength(Result.EmailBatch, Result.EmailBatchLength);
            for IIndex := 0 to Result.EmailBatchLength - 1 do
                Result.EmailBatch[IIndex] := ZbValidationFromJson(JArray.Objects[IIndex]);
        end;

        // parse errors
        JArray.Free;
        Found := JObject.Find('errors', JArray);
        if not Found then
            raise Exception.Create(
                'Field "errors" not found while parsing batch validation response'
            );
        Result.ErrorsLength := JArray.Count;
        if Result.ErrorsLength > 0 then
        begin
            SetLength(Result.Errors, Result.ErrorsLength);
            for IIndex := 0 to Result.ErrorsLength - 1 do
                Result.Errors[IIndex] := ZbBatchErrorFromJson(JArray.Objects[IIndex]);
        end;
    end;

    function ZbFileFeedbackFromJson(JsonContent: string): TZbFileFeedback;
    var
        JObject: TJSONObject;
    begin
        JObject := TJSONObject(GetJSON(JsonContent));

        Result.Success := JObject.Find('success').AsBoolean;
        Result.Message := JObject.Find('message').AsString;
        Result.FileName := StringOrNull(JObject, 'file_name');
        Result.FileId := StringOrNull(JObject, 'file_id');
    end;

    function ZbFileStatusFromJson(JsonContent: string): TZbFileStatus;
    var
        JObject: TJSONObject;
        PercentageAuxArray: array of String;
    begin
        JObject := TJSONObject(GetJSON(JsonContent));

        Result.Success := JObject.Find('success').AsBoolean;
        Result.FileId := JObject.Find('file_id').AsString;
        Result.FileName := JObject.Find('file_name').AsString;
        Result.FileStatus := JObject.Find('file_status').AsString;
        Result.ErrorReason := StringOrNull(JObject, 'error_reason');
        Result.ReturnUrl := StringOrNull(JObject, 'return_url');
        Result.UploadDate := ISO8601ToDate(JObject.Find('upload_date').AsString);

        // Percentage comes as a string; will parse it to float
        PercentageAuxArray := SplitString(
            JObject.Find('complete_percentage').AsString, '%'
        );
        if Length(PercentageAuxArray) > 0 then
            Result.CompletePercentage := StrToFloat(PercentageAuxArray[0])
        else
            Result.CompletePercentage := -1;
    end;

end.

