unit ZbStructures;

{$I zboptions.inc}{$H+}

interface

uses
    Classes, SysUtils, StrUtils, DateUtils,
    {$IFDEF FPC}
    fpjson, jsonparser,
    {$ELSE}
    System.JSON,
    {$ENDIF}
    ZbUtility;

type
    TZbJSONValue = {$IFDEF FPC}TJSONData{$ELSE}TJSONValue{$ENDIF};
    TZbJson = class(TObject)
    protected
        FCreated: Boolean;
        FJSONObject: TJSONObject;
    public
        class function Parse(JsonContent: String): TJSONObject;

        constructor Create(JObject: TJSONObject); {$IFNDEF FPC}override;{$ENDIF}
        constructor CreateWrap(JsonContent: String); {$IFNDEF FPC}override;{$ENDIF}
        function GetValue(Key: String): TZbJSONValue;
        destructor Destroy; override;
        function GetString(Key: String): String;
        function GetInteger(Key: String): Integer;
        function GetBoolean(Key: String): Boolean;
        function GetArray(Key: String; OUT JArray: TJSONArray): Boolean;
    end;

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
function ZbValidationFromJson(JsonObj: TZbJson): TZbValidationResult; overload;
function ZbValidationFromJson(JsonContent: string): TZbValidationResult; overload;
function ZbBatchErrorFromJson(JsonObj: TZbJson): TZbBatchError; overload;
function ZbBatchErrorFromJson(JsonContent: string): TZbBatchError; overload;
function ZbBatchValidationFromJson(JsonContent: string): TZBBatchValidation;
function ZbFileFeedbackFromJson(JsonContent: string): TZbFileFeedback;
function ZbFileStatusFromJson(JsonContent: string): TZbFileStatus;
procedure Register;

implementation

    procedure Register;
    begin
    end;


    constructor TZbJson.Create(JsonContent: String);
    begin
        inherited Create;
        FJSONObject := Parse(JsonContent);
        FCreated := True;
    end;

    constructor TZbJson.CreateWrap(JObject: TJSONObject);
    begin
        inherited Create;
        FJSONObject := JObject;
        FCreated := False;
    end;

    destructor TZbJson.Destroy;
    begin
        if FCreated and (FJSONObject <> nil) then FJSONObject.Free;
    end;

    class function TZbJson.Parse(JsonContent: String): TJSONObject;
    begin
        {$IFDEF FPC}
        Result := TJSONObject(GetJSON(JsonContent));
        {$ELSE}
        Result := TJSONObject(TJSONObject.ParseJSONValue(JsonContent, True));
        {$ENDIF}
    end;

    function TZbJson.GetValue(Key: String): TZbJSONValue;
    begin
        {$IFDEF FPC}
        Result := FJSONObject.Find(Key);
        {$ELSE}
        Result := FJSONObject.FindValue(Key);
        {$ENDIF}
    end;

    function TZbJson.GetString(Key: String): String;
    var
        JValue: TZbJSONValue;
    begin
        Result := '';
        JValue := GetValue(Key);
        {$IFDEF FPC}
        if (JValue = nil) or (JValue.IsNull) then exit;
        Result := JValue.AsString;
        {$ELSE}
        if (JValue = nil) or (JValue.Null) then exit;
        Result := JValue.AsType<String>;
        {$ENDIF}
    end;

    function TZbJson.GetInteger(Key: String): Integer;
    var
        JValue: TZbJSONValue;
    begin
        Result := 0;
        JValue := GetValue(Key);
        {$IFDEF FPC}
        if JValue.IsNull then exit;
        Result := JValue.AsInteger;
        {$ELSE}
        if JValue.Null then exit;
        Result := JValue.AsType<Integer>;
        {$ENDIF}
    end;

    function TZbJson.GetBoolean(Key: String): Boolean;
    var
        JValue: TZbJSONValue;
    begin
        Result := False;
        JValue := GetValue(Key);
        {$IFDEF FPC}
        if JValue.IsNull then exit;
        Result := JValue.AsBoolean;
        {$ELSE}
        if JValue.Null then exit;
        Result := JValue.AsType<Boolean>;
        {$ENDIF}
    end;

    function TZbJson.GetArray(Key: String; OUT JArray: TJSONArray): Boolean;
    begin
        {$IFDEF FPC}
        Result := FJSONObject.Find(Key, JArray);
        {$ELSE}
        Result := FJSONObject.TryGetValue<TJSONArray>(Key, JArray);
        {$ENDIF}
    end;

    function ZbApiUsageFromJson(JsonContent: string): TApiUsage;
    var
        JsonObj: TZbJson;

        function ExtractDate(JsonKey: String): TDate;
        var
            Day, Month, Year: Integer;
        begin
            SScanf(JsonObj.GetString(JsonKey), '%d/%d/%d', [@Month, @Day, @Year]);
            Result := EncodeDateTime(Year, Month, Day, 0, 0, 0, 0);
        end;

    begin
        JsonObj := TZbJson.Create(JsonContent);

        Result.Total :=                               JsonObj.GetInteger('total');
        Result.StatusValid :=                         JsonObj.GetInteger('status_valid');
        Result.StatusInvalid :=                       JsonObj.GetInteger('status_invalid');
        Result.StatusCatchAll :=                      JsonObj.GetInteger('status_catch_all');
        Result.StatusDoNotMail :=                     JsonObj.GetInteger('status_do_not_mail');
        Result.StatusSpamtrap :=                      JsonObj.GetInteger('status_spamtrap');
        Result.StatusUnknown :=                       JsonObj.GetInteger('status_unknown');
        Result.SubStatusToxic :=                      JsonObj.GetInteger('sub_status_toxic');
        Result.SubStatusDisposable :=                 JsonObj.GetInteger('sub_status_disposable');
        Result.SubStatusRoleBased :=                  JsonObj.GetInteger('sub_status_role_based');
        Result.SubStatusPossibleTrap :=               JsonObj.GetInteger('sub_status_possible_trap');
        Result.SubStatusGlobalSuppression :=          JsonObj.GetInteger('sub_status_global_suppression');
        Result.SubStatusTimeoutExceeded :=            JsonObj.GetInteger('sub_status_timeout_exceeded');
        Result.SubStatusMailServerTemporaryError :=   JsonObj.GetInteger('sub_status_mail_server_temporary_error');
        Result.SubStatusMailServerDidNotRespond :=    JsonObj.GetInteger('sub_status_mail_server_did_not_respond');
        Result.SubStatusGreylisted :=                 JsonObj.GetInteger('sub_status_greylisted');
        Result.SubStatusAntispamSystem :=             JsonObj.GetInteger('sub_status_antispam_system');
        Result.SubStatusDoesNotAcceptMail :=          JsonObj.GetInteger('sub_status_does_not_accept_mail');
        Result.SubStatusExceptionOccurred :=          JsonObj.GetInteger('sub_status_exception_occurred');
        Result.SubStatusFailedSyntaxCheck :=          JsonObj.GetInteger('sub_status_failed_syntax_check');
        Result.SubStatusMailboxNotFound :=            JsonObj.GetInteger('sub_status_mailbox_not_found');
        Result.SubStatusUnroutableIpAddress :=        JsonObj.GetInteger('sub_status_unroutable_ip_address');
        Result.SubStatusPossibleTypo :=               JsonObj.GetInteger('sub_status_possible_typo');
        Result.SubStatusNoDnsEntries :=               JsonObj.GetInteger('sub_status_no_dns_entries');
        Result.SubStatusRoleBasedCatchAll :=          JsonObj.GetInteger('sub_status_role_based_catch_all');
        Result.SubStatusMailboxQuotaExceeded :=       JsonObj.GetInteger('sub_status_mailbox_quota_exceeded');
        Result.SubStatusForcibleDisconnect :=         JsonObj.GetInteger('sub_status_forcible_disconnect');
        Result.SubStatusFailedSmtpConnection :=       JsonObj.GetInteger('sub_status_failed_smtp_connection');
        Result.SubStatusMxForward :=                  JsonObj.GetInteger('sub_status_mx_forward');
        Result.SubStatusAlternate :=                  JsonObj.GetInteger('sub_status_alternate');
        Result.SubStatusBlocked :=                    JsonObj.GetInteger('sub_status_blocked');
        Result.SubStatusAllowed :=                    JsonObj.GetInteger('sub_status_allowed');

        Result.StartDate := ExtractDate('start_date');
        Result.EndDate := ExtractDate('end_date');

    end;

    function ZbValidationFromJson(JsonContent: string): TZbValidationResult;
    begin
        Result := ZbValidationFromJson(TZbJson.Create(JsonContent));
    end;

    function ZbValidationFromJson(JsonObj: TZbJSon): TZbValidationResult;

        function ExtractDateTime(JsonKey: String): TDateTime;
        var
            Day, Month, Year, Hour, Minute, Second: Integer;
            Milis: Word;
        begin
            // %Y-%m-%d %H:%M:%S.%3f
            SScanf(
                JsonObj.GetString(JsonKey),
                '%d-%d-%d %d:%d:%d.%d',
                [@Year, @Month, @Day, @Hour, @Minute, @Second, @Milis]
            );
            Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Milis);
        end;
    begin
        Result.Address :=       JsonObj.GetString('address');
        Result.Status :=        JsonObj.GetString('status');
        Result.SubStatus :=     JsonObj.GetString('sub_status');
        Result.FreeEmail :=     JsonObj.GetBoolean('free_email');

        Result.DidYouMean :=    JsonObj.GetString('did_you_mean');
        Result.Account :=       JsonObj.GetString('account');
        Result.Domain :=        JsonObj.GetString('domain');
        Result.DomainAgeDays := JsonObj.GetString('domain_age_days');
        Result.SmtpProvider :=  JsonObj.GetString('smtp_provider');
        Result.MxRecord :=      JsonObj.GetString('mx_record');
        Result.MxFound :=       JsonObj.GetString('mx_found');
        Result.Firstname :=     JsonObj.GetString('firstname');
        Result.Lastname :=      JsonObj.GetString('lastname');
        Result.Gender :=        JsonObj.GetString('gender');
        Result.Country :=       JsonObj.GetString('country');
        Result.Region :=        JsonObj.GetString('region');
        Result.City :=          JsonObj.GetString('city');
        Result.Zipcode :=       JsonObj.GetString('zipcode');

        Result.ProcessedAt := ExtractDateTime('processed_at');
    end;


    function ZbBatchErrorFromJson(JsonContent: string): TZbBatchError;
    begin
        Result := ZbBatchErrorFromJson(TZbJson.Create(JsonContent));
    end;

    function ZbBatchErrorFromJson(JsonObj: TZbJSon): TZbBatchError;
    begin
        Result.Error := JsonObj.GetString('error');
        Result.EmailAddress := JsonObj.GetString('email_address');
    end;

    function ZbBatchValidationFromJson(JsonContent: String): TZBBatchValidation;
    var
        JsonObj: TZbJSon;
        JArray: TJSONArray;
        IIndex: Integer;
        Found: Boolean;
    begin
        JsonObj := TZbJson.Create(JsonContent);

        // parse emails validations
        Found := JsonObj.GetArray('email_batch', JArray);
        if not Found then
            raise Exception.Create(
                'Field "email_batch" not found while parsing batch validation response'
            );
        Result.EmailBatchLength := JArray.Count;
        if Result.EmailBatchLength > 0 then
        begin
            SetLength(Result.EmailBatch, Result.EmailBatchLength);
            for IIndex := 0 to Result.EmailBatchLength - 1 do
                Result.EmailBatch[IIndex] := ZbValidationFromJson(
                    TZbJson.CreateWrap(JArray.Objects[IIndex])
                );
        end;

        // parse errors
        JArray.Free;
        Found := JsonObj.GetArray('errors', JArray);
        if not Found then
            raise Exception.Create(
                'Field "errors" not found while parsing batch validation response'
            );
        Result.ErrorsLength := JArray.Count;
        if Result.ErrorsLength > 0 then
        begin
            SetLength(Result.Errors, Result.ErrorsLength);
            for IIndex := 0 to Result.ErrorsLength - 1 do
                Result.Errors[IIndex] := ZbBatchErrorFromJson(
                    TZbJson.CreateWrap(JArray.Objects[IIndex])
                );
        end;
        JArray.Free;
    end;

    function ZbFileFeedbackFromJson(JsonContent: string): TZbFileFeedback;
    var
        JsonObj: TZbJSon;
    begin
        JsonObj := TZbJSon.Create(JsonContent);

        Result.Success := JsonObj.GetBoolean('success');
        Result.Message := JsonObj.GetString('message');
        Result.FileName := JsonObj.GetString('file_name');
        Result.FileId := JsonObj.GetString('file_id');
    end;

    function ZbFileStatusFromJson(JsonContent: string): TZbFileStatus;
    var
        JsonObj: TZbJSon;
        PercentageAuxArray: array of String;
    begin
        JsonObj := TZbJSon.Create(JsonContent);

        Result.Success := JsonObj.GetBoolean('success');
        Result.FileId := JsonObj.GetString('file_id');
        Result.FileName := JsonObj.GetString('file_name');
        Result.FileStatus := JsonObj.GetString('file_status');
        Result.ErrorReason := JsonObj.GetString('error_reason');
        Result.ReturnUrl := JsonObj.GetString('return_url');
        Result.UploadDate := ISO8601ToDate(JsonObj.GetString('upload_date'));

        // Percentage comes as a string; will parse it to float
        PercentageAuxArray := SplitString(
            JsonObj.GetString('complete_percentage'), '%'
        );
        if Length(PercentageAuxArray) > 0 then
            Result.CompletePercentage := StrToFloat(PercentageAuxArray[0])
        else
            Result.CompletePercentage := -1;

    end;

end.

