unit ZbStructures;

{$I zboptions.inc}{$H+}

interface

uses
    Classes, SysUtils, StrUtils, DateUtils, Types,
    {$IFDEF FPC}
    fpjson, jsonparser,
    {$ELSE}
    System.JSON,
    {$ENDIF}
    ZbUtility;

type
    TZbJSONValue = {$IFDEF FPC}TJSONData{$ELSE}TJSONValue{$ENDIF};
    TZbJson = class
    protected
        FCreated: Boolean;
        FJSONObject: TJSONObject;
    public
        class function Parse(JsonContent: String): TJSONObject;

        constructor Create(JsonContent: String);
        constructor CreateWrap(JObject: TJSONObject);
        destructor Destroy; override;
        function GetValue(Key: String): TZbJSONValue;
        function GetString(Key: String): String;
        function GetInteger(Key: String): Integer;
        function GetBoolean(Key: String): Boolean;
        function GetArray(Key: String; OUT JArray: TJSONArray): Boolean;
        { The trimmed value when it is a JSON string, or ''. }
        function GetText(Key: String): String;
        { The string field Key, which the result cannot do without: present,
          not null, and a JSON string. Raises otherwise. }
        function GetRequiredString(Key: String): String;
        function KeyCount: Integer;
        function KeyAt(Index: Integer): String;
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
        SubStatusAcceptAll: Integer;
        SubStatusMxForward: Integer;
        SubStatusAlternate: Integer;
        SubStatusBlocked: Integer;
        SubStatusAllowed: Integer;
        SubStatusGold: Integer;
        SubStatusRoleBasedAcceptAll: Integer;
        SubStatusLeadingPeriodRemoved: Integer;
        SubStatusAliasAddress: Integer;
        StartDate: TDate;
        EndDate: TDate;
    end;

    TZbScoreEmailResult = record
        Email: String;
        Score: Integer;
        HasScore: Boolean;
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
        ActiveInDays: String;
        ActiveFirstSeen: String;
        SmtpProvider: String;
        CatchallDomain: boolean;
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
        { Returned with domain_info. }
        DomainWebsiteExists: String;
        DomainRegistrantCompanyName: String;
        { The API's "Message" key, returned instead of a result while a
          greylist retry is still running. }
        Message: String;
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
        FilePhase2Status: String;
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

    TZbDomainFormats = record
        Format: String;
        Confidence: String;
    end;

    TZbFindEmailResponse = record
        Email: String;
        EmailConfidence: String;
        Domain: String;
        CompanyName: String;
        DidYouMean: String;
        FailureReason: String;
    end;

    TZbDomainSearchResponse = record
        Domain: String;
        CompanyName: String;
        Format: String;
        Confidence: String;
        DidYouMean: String;
        FailureReason: String;
        OtherDomainFormats: array of TZbDomainFormats;
    end;

function ZbApiUsageFromJson(JsonContent: string): TApiUsage;
function ZbValidationFromJson(JsonObj: TZbJson): TZbValidationResult; {$IFNDEF FPC} overload; {$ENDIF}
function ZbValidationFromJson(JsonContent: string): TZbValidationResult; {$IFNDEF FPC} overload; {$ENDIF}
function ZbBatchErrorFromJson(JsonObj: TZbJson): TZbBatchError; {$IFNDEF FPC} overload; {$ENDIF}
function ZbBatchErrorFromJson(JsonContent: string): TZbBatchError; {$IFNDEF FPC} overload; {$ENDIF}
function ZbBatchValidationFromJson(JsonContent: string): TZBBatchValidation;
function ZbFileFeedbackFromJson(JsonContent: string): TZbFileFeedback;
function ZbFileStatusFromJson(JsonContent: string): TZbFileStatus;
{ True when getfile body is a JSON error/status object (incl. HTTP 200), aligned with other ZB SDKs. }
function ZbGetFileJsonIndicatesError(const JsonContent: string): Boolean;
(* The API's own message when a JSON body is an error with no "success" field
  to gate on: a non-empty "error", "error_message" or "Error" string, or ''.
  LIVE: an invalid or drained key answers validate, activity and getapiusage
  with {"error": ...}, and the filters with {"Error": ...}, on an HTTP 200. *)
function ZbErrorOnlyMessage(const Body: String): String;
{ Whether a JSON body is nothing but a message: an object with no "success"
  field whose keys are all message keys, at least one a non-empty string. }
function ZbMessageOnlyBody(const Body: String): Boolean;
(* For a call whose result is never a bare message - the email finder and
  domain search - raises on a message-only body. LIVE: guessformat answers an
  invalid or drained key with an HTTP 200 and {"Message": "..."}. Not for
  validate, which answers a repeated greylist_processing request with
  {"Message": "Greylist validation still in progress..."}, a result. *)
procedure ZbRaiseIfMessageOnly(const Response: TZbRequestResponse);
{ For a call that hands its raw JSON body to the caller: raises a ZbException
  with the response's status unless the body is one strict JSON value
  (ZbIsStrictJson). A proxy's text, a CSV labelled
  application/json or an empty body on a 2xx is not the API's answer. }
procedure ZbRequireJsonBody(const Response: TZbRequestResponse);
{ Whether Text is exactly one strict RFC 8259 JSON value, with only space,
  tab, CR and LF around it. fpjson's GetJSON is lenient (single quotes,
  unquoted keys, TRUE, leading zeros, raw control characters in strings), so
  every JSON body is checked with this first. }
function ZbIsStrictJson(const Text: String): Boolean;
function TZbDomainFormatsFromJson(JsonObj: TZbJson): TZbDomainFormats;
function TZbFindEmailResponseFromJson(JsonContent: string): TZbFindEmailResponse;
function TZbDomainSearchResponseFromJson(JsonContent: string): TZbDomainSearchResponse;
function TZbScoreEmailResultFromJson(JsonContent: string): TZbScoreEmailResult;
procedure Register;

implementation

    procedure Register;
    begin
    end;


    function TZbJson.GetText(Key: String): String;
    var
        JValue: TZbJSONValue;
    begin
        Result := '';
        JValue := GetValue(Key);
        if JValue = nil then
            Exit;
        {$IFDEF FPC}
        if JValue.JSONType <> jtString then
            Exit;
        Result := Trim(JValue.AsString);
        {$ELSE}
        if not (JValue is TJSONString) then
            Exit;
        Result := Trim(TJSONString(JValue).Value);
        {$ENDIF}
    end;

    function TZbJson.KeyCount: Integer;
    begin
        Result := FJSONObject.Count;
    end;

    function TZbJson.KeyAt(Index: Integer): String;
    begin
        {$IFDEF FPC}
        Result := FJSONObject.Names[Index];
        {$ELSE}
        Result := FJSONObject.Pairs[Index].JsonString.Value;
        {$ENDIF}
    end;

    { The parsed object, or nil when the body is not a JSON object or carries
      a "success" field (which ZbSuccessMeansFailure already judges). }
    function ZbObjectWithoutSuccess(const Body: String): TZbJson;
    var
        Trimmed: String;
    begin
        Result := nil;
        Trimmed := Trim(Body);
        if (Trimmed = '') or (Trimmed[1] <> '{') then
            Exit;
        try
            Result := TZbJson.Create(Trimmed);
        except
            Result := nil;
            Exit;
        end;
        if (Result.FJSONObject = nil) or (Result.GetValue('success') <> nil) then
            FreeAndNil(Result);
    end;

    function ZbErrorOnlyMessage(const Body: String): String;
    const
        ERROR_KEYS: array[0..2] of String = ('error', 'error_message', 'Error');
    var
        J: TZbJson;
        I: Integer;
    begin
        Result := '';
        J := ZbObjectWithoutSuccess(Body);
        if J = nil then
            Exit;
        try
            for I := Low(ERROR_KEYS) to High(ERROR_KEYS) do
            begin
                Result := J.GetText(ERROR_KEYS[I]);
                if Result <> '' then
                    Exit;
            end;
        finally
            J.Free;
        end;
    end;

    function ZbMessageOnlyBody(const Body: String): Boolean;
    const
        MESSAGE_KEYS: array[0..5] of String = ('error', 'message', 'error_message', 'Message', 'Error', 'detail');
    var
        J: TZbJson;
        I, K: Integer;
        Known, HasText: Boolean;
    begin
        Result := False;
        J := ZbObjectWithoutSuccess(Body);
        if J = nil then
            Exit;
        try
            HasText := False;
            for I := 0 to J.KeyCount - 1 do
            begin
                Known := False;
                for K := Low(MESSAGE_KEYS) to High(MESSAGE_KEYS) do
                    if J.KeyAt(I) = MESSAGE_KEYS[K] then
                        Known := True;
                if not Known then
                    Exit;
                if J.GetText(J.KeyAt(I)) <> '' then
                    HasText := True;
            end;
            Result := HasText;
        finally
            J.Free;
        end;
    end;

    function ZbIsStrictJson(const Text: String): Boolean;
    var
        P, Len: Integer;

        procedure SkipWs;
        begin
            while (P <= Len) and ((Text[P] = ' ') or (Text[P] = #9) or (Text[P] = #10) or (Text[P] = #13)) do
                Inc(P);
        end;

        function IsDigit: Boolean;
        begin
            Result := (P <= Len) and (Text[P] >= '0') and (Text[P] <= '9');
        end;

        function Digits: Boolean;
        begin
            Result := IsDigit;
            while IsDigit do
                Inc(P);
        end;

        function Number: Boolean;
        begin
            Result := False;
            if (P <= Len) and (Text[P] = '-') then
                Inc(P);
            if (P <= Len) and (Text[P] = '0') then
                Inc(P)
            else if not Digits then
                Exit;
            if (P <= Len) and (Text[P] = '.') then
            begin
                Inc(P);
                if not Digits then
                    Exit;
            end;
            if (P <= Len) and ((Text[P] = 'e') or (Text[P] = 'E')) then
            begin
                Inc(P);
                if (P <= Len) and ((Text[P] = '+') or (Text[P] = '-')) then
                    Inc(P);
                if not Digits then
                    Exit;
            end;
            Result := True;
        end;

        function IsHex(C: Char): Boolean;
        begin
            Result := ((C >= '0') and (C <= '9')) or ((C >= 'a') and (C <= 'f')) or ((C >= 'A') and (C <= 'F'));
        end;

        function Str: Boolean;
        var
            K: Integer;
        begin
            Result := False;
            if (P > Len) or (Text[P] <> '"') then
                Exit;
            Inc(P);
            while P <= Len do
            begin
                if Text[P] = '"' then
                begin
                    Inc(P);
                    Result := True;
                    Exit;
                end;
                if Ord(Text[P]) < 32 then
                    Exit;
                if Text[P] = '\' then
                begin
                    Inc(P);
                    if P > Len then
                        Exit;
                    if Text[P] = 'u' then
                    begin
                        for K := 1 to 4 do
                        begin
                            Inc(P);
                            if (P > Len) or not IsHex(Text[P]) then
                                Exit;
                        end;
                    end
                    else if Pos(Text[P], '"\/bfnrt') = 0 then
                        Exit;
                end;
                Inc(P);
            end;
        end;

        function Literal(const Word: String): Boolean;
        begin
            Result := Copy(Text, P, Length(Word)) = Word;
            if Result then
                Inc(P, Length(Word));
        end;

        function Value(Depth: Integer): Boolean;
        begin
            Result := False;
            if (P > Len) or (Depth > 512) then
                Exit;
            if Text[P] = '{' then
            begin
                Inc(P);
                SkipWs;
                if (P <= Len) and (Text[P] = '}') then
                begin
                    Inc(P);
                    Result := True;
                    Exit;
                end;
                while True do
                begin
                    if not Str then
                        Exit;
                    SkipWs;
                    if (P > Len) or (Text[P] <> ':') then
                        Exit;
                    Inc(P);
                    SkipWs;
                    if not Value(Depth + 1) then
                        Exit;
                    SkipWs;
                    if (P <= Len) and (Text[P] = ',') then
                    begin
                        Inc(P);
                        SkipWs;
                        Continue;
                    end;
                    Result := (P <= Len) and (Text[P] = '}');
                    if Result then
                        Inc(P);
                    Exit;
                end;
            end
            else if Text[P] = '[' then
            begin
                Inc(P);
                SkipWs;
                if (P <= Len) and (Text[P] = ']') then
                begin
                    Inc(P);
                    Result := True;
                    Exit;
                end;
                while True do
                begin
                    SkipWs;
                    if not Value(Depth + 1) then
                        Exit;
                    SkipWs;
                    if (P <= Len) and (Text[P] = ',') then
                    begin
                        Inc(P);
                        Continue;
                    end;
                    Result := (P <= Len) and (Text[P] = ']');
                    if Result then
                        Inc(P);
                    Exit;
                end;
            end
            else if Text[P] = '"' then
                Result := Str
            else if Text[P] = 't' then
                Result := Literal('true')
            else if Text[P] = 'f' then
                Result := Literal('false')
            else if Text[P] = 'n' then
                Result := Literal('null')
            else
                Result := Number;
        end;

    begin
        P := 1;
        Len := Length(Text);
        SkipWs;
        Result := Value(0);
        if Result then
        begin
            SkipWs;
            Result := P > Len;
        end;
    end;

    (* Raises unless Text is one strict JSON value (ZbIsStrictJson): GetJSON
      stops after the first value and ignores the rest, so {"a":1} followed by
      a proxy's text would pass as JSON, and it takes almost-JSON besides. *)
    procedure ZbRequireNothingAfterJson(const Text: String);
    begin
        if ZbIsStrictJson(Text) then
            Exit;
        { No message for a blank body, so ZbException.FromResponse says it was
          empty, with its status. }
        if Trim(Text) = '' then
            raise Exception.Create('');
        raise Exception.Create('ZeroBounce returned a response that is not valid JSON.');
    end;

    procedure ZbRequireJsonBody(const Response: TZbRequestResponse);
    var
        Text: String;
        Data: TZbJSONValue;
        Valid: Boolean;
        Error: ZbException;
    begin
        { The body as it came: only space, tab, CR and LF may surround the
          value, which Trim would not tell from other control characters. }
        Text := Response.Payload;
        Valid := False;
        if ZbIsStrictJson(Text) then
        begin
            try
                {$IFDEF FPC}
                Data := GetJSON(Text);
                {$ELSE}
                Data := TJSONObject.ParseJSONValue(Text, True);
                {$ENDIF}
                Valid := Data <> nil;
                Data.Free;
            except
                Valid := False;
            end;
        end;
        if not Valid then
        begin
            Error := ZbException.FromResponse('', Response);
            Error.MarkJsonError;
            raise Error;
        end;
    end;

    procedure ZbRaiseIfMessageOnly(const Response: TZbRequestResponse);
    const
        MESSAGE_KEYS: array[0..5] of String = ('Message', 'message', 'error', 'error_message', 'Error', 'detail');
    var
        J: TZbJson;
        Text: String;
        I: Integer;
        Error: ZbException;
    begin
        if not ZbMessageOnlyBody(Response.Payload) then
            Exit;
        Text := '';
        J := TZbJson.Create(Trim(Response.Payload));
        try
            for I := Low(MESSAGE_KEYS) to High(MESSAGE_KEYS) do
                if Text = '' then
                    Text := J.GetText(MESSAGE_KEYS[I]);
        finally
            J.Free;
        end;
        Error := ZbException.FromResponse(Text, Response);
        Error.MarkHttpError;
        raise Error;
    end;

    { Field types are part of the response shape (standard E7): a field that
      arrives with a JSON type its model does not take is not the API's answer.
      The call turns this into a ZbException with the HTTP status. }
    procedure ZbRaiseFieldType(const Key: String);
    begin
        raise Exception.CreateFmt(
            'ZeroBounce returned the field "%s" missing or with the wrong JSON type.', [Key]);
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
        if FCreated and (FJSONObject <> nil) then
            FJSONObject.Free;
        inherited Destroy;
    end;

    class function TZbJson.Parse(JsonContent: String): TJSONObject;
    var
        Data: TZbJSONValue;
    begin
        ZbRequireNothingAfterJson(JsonContent);
        {$IFDEF FPC}
        Data := GetJSON(JsonContent);
        {$ELSE}
        Data := TJSONObject.ParseJSONValue(JsonContent, True);
        {$ENDIF}
        { Every typed result is read from a JSON object. Another JSON value (a
          string, a number, true, null, an array) is not the API's answer
          either: it raises, and the call reports it as an error with the
          HTTP status, rather than a result filled with defaults. }
        if Data is TJSONObject then
            Result := TJSONObject(Data)
        else
        begin
            Data.Free;
            raise Exception.Create('ZeroBounce returned JSON that is not an object.');
        end;
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
        if JValue = nil then exit;

        {$IFDEF FPC}
        if JValue.IsNull then exit;
        if JValue.JSONType <> jtString then ZbRaiseFieldType(Key);
        Result := JValue.AsString;
        {$ELSE}
        if JValue.Null then exit;
        if not (JValue is TJSONString) then ZbRaiseFieldType(Key);
        Result := JValue.AsType<String>;
        {$ENDIF}
    end;

    function TZbJson.GetRequiredString(Key: String): String;
    var
        JValue: TZbJSONValue;
    begin
        JValue := GetValue(Key);
        {$IFDEF FPC}
        if (JValue = nil) or (JValue.JSONType <> jtString) then ZbRaiseFieldType(Key);
        {$ELSE}
        if (JValue = nil) or not (JValue is TJSONString) then ZbRaiseFieldType(Key);
        {$ENDIF}
        Result := GetString(Key);
    end;

    function TZbJson.GetInteger(Key: String): Integer;
    var
        JValue: TZbJSONValue;
    begin
        Result := 0;
        JValue := GetValue(Key);
        if JValue = nil then exit;

        {$IFDEF FPC}
        if JValue.IsNull then exit;
        { A JSON integer: fpjson would read true as 1, 5.5 as 6 and "5" as 5. }
        if not ((JValue is TJSONIntegerNumber) or (JValue is TJSONInt64Number)) then
            ZbRaiseFieldType(Key);
        Result := JValue.AsInteger;
        {$ELSE}
        if JValue.Null then exit;
        if not (JValue is TJSONNumber) or (Pos('.', JValue.Value) > 0) or
            (Pos('e', LowerCase(JValue.Value)) > 0) then
            ZbRaiseFieldType(Key);
        Result := JValue.AsType<Integer>;
        {$ENDIF}
    end;

    function TZbJson.GetBoolean(Key: String): Boolean;
    var
        JValue: TZbJSONValue;
    begin
        Result := False;
        JValue := GetValue(Key);
        if JValue = nil then exit;

        {$IFDEF FPC}
        if JValue.IsNull then exit;
        if JValue.JSONType <> jtBoolean then ZbRaiseFieldType(Key);
        Result := JValue.AsBoolean;
        {$ELSE}
        if JValue.Null then exit;
        if not (JValue is TJSONBool) then ZbRaiseFieldType(Key);
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

        function ExtractDate(DateString: String): TDate;
        var
            Day, Month, Year: Integer;
            {$IFNDEF FPC}
            ASplit: TStringDynArray;
            {$ENDIF}
        begin
            {$IFDEF FPC}
            SScanf(DateString, '%d/%d/%d', [@Month, @Day, @Year]);
            {$ELSE}
            ASplit := SplitString(DateString, '/');
            Month := StrToInt(ASplit[0]);
            Day := StrToInt(ASplit[1]);
            Year := StrToInt(ASplit[2]);
            {$ENDIF}
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
        Result.SubStatusAcceptAll :=                  JsonObj.GetInteger('sub_status_accept_all');
        Result.SubStatusMxForward :=                  JsonObj.GetInteger('sub_status_mx_forward');
        Result.SubStatusAlternate :=                  JsonObj.GetInteger('sub_status_alternate');
        Result.SubStatusBlocked :=                    JsonObj.GetInteger('sub_status_blocked');
        Result.SubStatusAllowed :=                    JsonObj.GetInteger('sub_status_allowed');
        Result.SubStatusGold :=                       JsonObj.GetInteger('sub_status_gold');
        Result.SubStatusRoleBasedAcceptAll :=         JsonObj.GetInteger('sub_status_role_based_accept_all');
        Result.SubStatusLeadingPeriodRemoved :=       JsonObj.GetInteger('sub_status_leading_period_removed');
        Result.SubStatusAliasAddress :=               JsonObj.GetInteger('sub_status_alias_address');

        Result.StartDate := ExtractDate(JsonObj.GetString('start_date'));
        Result.EndDate := ExtractDate(JsonObj.GetString('end_date'));

    end;

    function ZbValidationFromJson(JsonContent: string): TZbValidationResult;
    begin
        Result := ZbValidationFromJson(TZbJson.Create(JsonContent));
    end;

    function ZbValidationFromJson(JsonObj: TZbJSon): TZbValidationResult;

        function ExtractDateTime(DateString: String): TDateTime;
        var
            Day, Month, Year, Hour, Minute, Second: Integer;
            Milis: Word;
            {$IFNDEF FPC}
            ASplitMain: TStringDynArray;
            ASplitAux: TStringDynArray;
            {$ENDIF}
        begin
            // %Y-%m-%d %H:%M:%S.%3f
            {$IFDEF FPC}
            SScanf(
                DateString,
                '%d-%d-%d %d:%d:%d.%d',
                [@Year, @Month, @Day, @Hour, @Minute, @Second, @Milis]
            );
            {$ELSE}
            // no sscanf in Delphi
            ASplitMain := SplitString(DateString, ' ');

            // %Y-%m-%d
            ASplitAux := SplitString(ASplitMain[0], '-');
            Year := StrToInt(ASplitAux[0]);
            Month := StrToInt(ASplitAux[1]);
            Day := StrToInt(ASplitAux[1]);

            // %H:%M:%S.%3f
            ASplitAux := SplitString(ASplitMain[1], '.');
            if Length(ASplitAux) = 1 then
                Milis := 0
            else
                Milis := StrToInt(ASplitAux[1]);
            ASplitAux := SplitString(ASplitAux[0], ':');
            Hour := StrToInt(ASplitAux[0]);
            Minute := StrToInt(ASplitAux[1]);
            Second := StrToInt(ASplitAux[2]);
            {$ENDIF}
            Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Milis);
        end;
    begin
        { A result names its address and status, as JSON strings. The one
          answer without them is a greylist_processing validate still in
          progress, which carries its text in Message. }
        if JsonObj.GetText('Message') = '' then
        begin
            JsonObj.GetRequiredString('address');
            JsonObj.GetRequiredString('status');
        end;
        Result.Address :=       JsonObj.GetString('address');
        Result.Status :=        JsonObj.GetString('status');
        Result.SubStatus :=     JsonObj.GetString('sub_status');
        Result.FreeEmail :=     JsonObj.GetBoolean('free_email');

        Result.DidYouMean :=    JsonObj.GetString('did_you_mean');
        Result.Account :=       JsonObj.GetString('account');
        Result.Domain :=        JsonObj.GetString('domain');
        Result.DomainAgeDays := JsonObj.GetString('domain_age_days');
        Result.ActiveInDays := JsonObj.GetString('active_in_days');
        Result.ActiveFirstSeen := JsonObj.GetString('active_first_seen');
        Result.SmtpProvider :=  JsonObj.GetString('smtp_provider');
        Result.CatchallDomain := JsonObj.GetBoolean('catchall_domain');
        Result.MxRecord :=      JsonObj.GetString('mx_record');
        Result.MxFound :=       JsonObj.GetString('mx_found');
        Result.Firstname :=     JsonObj.GetString('firstname');
        Result.Lastname :=      JsonObj.GetString('lastname');
        Result.Gender :=        JsonObj.GetString('gender');
        Result.Country :=       JsonObj.GetString('country');
        Result.Region :=        JsonObj.GetString('region');
        Result.City :=          JsonObj.GetString('city');
        Result.Zipcode :=       JsonObj.GetString('zipcode');

        { A greylist retry that is still running answers with "Message" and no
          processed_at, so an absent date leaves ProcessedAt at zero rather
          than raising. }
        if Trim(JsonObj.GetString('processed_at')) = '' then
            Result.ProcessedAt := 0
        else
            Result.ProcessedAt := ExtractDateTime(JsonObj.GetString('processed_at'));

        Result.DomainWebsiteExists := JsonObj.GetString('domain_website_exists');
        Result.DomainRegistrantCompanyName := JsonObj.GetString('domain_registrant_company_name');
        { Note the capital M: that is the key the API uses. }
        Result.Message := JsonObj.GetString('Message');
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
        AListItem: TJSONObject;
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
            begin
                {$IFDEF FPC}
                AListItem := JArray.Objects[IIndex];
                {$ELSE}
                AListItem := JArray[IIndex] as TJSONObject;
                {$ENDIF}
                Result.EmailBatch[IIndex] := ZbValidationFromJson(
                    TZbJson.CreateWrap(AListItem)
                );
            end;
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
            begin
                {$IFDEF FPC}
                AListItem := JArray.Objects[IIndex];
                {$ELSE}
                AListItem := JArray[IIndex] as TJSONObject;
                {$ENDIF}
                Result.Errors[IIndex] := ZbBatchErrorFromJson(
                    TZbJson.CreateWrap(AListItem)
                );
            end;
        end;
        JArray.Free;
    end;

    function ZbGetFileJsonIndicatesError(const JsonContent: string): Boolean;
    var
        J: TZbJson;
        V: TZbJSONValue;
        Trimmed: String;

        function HasText(const Key: String): Boolean;
        var
            Value: TZbJSONValue;
        begin
            Value := J.GetValue(Key);
            {$IFDEF FPC}
            if (Value = nil) or Value.IsNull then
                Result := False
            else if Value.JSONType = jtString then
                Result := Length(Trim(Value.AsString)) > 0
            else
                Result := True;
            {$ELSE}
            if (Value = nil) or Value.Null then
                Result := False
            else if Value is TJSONString then
                Result := Length(Trim(Value.Value)) > 0
            else
                Result := True;
            {$ENDIF}
        end;

    begin
        Result := False;
        Trimmed := Trim(JsonContent);
        if (Length(Trimmed) < 1) or (Trimmed[1] <> '{') then
            Exit;
        try
            J := TZbJson.Create(JsonContent);
        except
            Exit;
        end;
        try
            V := J.GetValue('success');
            if V <> nil then
            begin
                {$IFDEF FPC}
                case V.JSONType of
                    jtBoolean:
                        if not V.AsBoolean then
                        begin
                            Result := True;
                            Exit;
                        end;
                    jtString:
                        if SameText(Trim(V.AsString), 'False') or SameText(Trim(V.AsString), 'false') then
                        begin
                            Result := True;
                            Exit;
                        end;
                end;
                {$ELSE}
                if V is TJSONBool then
                begin
                    if not TJSONBool(V).AsBoolean then
                    begin
                        Result := True;
                        Exit;
                    end;
                end
                else if V is TJSONString then
                begin
                    if SameText(Trim(TJSONString(V).Value), 'False') or
                        SameText(Trim(TJSONString(V).Value), 'false') then
                    begin
                        Result := True;
                        Exit;
                    end;
                end;
                {$ENDIF}
            end;
            { Any message, error or error_message that is not null or blank
              (a JSON object in place of a file is an error anyway). }
            if HasText('message') or HasText('error') or HasText('error_message') then
            begin
                Result := True;
                Exit;
            end;
            if J.GetValue('success') <> nil then
                Result := True;
        finally
            J.Free;
        end;
    end;

    function ZbFileFeedbackFromJson(JsonContent: string): TZbFileFeedback;
    var
        JsonObj: TZbJSon;
    begin
        JsonObj := TZbJSon.Create(JsonContent);
        try
            Result.Success := JsonObj.GetBoolean('success');
            Result.Message := JsonObj.GetString('message');
            Result.FileName := JsonObj.GetString('file_name');
            Result.FileId := JsonObj.GetString('file_id');
        finally
            JsonObj.Free;
        end;
    end;

    function ZbFileStatusFromJson(JsonContent: string): TZbFileStatus;
    var
        JsonObj: TZbJSon;
        PercentageAuxArray: TStringDynArray;
    begin
        JsonObj := TZbJSon.Create(JsonContent);
        try
            Result.Success := JsonObj.GetBoolean('success');
            Result.FileId := JsonObj.GetString('file_id');
            Result.FileName := JsonObj.GetString('file_name');
            Result.FileStatus := JsonObj.GetString('file_status');
            Result.FilePhase2Status := JsonObj.GetString('file_phase_2_status');
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
        finally
            JsonObj.Free;
        end;
    end;

    function TZbDomainFormatsFromJson(JsonObj: TZbJson): TZbDomainFormats;
    begin
        Result.Format := JsonObj.GetString('format');
        Result.Confidence := JsonObj.GetString('confidence');
    end;

    function TZbFindEmailResponseFromJson(JsonContent: string): TZbFindEmailResponse;
    var
        JsonObj: TZbJSon;
    begin
        JsonObj := TZbJson.Create(JsonContent);
        Result.Email := JsonObj.GetString('email');
        Result.EmailConfidence := JsonObj.GetString('email_confidence');
        Result.Domain := JsonObj.GetString('domain');
        Result.CompanyName := JsonObj.GetString('company_name');
        Result.DidYouMean := JsonObj.GetString('did_you_mean');
        Result.FailureReason := JsonObj.GetString('failure_reason');
    end;

    function TZbDomainSearchResponseFromJson(JsonContent: string): TZbDomainSearchResponse;
    var
        JsonObj: TZbJSon;
        JArray: TJSONArray;
        Found: boolean;
        IIndex: Integer;
        AListItem: TJSONObject;
    begin
        JsonObj := TZbJson.Create(JsonContent);
        Result.Domain := JsonObj.GetString('domain');
        Result.CompanyName := JsonObj.GetString('company_name');
        Result.Format := JsonObj.GetString('format');
        Result.Confidence := JsonObj.GetString('confidence');
        Result.DidYouMean := JsonObj.GetString('did_you_mean');
        Result.FailureReason := JsonObj.GetString('failure_reason');

        Found := JsonObj.GetArray('other_domain_formats', JArray);
        if not Found then
            raise Exception.Create(
                'Field "other_domain_formats" not found while parsing "find mail" response'
            );
        if JArray.Count > 0 then
        begin
            SetLength(Result.OtherDomainFormats, JArray.Count);
            for IIndex := 0 to JArray.Count - 1 do
            begin
                {$IFDEF FPC}
                AListItem := JArray.Objects[IIndex];
                {$ELSE}
                AListItem := JArray[IIndex] as TJSONObject;
                {$ENDIF}
                Result.OtherDomainFormats[IIndex] := TZbDomainFormatsFromJson(
                    TZbJson.CreateWrap(AListItem)
                );
            end;
        end;
    end;

    function TZbScoreEmailResultFromJson(JsonContent: string): TZbScoreEmailResult;
    var
        JsonObj: TZbJson;
        ScoreValue: TZbJSONValue;
    begin
        Result.Email := '';
        Result.Score := 0;
        Result.HasScore := False;
        JsonObj := TZbJson.Create(JsonContent);
        try
            Result.Email := JsonObj.GetString('email');
            ScoreValue := JsonObj.GetValue('score');
            if ScoreValue <> nil then
            begin
                Result.HasScore := True;
                {$IFDEF FPC}
                if ScoreValue.JSONType = jtNumber then
                    Result.Score := Trunc(ScoreValue.AsFloat)
                else
                    Result.Score := StrToInt(ScoreValue.AsString);
                {$ELSE}
                if ScoreValue is TJSONNumber then
                    Result.Score := TJSONNumber(ScoreValue).AsInt
                else
                    Result.Score := StrToInt(ScoreValue.Value);
                {$ENDIF}
            end;
        finally
            JsonObj.Free;
        end;
    end;

end.

