unit ZbWarmup;

{ Client for the ZeroBounce Warmup API (https://www.zerobounce.net/docs/warmup-api).

  The Warmup API is a separate product from email validation: its own host, its
  own team-scoped key, and an API-Key header instead of an api_key parameter.
  Call ZbWarmupInitialize with a Warmup API key; ZBInitialize stays for the
  validation key, and the two never mix.

  Every call returns the raw JSON body, or an empty string for the documented
  empty successes (HTTP 204 and an empty 200). }

{$I zboptions.inc}{$H+}

interface

uses
    Classes, SysUtils, ZbUtility;

type
    { ZB managed onboarding only. Empty fields are left out of the request. }
    TZbWarmupEmailContent = record
        CompanyName: String;
        CompanyWebsite: String;
        CompanyDescription: String;
        { Any of transactional, newsletter, marketing, comma separated. }
        TypesOfEmails: String;
        ExampleEmail: String;
    end;
    PTZbWarmupEmailContent = ^TZbWarmupEmailContent;

    { ZB managed onboarding only, and write-only on the API. }
    TZbWarmupManagedSending = record
        WeekdaysOnlySpecified: Boolean;
        WeekdaysOnly: Boolean;
        Timezone: String;
        TimezoneOffsetSpecified: Boolean;
        TimezoneOffset: Integer;
        { YYYY-MM-DD. }
        WarmupStartDate: String;
    end;
    PTZbWarmupManagedSending = ^TZbWarmupManagedSending;

    TZbWarmupAddSendingAccountOptions = record
        { Required for self_managed, ignored for zb_managed. }
        EngagementRuleIdSpecified: Boolean;
        EngagementRuleId: Integer;
        { Self-managed only: do not fail with 409 when the address is already
          registered. }
        IgnoreConflictSpecified: Boolean;
        IgnoreConflict: Boolean;
        EmailContent: PTZbWarmupEmailContent;
        ManagedSending: PTZbWarmupManagedSending;
    end;
    PTZbWarmupAddSendingAccountOptions = ^TZbWarmupAddSendingAccountOptions;

    TZbWarmupListFromAddressesOptions = record
        { Letters, digits and . - _ @ + only. }
        SearchFrom: String;
        OffsetSpecified: Boolean;
        Offset: Integer;
        LimitSpecified: Boolean;
        Limit: Integer;
    end;
    PTZbWarmupListFromAddressesOptions = ^TZbWarmupListFromAddressesOptions;

const
    ZB_WARMUP_SELF_MANAGED = 'self_managed';
    ZB_WARMUP_ZB_MANAGED = 'zb_managed';
    ZB_WARMUP_BASE_PATH = '/api/v1/warmup';
    { team_id is required by the schema but always replaced with the key's
      team, so any value does. }
    ZB_WARMUP_ANY_TEAM_ID = 1;

var
    { Tests may point this at a local stub. }
    ZbWarmupBaseURL: string = 'https://warmup-api.zerobounce.net';

{ Per-thread, like the validation key. }
threadvar
    ZbWarmupApiKey: string;

procedure ZbWarmupInitialize(ApiKey: String);

function ZbWarmupDefaultAddSendingAccountOptions: TZbWarmupAddSendingAccountOptions;
function ZbWarmupDefaultListFromAddressesOptions: TZbWarmupListFromAddressesOptions;

{ Sending accounts }
function ZbWarmupListSendingAccounts: String;
function ZbWarmupGetSendingAccount(const FromAddress: String): String;
function ZbWarmupAddSendingAccount(const FromAddress, OnboardingModel: String;
    Options: PTZbWarmupAddSendingAccountOptions): String;
function ZbWarmupUpdateSendingAccount(const FromAddress: String;
    EngagementRuleId, StatusId: Integer; AutoEnable: Boolean): String;
procedure ZbWarmupDeleteSendingAccount(const FromAddress: String);
procedure ZbWarmupChangeFromAddress(const OldFromAddress, NewFromAddress: String);
function ZbWarmupListFromAddresses(Options: PTZbWarmupListFromAddressesOptions): String;

{ Engagement rules }
function ZbWarmupGetEngagementRules: String;
procedure ZbWarmupSetEngagementRule(const CFrom: String; RuleId: Integer);
function ZbWarmupGetCustomEngagement(const CFrom: String): String;
function ZbWarmupCreateCustomEngagement(const CFrom: String;
    ReplyPct, ImportantPct, ClickPct: Integer): String;
function ZbWarmupUpdateCustomEngagement(const CFrom: String;
    ReplyPct, ImportantPct, ClickPct: Integer): String;
function ZbWarmupDeleteCustomEngagement(const CFrom: String;
    EngagementRuleId: Integer): String;

{ Seeds, progress and statistics }
function ZbWarmupGetSeeds: String;
function ZbWarmupDownloadSeeds: String; overload;
function ZbWarmupDownloadSeeds(const DownloadPath: String): String; overload;
function ZbWarmupGetSeedCount: String;
function ZbWarmupGetProgressToday: String;
function ZbWarmupGetSentToday: String;
function ZbWarmupGetEmailsPerDay: String;
function ZbWarmupGetStatistics(const FromAddress: String): String;
function ZbWarmupGetStatisticsPerDay(const FromAddress, StartDate, EndDate: String): String;
function ZbWarmupGetStatisticsDetailed(const CFrom: String): String;
function ZbWarmupDownloadStatistics(const CFrom: String): String; overload;
function ZbWarmupDownloadStatistics(const CFrom, DownloadPath: String): String; overload;
function ZbWarmupGetFromAddressStatistics(const FromAddress: String): String;

{ Deliverability checks }
function ZbWarmupCheckDomainDmarc(const FromAddress: String): String; overload;
function ZbWarmupCheckDomainDmarc(const FromAddress, DkimSelector: String): String; overload;
function ZbWarmupTestSmtpConfiguration(const FromAddress, Username, Password,
    Server: String; Port: Integer): String;

procedure Register;

implementation

procedure ZbWarmupInitialize(ApiKey: String);
begin
    if Trim(ApiKey) = '' then
        raise ZbException.Create('Empty parameter: api_key');
    ZbWarmupApiKey := ZbCheckedApiKey(ApiKey);
end;

function ZbWarmupDefaultAddSendingAccountOptions: TZbWarmupAddSendingAccountOptions;
begin
    FillChar(Result, SizeOf(Result), 0);
end;

function ZbWarmupDefaultListFromAddressesOptions: TZbWarmupListFromAddressesOptions;
begin
    FillChar(Result, SizeOf(Result), 0);
end;

procedure RequireWarmupKey;
begin
    if Trim(ZbWarmupApiKey) = '' then
        raise ZbException.Create(
            'The Warmup API is not initialized. Please call ZbWarmupInitialize(api_key) first'
        );
end;

{ The Warmup key never travels in a URL, but a transport error may quote the
  header, and test-smtp-configuration carries SMTP credentials in its body. }
function WarmupRedact(const Value: String; const Secrets: array of String): String;
var
    I: Integer;
begin
    Result := Value;
    if ZbWarmupApiKey <> '' then
        Result := StringReplace(Result, ZbWarmupApiKey, 'REDACTED', [rfReplaceAll]);
    for I := Low(Secrets) to High(Secrets) do
        if Secrets[I] <> '' then
            Result := StringReplace(Result, Secrets[I], 'REDACTED', [rfReplaceAll]);
end;

function RequiredParam(const Value, Name: String): String;
begin
    if Trim(Value) = '' then
        raise ZbException.Create(Concat('Empty parameter: ', Name));
    Result := Trim(Value);
end;

function InRange(Value, Low_, High_: Integer; const Name: String): Integer;
begin
    if (Value < Low_) or (Value > High_) then
        raise ZbException.Create(Format(
            'Invalid parameter: %s (must be %d to %d)', [Name, Low_, High_]
        ));
    Result := Value;
end;

function IsoDate(const Value, Name: String): String;
var
    I: Integer;
begin
    Result := Trim(Value);
    if Length(Result) <> 10 then
        raise ZbException.Create(Concat('Invalid parameter: ', Name, ' (expected YYYY-MM-DD)'));
    for I := 1 to 10 do
        if I in [5, 8] then
        begin
            if Result[I] <> '-' then
                raise ZbException.Create(Concat('Invalid parameter: ', Name, ' (expected YYYY-MM-DD)'));
        end
        else if not (Result[I] in ['0'..'9']) then
            raise ZbException.Create(Concat('Invalid parameter: ', Name, ' (expected YYYY-MM-DD)'));
end;

function JsonString(const Name, Value: String): String;
begin
    Result := Concat('"', Name, '":"', ZbJsonEscape(Value), '"');
end;

function JsonBool(const Name: String; Value: Boolean): String;
begin
    if Value then
        Result := Concat('"', Name, '":true')
    else
        Result := Concat('"', Name, '":false');
end;

function JsonInt(const Name: String; Value: Integer): String;
begin
    Result := Concat('"', Name, '":', IntToStr(Value));
end;

{ A JSON array of the documented email types, from a comma-separated list. }
function ZbWarmupEmailTypesJson(const TypesOfEmails: String): String;
var
    Parts: TStringList;
    I: Integer;
    Item: String;
begin
    Result := '';
    Parts := TStringList.Create;
    try
        Parts.Delimiter := ',';
        Parts.StrictDelimiter := True;
        Parts.DelimitedText := TypesOfEmails;
        for I := 0 to Parts.Count - 1 do
        begin
            Item := LowerCase(Trim(Parts[I]));
            if Item = '' then
                Continue;
            if (Item <> 'transactional') and (Item <> 'newsletter') and (Item <> 'marketing') then
                raise ZbException.Create(
                    'Invalid parameter: types_of_emails (transactional, newsletter and/or marketing)'
                );
            if Result <> '' then
                Result := Concat(Result, ',');
            Result := Concat(Result, '"', Item, '"');
        end;
    finally
        Parts.Free;
    end;
    Result := Concat('[', Result, ']');
end;

procedure AddField(var Body: String; const Field: String);
begin
    if Body <> '' then
        Body := Concat(Body, ',');
    Body := Concat(Body, Field);
end;

procedure AddQuery(var Query: String; const Name, Value: String);
begin
    if Query = '' then
        Query := '?'
    else
        Query := Concat(Query, '&');
    Query := Concat(Query, Name, '=', EncodeParam(Value));
end;

{ One Warmup request. Returns the raw body; HTTP 204 and an empty 200 are the
  documented empty successes and come back as ''. }
function WarmupSend(const Method, Path, Query, Body: String;
    const Secrets: array of String): TZbRequestResponse;
var
    Url: String;
begin
    RequireWarmupKey;
    Url := Concat(ZbWarmupBaseURL, ZB_WARMUP_BASE_PATH, Path, Query);
    try
        Result := ZBSendForeignKeyRequest(
            Method, Url, Body, JSON_CONTENT_TYPE, 'API-Key', ZbWarmupApiKey
        );
    except
        on e: ZbException do
        begin
            e.Message := WarmupRedact(e.Message, Secrets);
            e.Payload := WarmupRedact(e.Payload, Secrets);
            raise;
        end;
        on e: Exception do
            raise ZbException.Create(WarmupRedact(e.Message, Secrets));
    end;
end;

function WarmupJson(const Method, Path, Query, Body: String): String;
begin
    Result := Trim(WarmupSend(Method, Path, Query, Body, []).Payload);
end;

function WarmupJsonWithSecrets(const Method, Path, Query, Body: String;
    const Secrets: array of String): String;
begin
    Result := Trim(WarmupSend(Method, Path, Query, Body, Secrets).Payload);
end;

{ A CSV download. The API labels an error as JSON or HTML, so neither is a
  file. }
function WarmupCsv(const Path, Query, DownloadPath: String): String;
var
    Response: TZbRequestResponse;
    Handle: TFileStream;
    Directory: String;
begin
    Response := WarmupSend('GET', Path, Query, '', []);
    Result := Response.Payload;
    if (Trim(Result) = '') or (Pos(JSON_CONTENT_TYPE, LowerCase(Response.ContentType)) > 0) then
        raise ZbException.FromResponse('', Response);
    if DownloadPath = '' then
        Exit;
    Directory := ExtractFilePath(DownloadPath);
    if (Directory <> '') and not DirectoryExists(Directory) then
        ForceDirectories(Directory);
    Handle := TFileStream.Create(DownloadPath, fmCreate);
    try
        if Length(Result) > 0 then
            Handle.WriteBuffer(Result[1], Length(Result));
    finally
        Handle.Free;
    end;
end;

{ -- sending accounts ---------------------------------------------------- }

function ZbWarmupListSendingAccounts: String;
begin
    { An empty team answers with an empty list, not a 404. }
    Result := WarmupJson('GET', '/sending-accounts', '', '');
end;

function ZbWarmupGetSendingAccount(const FromAddress: String): String;
var
    Query: String;
begin
    Query := '';
    AddQuery(Query, 'from_address', RequiredParam(FromAddress, 'from_address'));
    Result := WarmupJson('GET', '/sending-account', Query, '');
end;

function ZbWarmupAddSendingAccount(const FromAddress, OnboardingModel: String;
    Options: PTZbWarmupAddSendingAccountOptions): String;
var
    Body, Content, Sending: String;
begin
    RequiredParam(FromAddress, 'from_address');
    if (OnboardingModel <> ZB_WARMUP_SELF_MANAGED) and (OnboardingModel <> ZB_WARMUP_ZB_MANAGED) then
        raise ZbException.Create(
            'Invalid parameter: onboarding_model (must be self_managed or zb_managed)'
        );

    Body := '';
    AddField(Body, JsonString('from_address', Trim(FromAddress)));
    AddField(Body, JsonString('onboarding_model', OnboardingModel));

    if OnboardingModel = ZB_WARMUP_SELF_MANAGED then
    begin
        if (Options = nil) or not Options^.EngagementRuleIdSpecified then
            raise ZbException.Create(
                'Empty parameter: engagement_rule_id (required for self_managed)'
            );
        AddField(Body, JsonInt(
            'engagement_rule_id',
            InRange(Options^.EngagementRuleId, 0, 4, 'engagement_rule_id')
        ));
        if (Options^.EmailContent <> nil) or (Options^.ManagedSending <> nil) then
            raise ZbException.Create(
                'Invalid parameter: email_content and managed_sending are for zb_managed'
            );
    end;

    if Options <> nil then
    begin
        if Options^.IgnoreConflictSpecified then
            AddField(Body, JsonBool('ignore_conflict', Options^.IgnoreConflict));

        if Options^.EmailContent <> nil then
        begin
            Content := '';
            if Options^.EmailContent^.CompanyName <> '' then
                AddField(Content, JsonString('company_name', Options^.EmailContent^.CompanyName));
            if Options^.EmailContent^.CompanyWebsite <> '' then
                AddField(Content, JsonString('company_website', Options^.EmailContent^.CompanyWebsite));
            if Options^.EmailContent^.CompanyDescription <> '' then
                AddField(Content, JsonString('company_description', Options^.EmailContent^.CompanyDescription));
            if Options^.EmailContent^.TypesOfEmails <> '' then
                AddField(Content, Concat(
                    '"types_of_emails":', ZbWarmupEmailTypesJson(Options^.EmailContent^.TypesOfEmails)
                ));
            if Options^.EmailContent^.ExampleEmail <> '' then
                AddField(Content, JsonString('example_email', Options^.EmailContent^.ExampleEmail));
            AddField(Body, Concat('"email_content":{', Content, '}'));
        end;

        if Options^.ManagedSending <> nil then
        begin
            Sending := '';
            if Options^.ManagedSending^.WeekdaysOnlySpecified then
                AddField(Sending, JsonBool('weekdays_only', Options^.ManagedSending^.WeekdaysOnly));
            if Options^.ManagedSending^.Timezone <> '' then
                AddField(Sending, JsonString('timezone', Options^.ManagedSending^.Timezone));
            if Options^.ManagedSending^.TimezoneOffsetSpecified then
                AddField(Sending, JsonInt(
                    'timezone_offset',
                    InRange(Options^.ManagedSending^.TimezoneOffset, -11, 14, 'timezone_offset')
                ));
            if Options^.ManagedSending^.WarmupStartDate <> '' then
                AddField(Sending, JsonString(
                    'warmup_start_date',
                    IsoDate(Options^.ManagedSending^.WarmupStartDate, 'warmup_start_date')
                ));
            AddField(Body, Concat('"managed_sending":{', Sending, '}'));
        end;
    end;

    { For a ZB managed account a 200 means accepted; the mailbox is
      provisioned afterwards. }
    Result := WarmupJson('POST', '/sending-accounts', '', Concat('{', Body, '}'));
end;

function ZbWarmupUpdateSendingAccount(const FromAddress: String;
    EngagementRuleId, StatusId: Integer; AutoEnable: Boolean): String;
var
    Body: String;
begin
    RequiredParam(FromAddress, 'from_address');
    Body := '';
    AddField(Body, JsonInt('team_id', ZB_WARMUP_ANY_TEAM_ID));
    AddField(Body, JsonString('from_address', Trim(FromAddress)));
    AddField(Body, JsonInt('engagement_rule_id',
        InRange(EngagementRuleId, 0, 4, 'engagement_rule_id')));
    { 0 to 8; 4 is Paused and 5 is Disabled. }
    AddField(Body, JsonInt('status_id', InRange(StatusId, 0, 8, 'status_id')));
    { auto_enable is accepted only for an account that is not active. }
    AddField(Body, JsonBool('auto_enable', AutoEnable));
    Result := WarmupJson('PUT', '/sending-accounts', '', Concat('{', Body, '}'));
end;

procedure ZbWarmupDeleteSendingAccount(const FromAddress: String);
var
    Query: String;
begin
    { Destructive and cannot be undone. }
    Query := '';
    AddQuery(Query, 'from_address', RequiredParam(FromAddress, 'from_address'));
    WarmupJson('DELETE', '/sending-accounts', Query, '');
end;

procedure ZbWarmupChangeFromAddress(const OldFromAddress, NewFromAddress: String);
var
    Body: String;
begin
    { Despite the path, nothing about SMTP credentials is stored. }
    Body := '';
    AddField(Body, JsonInt('team_id', ZB_WARMUP_ANY_TEAM_ID));
    AddField(Body, JsonString('old_from_address',
        RequiredParam(OldFromAddress, 'old_from_address')));
    AddField(Body, JsonString('new_from_address',
        RequiredParam(NewFromAddress, 'new_from_address')));
    WarmupJson('PUT', '/smtp-credentials', '', Concat('{', Body, '}'));
end;

function ZbWarmupListFromAddresses(Options: PTZbWarmupListFromAddressesOptions): String;
var
    Query: String;
    I: Integer;
begin
    Query := '';
    if Options <> nil then
    begin
        if Options^.SearchFrom <> '' then
        begin
            for I := 1 to Length(Options^.SearchFrom) do
                if not (Options^.SearchFrom[I] in
                        ['A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_', '@', '+']) then
                    raise ZbException.Create(
                        'Invalid parameter: search_from (letters, digits and . - _ @ + only)'
                    );
            AddQuery(Query, 'search_from', Options^.SearchFrom);
        end;
        { The API defaults these to 0 and 100. }
        if Options^.OffsetSpecified then
            AddQuery(Query, 'offset', IntToStr(InRange(Options^.Offset, 0, MaxInt, 'offset')));
        if Options^.LimitSpecified then
            AddQuery(Query, 'limit', IntToStr(InRange(Options^.Limit, 0, MaxInt, 'limit')));
    end;
    Result := WarmupJson('GET', '/from-addresses', Query, '');
end;

{ -- engagement rules ---------------------------------------------------- }

function ZbWarmupGetEngagementRules: String;
begin
    { -1 Custom, 0 Default, 1 Slow Pace, 2 Repair, 3 Boost, 4 Protect. }
    Result := WarmupJson('GET', '/engagement-rules', '', '');
end;

procedure ZbWarmupSetEngagementRule(const CFrom: String; RuleId: Integer);
var
    Query: String;
begin
    Query := '';
    AddQuery(Query, 'c_from', RequiredParam(CFrom, 'c_from'));
    WarmupJson('PUT', '/engagement-rule', Query,
        Concat('{', JsonInt('rule_id', InRange(RuleId, 0, 4, 'rule_id')), '}'));
end;

function ZbWarmupGetCustomEngagement(const CFrom: String): String;
begin
    Result := WarmupJson(
        'GET', Concat('/custom-engagement/', ZbPathSegment(CFrom, 'c_from')), '', ''
    );
end;

{ reply_pct 5-45, important_pct 5-40, click_pct 0-70 and at most
  important_pct x 1.75. }
function CustomEngagementBody(const CFrom: String;
    ReplyPct, ImportantPct, ClickPct: Integer): String;
var
    Body: String;
begin
    RequiredParam(CFrom, 'c_from');
    InRange(ReplyPct, 5, 45, 'reply_pct');
    InRange(ImportantPct, 5, 40, 'important_pct');
    InRange(ClickPct, 0, 70, 'click_pct');
    if ClickPct * 100 > ImportantPct * 175 then
        raise ZbException.Create('Invalid parameter: click_pct (at most important_pct x 1.75)');
    Body := '';
    AddField(Body, JsonString('c_from', Trim(CFrom)));
    AddField(Body, JsonInt('reply_pct', ReplyPct));
    AddField(Body, JsonInt('important_pct', ImportantPct));
    AddField(Body, JsonInt('click_pct', ClickPct));
    Result := Concat('{', Body, '}');
end;

function ZbWarmupCreateCustomEngagement(const CFrom: String;
    ReplyPct, ImportantPct, ClickPct: Integer): String;
begin
    { Starts a 7 day cooldown on updates. }
    Result := WarmupJson('POST', '/custom-engagement', '',
        CustomEngagementBody(CFrom, ReplyPct, ImportantPct, ClickPct));
end;

function ZbWarmupUpdateCustomEngagement(const CFrom: String;
    ReplyPct, ImportantPct, ClickPct: Integer): String;
begin
    { During the 7 day cooldown this fails with HTTP 429. }
    Result := WarmupJson('PUT', '/custom-engagement', '',
        CustomEngagementBody(CFrom, ReplyPct, ImportantPct, ClickPct));
end;

function ZbWarmupDeleteCustomEngagement(const CFrom: String;
    EngagementRuleId: Integer): String;
var
    Body: String;
begin
    { Falls back to standard rule EngagementRuleId, 1 to 4. }
    Body := '';
    AddField(Body, JsonString('c_from', RequiredParam(CFrom, 'c_from')));
    AddField(Body, JsonInt('engagement_rule_id',
        InRange(EngagementRuleId, 1, 4, 'engagement_rule_id')));
    Result := WarmupJson('DELETE', '/custom-engagement', '', Concat('{', Body, '}'));
end;

{ -- seeds, progress and statistics -------------------------------------- }

function ZbWarmupGetSeeds: String;
begin
    Result := WarmupJson('GET', '/seeds', '', '');
end;

function ZbWarmupDownloadSeeds: String;
begin
    Result := ZbWarmupDownloadSeeds('');
end;

function ZbWarmupDownloadSeeds(const DownloadPath: String): String;
begin
    { CSV with the columns seed,provider. }
    Result := WarmupCsv('/seeds/download', '', DownloadPath);
end;

function ZbWarmupGetSeedCount: String;
begin
    { Answers a bare number, not an object. }
    Result := WarmupJson('GET', '/seed-count', '', '');
end;

function ZbWarmupGetProgressToday: String;
begin
    Result := WarmupJson('GET', '/progress-today', '', '');
end;

function ZbWarmupGetSentToday: String;
begin
    Result := WarmupJson('GET', '/sent-today', '', '');
end;

function ZbWarmupGetEmailsPerDay: String;
begin
    Result := WarmupJson('GET', '/emails-per-day', '', '');
end;

function ZbWarmupGetStatistics(const FromAddress: String): String;
var
    Query: String;
begin
    { An address with no history answers with zeros, not an error. }
    Query := '';
    AddQuery(Query, 'from_address', RequiredParam(FromAddress, 'from_address'));
    Result := WarmupJson('GET', '/statistics', Query, '');
end;

function ZbWarmupGetStatisticsPerDay(const FromAddress, StartDate, EndDate: String): String;
var
    Query, StartsAt, EndsAt: String;
begin
    RequiredParam(FromAddress, 'from_address');
    StartsAt := IsoDate(StartDate, 'start_date');
    EndsAt := IsoDate(EndDate, 'end_date');
    if StartsAt > EndsAt then
        raise ZbException.Create('Invalid parameter: start_date is later than end_date');
    Query := '';
    AddQuery(Query, 'from_address', Trim(FromAddress));
    AddQuery(Query, 'start_date', StartsAt);
    AddQuery(Query, 'end_date', EndsAt);
    Result := WarmupJson('GET', '/statistics-per-day', Query, '');
end;

function ZbWarmupGetStatisticsDetailed(const CFrom: String): String;
var
    Query: String;
begin
    Query := '';
    AddQuery(Query, 'c_from', RequiredParam(CFrom, 'c_from'));
    Result := WarmupJson('GET', '/statistics-detailed', Query, '');
end;

function ZbWarmupDownloadStatistics(const CFrom: String): String;
begin
    Result := ZbWarmupDownloadStatistics(CFrom, '');
end;

function ZbWarmupDownloadStatistics(const CFrom, DownloadPath: String): String;
var
    Query: String;
begin
    Query := '';
    AddQuery(Query, 'c_from', RequiredParam(CFrom, 'c_from'));
    Result := WarmupCsv('/statistics/download', Query, DownloadPath);
end;

function ZbWarmupGetFromAddressStatistics(const FromAddress: String): String;
var
    Query: String;
begin
    Query := '';
    AddQuery(Query, 'from_address', RequiredParam(FromAddress, 'from_address'));
    Result := WarmupJson('GET', '/from-address-statistics', Query, '');
end;

{ -- deliverability checks ----------------------------------------------- }

function ZbWarmupCheckDomainDmarc(const FromAddress: String): String;
begin
    Result := ZbWarmupCheckDomainDmarc(FromAddress, '');
end;

function ZbWarmupCheckDomainDmarc(const FromAddress, DkimSelector: String): String;
var
    Query: String;
begin
    RequiredParam(FromAddress, 'from_address');
    { LIVE: this takes an email address. The docs describe a domain, but a bare
      domain is answered with "Please provide a valid email address". }
    if Pos('@', FromAddress) = 0 then
        raise ZbException.Create(
            'Invalid parameter: from_address (an email address, e.g. sender@example.com)'
        );
    Query := '';
    AddQuery(Query, 'from_address', Trim(FromAddress));
    if DkimSelector <> '' then
        AddQuery(Query, 'dkim_selector', DkimSelector);
    { Answers SPF_status, DKIM_status, DMARC_status and the dkim_selector that
      was checked. }
    Result := WarmupJson('GET', '/check-domain-dmarc', Query, '');
end;

function ZbWarmupTestSmtpConfiguration(const FromAddress, Username, Password,
    Server: String; Port: Integer): String;
var
    Body: String;
begin
    { Nothing is stored, and the test message goes to a ZeroBounce test
      recipient, not FromAddress.

      LIVE: a failed SMTP login is answered with HTTP 500, not a 4xx. }
    Body := '';
    AddField(Body, JsonString('from_address', RequiredParam(FromAddress, 'from_address')));
    AddField(Body, JsonString('username', RequiredParam(Username, 'username')));
    AddField(Body, JsonString('password', RequiredParam(Password, 'password')));
    AddField(Body, JsonString('server', RequiredParam(Server, 'server')));
    AddField(Body, JsonInt('port', InRange(Port, 1, 65535, 'port')));
    Result := WarmupJsonWithSecrets(
        'POST', '/test-smtp-configuration', '', Concat('{', Body, '}'),
        [Password, Username]
    );
end;

procedure Register;
begin
end;

initialization
    ZbWarmupApiKey := '';

end.
