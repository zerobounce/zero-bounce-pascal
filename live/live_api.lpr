program live_api;

{ Opt-in live API suite: calls the SDK against the real API to check payloads
  and response shapes.

  It is its own program, outside tests/, so the offline suite never builds or
  runs it:

    fpc -Fu./src -Mobjfpc -FE/tmp live/live_api.lpr
    export ZEROBOUNCE_API_KEY=...          # validation API (skipped without it)
    export ZEROBOUNCE_WARMUP_API_KEY=...   # Warmup API (skipped without it)
    /tmp/live_api

  By default only free calls run: sandbox addresses (no credits), reads, and
  requests expected to fail (unknown file ids, S3/SFTP without an integration)
  that still prove the API accepts the payload. The free tier checks that the
  credit balance did not change. Two flags opt in to more:

    ZEROBOUNCE_LIVE_SPEND=1     spends credits: scoring, activity, email
                                finder, and a bulk round trip whose job is
                                deleted again
    ZEROBOUNCE_LIVE_CHANGES=1   reversible account changes, each undone even
                                when a test fails: a test filter, and a
                                self-managed Warmup sending account

  Never called, because the API cannot undo them: creating an Inbox Placement
  test (it cannot be deleted while running), setting the Inbox Placement
  webhook (it cannot be cleared), and ZB managed Warmup onboarding (it starts
  mailbox provisioning).

  No key is ever written to a file or printed. Keys come from the environment. }

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
    cthreads,
{$ENDIF}
    Classes, SysUtils, DateUtils,
    ZbBulk, ZbGeneric, ZbPriority3, ZbStructures, ZbUtility, ZbValidation, ZbWarmup;

const
    { Well formed but matching no file: a malformed id can be Cloudflare
      blocked instead of answered. }
    UNKNOWN_ID = '0d1b7a4e-1111-2222-3333-444455556666';
    CALLBACK = 'https://example.com/zerobounce-sdk-live-test';
    SANDBOX = 'valid@example.com';

var
    Passed: Integer = 0;
    Failed: Integer = 0;
    Skipped: Integer = 0;
    Failures: TStringList;
    ApiKey: String = '';
    WarmupKey: String = '';
    SpendFlag: Boolean = False;
    ChangesFlag: Boolean = False;

function Env(const Name: String): String;
begin
    Result := Trim(GetEnvironmentVariable(Name));
end;

function Redact(const Text: String): String;
begin
    Result := Text;
    if ApiKey <> '' then
        Result := StringReplace(Result, ApiKey, '***', [rfReplaceAll]);
    if WarmupKey <> '' then
        Result := StringReplace(Result, WarmupKey, '***', [rfReplaceAll]);
end;

procedure Pass(const Name: String);
begin
    Inc(Passed);
    WriteLn('  + ', Name);
end;

procedure Fail_(const Name, Problem: String);
begin
    Inc(Failed);
    Failures.Add(Concat(Name, ': ', Redact(Problem)));
    WriteLn('  X ', Name);
    WriteLn('      ', Redact(Problem));
end;

procedure Skip(const Name, Reason: String);
begin
    Inc(Skipped);
    WriteLn('  - SKIP ', Name, ' (', Reason, ')');
end;

{ A check either passes, or records why it did not. The body returns '' when
  all is well. }
type
    TCheckBody = function: String;

procedure Check(const Name: String; Body: TCheckBody);
var
    Problem: String;
begin
    try
        Problem := Body();
    except
        on e: Exception do
            Problem := Redact(e.Message);
    end;
    if Problem = '' then
        Pass(Name)
    else
        Fail_(Name, Problem);
end;

function DaysAgoIso(Days: Integer): String;
begin
    Result := FormatDateTime('yyyy-mm-dd', IncDay(Date, -Days));
end;

{ -- validation API, free ------------------------------------------------- }

var
    CreditsBefore: Integer = -2;

function CheckCredits: String;
begin
    CreditsBefore := ZbGetCredits;
    if CreditsBefore < -1 then
        Exit('getcredits did not answer a balance');
    Result := '';
end;

function CheckApiUsage: String;
begin
    ZbGetApiUsage(IncDay(Date, -7), Date);
    Result := '';
end;

function CheckValidateSandbox: String;
var
    Validation: TZbValidationResult;
begin
    Validation := ZbValidateEmail(SANDBOX);
    if Validation.Address <> SANDBOX then
        Exit('address did not echo');
    Result := '';
end;

function CheckValidatePost: String;
var
    Options: TZbValidateOptions;
begin
    { No billable add-on here: activity_data, verify_plus and domain_info are
      charged even on a sandbox address, so they live in the SPEND tier. }
    Options := ZbDefaultValidateOptions;
    Options.UsePost := 1;
    Options.Timeout := 30;
    ZbValidateEmail(SANDBOX, '', Options);
    Result := '';
end;

function CheckValidateBatchTimeoutIsAString: String;
var
    Options: TZbValidateOptions;
    Batch: TZBBatchValidation;
    Emails: array[0..0] of TZbEmailAndIp;
    I: Integer;
begin
    { LIVE: sent as a JSON number the API rejects the whole batch with the
      account-level "Invalid API Key or your account ran out of credits". }
    Options := ZbDefaultValidateOptions;
    Options.Timeout := 30;
    Emails[0].Email := SANDBOX;
    Emails[0].Ip := '';
    Batch := ZbBatchValidateEmails(Emails, Options);
    for I := 0 to Batch.ErrorsLength - 1 do
        if LowerCase(Batch.Errors[I].EmailAddress) = 'all' then
            Exit(Concat('account-level error: ', Batch.Errors[I].Error));
    Result := '';
end;

function CheckUnknownFileIdIsShort: String;
begin
    try
        ZbBulkValidationFileStatusCheck(UNKNOWN_ID);
        Exit('filestatus unexpectedly succeeded');
    except
        on e: Exception do
        begin
            if Length(e.Message) >= 500 then
                Exit('message not short and clean');
            if Pos('<html', LowerCase(e.Message)) > 0 then
                Exit('message is an HTML page');
            Exit('');
        end;
    end;
end;

function CheckListS3Files: String;
begin
    { No S3 integration configured is an expected, documented failure; only an
      unreadable error is a problem. }
    try
        ZbListS3Files('', '');
    except
        on e: Exception do
            if Length(e.Message) >= 500 then
                Exit('message too long');
    end;
    Result := '';
end;

function CheckS3JobNeedsReturnUrl: String;
var
    Options: TZbBulkFileJobOptions;
begin
    Options := ZbDefaultBulkFileJobOptions;
    Options.FileName := 'f.csv';
    try
        ZbEmailFinderS3File(Options);
        Exit('a payload without return_url was accepted');
    except
        on e: Exception do
            if Pos('return_url', e.Message) = 0 then
                Exit(Concat('unexpected message: ', e.Message));
    end;
    Result := '';
end;

function CheckCompleteS3Payload: String;
var
    Options: TZbBulkFileJobOptions;
begin
    { Without an S3 integration this fails on the integration, not the
      payload. }
    Options := ZbDefaultBulkFileJobOptions;
    Options.FileName := 'f.csv';
    Options.ReturnUrl := CALLBACK;
    try
        ZbEmailFinderS3File(Options);
    except
        on e: Exception do
            if (Pos('return URL is not permitted', e.Message) > 0) or
               (Pos('This field is required', e.Message) > 0) then
                Exit(Concat('payload rejected: ', Redact(e.Message)));
    end;
    Result := '';
end;

function CheckFiltersList: String;
begin
    ZbListFilters(False);
    Result := '';
end;

function CheckInboxPlacementBearer: String;
begin
    { LIVE: an api_key parameter is rejected as unauthorized here. }
    ZbInboxPlacementProviders;
    Result := '';
end;

function CheckInboxPlacementTestTypes: String;
begin
    ZbInboxPlacementTestTypes;
    Result := '';
end;

function CheckInboxPlacementListing: String;
var
    Options: TZbListInboxPlacementTestsOptions;
begin
    { LIVE: the listing is GET /tests (plural); /test is the single-test path
      and answers 'Method "GET" not allowed'. }
    FillChar(Options, SizeOf(Options), 0);
    Options.PageSpecified := True;
    Options.Page := 1;
    Options.PageSizeSpecified := True;
    Options.PageSize := 10;
    ZbListInboxPlacementTests(@Options);
    ZbInboxPlacementWebhook;
    Result := '';
end;

function CheckListEvaluatorUnknownId: String;
begin
    try
        ZbGetListEvaluatorResult(UNKNOWN_ID);
        Exit('an unknown evaluation id unexpectedly succeeded');
    except
        on e: Exception do
            if Length(e.Message) >= 500 then
                Exit('message too long');
    end;
    Result := '';
end;

function CheckSendEvaluatedFileIsAForm: String;
begin
    { LIVE: the file id is a form field. Appended to the path it is a 404. }
    try
        ZbSendEvaluatedFile(UNKNOWN_ID);
        Exit('an unknown evaluation id unexpectedly succeeded');
    except
        on e: Exception do
            if Pos('not allowed', e.Message) > 0 then
                Exit(Concat('wrong method or path: ', Redact(e.Message)));
    end;
    Result := '';
end;

function CheckCreditsUnchanged: String;
var
    After: Integer;
begin
    After := ZbGetCredits;
    if After <> CreditsBefore then
        Exit(Format('credits went from %d to %d', [CreditsBefore, After]));
    Result := '';
end;

procedure ValidationFree;
begin
    WriteLn('Validation API (free calls)');
    Check('getcredits returns a balance', @CheckCredits);
    Check('getapiusage parses the documented date formats', @CheckApiUsage);
    Check('validate on a sandbox address spends nothing', @CheckValidateSandbox);
    Check('validate works over POST as well as GET', @CheckValidatePost);
    Check('LIVE: validatebatch accepts timeout as a string', @CheckValidateBatchTimeoutIsAString);
    Check('an unknown file id is a short, clean error', @CheckUnknownFileIdIsShort);
    Check('listS3files answers (empty without an S3 integration)', @CheckListS3Files);
    Check('LIVE: S3/SFTP jobs reject a payload without return_url before sending',
          @CheckS3JobNeedsReturnUrl);
    Check('LIVE: a complete S3 job payload is accepted by the API', @CheckCompleteS3Payload);
    Check('filters list', @CheckFiltersList);
    Check('LIVE: the Inbox Placement Tester accepts a Bearer key', @CheckInboxPlacementBearer);
    Check('inbox placement test types', @CheckInboxPlacementTestTypes);
    Check('LIVE: inbox placement tests are listed from /tests, and the webhook reads back',
          @CheckInboxPlacementListing);
    Check('list evaluator result for an unknown id fails cleanly', @CheckListEvaluatorUnknownId);
    Check('LIVE: sendEvaluatedFile posts the file id as a form field',
          @CheckSendEvaluatedFileIsAForm);
    Check('the free calls did not change the credit balance', @CheckCreditsUnchanged);
end;

{ -- validation API, spends credits --------------------------------------- }

var
    BulkFileId: String = '';

function CheckValidateEveryOption: String;
var
    Options: TZbValidateOptions;
begin
    { LIVE: activity_data, verify_plus and domain_info are billable add-ons and
      are charged even for a sandbox address, so this is not free. }
    Options := ZbDefaultValidateOptions;
    Options.Timeout := 30;
    Options.ActivityData := 1;
    Options.VerifyPlus := 1;
    Options.DomainInfo := 1;
    Options.GreylistProcessing := 1;
    Options.CallbackUrl := CALLBACK;
    ZbValidateEmail(SANDBOX, '', Options);
    Result := '';
end;

function CheckScoreEmail: String;
begin
    ZbScoreEmail(SANDBOX);
    Result := '';
end;

function CheckScoreEmailPost: String;
begin
    ZbScoreEmailPost(SANDBOX);
    Result := '';
end;

function CheckActivityData: String;
begin
    ZbActivityData(SANDBOX);
    Result := '';
end;

function CheckEmailFinder: String;
begin
    ZbFindEmailByDomain('example.com', 'John', 'Doe');
    Result := '';
end;

function CheckDomainSearch: String;
begin
    ZbDomainSearchByDomain('example.com');
    Result := '';
end;

function CheckBulkRoundTrip: String;
var
    Params: TZbBulkParams;
    Feedback: TZbFileFeedback;
    Status: TZbFileStatus;
begin
    FillChar(Params, SizeOf(Params), 0);
    Params.EmailAddressColumn := 1;
    Params.HasHeaderRow := True;
    Feedback := ZbBulkValidationFileSubmit(
        'email'#10'valid@example.com'#10'invalid@example.com'#10, Params
    );
    if Feedback.FileId = '' then
        Exit('no file_id in the upload response');
    BulkFileId := Feedback.FileId;
    Status := ZbBulkValidationFileStatusCheck(BulkFileId);
    if Status.FileId <> BulkFileId then
        Exit('status did not echo the file id');
    Result := '';
end;

function CheckBulkJobDeleted: String;
var
    Attempt: Integer;
    Last: String;
begin
    { LIVE: a just-accepted job answers "File cannot be found" for a short
      while, so the delete is retried rather than leaving it behind. }
    Last := '';
    for Attempt := 1 to 12 do
    begin
        try
            ZbBulkValidationResultDelete(BulkFileId);
            Exit('');
        except
            on e: Exception do
                Last := Redact(e.Message);
        end;
        Sleep(10000);
    end;
    Result := Concat('could not delete ', BulkFileId, ' after retrying: ', Last);
end;

procedure ValidationSpend;
begin
    WriteLn;
    WriteLn('Validation API (ZEROBOUNCE_LIVE_SPEND=1)');
    Check('validate accepts every documented option', @CheckValidateEveryOption);
    Check('scoreEmail', @CheckScoreEmail);
    Check('scoreEmail over POST', @CheckScoreEmailPost);
    Check('getActivityData', @CheckActivityData);
    Check('email finder', @CheckEmailFinder);
    Check('domain search', @CheckDomainSearch);
    Check('bulk validation round trip: upload, status, delete', @CheckBulkRoundTrip);
    if BulkFileId = '' then
        Skip('the uploaded validation job is deleted again',
             'upload did not produce a file id')
    else
        Check('the uploaded validation job is deleted again', @CheckBulkJobDeleted);
end;

{ -- validation API, reversible account changes ---------------------------- }

var
    FilterValue: String = '';
    FilterAdded: Boolean = False;

function CheckAddFilter: String;
begin
    ZbAddFilter('allow', 'domain', FilterValue);
    FilterAdded := True;
    Result := '';
end;

function CheckDeleteFilter: String;
begin
    ZbDeleteFilter('allow', 'domain', FilterValue);
    Result := '';
end;

procedure ValidationChanges;
begin
    WriteLn;
    WriteLn('Validation API (ZEROBOUNCE_LIVE_CHANGES=1)');
    FilterValue := Format('zb-sdk-live-%d.example.com', [DateTimeToUnix(Now)]);
    Check('addFilter creates a reversible allow rule', @CheckAddFilter);
    if not FilterAdded then
        Skip('the test filter is removed again', 'filter was not created')
    else
        Check('the test filter is removed again', @CheckDeleteFilter);
end;

{ -- Warmup API ------------------------------------------------------------ }

var
    KnownFromAddress: String = '';

function CheckWarmupSendingAccounts: String;
begin
    ZbWarmupListSendingAccounts;
    Result := '';
end;

function CheckWarmupFromAddresses: String;
begin
    ZbWarmupListFromAddresses(nil);
    Result := '';
end;

function CheckWarmupEngagementRules: String;
begin
    ZbWarmupGetEngagementRules;
    Result := '';
end;

function CheckWarmupSeeds: String;
begin
    ZbWarmupGetSeeds;
    Result := '';
end;

function CheckWarmupSeedCount: String;
begin
    { Answers a bare number, not an object. }
    ZbWarmupGetSeedCount;
    Result := '';
end;

function CheckWarmupProgressToday: String;
begin
    ZbWarmupGetProgressToday;
    Result := '';
end;

function CheckWarmupSentToday: String;
begin
    ZbWarmupGetSentToday;
    Result := '';
end;

function CheckWarmupEmailsPerDay: String;
begin
    ZbWarmupGetEmailsPerDay;
    Result := '';
end;

function CheckWarmupDownloadSeeds: String;
var
    Csv: String;
begin
    Csv := ZbWarmupDownloadSeeds('');
    if Trim(Csv) = '' then
        Exit('empty CSV body');
    if Csv[1] = '{' then
        Exit('got JSON instead of CSV');
    Result := '';
end;

function CheckDmarcRejectsBareDomain: String;
begin
    { LIVE: a bare domain is answered with "Please provide a valid email
      address", although the docs describe a domain. }
    try
        ZbWarmupCheckDomainDmarc('example.com');
        Exit('a bare domain unexpectedly succeeded');
    except
        on e: Exception do
            if Pos('email address', e.Message) = 0 then
                Exit(Concat('unexpected message: ', Redact(e.Message)));
    end;
    Result := '';
end;

function CheckDmarcAcceptsEmail: String;
begin
    ZbWarmupCheckDomainDmarc('sender@example.com');
    Result := '';
end;

function CheckWarmupStatistics: String;
begin
    ZbWarmupGetStatistics(KnownFromAddress);
    Result := '';
end;

function CheckWarmupStatisticsPerDay: String;
begin
    ZbWarmupGetStatisticsPerDay(KnownFromAddress, DaysAgoIso(6), DaysAgoIso(0));
    Result := '';
end;

function CheckWarmupStatisticsDetailed: String;
begin
    ZbWarmupGetStatisticsDetailed(KnownFromAddress);
    ZbWarmupDownloadStatistics(KnownFromAddress);
    Result := '';
end;

function CheckWarmupFromAddressStatistics: String;
begin
    ZbWarmupGetFromAddressStatistics(KnownFromAddress);
    Result := '';
end;

function CheckUnknownWarmupResource: String;
begin
    try
        ZbWarmupGetCustomEngagement('no-such-address@example.com');
        Exit('an unknown address unexpectedly succeeded');
    except
        on e: ZbException do
        begin
            if e.StatusCode = 0 then
                Exit('no status code on the error');
            if (WarmupKey <> '') and (Pos(WarmupKey, e.Message) > 0) then
                Exit('the key leaked into the message');
        end;
    end;
    Result := '';
end;

{ The from-address of the first sending account this team has, or '': the
  statistics endpoints answer "Sending account ... was not found" for anything
  else. }
function FirstSendingAddress: String;
var
    Body: String;
    At, Quote: Integer;
begin
    Result := '';
    try
        Body := ZbWarmupListSendingAccounts;
    except
        on e: Exception do
            Exit('');
    end;
    At := Pos('"from_address"', Body);
    if At = 0 then
        At := Pos('"c_from"', Body);
    if At = 0 then
        Exit('');
    Body := Copy(Body, At, MaxInt);
    At := Pos(':', Body);
    if At = 0 then
        Exit('');
    Body := Copy(Body, At + 1, MaxInt);
    At := Pos('"', Body);
    if At = 0 then
        Exit('');
    Body := Copy(Body, At + 1, MaxInt);
    Quote := Pos('"', Body);
    if Quote <= 1 then
        Exit('');
    Result := Copy(Body, 1, Quote - 1);
end;

procedure WarmupFree;
const
    NO_ACCOUNT = 'this team has no sending account';
begin
    WriteLn;
    WriteLn('Warmup API (reads)');
    Check('listSendingAccounts', @CheckWarmupSendingAccounts);
    Check('listFromAddresses', @CheckWarmupFromAddresses);
    Check('getEngagementRules', @CheckWarmupEngagementRules);
    Check('getSeeds', @CheckWarmupSeeds);
    Check('getSeedCount', @CheckWarmupSeedCount);
    Check('getProgressToday', @CheckWarmupProgressToday);
    Check('getSentToday', @CheckWarmupSentToday);
    Check('getEmailsPerDay', @CheckWarmupEmailsPerDay);
    Check('downloadSeeds returns CSV text', @CheckWarmupDownloadSeeds);
    Check('LIVE: checkDomainDmarc rejects a bare domain before sending',
          @CheckDmarcRejectsBareDomain);
    Check('LIVE: checkDomainDmarc accepts an email address', @CheckDmarcAcceptsEmail);

    KnownFromAddress := FirstSendingAddress;
    if KnownFromAddress = '' then
    begin
        Skip('statistics for a registered from-address', NO_ACCOUNT);
        Skip('statistics per day over a short range', NO_ACCOUNT);
        Skip('detailed statistics and the CSV download for that address', NO_ACCOUNT);
        Skip('from-address statistics', NO_ACCOUNT);
    end
    else
    begin
        Check('statistics for a registered from-address', @CheckWarmupStatistics);
        Check('statistics per day over a short range', @CheckWarmupStatisticsPerDay);
        Check('detailed statistics and the CSV download for that address',
              @CheckWarmupStatisticsDetailed);
        Check('from-address statistics', @CheckWarmupFromAddressStatistics);
    end;

    Check('an unknown Warmup resource fails with a status code', @CheckUnknownWarmupResource);
end;

var
    WarmupAddress: String = '';
    WarmupAccountAdded: Boolean = False;

function CheckAddSendingAccount: String;
var
    Options: TZbWarmupAddSendingAccountOptions;
begin
    Options := ZbWarmupDefaultAddSendingAccountOptions;
    Options.EngagementRuleIdSpecified := True;
    Options.EngagementRuleId := 0;
    Options.IgnoreConflictSpecified := True;
    Options.IgnoreConflict := True;
    ZbWarmupAddSendingAccount(WarmupAddress, ZB_WARMUP_SELF_MANAGED, @Options);
    WarmupAccountAdded := True;
    Result := '';
end;

function CheckDeleteSendingAccount: String;
begin
    ZbWarmupDeleteSendingAccount(WarmupAddress);
    Result := '';
end;

procedure WarmupChanges;
begin
    WriteLn;
    WriteLn('Warmup API (ZEROBOUNCE_LIVE_CHANGES=1)');
    WarmupAddress := Format('zb-sdk-live-%d@example.com', [DateTimeToUnix(Now)]);
    Check('a self-managed sending account can be added', @CheckAddSendingAccount);
    if not WarmupAccountAdded then
        Skip('the test sending account is removed again', 'account was not created')
    else
        Check('the test sending account is removed again', @CheckDeleteSendingAccount);
end;

{ -- main ------------------------------------------------------------------ }

procedure KeyLine(const Label_, Value: String);
begin
    if Value = '' then
        WriteLn('  ', Label_, 'MISSING')
    else
        WriteLn('  ', Label_, 'set');
end;

var
    I: Integer;

begin
    ApiKey := Env('ZEROBOUNCE_API_KEY');
    WarmupKey := Env('ZEROBOUNCE_WARMUP_API_KEY');
    SpendFlag := Env('ZEROBOUNCE_LIVE_SPEND') = '1';
    ChangesFlag := Env('ZEROBOUNCE_LIVE_CHANGES') = '1';
    Failures := TStringList.Create;
    try
        WriteLn('ZeroBounce live API suite');
        KeyLine('validation key: ', ApiKey);
        KeyLine('warmup key:     ', WarmupKey);
        WriteLn(Format('  flags:          SPEND=%d CHANGES=%d',
            [Ord(SpendFlag), Ord(ChangesFlag)]));
        WriteLn;

        if ApiKey <> '' then
        begin
            ZBInitialize(ApiKey);
            ValidationFree;
            if SpendFlag then
                ValidationSpend;
            if ChangesFlag then
                ValidationChanges;
        end;

        if WarmupKey <> '' then
        begin
            ZbWarmupInitialize(WarmupKey);
            WarmupFree;
            if ChangesFlag then
                WarmupChanges;
        end;

        WriteLn;
        WriteLn(Format('%d passed, %d failed, %d skipped', [Passed, Failed, Skipped]));
        if Failures.Count > 0 then
        begin
            WriteLn;
            WriteLn('Failures:');
            for I := 0 to Failures.Count - 1 do
                WriteLn('  - ', Failures[I]);
        end;
    finally
        Failures.Free;
    end;
    if Failed > 0 then
        Halt(1);
end.
