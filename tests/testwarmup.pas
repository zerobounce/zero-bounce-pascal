unit TestWarmup;

{ Offline tests for the Warmup API client.

  Every request is answered from ZbResponseMock; nothing here reaches the live
  API. The assertions are on the url, the method, the body and the one header
  that actually went out. }

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    {$IFDEF UNIX}BaseUnix,{$ENDIF}
    BaseTest, ZbUtility, ZbWarmup;

type

    TTestWarmup = class(TBaseTestCase)
    const
        MOCK_WARMUP_KEY = 'mock_warmup_key';
        ROOT = 'https://warmup-api.zerobounce.net/api/v1/warmup';
    protected
        procedure MockOk(const Payload: String);
        procedure AssertNothingSent;
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TestEveryCallCarriesTheApiKeyHeaderAndItsOwnHost;
        procedure TestAnUninitializedClientSendsNothing;
        procedure TestTheWarmupKeyIsCheckedLikeTheValidationKey;
        procedure TestReadsUseTheDocumentedPathsAndQueries;
        procedure TestEmptySuccessesComeBackAsAnEmptyString;
        procedure TestAddSendingAccountSelfManaged;
        procedure TestAddSendingAccountZbManaged;
        procedure TestAddSendingAccountSendsEveryDocumentedField;
        procedure TestTheTimezoneOffsetMayBeFractional;
        procedure TestUpdateAndDeleteSendingAccount;
        procedure TestChangeFromAddressUsesTheSmtpCredentialsPath;
        procedure TestListFromAddressesChecksItsArguments;
        procedure TestEngagementRules;
        procedure TestCustomEngagementRanges;
        procedure TestStatisticsPerDayChecksItsDates;
        procedure TestCheckDomainDmarcNeedsAnEmailAddress;
        procedure TestTestSmtpConfigurationChecksThePort;
        procedure TestCsvDownloadsRejectAJsonAnswer;
        procedure TestCsvDownloadsRejectEveryNonFileAnswer;
        procedure TestBlankArgumentsAreRejectedBeforeSending;
        procedure TestErrorsNeverCarryTheKeyOrTheSmtpCredentials;
        procedure TestSmtpCredentialsAreMaskedInEveryForm;
        procedure TestTheSmtpPasswordIsMaskedInEveryEncodedForm;
{$IFDEF UNIX}
        procedure TestAFailedWriteKeepsTheFileAlreadyAtThePath;
{$ENDIF}
    end;

implementation

const
    OK_JSON = '{"ok":true}';
    CSV_TYPE = 'text/csv';

procedure TTestWarmup.SetUp;
begin
    inherited SetUp;
    ZbWarmupInitialize(MOCK_WARMUP_KEY);
    ZbWarmupBaseURL := 'https://warmup-api.zerobounce.net';
    ZbLastRequestMethod := '';
    ZbLastRequestBody := '';
    ZbLastRequestHeader := '';
end;

procedure TTestWarmup.TearDown;
begin
    ZbWarmupApiKey := '';
    inherited TearDown;
end;

procedure TTestWarmup.MockOk(const Payload: String);
begin
    ZBMockResponse(200, Payload, JSON_CONTENT_TYPE);
    ZbResponseMock.UrlCalled := '';
end;

procedure TTestWarmup.AssertNothingSent;
begin
    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;

procedure TTestWarmup.TestEveryCallCarriesTheApiKeyHeaderAndItsOwnHost;
begin
    MockOk(OK_JSON);
    ZbWarmupListSendingAccounts;

    AssertEquals('GET', ZbLastRequestMethod);
    { Its own host and base path, not the validation API's. }
    AssertEquals(Concat(ROOT, '/sending-accounts'), ZbResponseMock.UrlCalled);
    { The Warmup API authenticates with an API-Key header; the validation
      api_key never appears. }
    AssertEquals(Concat('API-Key: ', MOCK_WARMUP_KEY), ZbLastRequestHeader);
    AssertEquals('the api_key parameter is not used', 0,
        Pos('api_key', ZbResponseMock.UrlCalled));
end;

procedure TTestWarmup.TestAnUninitializedClientSendsNothing;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);
    ZbWarmupApiKey := '';
    Raised := False;
    try
        ZbWarmupGetSeeds;
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('ZbWarmupInitialize', e.Message) > 0);
        end;
    end;
    AssertTrue('an uninitialized client should refuse to send', Raised);
    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;

procedure TTestWarmup.TestTheWarmupKeyIsCheckedLikeTheValidationKey;
var
    Raised: Boolean;
begin
    { A key with surrounding whitespace or control characters would otherwise
      be quoted, unredacted, in a transport error. }
    Raised := False;
    try
        ZbWarmupInitialize(Concat(' ', MOCK_WARMUP_KEY));
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('whitespace', e.Message) > 0);
        end;
    end;
    AssertTrue('a padded key should be rejected', Raised);

    Raised := False;
    try
        ZbWarmupInitialize('   ');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('api_key', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank key should be rejected', Raised);
end;

procedure TTestWarmup.TestReadsUseTheDocumentedPathsAndQueries;
begin
    MockOk(OK_JSON);

    ZbWarmupGetSendingAccount('sender@example.com');
    AssertEquals(
        Concat(ROOT, '/sending-account?from_address=sender@example.com'),
        ZbResponseMock.UrlCalled
    );

    ZbWarmupGetEngagementRules;
    AssertEquals(Concat(ROOT, '/engagement-rules'), ZbResponseMock.UrlCalled);

    ZbWarmupGetSeeds;
    AssertEquals(Concat(ROOT, '/seeds'), ZbResponseMock.UrlCalled);

    ZbWarmupGetSeedCount;
    AssertEquals(Concat(ROOT, '/seed-count'), ZbResponseMock.UrlCalled);

    ZbWarmupGetProgressToday;
    AssertEquals(Concat(ROOT, '/progress-today'), ZbResponseMock.UrlCalled);

    ZbWarmupGetSentToday;
    AssertEquals(Concat(ROOT, '/sent-today'), ZbResponseMock.UrlCalled);

    ZbWarmupGetEmailsPerDay;
    AssertEquals(Concat(ROOT, '/emails-per-day'), ZbResponseMock.UrlCalled);

    ZbWarmupGetStatistics('sender@example.com');
    AssertEquals(
        Concat(ROOT, '/statistics?from_address=sender@example.com'),
        ZbResponseMock.UrlCalled
    );

    ZbWarmupGetStatisticsDetailed('sender@example.com');
    AssertEquals(
        Concat(ROOT, '/statistics-detailed?c_from=sender@example.com'),
        ZbResponseMock.UrlCalled
    );

    ZbWarmupGetFromAddressStatistics('sender@example.com');
    AssertEquals(
        Concat(ROOT, '/from-address-statistics?from_address=sender@example.com'),
        ZbResponseMock.UrlCalled
    );

    { c_from is one path segment here, so a slash must stay encoded. }
    ZbWarmupGetCustomEngagement('a b/c');
    AssertEquals(
        Concat(ROOT, '/custom-engagement/a%20b%2Fc'), ZbResponseMock.UrlCalled
    );
end;

procedure TTestWarmup.TestEmptySuccessesComeBackAsAnEmptyString;
begin
    { 204 and an empty 200 are the documented empty successes. }
    ZBMockResponse(204, '', JSON_CONTENT_TYPE);
    AssertEquals('', ZbWarmupGetSentToday);

    ZBMockResponse(200, '   ', JSON_CONTENT_TYPE);
    AssertEquals('', ZbWarmupGetSentToday);
end;

procedure TTestWarmup.TestAddSendingAccountSelfManaged;
var
    Options: TZbWarmupAddSendingAccountOptions;
    Content: TZbWarmupEmailContent;
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    Options := ZbWarmupDefaultAddSendingAccountOptions;
    Options.EngagementRuleIdSpecified := True;
    Options.EngagementRuleId := 2;
    Options.IgnoreConflictSpecified := True;
    Options.IgnoreConflict := True;
    ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_SELF_MANAGED, @Options);

    AssertEquals('POST', ZbLastRequestMethod);
    AssertEquals(Concat(ROOT, '/sending-accounts'), ZbResponseMock.UrlCalled);
    AssertEquals(
        '{"from_address":"sender@example.com","onboarding_model":"self_managed",'
        + '"engagement_rule_id":2,"ignore_conflict":true}',
        ZbLastRequestBody
    );

    { A self-managed account needs an engagement rule. }
    ZbResponseMock.UrlCalled := '';
    Options := ZbWarmupDefaultAddSendingAccountOptions;
    Raised := False;
    try
        ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_SELF_MANAGED, @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('engagement_rule_id', e.Message) > 0);
        end;
    end;
    AssertTrue('a self-managed account without a rule should be rejected', Raised);
    AssertNothingSent;

    { and the ZB managed extras belong to the other model. }
    FillChar(Content, SizeOf(Content), 0);
    Content.CompanyName := 'Example';
    Options.EngagementRuleIdSpecified := True;
    Options.EngagementRuleId := 0;
    Options.EmailContent := @Content;
    Raised := False;
    try
        ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_SELF_MANAGED, @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('zb_managed', e.Message) > 0);
        end;
    end;
    AssertTrue('email_content is for zb_managed only', Raised);

    Raised := False;
    try
        ZbWarmupAddSendingAccount('sender@example.com', 'whatever', nil);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('onboarding_model', e.Message) > 0);
        end;
    end;
    AssertTrue('an unknown onboarding model should be rejected', Raised);
end;

procedure TTestWarmup.TestAddSendingAccountZbManaged;
var
    Options: TZbWarmupAddSendingAccountOptions;
    Content: TZbWarmupEmailContent;
    Sending: TZbWarmupManagedSending;
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    FillChar(Content, SizeOf(Content), 0);
    Content.CompanyName := 'Example';
    Content.CompanyWebsite := 'https://example.com';
    Content.TypesOfEmails := 'newsletter, marketing';

    FillChar(Sending, SizeOf(Sending), 0);
    Sending.WeekdaysOnlySpecified := True;
    Sending.WeekdaysOnly := True;
    Sending.Timezone := 'Europe/Bucharest';
    Sending.TimezoneOffsetSpecified := True;
    Sending.TimezoneOffset := 3;
    Sending.WarmupStartDate := '2026-11-02';

    Options := ZbWarmupDefaultAddSendingAccountOptions;
    Options.EmailContent := @Content;
    Options.ManagedSending := @Sending;
    ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_ZB_MANAGED, @Options);

    AssertEquals(
        '{"from_address":"sender@example.com","onboarding_model":"zb_managed",'
        + '"email_content":{"company_name":"Example",'
        + '"company_website":"https://example.com",'
        + '"types_of_emails":["newsletter","marketing"]},'
        + '"managed_sending":{"weekdays_only":true,"timezone":"Europe/Bucharest",'
        + '"timezone_offset":3,"warmup_start_date":"2026-11-02"}}',
        ZbLastRequestBody
    );

    ZbResponseMock.UrlCalled := '';
    Content.TypesOfEmails := 'postcards';
    Raised := False;
    try
        ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_ZB_MANAGED, @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('types_of_emails', e.Message) > 0);
        end;
    end;
    AssertTrue('an unknown email type should be rejected', Raised);
    AssertNothingSent;

    Content.TypesOfEmails := 'marketing';
    Sending.TimezoneOffset := 15;
    Raised := False;
    try
        ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_ZB_MANAGED, @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('timezone_offset', e.Message) > 0);
        end;
    end;
    AssertTrue('an offset outside -11..14 should be rejected', Raised);

    Sending.TimezoneOffset := 3;
    Sending.WarmupStartDate := '02/11/2026';
    Raised := False;
    try
        ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_ZB_MANAGED, @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('warmup_start_date', e.Message) > 0);
        end;
    end;
    AssertTrue('a non ISO start date should be rejected', Raised);
end;

procedure TTestWarmup.TestAddSendingAccountSendsEveryDocumentedField;
var
    Options: TZbWarmupAddSendingAccountOptions;
    Content: TZbWarmupEmailContent;
    Sending: TZbWarmupManagedSending;
begin
    { https://www.zerobounce.net/docs/warmup-api/adding-a-sending-account }
    MockOk(OK_JSON);
    Content.CompanyName := 'Example';
    Content.CompanyWebsite := 'https://example.com';
    Content.CompanyDescription := 'We sell "things"';
    Content.TypesOfEmails := 'transactional,newsletter,marketing';
    Content.ExampleEmail := 'Hello,'#10'Welcome.';
    Sending.WeekdaysOnlySpecified := True;
    Sending.WeekdaysOnly := False;
    Sending.Timezone := 'Asia/Kolkata';
    Sending.TimezoneOffsetSpecified := True;
    Sending.TimezoneOffset := 5.5;
    Sending.WarmupStartDate := '2026-11-02';
    Options := ZbWarmupDefaultAddSendingAccountOptions;
    Options.IgnoreConflictSpecified := True;
    Options.IgnoreConflict := True;
    Options.EmailContent := @Content;
    Options.ManagedSending := @Sending;
    ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_ZB_MANAGED, @Options);
    AssertEquals(
        '{"from_address":"sender@example.com","onboarding_model":"zb_managed",'
        + '"ignore_conflict":true,'
        + '"email_content":{"company_name":"Example",'
        + '"company_website":"https://example.com",'
        + '"company_description":"We sell \"things\"",'
        + '"types_of_emails":["transactional","newsletter","marketing"],'
        + '"example_email":"Hello,\nWelcome."},'
        + '"managed_sending":{"weekdays_only":false,"timezone":"Asia/Kolkata",'
        + '"timezone_offset":5.5,"warmup_start_date":"2026-11-02"}}',
        ZbLastRequestBody
    );
end;

procedure TTestWarmup.TestTheTimezoneOffsetMayBeFractional;
const
    ACCEPTED: array[0..5] of Double = (5.5, 3.0, -11.0, 14.0, -9.5, 5.75);
    SENT: array[0..5] of String = ('5.5', '3', '-11', '14', '-9.5', '5.75');
    REJECTED: array[0..3] of Double = (-11.5, 14.5, 15, -12);
var
    Options: TZbWarmupAddSendingAccountOptions;
    Sending: TZbWarmupManagedSending;
    I: Integer;
    Raised: Boolean;
begin
    MockOk(OK_JSON);
    FillChar(Sending, SizeOf(Sending), 0);
    Sending.TimezoneOffsetSpecified := True;
    Options := ZbWarmupDefaultAddSendingAccountOptions;
    Options.ManagedSending := @Sending;
    for I := Low(ACCEPTED) to High(ACCEPTED) do
    begin
        Sending.TimezoneOffset := ACCEPTED[I];
        ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_ZB_MANAGED, @Options);
        AssertEquals(SENT[I],
            '{"from_address":"sender@example.com","onboarding_model":"zb_managed",'
            + '"managed_sending":{"timezone_offset":' + SENT[I] + '}}',
            ZbLastRequestBody);
    end;
    for I := Low(REJECTED) to High(REJECTED) do
    begin
        ZbResponseMock.UrlCalled := '';
        Sending.TimezoneOffset := REJECTED[I];
        Raised := False;
        try
            ZbWarmupAddSendingAccount('sender@example.com', ZB_WARMUP_ZB_MANAGED, @Options);
        except on e: ZbException do
            begin
                Raised := True;
                AssertTrue(e.Message, Pos('timezone_offset', e.Message) > 0);
            end;
        end;
        AssertTrue(Format('timezone_offset %g should be rejected', [REJECTED[I]]), Raised);
        AssertNothingSent;
    end;
end;

procedure TTestWarmup.TestUpdateAndDeleteSendingAccount;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    ZbWarmupUpdateSendingAccount('sender@example.com', 1, 4, True);
    AssertEquals('PUT', ZbLastRequestMethod);
    AssertEquals(Concat(ROOT, '/sending-accounts'), ZbResponseMock.UrlCalled);
    AssertEquals(
        '{"team_id":1,"from_address":"sender@example.com","engagement_rule_id":1,'
        + '"status_id":4,"auto_enable":true}',
        ZbLastRequestBody
    );

    ZbWarmupDeleteSendingAccount('sender@example.com');
    AssertEquals('DELETE', ZbLastRequestMethod);
    AssertEquals(
        Concat(ROOT, '/sending-accounts?from_address=sender@example.com'),
        ZbResponseMock.UrlCalled
    );

    ZbResponseMock.UrlCalled := '';
    Raised := False;
    try
        ZbWarmupUpdateSendingAccount('sender@example.com', 9, 4, False);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('engagement_rule_id', e.Message) > 0);
        end;
    end;
    AssertTrue('a rule outside 0..4 should be rejected', Raised);

    Raised := False;
    try
        ZbWarmupUpdateSendingAccount('sender@example.com', 1, 9, False);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('status_id', e.Message) > 0);
        end;
    end;
    AssertTrue('a status outside 0..8 should be rejected', Raised);
    AssertNothingSent;
end;

procedure TTestWarmup.TestChangeFromAddressUsesTheSmtpCredentialsPath;
begin
    MockOk(OK_JSON);
    { Despite the path, nothing about SMTP credentials is stored. }
    ZbWarmupChangeFromAddress('old@example.com', 'new@example.com');
    AssertEquals('PUT', ZbLastRequestMethod);
    AssertEquals(Concat(ROOT, '/smtp-credentials'), ZbResponseMock.UrlCalled);
    AssertEquals(
        '{"team_id":1,"old_from_address":"old@example.com",'
        + '"new_from_address":"new@example.com"}',
        ZbLastRequestBody
    );
end;

procedure TTestWarmup.TestListFromAddressesChecksItsArguments;
var
    Options: TZbWarmupListFromAddressesOptions;
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    ZbWarmupListFromAddresses(nil);
    AssertEquals(Concat(ROOT, '/from-addresses'), ZbResponseMock.UrlCalled);

    Options := ZbWarmupDefaultListFromAddressesOptions;
    Options.SearchFrom := 'sender@example.com';
    Options.OffsetSpecified := True;
    Options.Offset := 20;
    Options.LimitSpecified := True;
    Options.Limit := 50;
    ZbWarmupListFromAddresses(@Options);
    AssertEquals(
        Concat(ROOT, '/from-addresses?search_from=sender@example.com&offset=20&limit=50'),
        ZbResponseMock.UrlCalled
    );

    ZbResponseMock.UrlCalled := '';
    Options.SearchFrom := 'a b';
    Raised := False;
    try
        ZbWarmupListFromAddresses(@Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('search_from', e.Message) > 0);
        end;
    end;
    AssertTrue('search_from takes letters, digits and . - _ @ + only', Raised);
    AssertNothingSent;
end;

procedure TTestWarmup.TestEngagementRules;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    ZbWarmupSetEngagementRule('sender@example.com', 3);
    AssertEquals('PUT', ZbLastRequestMethod);
    AssertEquals(
        Concat(ROOT, '/engagement-rule?c_from=sender@example.com'),
        ZbResponseMock.UrlCalled
    );
    AssertEquals('{"rule_id":3}', ZbLastRequestBody);

    ZbWarmupDeleteCustomEngagement('sender@example.com', 2);
    AssertEquals('DELETE', ZbLastRequestMethod);
    AssertEquals(Concat(ROOT, '/custom-engagement'), ZbResponseMock.UrlCalled);
    AssertEquals(
        '{"c_from":"sender@example.com","engagement_rule_id":2}', ZbLastRequestBody
    );

    { Falling back to the custom rule itself is not a standard rule. }
    ZbResponseMock.UrlCalled := '';
    Raised := False;
    try
        ZbWarmupDeleteCustomEngagement('sender@example.com', 0);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('engagement_rule_id', e.Message) > 0);
        end;
    end;
    AssertTrue('the fallback rule must be 1 to 4', Raised);
    AssertNothingSent;
end;

procedure TTestWarmup.TestCustomEngagementRanges;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    ZbWarmupCreateCustomEngagement('sender@example.com', 10, 20, 30);
    AssertEquals('POST', ZbLastRequestMethod);
    AssertEquals(Concat(ROOT, '/custom-engagement'), ZbResponseMock.UrlCalled);
    AssertEquals(
        '{"c_from":"sender@example.com","reply_pct":10,"important_pct":20,"click_pct":30}',
        ZbLastRequestBody
    );

    ZbWarmupUpdateCustomEngagement('sender@example.com', 10, 20, 30);
    AssertEquals('PUT', ZbLastRequestMethod);

    ZbResponseMock.UrlCalled := '';
    Raised := False;
    try
        ZbWarmupCreateCustomEngagement('sender@example.com', 4, 20, 10);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('reply_pct', e.Message) > 0);
        end;
    end;
    AssertTrue('reply_pct is 5 to 45', Raised);

    Raised := False;
    try
        ZbWarmupCreateCustomEngagement('sender@example.com', 10, 41, 10);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('important_pct', e.Message) > 0);
        end;
    end;
    AssertTrue('important_pct is 5 to 40', Raised);

    { click_pct is capped at important_pct x 1.75 as well as at 70. }
    Raised := False;
    try
        ZbWarmupCreateCustomEngagement('sender@example.com', 10, 20, 36);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('click_pct', e.Message) > 0);
        end;
    end;
    AssertTrue('click_pct is at most important_pct x 1.75', Raised);
    AssertNothingSent;

    { and exactly 1.75 x is still allowed. }
    ZbWarmupCreateCustomEngagement('sender@example.com', 10, 20, 35);
    AssertEquals(Concat(ROOT, '/custom-engagement'), ZbResponseMock.UrlCalled);
end;

procedure TTestWarmup.TestStatisticsPerDayChecksItsDates;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    ZbWarmupGetStatisticsPerDay('sender@example.com', '2026-10-01', '2026-10-07');
    AssertEquals(
        Concat(ROOT, '/statistics-per-day?from_address=sender@example.com',
               '&start_date=2026-10-01&end_date=2026-10-07'),
        ZbResponseMock.UrlCalled
    );

    ZbResponseMock.UrlCalled := '';
    Raised := False;
    try
        ZbWarmupGetStatisticsPerDay('sender@example.com', '10/01/2026', '2026-10-07');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('start_date', e.Message) > 0);
        end;
    end;
    AssertTrue('dates are YYYY-MM-DD', Raised);

    Raised := False;
    try
        ZbWarmupGetStatisticsPerDay('sender@example.com', '2026-10-08', '2026-10-07');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('later than', e.Message) > 0);
        end;
    end;
    AssertTrue('the range must not run backwards', Raised);
    AssertNothingSent;
end;

procedure TTestWarmup.TestCheckDomainDmarcNeedsAnEmailAddress;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    ZbWarmupCheckDomainDmarc('sender@example.com', 'selector1');
    AssertEquals(
        Concat(ROOT, '/check-domain-dmarc?from_address=sender@example.com',
               '&dkim_selector=selector1'),
        ZbResponseMock.UrlCalled
    );

    ZbWarmupCheckDomainDmarc('sender@example.com');
    AssertEquals(
        Concat(ROOT, '/check-domain-dmarc?from_address=sender@example.com'),
        ZbResponseMock.UrlCalled
    );

    { LIVE: the docs describe a domain, but a bare domain is answered with
      "Please provide a valid email address". }
    ZbResponseMock.UrlCalled := '';
    Raised := False;
    try
        ZbWarmupCheckDomainDmarc('example.com');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('email address', e.Message) > 0);
        end;
    end;
    AssertTrue('a bare domain should be rejected before sending', Raised);
    AssertNothingSent;
end;

procedure TTestWarmup.TestTestSmtpConfigurationChecksThePort;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    ZbWarmupTestSmtpConfiguration(
        'sender@example.com', 'sender@example.com', 'secret', 'smtp.example.com', 587
    );
    AssertEquals('POST', ZbLastRequestMethod);
    AssertEquals(Concat(ROOT, '/test-smtp-configuration'), ZbResponseMock.UrlCalled);
    AssertEquals(
        '{"from_address":"sender@example.com","username":"sender@example.com",'
        + '"password":"secret","server":"smtp.example.com","port":587}',
        ZbLastRequestBody
    );

    ZbResponseMock.UrlCalled := '';
    Raised := False;
    try
        ZbWarmupTestSmtpConfiguration(
            'sender@example.com', 'sender@example.com', 'secret', 'smtp.example.com', 0
        );
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('port', e.Message) > 0);
        end;
    end;
    AssertTrue('the port must be 1 to 65535', Raised);
    AssertNothingSent;
end;

procedure TTestWarmup.TestCsvDownloadsRejectAJsonAnswer;
var
    Raised: Boolean;
begin
    ZBMockResponse(200, 'seed,provider'#10'a@example.com,gmail', CSV_TYPE);
    ZbResponseMock.UrlCalled := '';
    AssertTrue(Pos('seed,provider', ZbWarmupDownloadSeeds('')) = 1);
    AssertEquals(Concat(ROOT, '/seeds/download'), ZbResponseMock.UrlCalled);

    ZBMockResponse(200, 'date,sent'#10'2026-10-01,10', CSV_TYPE);
    ZbWarmupDownloadStatistics('sender@example.com');
    AssertEquals(
        Concat(ROOT, '/statistics/download?c_from=sender@example.com'),
        ZbResponseMock.UrlCalled
    );

    { An error comes back labelled as JSON, so it is not a file. }
    ZBMockResponse(200, '{"detail":"nothing here"}', JSON_CONTENT_TYPE);
    Raised := False;
    try
        ZbWarmupDownloadSeeds('');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('nothing here', e.Message) > 0);
        end;
    end;
    AssertTrue('a JSON answer is not a CSV download', Raised);
end;

procedure TTestWarmup.TestCsvDownloadsRejectEveryNonFileAnswer;
const
    BODIES: array[0..5] of String = (
        '{"message":"No seeds"}', '  [{"error":"nope"}]', '', '<!DOCTYPE html><html><title>Blocked</title></html>',
        'Blocked', 'seed,provider'
    );
    TYPES: array[0..5] of String = (
        'text/csv', 'text/csv', 'text/csv', 'text/csv', 'text/html; charset=utf-8', 'application/json'
    );
var
    I, Call: Integer;
    Raised: Boolean;
    Saved: String;
begin
    { D1-D3: a JSON body or content type, an HTML page by body or content
      type, and an empty body are errors with the status, and no file. }
    Saved := GetTempDir(False) + 'zb-warmup-nonfile.csv';
    for I := Low(BODIES) to High(BODIES) do
        for Call := 0 to 1 do
        begin
            DeleteFile(Saved);
            ZBMockResponse(200, BODIES[I], TYPES[I]);
            Raised := False;
            try
                if Call = 0 then
                    ZbWarmupDownloadSeeds(Saved)
                else
                    ZbWarmupDownloadStatistics('sender@example.com', Saved);
            except on e: ZbException do
                begin
                    Raised := True;
                    AssertEquals(BODIES[I], 200, e.StatusCode);
                end;
            end;
            AssertTrue(Format('%d/%d %s (%s) is not a CSV download', [I, Call, BODIES[I], TYPES[I]]), Raised);
            AssertFalse('a file was saved for ' + BODIES[I], FileExists(Saved));
        end;
end;

procedure TTestWarmup.TestBlankArgumentsAreRejectedBeforeSending;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    Raised := False;
    try
        ZbWarmupGetSendingAccount('  ');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('from_address', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank from_address should be rejected', Raised);

    Raised := False;
    try
        ZbWarmupGetStatisticsDetailed('  ');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('c_from', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank c_from should be rejected', Raised);

    { A path segment of '.' or '..' would walk the request out of the
      endpoint it was meant for. }
    Raised := False;
    try
        ZbWarmupGetCustomEngagement('..');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('c_from', e.Message) > 0);
        end;
    end;
    AssertTrue('a dot-dot path segment should be rejected', Raised);

    Raised := False;
    try
        ZbWarmupChangeFromAddress('old@example.com', ' ');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('new_from_address', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank new_from_address should be rejected', Raised);
    AssertNothingSent;
end;

procedure TTestWarmup.TestErrorsNeverCarryTheKeyOrTheSmtpCredentials;
const
    USER = 'smtp-user-42';
    PASSWORD = 'smtp-pass-77';
var
    Raised: Boolean;
begin
    { The API may quote what it was sent. The Warmup key and the SMTP
      credentials of test-smtp-configuration stay out of both the message and
      the payload of the error. }
    ZBMockResponse(500, Concat('{"error":"login failed for ', USER, ' with ', PASSWORD,
        ' (key ', MOCK_WARMUP_KEY, ')"}'));
    Raised := False;
    try
        ZbWarmupTestSmtpConfiguration('from@example.com', USER, PASSWORD, 'smtp.example.com', 587);
    except on e: ZbException do
        begin
            Raised := True;
            AssertEquals(500, e.StatusCode);
            AssertTrue('the warmup key leaked: ' + e.Message, Pos(MOCK_WARMUP_KEY, e.Message) = 0);
            AssertTrue('the password leaked: ' + e.Message, Pos(PASSWORD, e.Message) = 0);
            AssertTrue('the username leaked: ' + e.Message, Pos(USER, e.Message) = 0);
            AssertTrue('the warmup key leaked: ' + e.Payload, Pos(MOCK_WARMUP_KEY, e.Payload) = 0);
            AssertTrue('the password leaked: ' + e.Payload, Pos(PASSWORD, e.Payload) = 0);
            AssertTrue('the username leaked: ' + e.Payload, Pos(USER, e.Payload) = 0);
            AssertTrue(e.Message, Pos('REDACTED', e.Message) > 0);
        end;
    end;
    AssertTrue('an HTTP 500 should raise', Raised);
end;

procedure TTestWarmup.TestSmtpCredentialsAreMaskedInEveryForm;
const
    { Raw, url-encoded, JSON-escaped with '/' escaped and not. }
    FORMS: array[0..7] of String = (
        'Pa/ss"w%rd+', 'Pa%2Fss%22w%25rd%2B', 'Pa\/ss\"w%rd+', 'Pa/ss\"w%rd+',
        'us"er/n+%', 'us%22er%2Fn%2B%25', 'us\"er\/n+%', 'us\"er/n+%'
    );
var
    Body, Text: String;
    I, Answer: Integer;
    Raised: Boolean;
begin
    { A server or proxy may echo the request body (JSON-escaped) or a form
      (url-encoded) as well as the raw value; no form of the SMTP credentials
      may reach the error. A text body is not parsed, so nothing decodes the
      escaped forms first. }
    Body := 'SMTP login failed:';
    for I := Low(FORMS) to High(FORMS) do
        Body := Body + ' [' + FORMS[I] + ']';
    for Answer := 0 to 1 do
    begin
        if Answer = 0 then
            ZBMockResponse(500, Body, 'text/plain')
        else
            { JSON can carry every form but the raw ones. }
            ZBMockResponse(500, Concat('{"error":"login failed ', FORMS[1], ' ', FORMS[2], ' ', FORMS[3], ' ',
                FORMS[5], ' ', FORMS[6], ' ', FORMS[7], '"}'), 'application/json');
        Raised := False;
        try
            ZbWarmupTestSmtpConfiguration('from@example.com', FORMS[4], FORMS[0], 'smtp.example.com', 587);
        except on e: ZbException do
            begin
                Raised := True;
                AssertEquals(500, e.StatusCode);
                Text := e.Message + ' | ' + e.Payload;
                for I := Low(FORMS) to High(FORMS) do
                    AssertTrue(Format('form %d leaked: %s', [I, Text]), Pos(FORMS[I], Text) = 0);
                AssertTrue(Text, Pos('REDACTED', e.Message) > 0);
            end;
        end;
        AssertTrue('an HTTP 500 should raise', Raised);
    end;
end;

procedure TTestWarmup.TestTheSmtpPasswordIsMaskedInEveryEncodedForm;
const
    { A password with a space and every character JSON or URL encoding
      changes, in every form a server echoing the request could quote it.
      Written out by hand, so each form ZbMaskSecret knows is needed:
      dropping any one leaves its bracket unmasked. }
    PASSWORD = 'a b"c/d+e%f&g=h<i>j''k\l';
    FORMS: array[0..9] of String = (
        'a b"c/d+e%f&g=h<i>j''k\l',
        { JSON-escaped, '/' escaped (fpjson) and not }
        'a b\"c\/d+e%f&g=h<i>j''k\\l',
        'a b\"c/d+e%f&g=h<i>j''k\\l',
        { URL-encoded, a space as %20 and as '+' }
        'a%20b%22c%2Fd%2Be%25f%26g%3Dh%3Ci%3Ej%27k%5Cl',
        'a+b%22c%2Fd%2Be%25f%26g%3Dh%3Ci%3Ej%27k%5Cl',
        { URL-encoded after JSON escaping, each JSON form, %20 and '+' }
        'a%20b%5C%22c%5C%2Fd%2Be%25f%26g%3Dh%3Ci%3Ej%27k%5C%5Cl',
        'a+b%5C%22c%5C%2Fd%2Be%25f%26g%3Dh%3Ci%3Ej%27k%5C%5Cl',
        'a%20b%5C%22c%2Fd%2Be%25f%26g%3Dh%3Ci%3Ej%27k%5C%5Cl',
        'a+b%5C%22c%2Fd%2Be%25f%26g%3Dh%3Ci%3Ej%27k%5C%5Cl',
        { As EncodeParam (HTTPEncode) puts it on a query string: a space as
          '+', and ' left as it is }
        'a+b%22c%2Fd%2Be%25f%26g%3Dh%3Ci%3Ej''k%5Cl'
    );
var
    Body, Expected: String;
    I: Integer;
    Raised: Boolean;
begin
    Body := 'SMTP login failed:';
    Expected := 'SMTP login failed:';
    for I := Low(FORMS) to High(FORMS) do
    begin
        Body := Body + ' [' + FORMS[I] + ']';
        Expected := Expected + ' [REDACTED]';
    end;
    ZBMockResponse(500, Body, 'text/plain');
    Raised := False;
    try
        ZbWarmupTestSmtpConfiguration('from@example.com', 'smtp-user', PASSWORD, 'smtp.example.com', 587);
    except on e: ZbException do
        begin
            Raised := True;
            AssertEquals(500, e.StatusCode);
            AssertTrue(e.Message + ' | ' + e.Payload, Pos(Expected, e.Message + ' | ' + e.Payload) > 0);
            AssertTrue(e.Payload, Pos(Expected, e.Payload) > 0);
        end;
    end;
    AssertTrue('an HTTP 500 should raise', Raised);
end;

{$IFDEF UNIX}
procedure TTestWarmup.TestAFailedWriteKeepsTheFileAlreadyAtThePath;
const
    CSV = 'seed,provider'#10'seed@example.com,gmail'#10;
var
    Dir, Path, Kept: String;
    Saved, Tight: TRLimit;
    Previous: SignalHandler;
    Call, Parts: Integer;
    Raised: Boolean;
    Found: TSearchRec;

    procedure WriteText(const Name, Text: String);
    var
        Stream: TFileStream;
    begin
        Stream := TFileStream.Create(Name, fmCreate);
        try
            Stream.WriteBuffer(Text[1], Length(Text));
        finally
            Stream.Free;
        end;
    end;

    function ReadText(const Name: String): String;
    var
        Lines: TStringList;
    begin
        if not FileExists(Name) then
            Exit('<missing>');
        Lines := TStringList.Create;
        try
            Lines.LoadFromFile(Name);
            Result := Lines.Text;
        finally
            Lines.Free;
        end;
    end;

    function CountEntries: Integer;
    begin
        Result := 0;
        if FindFirst(Dir + '*', faAnyFile, Found) = 0 then
        begin
            repeat
                if (Found.Name <> '.') and (Found.Name <> '..') then
                    Inc(Result);
            until FindNext(Found) <> 0;
            FindClose(Found);
        end;
    end;

begin
    { D4: the CSV downloads opened DownloadPath with fmCreate, so a write that
      failed (a file size limit, a full disk) destroyed a file already there
      and left a partial one. They now write beside it and move the file into
      place only when it is complete. }
    Dir := IncludeTrailingPathDelimiter(GetTempDir(False) + Format('zb-keep-existing-%d', [GetProcessID]));
    ForceDirectories(Dir);
    Path := Dir + 'seeds.csv';
    AssertEquals(0, FpGetRLimit(RLIMIT_FSIZE, @Saved));
    Tight := Saved;
    Tight.rlim_cur := 8;
    for Call := 0 to 1 do
    begin
        WriteText(Path, 'OLD');
        ZBMockResponse(200, CSV, CSV_TYPE);
        Raised := False;
        Previous := FpSignal(SIGXFSZ, SignalHandler(SIG_IGN));
        AssertEquals(0, FpSetRLimit(RLIMIT_FSIZE, @Tight));
        try
            try
                if Call = 0 then
                    ZbWarmupDownloadSeeds(Path)
                else
                    ZbWarmupDownloadStatistics('sender@example.com', Path);
            except
                on e: Exception do
                    Raised := True;
            end;
        finally
            FpSetRLimit(RLIMIT_FSIZE, @Saved);
            FpSignal(SIGXFSZ, Previous);
        end;
        Kept := ReadText(Path);
        Parts := CountEntries;
        AssertTrue(Format('call %d: a failed write should raise', [Call]), Raised);
        AssertEquals(Format('call %d destroyed the file already there', [Call]), 'OLD', Trim(Kept));
        AssertEquals(Format('call %d left a partial file beside it', [Call]), 1, Parts);
    end;
    { Without the limit the download replaces it. }
    ZBMockResponse(200, CSV, CSV_TYPE);
    ZbWarmupDownloadSeeds(Path);
    AssertEquals(CSV, ReadText(Path));
    AssertEquals(1, CountEntries);
    DeleteFile(Path);
    RemoveDir(Dir);
end;
{$ENDIF}

initialization
    RegisterTest(TTestWarmup);

end.
