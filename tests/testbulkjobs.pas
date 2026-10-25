unit TestBulkJobs;

{ Offline tests for the endpoints ported from the Python SDK that were not
  already on main: the WHOIS and greylist validate options, the S3 email finder
  / domain search, the SFTP/FTP bulk jobs, and the Inbox Placement Tester.

  Every request is answered by ZbMockResponse; nothing here reaches the live
  API. The assertions are on the url and the body that actually went out. }

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    BaseTest, ZbPriority3, ZbStructures, ZbUtility, ZbValidation;

type

    TTestBulkJobs = class(TBaseTestCase)
    protected
        procedure MockOk(const Payload: String);
        procedure AssertBodyField(const Name, Value: String);
        procedure RefuteBodyField(const Name: String);
        procedure AssertQueryField(const Name, Value: String);
        procedure RefuteQueryField(const Name: String);
        procedure SetUp; override;
    published
        procedure TestValidateSendsTheNewOptionsOnlyWhenSet;
        procedure TestValidatePostSendsTheSameFieldsAsAForm;
        procedure TestCallbackUrlWithoutGreylistProcessingSendsNoRequest;
        procedure TestValidateTimeoutOutsideRangeSendsNoRequest;
        procedure TestGreylistMessageLandsOnTheResult;
        procedure TestListEvaluatorUsesItsOwnBaseUrlAndEncodesTheFileId;
        procedure TestListEvaluatorRejectsABlankFileId;
        procedure TestSendEvaluatedFilePostsTheFileIdAsAFormField;
        procedure TestS3AndSftpJobsSendTheDocumentedFieldsOnly;
        procedure TestEveryBulkFileJobRequiresFileNameAndReturnUrl;
        procedure TestExportsUseTheirOwnDestinationField;
        procedure TestBulkJobsRejectInvalidArguments;
        procedure TestInboxPlacementUsesItsOwnHostAndCamelCaseJson;
        procedure TestInboxPlacementRejectsBlankArguments;
        procedure TestValidateS3FileSendsTheDocumentedFieldsOnly;
        procedure TestScoreS3FileSendsTheDocumentedFieldsOnly;
        procedure TestS3FileJobsRequireFileNameAndReturnUrl;
        procedure TestS3FileJobErrorBodyReachesTheCaller;
        procedure TestEmailFinderFileNeedsOneOfTheNameColumns;
        procedure TestCreateInboxPlacementTestSendsProvidersAndSeeds;
        procedure TestCreateInboxPlacementTestChecksItsArguments;
        procedure TestListInboxPlacementTestsFiltersAndBounds;
    end;

implementation

const
    FILE_FEEDBACK_JSON = '{"success":true,"message":"ok","file_name":"f.csv","file_id":"fid"}';
    OK_JSON = '{"ok":true}';

procedure TTestBulkJobs.SetUp;
begin
    inherited SetUp;
    BULK_URI := 'https://bulkapi.zerobounce.net/v2';
    ZbBulkRootURL := 'https://bulkapi.zerobounce.net';
    ZbListEvaluatorURL := 'https://bulkapi.zerobounce.net/v2/listevaluator';
    ZbInboxPlacementURL := 'https://bulkapi.zerobounce.net/api/ipt';
    ZbLastRequestMethod := '';
    ZbLastRequestBody := '';
    ZbLastRequestHeader := '';
end;

procedure TTestBulkJobs.MockOk(const Payload: String);
begin
    ZBMockResponse(200, Payload, JSON_CONTENT_TYPE);
end;

procedure TTestBulkJobs.AssertBodyField(const Name, Value: String);
begin
    AssertTrue(
        Concat('body ', ZbLastRequestBody, ' should contain ', Name, '=', Value),
        Pos(Concat(Name, '=', EncodeParam(Value)), ZbLastRequestBody) > 0
    );
end;

procedure TTestBulkJobs.RefuteBodyField(const Name: String);
begin
    AssertFalse(
        Concat('body ', ZbLastRequestBody, ' should not contain ', Name),
        Pos(Concat(Name, '='), ZbLastRequestBody) > 0
    );
end;

procedure TTestBulkJobs.AssertQueryField(const Name, Value: String);
begin
    AssertTrue(
        Concat('url ', ZbResponseMock.UrlCalled, ' should contain ', Name, '=', Value),
        Pos(Concat(Name, '=', EncodeParam(Value)), ZbResponseMock.UrlCalled) > 0
    );
end;

procedure TTestBulkJobs.RefuteQueryField(const Name: String);
begin
    AssertFalse(
        Concat('url ', ZbResponseMock.UrlCalled, ' should not contain ', Name),
        Pos(Concat('&', Name, '='), ZbResponseMock.UrlCalled) > 0
    );
end;

procedure TTestBulkJobs.TestValidateSendsTheNewOptionsOnlyWhenSet;
var
    Options: TZbValidateOptions;
begin
    MockOk('{"address":"a@example.com","status":"valid"}');

    Options := ZbDefaultValidateOptions;
    ZbValidateEmail('a@example.com', '', Options);
    RefuteQueryField('timeout');
    RefuteQueryField('activity_data');
    RefuteQueryField('domain_info');
    RefuteQueryField('greylist_processing');
    RefuteQueryField('callback_url');

    Options.Timeout := 30;
    Options.ActivityData := 1;
    Options.VerifyPlus := 0;
    Options.DomainInfo := 1;
    Options.GreylistProcessing := 1;
    Options.CallbackUrl := 'https://example.com/greylist';
    ZbValidateEmail('a@example.com', '', Options);

    AssertQueryField('timeout', '30');
    { Booleans go on the wire as the strings 'true'/'false'. }
    AssertQueryField('activity_data', 'true');
    AssertQueryField('verify_plus', 'false');
    AssertQueryField('domain_info', 'true');
    AssertQueryField('greylist_processing', 'true');
    AssertQueryField('callback_url', 'https://example.com/greylist');
end;

procedure TTestBulkJobs.TestValidatePostSendsTheSameFieldsAsAForm;
var
    Options: TZbValidateOptions;
begin
    MockOk('{"address":"a@example.com","status":"valid"}');

    Options := ZbDefaultValidateOptions;
    Options.UsePost := 1;
    Options.DomainInfo := 1;
    ZbValidateEmail('a@example.com', '', Options);

    AssertEquals('POST', ZbLastRequestMethod);
    AssertBodyField('email', 'a@example.com');
    AssertBodyField('domain_info', 'true');
end;

procedure TTestBulkJobs.TestCallbackUrlWithoutGreylistProcessingSendsNoRequest;
var
    Options: TZbValidateOptions;
    Raised: Boolean;
begin
    MockOk(OK_JSON);
    ZbResponseMock.UrlCalled := '';

    Options := ZbDefaultValidateOptions;
    Options.CallbackUrl := 'https://example.com/greylist';
    Raised := False;
    try
        ZbValidateEmail('a@example.com', '', Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('greylist_processing', e.Message) > 0);
        end;
    end;
    AssertTrue('callback_url without greylist_processing should be rejected', Raised);

    { Explicit false is still not enough. }
    Options.GreylistProcessing := 0;
    Raised := False;
    try
        ZbValidateEmail('a@example.com', '', Options);
    except on e: Exception do
        Raised := True;
    end;
    AssertTrue('greylist_processing false should be rejected', Raised);

    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;

procedure TTestBulkJobs.TestValidateTimeoutOutsideRangeSendsNoRequest;
var
    Options: TZbValidateOptions;
    Raised: Boolean;
begin
    MockOk(OK_JSON);
    ZbResponseMock.UrlCalled := '';

    Options := ZbDefaultValidateOptions;
    Options.Timeout := 2;
    Raised := False;
    try
        ZbValidateEmail('a@example.com', '', Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('timeout', e.Message) > 0);
        end;
    end;
    AssertTrue('a timeout below 3 should be rejected', Raised);
    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;

procedure TTestBulkJobs.TestGreylistMessageLandsOnTheResult;
var
    Validation: TZbValidationResult;
begin
    { While a greylist retry is still running the API answers with "Message"
      instead of a result. }
    MockOk('{"address":"a@example.com","Message":"Greylisted email, we will retry",'
        + '"domain_website_exists":"true","domain_registrant_company_name":"Example Inc"}');

    Validation := ZbValidateEmail('a@example.com', '');

    AssertEquals('Greylisted email, we will retry', Validation.Message);
    AssertEquals('true', Validation.DomainWebsiteExists);
    AssertEquals('Example Inc', Validation.DomainRegistrantCompanyName);
end;




procedure TTestBulkJobs.TestListEvaluatorUsesItsOwnBaseUrlAndEncodesTheFileId;
begin
    MockOk(OK_JSON);

    ZbGetListEvaluatorResult('a b/c');

    { The file id is one path segment, so a slash in it must stay encoded, and
      a space must not become a plus. }
    AssertEndpointCalled('/v2/listevaluator/a%20b%2Fc/');
end;

procedure TTestBulkJobs.TestListEvaluatorRejectsABlankFileId;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);
    ZbResponseMock.UrlCalled := '';
    Raised := False;
    try
        ZbGetListEvaluatorResult('   ');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('file_id', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank file id should be rejected', Raised);
    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;

procedure TTestBulkJobs.TestSendEvaluatedFilePostsTheFileIdAsAFormField;
var
    Raised: Boolean;
begin
    MockOk(FILE_FEEDBACK_JSON);

    { LIVE: send-evaluated-file takes the file id as a form field. Appended to
      the path instead it answers 404, and the trailing slash is part of the
      endpoint. }
    ZbSendEvaluatedFile('fid-1');
    AssertEquals('POST', ZbLastRequestMethod);
    AssertEquals(
        'https://bulkapi.zerobounce.net/v2/send-evaluated-file/',
        ZbResponseMock.UrlCalled
    );
    AssertBodyField('file_id', 'fid-1');

    ZbResponseMock.UrlCalled := '';
    Raised := False;
    try
        ZbSendEvaluatedFile('  ');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('file_id', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank file id should be rejected', Raised);
    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;

procedure TTestBulkJobs.TestS3AndSftpJobsSendTheDocumentedFieldsOnly;
var
    Options: TZbBulkFileJobOptions;
begin
    MockOk(FILE_FEEDBACK_JSON);

    Options := ZbDefaultBulkFileJobOptions;
    Options.FileName := 'lists/leads.csv';
    Options.ReturnUrl := 'https://example.com/done';
    ZbEmailFinderS3File(Options);

    AssertEquals('POST', ZbLastRequestMethod);
    { The S3 and SFTP jobs live on the bulkapi root, WITHOUT /v2. }
    AssertEquals(
        'https://bulkapi.zerobounce.net/emailFinderS3file', ZbResponseMock.UrlCalled
    );
    AssertBodyField('file_name', 'lists/leads.csv');
    AssertBodyField('return_url', 'https://example.com/done');
    AssertBodyField('has_header_row', 'true');
    { Options that were never set stay off the request entirely. }
    RefuteBodyField('remove_duplicates');

    Options.HasHeaderRow := False;
    Options.RemoveDuplicatesSpecified := True;
    Options.RemoveDuplicates := True;
    ZbDomainSearchS3File(Options);

    AssertEquals(
        'https://bulkapi.zerobounce.net/domainSearchS3file', ZbResponseMock.UrlCalled
    );
    AssertBodyField('has_header_row', 'false');
    AssertBodyField('remove_duplicates', 'true');

    Options := ZbDefaultBulkFileJobOptions;
    Options.FileName := 'my_file.txt';
    Options.ReturnUrl := 'https://example.com/done';
    ZbScoreSFTPFile(Options);
    AssertEquals('https://bulkapi.zerobounce.net/scoreSFTPfile', ZbResponseMock.UrlCalled);
    ZbEmailFinderSFTPFile(Options);
    AssertEquals('https://bulkapi.zerobounce.net/emailFinderSFTPfile', ZbResponseMock.UrlCalled);
    ZbDomainSearchSFTPFile(Options);
    AssertEquals('https://bulkapi.zerobounce.net/domainSearchSFTPfile', ZbResponseMock.UrlCalled);
    ZbValidateSFTPFile(Options);
    AssertEquals('https://bulkapi.zerobounce.net/validateSFTPfile', ZbResponseMock.UrlCalled);
end;

procedure TTestBulkJobs.TestEveryBulkFileJobRequiresFileNameAndReturnUrl;
var
    Options: TZbBulkFileJobOptions;
    Raised: Boolean;

    procedure ExpectRejected(const Fragment: String);
    begin
        AssertTrue(Concat('expected a rejection mentioning ', Fragment), Raised);
        AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
    end;

begin
    MockOk(FILE_FEEDBACK_JSON);

    { LIVE: the docs call file_name optional for validateSFTPfile, but the API
      answers "Empty parameter: file_name" without it, and every job needs a
      return_url as well. Both are checked before the request goes out. }
    ZbResponseMock.UrlCalled := '';
    Options := ZbDefaultBulkFileJobOptions;
    Options.ReturnUrl := 'https://example.com/done';
    Raised := False;
    try
        ZbValidateSFTPFile(Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('file_name', e.Message) > 0);
        end;
    end;
    ExpectRejected('file_name');

    ZbResponseMock.UrlCalled := '';
    Options := ZbDefaultBulkFileJobOptions;
    Options.FileName := 'my_file.txt';
    Raised := False;
    try
        ZbValidateSFTPFile(Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('return_url', e.Message) > 0);
        end;
    end;
    ExpectRejected('return_url');

    { With both present the job goes out and carries them. }
    Options.ReturnUrl := 'https://example.com/done';
    ZbValidateSFTPFile(Options);
    AssertEquals('https://bulkapi.zerobounce.net/validateSFTPfile', ZbResponseMock.UrlCalled);
    AssertBodyField('file_name', 'my_file.txt');
    AssertBodyField('return_url', 'https://example.com/done');
end;

procedure TTestBulkJobs.TestExportsUseTheirOwnDestinationField;
begin
    MockOk(FILE_FEEDBACK_JSON);

    { exportSFTPfile takes export_path, not the export_bucket exportS3file uses. }
    ZbExportSFTPFile('fid', '/exports');
    AssertEquals('https://bulkapi.zerobounce.net/exportSFTPfile', ZbResponseMock.UrlCalled);
    AssertBodyField('file_id', 'fid');
    AssertBodyField('export_path', '/exports');
    RefuteBodyField('export_bucket');

    ZbExportSFTPFile('fid', '');
    RefuteBodyField('export_path');

    ZbExportS3File('fid', 'bucket-out');
    AssertEquals('https://bulkapi.zerobounce.net/exportS3file', ZbResponseMock.UrlCalled);
    AssertBodyField('export_bucket', 'bucket-out');

    ZbExportS3File('fid', '');
    RefuteBodyField('export_bucket');
end;

procedure TTestBulkJobs.TestBulkJobsRejectInvalidArguments;
var
    Options: TZbBulkFileJobOptions;
    Raised: Boolean;

    procedure ExpectRejected(const Fragment: String);
    begin
        AssertTrue(Concat('expected a rejection mentioning ', Fragment), Raised);
    end;

begin
    MockOk(FILE_FEEDBACK_JSON);
    ZbResponseMock.UrlCalled := '';

    Options := ZbDefaultBulkFileJobOptions;
    Raised := False;
    try
        ZbEmailFinderS3File(Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('file_name', e.Message) > 0);
        end;
    end;
    ExpectRejected('file_name');

    { A blank name is rejected just like a missing one. }
    Options.FileName := '   ';
    Raised := False;
    try
        ZbValidateSFTPFile(Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('file_name', e.Message) > 0);
        end;
    end;
    ExpectRejected('file_name');

    Raised := False;
    try
        ZbExportSFTPFile('  ', '');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('file_id', e.Message) > 0);
        end;
    end;
    ExpectRejected('file_id');

    Raised := False;
    try
        ZbExportSFTPFile('fid', ' ');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('export_path', e.Message) > 0);
        end;
    end;
    ExpectRejected('export_path');

    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;


procedure TTestBulkJobs.TestInboxPlacementUsesItsOwnHostAndCamelCaseJson;
var
    CreateOptions: TZbCreateInboxPlacementTestOptions;
    ListOptions: TZbListInboxPlacementTestsOptions;
begin
    MockOk(OK_JSON);

    ZbInboxPlacementProviders;
    AssertEndpointCalled('/api/ipt/providers');
    { LIVE: the key travels in an Authorization: Bearer header. Sent as an
      api_key query parameter the request is rejected as unauthorized, and the
      key would end up in whatever logs the query string. }
    AssertEquals(
        Concat('Authorization: Bearer ', MOCK_API_KEY), ZbLastRequestHeader
    );
    RefuteQueryField('api_key');

    ZbInboxPlacementTestTypes;
    AssertEndpointCalled('/api/ipt/test-types');

    FillChar(CreateOptions, SizeOf(CreateOptions), 0);
    CreateOptions.TestTypeSpecified := True;
    CreateOptions.TestType := 1;
    CreateOptions.SenderEmail := 'news@example.com';
    CreateOptions.WebhookUrl := 'https://example.com/ipt';
    ZbCreateInboxPlacementTest('Launch', @CreateOptions);

    AssertEquals('POST', ZbLastRequestMethod);
    AssertEndpointCalled('/api/ipt/test');
    { Bodies are camelCase JSON, and unset fields are left out. }
    AssertEquals(
        '{"name":"Launch","testType":1,"senderEmail":"news@example.com",'
        + '"webhookUrl":"https://example.com/ipt"}',
        ZbLastRequestBody
    );

    ZbCreateInboxPlacementTest('Launch', nil);
    AssertEquals('{"name":"Launch"}', ZbLastRequestBody);

    FillChar(ListOptions, SizeOf(ListOptions), 0);
    ListOptions.Status := 'finished';
    ListOptions.PageSpecified := True;
    ListOptions.Page := 1;
    ListOptions.PageSizeSpecified := True;
    ListOptions.PageSize := 25;
    ZbListInboxPlacementTests(@ListOptions);
    { LIVE: the listing is GET /tests (plural). /test is the single-test path
      and answers 'Method "GET" not allowed'. }
    AssertEndpointCalled('/api/ipt/tests?');
    AssertQueryField('status', 'finished');
    AssertQueryField('page', '1');
    { LIVE: page_size, not pageSize. The camelCase spelling is accepted and
      then silently ignored. }
    AssertQueryField('page_size', '25');
    RefuteQueryField('pageSize');

    ZbListInboxPlacementTests(nil);
    RefuteQueryField('status');
    RefuteQueryField('page');

    ZbDeleteInboxPlacementTest('a b/c');
    AssertEquals('DELETE', ZbLastRequestMethod);
    AssertEndpointCalled('/api/ipt/test/a%20b%2Fc');

    ZbSetInboxPlacementWebhook('https://example.com/ipt');
    AssertEquals('PUT', ZbLastRequestMethod);
    AssertEndpointCalled('/api/ipt/webhook');
    AssertEquals('{"webhookUrl":"https://example.com/ipt"}', ZbLastRequestBody);

    ZbInboxPlacementWebhook;
    AssertEndpointCalled('/api/ipt/webhook');
end;

procedure TTestBulkJobs.TestInboxPlacementRejectsBlankArguments;
var
    Raised: Boolean;
begin
    MockOk(OK_JSON);
    ZbResponseMock.UrlCalled := '';

    Raised := False;
    try
        ZbInboxPlacementTest('   ');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('test_id', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank test id should be rejected', Raised);

    Raised := False;
    try
        ZbCreateInboxPlacementTest(' ', nil);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('name', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank name should be rejected', Raised);

    Raised := False;
    try
        ZbSetInboxPlacementWebhook('');
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('webhook_url', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank webhook url should be rejected', Raised);

    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;

{ Ops 31 and 32. LIVE: POST /validateS3file without return_url is answered
  with 400 "Provided return URL is not permitted: None", so the field is
  always sent and a missing one is refused before the call. The singular
  remove_duplicate and the allow_phase_2 this used to send are not part of
  the documented request and are gone. }
procedure TTestBulkJobs.TestValidateS3FileSendsTheDocumentedFieldsOnly;
var
    Options: TZbValidateS3FileOptions;
begin
    MockOk(FILE_FEEDBACK_JSON);

    FillChar(Options, SizeOf(Options), 0);
    Options.FileName := 'lists/leads.csv';
    Options.ReturnUrl := 'https://example.com/done';
    ZbValidateS3File(Options);

    AssertEquals('POST', ZbLastRequestMethod);
    { The S3 jobs live on the bulkapi root, WITHOUT /v2. }
    AssertEquals(
        'https://bulkapi.zerobounce.net/validateS3file', ZbResponseMock.UrlCalled
    );
    AssertBodyField('file_name', 'lists/leads.csv');
    AssertBodyField('return_url', 'https://example.com/done');
    { The key goes in the form body, not the query string. }
    AssertBodyField('api_key', MOCK_API_KEY);
    RefuteQueryField('api_key');
    { Nothing the caller left unset goes out. }
    RefuteBodyField('has_header_row');
    RefuteBodyField('remove_duplicates');
    RefuteBodyField('email_address_column');
    RefuteBodyField('first_name_column');
    RefuteBodyField('gender_column');
    RefuteBodyField('ip_address_column');
    { Neither of the two fields that were never part of the documented job. }
    RefuteBodyField('remove_duplicate');
    RefuteBodyField('allow_phase_2');

    Options.HasHeaderRowSpecified := True;
    Options.HasHeaderRow := False;
    Options.RemoveDuplicatesSpecified := True;
    Options.RemoveDuplicates := True;
    Options.EmailAddressColumnSpecified := True;
    Options.EmailAddressColumn := 1;
    Options.FirstNameColumn := 2;
    Options.LastNameColumn := 3;
    Options.GenderColumn := 4;
    Options.IpAddressColumn := 5;
    ZbValidateS3File(Options);

    AssertBodyField('has_header_row', 'false');
    AssertBodyField('remove_duplicates', 'true');
    AssertBodyField('email_address_column', '1');
    AssertBodyField('first_name_column', '2');
    AssertBodyField('last_name_column', '3');
    AssertBodyField('gender_column', '4');
    AssertBodyField('ip_address_column', '5');
    { remove_duplicates is sent; the singular form is still not. }
    RefuteBodyField('remove_duplicate');
end;

{ Op 32. The options record had no ReturnUrl at all, so return_url could never
  be sent and the call could not succeed against the live API. }
procedure TTestBulkJobs.TestScoreS3FileSendsTheDocumentedFieldsOnly;
var
    Options: TZbScoreS3FileOptions;
    Feedback: TZBFileFeedback;
begin
    MockOk(FILE_FEEDBACK_JSON);

    FillChar(Options, SizeOf(Options), 0);
    Options.FileName := 'lists/leads.csv';
    Options.ReturnUrl := 'https://example.com/done';
    Options.RemoveDuplicatesSpecified := True;
    Options.RemoveDuplicates := False;
    Options.EmailAddressColumnSpecified := True;
    Options.EmailAddressColumn := 2;
    Feedback := ZbScoreS3File(Options);

    AssertEquals('POST', ZbLastRequestMethod);
    AssertEquals(
        'https://bulkapi.zerobounce.net/scoreS3file', ZbResponseMock.UrlCalled
    );
    AssertBodyField('file_name', 'lists/leads.csv');
    AssertBodyField('return_url', 'https://example.com/done');
    AssertBodyField('remove_duplicates', 'false');
    AssertBodyField('email_address_column', '2');
    AssertBodyField('api_key', MOCK_API_KEY);
    RefuteQueryField('api_key');
    RefuteBodyField('remove_duplicate');
    RefuteBodyField('has_header_row');

    { The 200 body is parsed into the feedback record. }
    AssertTrue(Feedback.Success);
    AssertEquals('fid', Feedback.FileId);
    AssertEquals('ok', Feedback.Message);
end;

procedure TTestBulkJobs.TestS3FileJobsRequireFileNameAndReturnUrl;
var
    ValidateOptions: TZbValidateS3FileOptions;
    ScoreOptions: TZbScoreS3FileOptions;
    Raised: Boolean;
begin
    MockOk(FILE_FEEDBACK_JSON);

    { Same rail the six sibling jobs go through, and the same two messages. }
    ZbResponseMock.UrlCalled := '';
    FillChar(ValidateOptions, SizeOf(ValidateOptions), 0);
    ValidateOptions.ReturnUrl := 'https://example.com/done';
    Raised := False;
    try
        ZbValidateS3File(ValidateOptions);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('file_name', e.Message) > 0);
        end;
    end;
    AssertTrue('a missing file_name should be rejected', Raised);

    FillChar(ValidateOptions, SizeOf(ValidateOptions), 0);
    ValidateOptions.FileName := 'lists/leads.csv';
    Raised := False;
    try
        ZbValidateS3File(ValidateOptions);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('return_url', e.Message) > 0);
        end;
    end;
    AssertTrue('a missing return_url should be rejected', Raised);

    { A blank one is refused just like a missing one. }
    ValidateOptions.ReturnUrl := '   ';
    Raised := False;
    try
        ZbValidateS3File(ValidateOptions);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('return_url', e.Message) > 0);
        end;
    end;
    AssertTrue('a blank return_url should be rejected', Raised);

    FillChar(ScoreOptions, SizeOf(ScoreOptions), 0);
    ScoreOptions.FileName := 'lists/leads.csv';
    Raised := False;
    try
        ZbScoreS3File(ScoreOptions);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('return_url', e.Message) > 0);
        end;
    end;
    AssertTrue('scoreS3file should need a return_url too', Raised);

    FillChar(ScoreOptions, SizeOf(ScoreOptions), 0);
    ScoreOptions.ReturnUrl := 'https://example.com/done';
    Raised := False;
    try
        ZbScoreS3File(ScoreOptions);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('file_name', e.Message) > 0);
        end;
    end;
    AssertTrue('scoreS3file should need a file_name too', Raised);

    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);
end;

{ The error body the live API answers an S3 job with reaches the caller with
  its status, rather than being parsed as a file feedback record. }
procedure TTestBulkJobs.TestS3FileJobErrorBodyReachesTheCaller;
var
    Options: TZbValidateS3FileOptions;
    Raised: Boolean;
begin
    ZBMockResponse(
        400,
        '{"error_message":"Provided return URL is not permitted: None"}',
        JSON_CONTENT_TYPE
    );
    FillChar(Options, SizeOf(Options), 0);
    Options.FileName := 'lists/leads.csv';
    Options.ReturnUrl := 'https://example.com/done';
    Raised := False;
    try
        ZbValidateS3File(Options);
    except on e: ZbException do
        begin
            Raised := True;
            AssertEquals(400, e.StatusCode);
            AssertTrue(e.Payload, Pos('not permitted', e.Payload) > 0);
        end;
    end;
    AssertTrue('an HTTP 400 should be raised', Raised);

    { A 200 whose body is not the feedback JSON is a Json Error carrying the
      body, not a half-filled record. }
    ZBMockResponse(200, 'Access denied', 'text/plain');
    Raised := False;
    try
        ZbValidateS3File(Options);
    except on e: ZbException do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('Json Error', e.Message) > 0);
            AssertEquals('Access denied', e.Payload);
        end;
    end;
    AssertTrue('an unparseable 200 should be raised', Raised);
end;

{ Op 20. The docs make domain_column required and then one of the two name
  columns; only the first was checked, so a file with neither went out and
  came back rejected. }
procedure TTestBulkJobs.TestEmailFinderFileNeedsOneOfTheNameColumns;
var
    Options: TZbEmailFinderSendFileOptions;
    Raised: Boolean;
begin
    MockOk(FILE_FEEDBACK_JSON);
    ZbResponseMock.UrlCalled := '';

    FillChar(Options, SizeOf(Options), 0);
    Raised := False;
    try
        ZbSendEmailFinderFile('a@example.com', 1, @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('first_name_column', e.Message) > 0);
            AssertTrue(e.Message, Pos('full_name_column', e.Message) > 0);
        end;
    end;
    AssertTrue('neither name column should be rejected', Raised);

    { No options at all is the same case. }
    Raised := False;
    try
        ZbSendEmailFinderFile('a@example.com', 1, nil);
    except on e: Exception do
        Raised := True;
    end;
    AssertTrue('a nil options pointer should be rejected', Raised);

    { domain_column is still checked first. }
    Options.FullNameColumn := 2;
    Raised := False;
    try
        ZbSendEmailFinderFile('a@example.com', 0, @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('domain_column', e.Message) > 0);
        end;
    end;
    AssertTrue('a missing domain_column should be rejected', Raised);

    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);

    { Either name column on its own is enough. }
    ZbSendEmailFinderFile('a@example.com', 1, @Options);
    AssertEquals(
        'https://bulkapi.zerobounce.net/email-finder/sendfile',
        ZbResponseMock.UrlCalled
    );

    FillChar(Options, SizeOf(Options), 0);
    Options.FirstNameColumn := 3;
    ZbSendEmailFinderFile('a@example.com', 1, @Options);
    AssertEquals(
        'https://bulkapi.zerobounce.net/email-finder/sendfile',
        ZbResponseMock.UrlCalled
    );
end;

{ Op 43. providerIds, seedIds and providers[] had no home in the options
  record, so there was no way to choose the mailboxes a test runs against. }
procedure TTestBulkJobs.TestCreateInboxPlacementTestSendsProvidersAndSeeds;
var
    Options: TZbCreateInboxPlacementTestOptions;
begin
    MockOk(OK_JSON);

    FillChar(Options, SizeOf(Options), 0);
    Options.TestTypeSpecified := True;
    Options.TestType := 2;
    Options.ProviderIds := TZbInboxPlacementIds.Create(7, 9);
    Options.SeedIds := TZbInboxPlacementIds.Create(11);
    SetLength(Options.Providers, 2);
    Options.Providers[0].Id := 3;
    Options.Providers[0].MaxSeedsSpecified := True;
    Options.Providers[0].MaxSeeds := 5;
    { maxSeeds is optional: without it the provider's own default applies. }
    Options.Providers[1].Id := 4;
    Options.WebhookUrl := 'https://example.com/ipt';

    ZbCreateInboxPlacementTest('Launch', @Options);

    AssertEquals('POST', ZbLastRequestMethod);
    AssertEquals(
        'https://bulkapi.zerobounce.net/api/ipt/test', ZbResponseMock.UrlCalled
    );
    { LIVE: the key travels in an Authorization: Bearer header, never the URL. }
    AssertEquals(
        Concat('Authorization: Bearer ', MOCK_API_KEY), ZbLastRequestHeader
    );
    RefuteQueryField('api_key');
    AssertEquals(
        '{"name":"Launch","testType":2,"providerIds":[7,9],"seedIds":[11],'
        + '"providers":[{"id":3,"maxSeeds":5},{"id":4}],'
        + '"webhookUrl":"https://example.com/ipt"}',
        ZbLastRequestBody
    );

    { Empty arrays stay out of the body entirely. }
    FillChar(Options, SizeOf(Options), 0);
    ZbCreateInboxPlacementTest('Launch', @Options);
    AssertEquals('{"name":"Launch"}', ZbLastRequestBody);
end;

procedure TTestBulkJobs.TestCreateInboxPlacementTestChecksItsArguments;
var
    Options: TZbCreateInboxPlacementTestOptions;
    Raised: Boolean;
begin
    MockOk(OK_JSON);
    ZbResponseMock.UrlCalled := '';

    { name is capped at 64 characters. }
    Raised := False;
    try
        ZbCreateInboxPlacementTest(StringOfChar('x', 65), nil);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('name', e.Message) > 0);
        end;
    end;
    AssertTrue('a name over 64 characters should be rejected', Raised);

    FillChar(Options, SizeOf(Options), 0);
    Options.TestTypeSpecified := True;
    Options.TestType := 4;
    Raised := False;
    try
        ZbCreateInboxPlacementTest('Launch', @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('testType', e.Message) > 0);
        end;
    end;
    AssertTrue('a testType outside 1..3 should be rejected', Raised);

    { Test type 1 is the one you send yourself, so senderEmail is required. }
    Options.TestType := 1;
    Raised := False;
    try
        ZbCreateInboxPlacementTest('Launch', @Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('senderEmail', e.Message) > 0);
        end;
    end;
    AssertTrue('testType 1 without a senderEmail should be rejected', Raised);

    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);

    { Exactly 64 characters is allowed, and so is type 1 with a sender. }
    ZbCreateInboxPlacementTest(StringOfChar('x', 64), nil);
    AssertEndpointCalled('/api/ipt/test');
    Options.SenderEmail := 'news@example.com';
    ZbCreateInboxPlacementTest('Launch', @Options);
    AssertEquals(
        '{"name":"Launch","testType":1,"senderEmail":"news@example.com"}',
        ZbLastRequestBody
    );

    { Types 2 and 3 do not need one. }
    FillChar(Options, SizeOf(Options), 0);
    Options.TestTypeSpecified := True;
    Options.TestType := 3;
    ZbCreateInboxPlacementTest('Launch', @Options);
    AssertEquals('{"name":"Launch","testType":3}', ZbLastRequestBody);
end;

{ Op 45. LIVE: the page size parameter is page_size; pageSize is accepted and
  then silently ignored, which looks like a working filter until you count
  the rows. createdAfter and createdBefore had no home in the record at all. }
procedure TTestBulkJobs.TestListInboxPlacementTestsFiltersAndBounds;
var
    Options: TZbListInboxPlacementTestsOptions;
    Raised: Boolean;
begin
    MockOk(OK_JSON);

    FillChar(Options, SizeOf(Options), 0);
    Options.Status := 'running';
    Options.CreatedAfter := '2026-01-01T00:00:00Z';
    Options.CreatedBefore := '2026-02-01T00:00:00Z';
    Options.PageSpecified := True;
    Options.Page := 2;
    Options.PageSizeSpecified := True;
    Options.PageSize := 100;
    ZbListInboxPlacementTests(@Options);

    AssertEquals('GET', ZbLastRequestMethod);
    AssertEndpointCalled('/api/ipt/tests?');
    AssertQueryField('status', 'running');
    AssertQueryField('createdAfter', '2026-01-01T00:00:00Z');
    AssertQueryField('createdBefore', '2026-02-01T00:00:00Z');
    AssertQueryField('page', '2');
    AssertQueryField('page_size', '100');
    RefuteQueryField('pageSize');
    RefuteQueryField('api_key');

    { page_size is bounded at 1..100 on both ends. }
    ZbResponseMock.UrlCalled := '';
    Options.PageSize := 101;
    Raised := False;
    try
        ZbListInboxPlacementTests(@Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('page_size', e.Message) > 0);
        end;
    end;
    AssertTrue('a page_size over 100 should be rejected', Raised);

    Options.PageSize := 0;
    Raised := False;
    try
        ZbListInboxPlacementTests(@Options);
    except on e: Exception do
        Raised := True;
    end;
    AssertTrue('a page_size below 1 should be rejected', Raised);

    { status is one of the two documented states. }
    Options.PageSize := 25;
    Options.Status := 'pending';
    Raised := False;
    try
        ZbListInboxPlacementTests(@Options);
    except on e: Exception do
        begin
            Raised := True;
            AssertTrue(e.Message, Pos('status', e.Message) > 0);
        end;
    end;
    AssertTrue('an undocumented status should be rejected', Raised);

    AssertEquals('no request should have been sent', '', ZbResponseMock.UrlCalled);

    Options.Status := 'finished';
    ZbListInboxPlacementTests(@Options);
    AssertQueryField('status', 'finished');
    AssertQueryField('page_size', '25');

    { Nothing set means an unfiltered listing. }
    FillChar(Options, SizeOf(Options), 0);
    ZbListInboxPlacementTests(@Options);
    RefuteQueryField('status');
    RefuteQueryField('createdAfter');
    RefuteQueryField('page_size');
end;

initialization

    RegisterTest(TTestBulkJobs);

end.
