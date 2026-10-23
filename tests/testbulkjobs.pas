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
        procedure TestS3AndSftpJobsSendTheDocumentedFieldsOnly;
        procedure TestValidateSftpFileNameIsOptional;
        procedure TestExportsUseTheirOwnDestinationField;
        procedure TestBulkJobsRejectInvalidArguments;
        procedure TestInboxPlacementUsesItsOwnHostAndCamelCaseJson;
        procedure TestInboxPlacementRejectsBlankArguments;
    end;

implementation

const
    FILE_FEEDBACK_JSON = '{"success":true,"message":"ok","file_name":"f.csv","file_id":"fid"}';
    OK_JSON = '{"ok":true}';

procedure TTestBulkJobs.SetUp;
begin
    inherited SetUp;
    ZbBulkRootURL := 'https://bulkapi.zerobounce.net';
    ZbListEvaluatorURL := 'https://bulkapi.zerobounce.net/v2/listevaluator';
    ZbInboxPlacementURL := 'https://bulkapi.zerobounce.net/api/ipt';
    ZbLastRequestMethod := '';
    ZbLastRequestBody := '';
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

procedure TTestBulkJobs.TestS3AndSftpJobsSendTheDocumentedFieldsOnly;
var
    Options: TZbBulkFileJobOptions;
begin
    MockOk(FILE_FEEDBACK_JSON);

    Options := ZbDefaultBulkFileJobOptions;
    Options.FileName := 'lists/leads.csv';
    ZbEmailFinderS3File(Options);

    AssertEquals('POST', ZbLastRequestMethod);
    { The S3 and SFTP jobs live on the bulkapi root, WITHOUT /v2. }
    AssertEquals(
        'https://bulkapi.zerobounce.net/emailFinderS3file', ZbResponseMock.UrlCalled
    );
    AssertBodyField('file_name', 'lists/leads.csv');
    AssertBodyField('has_header_row', 'true');
    { Unset options stay off the request entirely. }
    RefuteBodyField('remove_duplicates');
    RefuteBodyField('return_url');

    Options.HasHeaderRow := False;
    Options.RemoveDuplicatesSpecified := True;
    Options.RemoveDuplicates := True;
    Options.ReturnUrl := 'https://example.com/done';
    ZbDomainSearchS3File(Options);

    AssertEquals(
        'https://bulkapi.zerobounce.net/domainSearchS3file', ZbResponseMock.UrlCalled
    );
    AssertBodyField('has_header_row', 'false');
    AssertBodyField('remove_duplicates', 'true');
    AssertBodyField('return_url', 'https://example.com/done');

    Options := ZbDefaultBulkFileJobOptions;
    Options.FileName := 'my_file.txt';
    ZbScoreSFTPFile(Options);
    AssertEquals('https://bulkapi.zerobounce.net/scoreSFTPfile', ZbResponseMock.UrlCalled);
    ZbEmailFinderSFTPFile(Options);
    AssertEquals('https://bulkapi.zerobounce.net/emailFinderSFTPfile', ZbResponseMock.UrlCalled);
    ZbDomainSearchSFTPFile(Options);
    AssertEquals('https://bulkapi.zerobounce.net/domainSearchSFTPfile', ZbResponseMock.UrlCalled);
end;

procedure TTestBulkJobs.TestValidateSftpFileNameIsOptional;
var
    Options: TZbBulkFileJobOptions;
begin
    MockOk(FILE_FEEDBACK_JSON);

    { file_name is optional for validateSFTPfile alone: without it the input
      bucket from the account's settings is used. }
    Options := ZbDefaultBulkFileJobOptions;
    ZbValidateSFTPFile(Options);

    AssertEquals('https://bulkapi.zerobounce.net/validateSFTPfile', ZbResponseMock.UrlCalled);
    RefuteBodyField('file_name');
    AssertBodyField('has_header_row', 'true');

    Options.FileName := 'my_file.txt';
    ZbValidateSFTPFile(Options);
    AssertBodyField('file_name', 'my_file.txt');
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

    { An unset name is allowed for validateSFTPfile, but a blank one is not. }
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
    { The API key goes on the query string here, not in the body. }
    AssertQueryField('api_key', MOCK_API_KEY);

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
    AssertQueryField('status', 'finished');
    AssertQueryField('page', '1');
    AssertQueryField('pageSize', '25');

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

initialization

    RegisterTest(TTestBulkJobs);

end.
