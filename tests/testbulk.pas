unit TestBulk;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry, fgl,
    BaseTest, MockValues, ZbBulk, ZbStructures, ZbUtility;

type

    TStringMapping = specialize TFPGMap < String, String >;

    TTestBulk= class(TBaseTestCase)
    const
        RESULT_CONTENT_MOCK = 'content_string_to_be_considered_as_result';
        MOCK_FILE_CONTENT = 'file_content';

        OCTET_STREAM_TYPE = 'application/octet-stream';
        MOCK_FILE_ID = 'aaaaaaaa-aaaa-4aaa-aaaa-aaaaaaaaaaaa';
        SUBMIT_PARAM: TZbBulkParams = (
            EmailAddressColumn: 2;
            FirstNameColumn: 1;
            LastNameColumn: 3;
            GenderColumn: 4;
            IpAddressColumn: 5;
            HasHeaderRow: True;
            RemoveDuplicate: True
        );

    protected
        // if does not find and match values, it issues assertion errors
        procedure ExpectKeyInParamsMap(ParamsMap: TStringMapping; Key, ExpectedData: String);
        // if does find, it issues assertion errors
        procedure NotExpectKeyInParamsMap(ParamsMap: TStringMapping; Key: String);
    published

        { TEST THAT SUBMIT FORM WILL CONTAIN PROPER DATA }

        procedure TestSubmitParamProviderEmpty;
        procedure TestSubmitParamProviderPartial;
        procedure TestSubmitParamProviderFull;

        { TEST GENERIC FUNCTIONALITY OF THE BULK IMPORT, VIA BULK VALIDATION }

        procedure TestFileSubmitError;
        procedure TestFileStatusCheckError;
        procedure TestResultFetchError;
        procedure TestResultDeleteError;
        procedure TestFileSubmitOk;
        procedure TestFileStatusCheckOk;
        procedure TestResultFetchOkJson;
        procedure TestResultFetchOkContent;
        procedure TestResultDeleteNotFound;
        procedure TestResultDeleteOk;

        { TEST THAT EACH INDIVIDUAL METHOD WILL CALL EXPECTED ENDPOINT }

        procedure TestBulkValidationFileSubmitEndpoint;
        procedure TestBulkValidationFileStatusCheckEndpoint;
        procedure TestBulkValidationResultFetchEndpoint;
        procedure TestBulkValidationResultDeleteEndpoint;
        procedure TestAiScoringFileSubmitEndpoint;
        procedure TestAiScoringFileStatusCheckEndpoint;
        procedure TestAiScoringResultFetchEndpoint;
        procedure TestAiScoringResultDeleteEndpoint;

    end;


const
    SUBMIT_PARAM_EMPTY: TZbBulkParams = ();
    SUBMIT_PARAM_PARTIAL: TZbBulkParams = (
        EmailAddressColumn: 2;
        FirstNameColumn: 1
    );
    SUBMIT_PARAM_FULL: TZbBulkParams = (
        EmailAddressColumn: 2;
        FirstNameColumn: 1;
        LastNameColumn: 3;
        GenderColumn: 4;
        IpAddressColumn: 5;
        HasHeaderRow: True;
        RemoveDuplicate: True
    );

// TODO: test:
// GenericFileSubmit
// GenericFileStatusCheck
// GenericResultFetch
// GenericResultDelete

implementation


function FileParamsMapFromRecord(FileParams: TZbBulkParams): TStringMapping;
var
    FormData: TStrings;
    Index: Integer;
    Key, Data: String;
begin
    try
        FormData := ZbFromDataFromFileSubmitRecord(FileParams);
        Result := TStringMapping.Create;
        for Index := 0 to FormData.Count -1 do
        begin
            FormData.GetNameValue(Index, Key, Data);
            Result.InsertKeyData(Index, Key, Data);
        end;
    finally
        FormData.Free;
    end;
end;

procedure TTestBulk.ExpectKeyInParamsMap(ParamsMap: TStringMapping; Key, ExpectedData: String);
var
    IndexFound: Integer;
    FoundData: String;
begin
    IndexFound := ParamsMap.IndexOf(Key);
    AssertTrue('No value for field "' + Key + '" in form params', IndexFound <> -1);
    FoundData := ParamsMap.Data[IndexFound];
    AssertEquals('Value for field "' + Key + '" was unexpected', FoundData, ExpectedData);
end;

procedure TTestBulk.NotExpectKeyInParamsMap(ParamsMap: TStringMapping; Key: String);
var
    IndexFound: Integer;
    FoundData: String;
begin
    FoundData := '';
    IndexFound := ParamsMap.IndexOf(Key);
    if IndexFound <> -1 then
    begin
       FoundData := ParamsMap.Data[IndexFound];
       Fail(
            'No value was expected for field "' + Key + '" in form params. ' +
            'Found: ' + FoundData
       );
    end;
end;


procedure TTestBulk.TestSubmitParamProviderEmpty;
var
    ParamsMap: TStringMapping;
begin
    ParamsMap := FileParamsMapFromRecord(SUBMIT_PARAM_EMPTY);
    try
        ExpectKeyInParamsMap(ParamsMap, 'api_key', MOCK_API_KEY);
        ExpectKeyInParamsMap(ParamsMap, 'email_address_column', '1');
        ExpectKeyInParamsMap(ParamsMap, 'has_header_row', 'false');
        ExpectKeyInParamsMap(ParamsMap, 'remove_duplicate', 'false');

        NotExpectKeyInParamsMap(ParamsMap, 'first_name_column');
        NotExpectKeyInParamsMap(ParamsMap, 'last_name_column');
        NotExpectKeyInParamsMap(ParamsMap, 'gender_column');
        NotExpectKeyInParamsMap(ParamsMap, 'ip_address_column');
    finally
        ParamsMap.Free;
    end;
end;

procedure TTestBulk.TestSubmitParamProviderPartial;
var
    ParamsMap: TStringMapping;
begin
    ParamsMap := FileParamsMapFromRecord(SUBMIT_PARAM_PARTIAL);
    try
        ExpectKeyInParamsMap(ParamsMap, 'api_key', MOCK_API_KEY);
        ExpectKeyInParamsMap(ParamsMap, 'email_address_column', '2');
        ExpectKeyInParamsMap(ParamsMap, 'first_name_column', '1');
        ExpectKeyInParamsMap(ParamsMap, 'has_header_row', 'false');
        ExpectKeyInParamsMap(ParamsMap, 'remove_duplicate', 'false');

        NotExpectKeyInParamsMap(ParamsMap, 'last_name_column');
        NotExpectKeyInParamsMap(ParamsMap, 'gender_column');
        NotExpectKeyInParamsMap(ParamsMap, 'ip_address_column');
    finally
        ParamsMap.Free;
    end;
end;

procedure TTestBulk.TestSubmitParamProviderFull;
var
    ParamsMap: TStringMapping;
begin
    ParamsMap := FileParamsMapFromRecord(SUBMIT_PARAM_FULL);
    try
        ExpectKeyInParamsMap(ParamsMap, 'api_key', MOCK_API_KEY);
        ExpectKeyInParamsMap(ParamsMap, 'email_address_column', '2');
        ExpectKeyInParamsMap(ParamsMap, 'first_name_column', '1');
        ExpectKeyInParamsMap(ParamsMap, 'last_name_column', '3');
        ExpectKeyInParamsMap(ParamsMap, 'gender_column', '4');
        ExpectKeyInParamsMap(ParamsMap, 'ip_address_column', '5');
        ExpectKeyInParamsMap(ParamsMap, 'has_header_row', 'true');
        ExpectKeyInParamsMap(ParamsMap, 'remove_duplicate', 'true');
    finally
        ParamsMap.Free;
    end;
end;

procedure TTestBulk.TestFileSubmitError;
begin
    ZBMockResponse(400, ERROR_PAYLOAD);
    try
        BulkValidationFileSubmit(MOCK_FILE_CONTENT, SUBMIT_PARAM);
        Fail('function should have raised exception');
    except
        on e: ZbException do
        begin
            AssertEquals('status code', e.StatusCode, 400);
            AssertTrue('error message', e.Message.Contains(ERROR_MESSAGE));
        end;
    end;
end;

procedure TTestBulk.TestFileStatusCheckError;
begin
    ZBMockResponse(400, ERROR_PAYLOAD);
    try
        BulkValidationFileStatusCheck(MOCK_FILE_ID);
        Fail('function should have raised exception');
    except
        on e: ZbException do
        begin
            AssertEquals('status code', e.StatusCode, 400);
            AssertTrue('error message', e.Message.Contains(ERROR_MESSAGE));
        end;
    end;
end;

procedure TTestBulk.TestResultFetchError;
begin
    ZBMockResponse(400, ERROR_PAYLOAD);
    try
        BulkValidationResultFetch(MOCK_FILE_ID);
        Fail('function should have raised exception');
    except
        on e: ZbException do
        begin
            AssertEquals('status code', e.StatusCode, 400);
            AssertTrue('error message', e.Message.Contains(ERROR_MESSAGE));
        end;
    end;
end;

procedure TTestBulk.TestResultDeleteError;
begin
    ZBMockResponse(400, ERROR_PAYLOAD);
    try
        BulkValidationResultDelete(MOCK_FILE_ID);
        Fail('function should have raised exception');
    except
        on e: ZbException do
        begin
            AssertEquals('status code', e.StatusCode, 400);
            AssertTrue('error message', e.Message.Contains(ERROR_MESSAGE));
        end;
    end;
end;

procedure TTestBulk.TestFileSubmitOk;
var
    response: TZBFileFeedback;
begin
    ZBMockResponse(200, BULK_SUBMIT_OK);
    response := BulkValidationFileSubmit(MOCK_FILE_CONTENT, SUBMIT_PARAM);

    AssertEquals('success', response.Success, True);
    AssertTrue('message does not have content', Length(response.Message) > 0);
    AssertTrue('file_name does not have content', Length(response.FileName) > 0);
    AssertTrue('file_id does not have content', Length(response.FileId) > 0);
end;

procedure TTestBulk.TestFileStatusCheckOk;
var
    response: TZBFileStatus;
begin
    ZBMockResponse(200, BULK_STATUS_OK);
    response := BulkValidationFileStatusCheck(MOCK_FILE_ID);

    AssertEquals('complete_percentage', response.CompletePercentage, 100.0);
    AssertEquals('success', response.Success, True);

    AssertTrue('file_id does not have content', Length(response.FileId) > 0);
    AssertTrue('file_name does not have content', Length(response.FileName) > 0);
    AssertTrue('file_status does not have content', Length(response.FileStatus) > 0);
    AssertTrue('return_url does not have content', Length(response.ReturnUrl) > 0);

    AssertTrue('error_reason should not have content', Length(response.ErrorReason) = 0);

    AssertEquals(
        'upload_date',
        response.UploadDate,
        EncodeDateTime(2023, 4, 26, 17, 52, 23, 0)
    );
end;

procedure TTestBulk.TestResultFetchOkJson;
var
    response: TZBBulkResponse;
begin
    ZBMockResponse(200, BULK_RESULT_DELETED);
    response := BulkValidationResultFetch(MOCK_FILE_ID);

    AssertFalse(response.HasContent);
    AssertEquals('success', response.Feedback.Success, False);
    AssertTrue('message does not have content', Length(response.Feedback.Message) > 0);
    AssertTrue('file_name should not have content', Length(response.Feedback.FileName) = 0);
    AssertTrue('file_id should not have content', Length(response.Feedback.FileId) = 0);
end;

procedure TTestBulk.TestResultFetchOkContent;
var
    response: TZBBulkResponse;
begin
    ZBMockResponse(200, RESULT_CONTENT_MOCK, OCTET_STREAM_TYPE);
    response := BulkValidationResultFetch(MOCK_FILE_ID);

    AssertTrue(response.HasContent);
    AssertEquals(response.Content, RESULT_CONTENT_MOCK);
end;

procedure TTestBulk.TestResultDeleteNotFound;
var
    response: TZBFileFeedback;
begin
    ZBMockResponse(200, BULK_DELETE_NOT_FOUND);
    response := BulkValidationResultDelete(MOCK_FILE_ID);

    AssertEquals('success', response.Success, False);
    AssertTrue('message does not have content', Length(response.Message) > 0);
    AssertTrue('file_name should not have content', Length(response.FileName) = 0);
    AssertTrue('file_id should not have content', Length(response.FileId) = 0);
end;

procedure TTestBulk.TestResultDeleteOk;
var
    response: TZBFileFeedback;
begin
    ZBMockResponse(200, BULK_DELETE_OK);
    response := BulkValidationResultDelete(MOCK_FILE_ID);

    AssertEquals('success', response.Success, True);
    AssertTrue('message does not have content', Length(response.Message) > 0);
    AssertTrue('file_name does not have content', Length(response.FileName) > 0);
    AssertTrue('file_id does not have content', Length(response.FileId) > 0);
end;

procedure TTestBulk.TestBulkValidationFileSubmitEndpoint;
begin
    ZBMockResponse(200, BULK_SUBMIT_OK);
    BulkValidationFileSubmit(MOCK_FILE_CONTENT, SUBMIT_PARAM);
    AssertEndpointCalled(ENDPOINT_FILE_SEND);
    if not ZbResponseMock.UrlCalled.Contains(BULK_URI) then
        Fail('BulkValidationFileSubmit not using BULK_URI');
end;

procedure TTestBulk.TestBulkValidationFileStatusCheckEndpoint;
begin
    ZBMockResponse(200, BULK_STATUS_OK);
    BulkValidationFileStatusCheck(MOCK_FILE_ID);
    AssertEndpointCalled(ENDPOINT_FILE_STATUS);
    if not ZbResponseMock.UrlCalled.Contains(BULK_URI) then
        Fail('BulkValidationFileStatusCheck not using BULK_URI');
end;

procedure TTestBulk.TestBulkValidationResultFetchEndpoint;
begin
    ZBMockResponse(200, BULK_RESULT_DELETED);
    BulkValidationResultFetch(MOCK_FILE_ID);
    AssertEndpointCalled(ENDPOINT_FILE_RESULT);
    if not ZbResponseMock.UrlCalled.Contains(BULK_URI) then
        Fail('BulkValidationResultFetch not using BULK_URI');
end;

procedure TTestBulk.TestBulkValidationResultDeleteEndpoint;
begin
    ZBMockResponse(200, BULK_DELETE_NOT_FOUND);
    BulkValidationResultDelete(MOCK_FILE_ID);
    AssertEndpointCalled(ENDPOINT_FILE_DELETE);
    if not ZbResponseMock.UrlCalled.Contains(BULK_URI) then
        Fail('BulkValidationResultDelete not using BULK_URI');
end;

procedure TTestBulk.TestAiScoringFileSubmitEndpoint;
begin
    ZBMockResponse(200, BULK_SUBMIT_OK);
    AiScoringFileSubmit(MOCK_FILE_CONTENT, SUBMIT_PARAM);
    AssertEndpointCalled(ENDPOINT_SCORING_SEND);
    if not ZbResponseMock.UrlCalled.Contains(BULK_URI) then
        Fail('AiScoringFileSubmit not using BULK_URI');
end;

procedure TTestBulk.TestAiScoringFileStatusCheckEndpoint;
begin
    ZBMockResponse(200, BULK_STATUS_OK);
    AiScoringFileStatusCheck(MOCK_FILE_ID);
    AssertEndpointCalled(ENDPOINT_SCORING_STATUS);
    if not ZbResponseMock.UrlCalled.Contains(BULK_URI) then
        Fail('AiScoringFileStatusCheck not using BULK_URI');
end;

procedure TTestBulk.TestAiScoringResultFetchEndpoint;
begin
    ZBMockResponse(200, BULK_RESULT_DELETED);
    AiScoringResultFetch(MOCK_FILE_ID);
    AssertEndpointCalled(ENDPOINT_SCORING_RESULT);
    if not ZbResponseMock.UrlCalled.Contains(BULK_URI) then
        Fail('AiScoringResultFetch not using BULK_URI');
end;

procedure TTestBulk.TestAiScoringResultDeleteEndpoint;
begin
    ZBMockResponse(200, BULK_DELETE_NOT_FOUND);
    AiScoringResultDelete(MOCK_FILE_ID);
    AssertEndpointCalled(ENDPOINT_SCORING_DELETE);
    if not ZbResponseMock.UrlCalled.Contains(BULK_URI) then
        Fail('AiScoringResultDelete not using BULK_URI');
end;


initialization

    RegisterTest(TTestBulk);
end.

