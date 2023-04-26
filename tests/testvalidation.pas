unit TestValidation;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry, fpjson, jsonparser,
    MockValues, ZbStructures, ZbValidation, ZbUtility;

type

    TTestValidation= class(TTestCase)
    protected
        procedure TearDown; override;
    published
        procedure TestEmailValidationEndpoint;
        procedure TestEmailValidationOk;
        procedure TestEmailValidationError;
        procedure TestBatchBodyEncoder;
        procedure TestBatchBodyEncoderOneEmail;
        procedure TestBatchBodyEncoderNoEmails;
        procedure TestBatchEmailValidationEndpoint;
        procedure TestBatchEmailValidationErrorResponse;
        procedure TestBatchEmailValidationErrorInPayload;
        procedure TestBatchEmailValidationOkPayload;
    end;

const
    MOCK_API_KEY = 'mock_api_key';

    EmailsAndIps0: array of TZbEmailAndIp = ();
    EmailsAndIps1: array of TZbEmailAndIp = (
        (Email: 'valid@example.com'; Ip: '1.1.1.1')
    );
    EmailsAndIps2: array of TZbEmailAndIp = (
        (Email: 'valid@example.com'; Ip: '1.1.1.1'),
        (Email: 'invalid@example.com'; Ip: '')
    );

implementation

procedure TTestValidation.TearDown;
begin
    if ZbResponseMock.Headers <> nil then
        ZbResponseMock.Headers.Free;
    ZbResponseMock := cDefaultMock;
end;


procedure TTestValidation.TestEmailValidationEndpoint;
begin
    ZBMockResponse(200, BATCH_VALIDATE_OK);
    ZbBatchValidateEmails(EmailsAndIps2);
    AssertTrue(
        'Endpoint ' + ENDPOINT_VALIDATE + ' not called',
        ZbResponseMock.UrlCalled.Contains(ENDPOINT_VALIDATE)
    );
end;


procedure TTestValidation.TestEmailValidationOk;
var
   validation: TZbValidationResult;
begin
    ZBMockResponse(200, VALDATION_RESPONSE_VALID);
    validation := ZbValidateEmail('valid@example.com');

    AssertEquals('address', validation.Address, 'valid@example.com');
    AssertEquals('did_you_mean', validation.DidYouMean, '');
    AssertEquals('free_email', validation.FreeEmail, FALSE);

    AssertEquals(
        'processed_at',
        validation.ProcessedAt,
        EncodeDateTime(2023, 04, 25, 13, 08, 24, 269)
    );
end;

procedure TTestValidation.TestEmailValidationError;
begin
    ZBMockResponse(400, ERROR_PAYLOAD);

    try
        ZbValidateEmail('valid@example.com');
        Fail('test should have raised exception');
    except
        on e: ZbException do
        begin
            AssertEquals('status code', e.StatusCode, 400);
            AssertTrue('error message', e.Message.Contains(ERROR_MESSAGE));
        end;
    end;
end;

procedure TTestValidation.TestBatchBodyEncoder;
var
    JObject: TJSONObject;
    JArray: TJSONArray;
    IsValid: Boolean;
begin
    ZBSetApiKey(MOCK_API_KEY);

    JObject := TJSONObject(GetJSON(ZbBatchRequestBodyFromEmails(EmailsAndIps2)));
    AssertEquals('api_key', JObject.Find('api_key').AsString, MOCK_API_KEY);
    IsValid := JObject.Find('email_batch', JArray);
    AssertTrue('"email_batch" not found', IsValid);

    AssertEquals('first email', JArray.Objects[0].Find('email_address').AsString, EmailsAndIps2[0].Email);
    AssertEquals('first ip', JArray.Objects[0].Find('ip_address').AsString, EmailsAndIps2[0].Ip);
    AssertEquals('second email', JArray.Objects[1].Find('email_address').AsString, EmailsAndIps2[1].Email);
    AssertEquals('second ip', JArray.Objects[1].Find('ip_address').AsString, EmailsAndIps2[1].Ip);
end;

procedure TTestValidation.TestBatchBodyEncoderOneEmail;
var
    JObject: TJSONObject;
    JArray: TJSONArray;
    IsValid: Boolean;
begin
    ZBSetApiKey(MOCK_API_KEY);

    JObject := TJSONObject(GetJSON(ZbBatchRequestBodyFromEmails(EmailsAndIps1)));
    AssertEquals('api_key', JObject.Find('api_key').AsString, MOCK_API_KEY);
    IsValid := JObject.Find('email_batch', JArray);
    AssertTrue('"email_batch" not found', IsValid);

    AssertEquals('array size', JArray.Count, 1);
    AssertEquals('first email', JArray.Objects[0].Find('email_address').AsString, EmailsAndIps1[0].Email);
    AssertEquals('first ip', JArray.Objects[0].Find('ip_address').AsString, EmailsAndIps1[0].Ip);
end;

procedure TTestValidation.TestBatchBodyEncoderNoEmails;
var
    JObject: TJSONObject;
    JArray: TJSONArray;
    IsValid: Boolean;
begin
    ZBSetApiKey(MOCK_API_KEY);

    JObject := TJSONObject(GetJSON(ZbBatchRequestBodyFromEmails(EmailsAndIps0)));
    AssertEquals('api_key', JObject.Find('api_key').AsString, MOCK_API_KEY);
    IsValid := JObject.Find('email_batch', JArray);
    AssertTrue('"email_batch" not found', IsValid);
    AssertEquals('array size', JArray.Count, 0);
end;


procedure TTestValidation.TestBatchEmailValidationEndpoint;
begin
    ZBMockResponse(200, BATCH_VALIDATE_OK);
    ZbBatchValidateEmails(EmailsAndIps2);
    AssertTrue(
        'Endpoint '+ ENDPOINT_BATCH_VALIDATE + ' not called',
        ZbResponseMock.UrlCalled.Contains(ENDPOINT_BATCH_VALIDATE)
    );
end;


procedure TTestValidation.TestBatchEmailValidationErrorResponse;
begin
    ZBMockResponse(400, ERROR_PAYLOAD);

    try
        ZbBatchValidateEmails(['valid@example.com']);
        Fail('test should have raised exception');
    except
        on e: ZbException do
        begin
            AssertEquals('status code', e.StatusCode, 400);
            AssertTrue('error message', e.Message.Contains(ERROR_MESSAGE));
        end;
    end;
end;

procedure TTestValidation.TestBatchEmailValidationErrorInPayload;
var
    validation: TZBBatchValidation;
begin
    ZBMockResponse(200, BATCH_VALIDATE_ERROR);
    validation := ZbBatchValidateEmails(EmailsAndIps2);

    AssertEquals('email_batch length', validation.EmailBatchLength, 0);
    AssertEquals('errors length', validation.ErrorsLength, 1);
    AssertEquals('email_address in error', validation.Errors[0].EmailAddress, 'all');
end;

procedure TTestValidation.TestBatchEmailValidationOkPayload;
var
    validation: TZBBatchValidation;
begin
    ZBMockResponse(200, BATCH_VALIDATE_OK);
    validation := ZbBatchValidateEmails(EmailsAndIps2);

    AssertEquals('email_batch length', validation.EmailBatchLength, 2);
    AssertEquals('errors length', validation.ErrorsLength, 0);
    AssertEquals('status of first validation', validation.EmailBatch[0].Status, 'valid');
    AssertEquals('status of first validation', validation.EmailBatch[1].Status, 'invalid');
end;


initialization
    RegisterTest(TTestValidation);
end.

