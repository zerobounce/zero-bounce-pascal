unit TestValidation;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testutils, testregistry, fpjson, jsonparser,
    ZbValidation, ZbUtility;

type

    TTestValidation= class(TTestCase)
    protected
        procedure TearDown; override;
    published
        procedure TestEmailValidation;
        procedure TestBatchBodyEncoder;
        procedure TestBatchBodyEncoderOneEmail;
        procedure TestBatchBodyEncoderNoEmails;
        procedure TestBatchEmailValidationErrorPayload;
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


procedure TTestValidation.TestEmailValidation;
begin
    // TODO:
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


procedure TestBatchEmailValidationErrorPayload;
begin
    // TODO:
end;

procedure TestBatchEmailValidationOkPayload;
begin
    // TODO:
end;


initialization
    RegisterTest(TTestValidation);
end.

