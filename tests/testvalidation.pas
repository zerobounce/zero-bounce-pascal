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
        procedure TestBatchBodyEncoder;
        procedure TestBatchBodyEncoderOneEmail;
        procedure TestBatchBodyEncoderNoEmails;
    end;

implementation

procedure TTestValidation.TearDown;
begin
    if ZbResponseMock.Headers <> nil then
        ZbResponseMock.Headers.Free;
    ZbResponseMock := cDefaultMock;
end;


procedure TTestValidation.TestBatchBodyEncoder;
const
    MOCK_API_KEY = 'mock_api_key';

    EmailsAndIps:array of TZbEmailAndIp = (
        (Email: 'valid@example.com'; Ip: '1.1.1.1'),
        (Email: 'invalid@example.com'; Ip: '')
    );
var
    JObject: TJSONObject;
    JArray: TJSONArray;
    IsValid: Boolean;
begin
    ZBSetApiKey(MOCK_API_KEY);

    JObject := TJSONObject(GetJSON(ZbBatchRequestBodyFromEmails(EmailsAndIps)));
    AssertEquals('api_key', JObject.Find('api_key').AsString, MOCK_API_KEY);
    IsValid := JObject.Find('email_batch', JArray);
    AssertTrue('"email_batch" not found', IsValid);

    AssertEquals('array size', JArray.Count, 2);
    AssertEquals('first email', JArray.Objects[0].Find('email').AsString, EmailsAndIps[0].Email);
    AssertEquals('first ip', JArray.Objects[0].Find('ip').AsString, EmailsAndIps[0].Ip);
    AssertEquals('second email', JArray.Objects[0].Find('email').AsString, EmailsAndIps[1].Email);
    AssertEquals('second ip', JArray.Objects[0].Find('ip').AsString, EmailsAndIps[1].Ip);
end;

procedure TTestValidation.TestBatchBodyEncoderOneEmail;
const
    MOCK_API_KEY = 'mock_api_key';
    EmailsAndIps:array of TZbEmailAndIp = ((Email: 'valid@example.com'; Ip: '1.1.1.1'));
var
    JObject: TJSONObject;
    JArray: TJSONArray;
    IsValid: Boolean;
begin
    ZBSetApiKey(MOCK_API_KEY);

    JObject := TJSONObject(GetJSON(ZbBatchRequestBodyFromEmails(EmailsAndIps)));
    AssertEquals('api_key', JObject.Find('api_key').AsString, MOCK_API_KEY);
    IsValid := JObject.Find('email_batch', JArray);
    AssertTrue('"email_batch" not found', IsValid);

    AssertEquals('array size', JArray.Count, 1);
    AssertEquals('first email', JArray.Objects[0].Find('email').AsString, EmailsAndIps[0].Email);
    AssertEquals('first ip', JArray.Objects[0].Find('ip').AsString, EmailsAndIps[0].Ip);
end;

procedure TTestValidation.TestBatchBodyEncoderNoEmails;
const
    MOCK_API_KEY = 'mock_api_key';
    EmailsAndIps:array of TZbEmailAndIp = ();
var
    JObject: TJSONObject;
    JArray: TJSONArray;
    IsValid: Boolean;
begin
    ZBSetApiKey(MOCK_API_KEY);

    JObject := TJSONObject(GetJSON(ZbBatchRequestBodyFromEmails(EmailsAndIps)));
    AssertEquals('api_key', JObject.Find('api_key').AsString, MOCK_API_KEY);
    IsValid := JObject.Find('email_batch', JArray);
    AssertTrue('"email_batch" not found', IsValid);
    AssertEquals('array size', JArray.Count, 0);
end;


initialization
    RegisterTest(TTestValidation);
end.

