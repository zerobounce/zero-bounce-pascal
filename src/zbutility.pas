unit ZbUtility;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fphttpclient;

type
    TZbRequestResponse = record
    StatusCode: integer;
    Payload: String;
    Headers: TStrings;
    UrlCalled: String;
	end;

    HTTPClientClass = Class of TFPHTTPClient;
    ZbException = Class(Exception)
    public
        StatusCode: Integer;
        Payload: String;
        constructor FromResponse(AMessage: String; response: TZbRequestResponse);
        procedure MarkHttpError;
        procedure MarkJsonError;
    end;

const
    BASE_URI = 'https://api.zerobounce.net/v2';
    BULK_URI = 'https://bulkapi.zerobounce.net/v2';
    ENDPOINT_CREDITS = '/getcredits';
    ENDPOINT_ACTIVITY_DATA = '/activity';
    ENDPOINT_VALIDATE = '/validate';
    ENDPOINT_API_USAGE = '/getapiusage';
    ENDPOINT_BATCH_VALIDATE = '/validatebatch';
    ENDPOINT_FILE_SEND = '/sendfile';
    ENDPOINT_FILE_STATUS = '/filestatus';
    ENDPOINT_FILE_RESULT = '/getfile';
    ENDPOINT_FILE_DELETE = '/deletefile';
    ENDPOINT_SCORING_SEND = '/scoring/sendfile';
    ENDPOINT_SCORING_STATUS = '/scoring/filestatus';
    ENDPOINT_SCORING_RESULT = '/scoring/getfile';
    ENDPOINT_SCORING_DELETE = '/scoring/deletefile';
    cDefaultMock: TZbRequestResponse = (
        StatusCode: 0; Payload: ''; Headers: nil; UrlCalled: ''
    );

var
    ZbApiKey: string = '';
    ZbResponseMock: TZbRequestResponse = (
        StatusCode: 0; Payload: ''; Headers: nil; UrlCalled: ''
    );

    procedure ZBSetApiKey ( ApiKey : string );
    function ZBGetRequest(url: String): TZbRequestResponse;
    function ZBPostRequest(url: String; JsonParam: String): TZbRequestResponse;
    procedure ZBMockResponse(StatusCode: integer; Payload: String);
    procedure ZBMockResponse(StatusCode: integer; Payload: String; Headers: TStrings);
    procedure Register;
implementation

    constructor ZbException.FromResponse(AMessage: String; response: TZbRequestResponse);
    var
        NewMessage: String;
    begin
        Payload := response.Payload;
        StatusCode := response.StatusCode;
        NewMessage := Concat(AMessage, sLineBreak, 'Url:', sLineBreak, Payload);
        NewMessage := Concat(NewMessage, sLineBreak, 'Status code: ', format('%d', [StatusCode]));
        NewMessage := Concat(NewMessage, sLineBreak, 'Payload:', sLineBreak, Payload);
        Create(NewMessage);
    end;

    procedure ZbException.MarkHttpError;
    begin
        Self.Message := 'Http Error: ' + Self.Message;
	end;

    procedure ZbException.MarkJsonError;
    begin
         Self.Message := 'Json Error: ' + Self.Message;
	end;

    procedure ZBSetApiKey ( ApiKey : string );
    begin
        ZbApiKey := ApiKey;
    end;

    function ZBGetRequest(url: String): TZbRequestResponse;
    var
        response: TZbRequestResponse;
        Client: TFPHTTPClient;
        error: ZbException;
    begin
        if ZbResponseMock.StatusCode <> 0 then
        begin
            response := ZbResponseMock;
            ZbResponseMock.UrlCalled := url;
		end
		else
        begin
            Client := TFPHTTPClient.Create(nil);
            try
                response.Payload := Client.Get(url);
                response.StatusCode := Client.ResponseStatusCode;
                response.Headers := Client.ResponseHeaders;
            finally
                Client.Free;
            end;
        end;

        // check for failure
        if response.StatusCode > 299 then
        begin
            error := ZbException.FromResponse('Request failed', response);
            error.MarkHttpError;
            raise error;
        end;

        response.UrlCalled := url;
        Result := response;
    end;

    function ZBPostRequest(url: String; JsonParam: String): TZbRequestResponse;
    var
        response: TZbRequestResponse;
        Client: TFPHTTPClient;
        error: ZbException;
    begin
        if ZbResponseMock.StatusCode <> 0 then
        begin
            response := ZbResponseMock;
            ZbResponseMock.UrlCalled := url;
		end
		else
        begin
            Client := TFPHTTPClient.Create(nil);

            Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
            Client.AddHeader('Accept', 'application/json');
            Client.RequestBody := TRawByteStringStream.Create(JsonParam);
            try
                response.Payload := Client.Post(url);
                response.StatusCode := Client.ResponseStatusCode;
                response.Headers := Client.ResponseHeaders;
            finally
                Client.Free;
            end;
        end;

        // check for failure
        if response.StatusCode > 299 then
        begin
            error := ZbException.FromResponse('Request failed', response);
            error.MarkHttpError;
            raise error;
        end;

        response.UrlCalled := url;
        Result := response;
    end;

    procedure ZBMockResponse(StatusCode: integer; Payload: String);
    begin
        ZbResponseMock.StatusCode := StatusCode;
        ZbResponseMock.Payload := Payload;
    end;

    procedure ZBMockResponse(StatusCode: integer; Payload: String; Headers: TStrings);
    begin
        ZBMockResponse(StatusCode, Payload);
        if ZbResponseMock.Headers <> nil then
           ZbResponseMock.Headers.Free;
        ZbResponseMock.Headers := Headers;
    end;

    procedure Register;
    begin
    end;

end.

