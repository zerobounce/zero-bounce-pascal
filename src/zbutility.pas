unit ZbUtility;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, openssl, opensslsockets, fphttpclient;

type
    TZbRequestResponse = record
        StatusCode: Integer;
        Payload: String;
        ContentType: String;
        UrlCalled: String;
	end;

    ZbException = Class(Exception)
    public
        StatusCode: Integer;
        Payload: String;
        constructor FromResponse(AMessage: String; response: TZbRequestResponse);
        procedure MarkHttpError;
        procedure MarkJsonError;
    end;

const
    JSON_CONTENT_TYPE = 'application/json';
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
    cDefaultMock: TZbRequestResponse = ();

var
    ZbApiKey: string = '';
    ZbResponseMock: TZbRequestResponse = ();

    procedure ZBSetApiKey ( ApiKey : string );
    function ZBGetRequest(url: String): TZbRequestResponse;
    // performs a POST request with a raw JSON body
    function ZBPostRequest(url: String; JsonParam: String): TZbRequestResponse;
    // performs a POST request with a multi-part form body
    function ZBPostRequest(url: String; FormData: TStrings; FileContent: String): TZbRequestResponse;
    procedure ZBMockResponse(StatusCode: integer; Payload, ContentType: String);
    procedure ZBMockResponse(StatusCode: integer; Payload: String);
    procedure Register;
implementation

    constructor ZbException.FromResponse(AMessage: String; response: TZbRequestResponse);
    var
        NewMessage: String;
    begin
        Payload := response.Payload;
        StatusCode := response.StatusCode;
        NewMessage := Concat(AMessage, sLineBreak, 'Url:', sLineBreak, response.UrlCalled);
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
        Client: TFPHTTPClient;
        error: ZbException;
    begin
        if ZbResponseMock.StatusCode <> 0 then
        begin
            ZbResponseMock.UrlCalled := url;
            Result := ZbResponseMock;
		end
		else
        begin
            InitSSLInterface;

            Result.UrlCalled := url;
            Client := TFPHTTPClient.Create(nil);
            try
                Result.Payload := Client.Get(url);
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                Client.Free;
            end;
        end;

        // check for failure
        if Result.StatusCode > 299 then
        begin
            error := ZbException.FromResponse('Request failed', Result);
            error.MarkHttpError;
            raise error;
        end;
    end;

    function ZBPostRequest(url: String; JsonParam: String): TZbRequestResponse;
    var
        Client: TFPHTTPClient;
        error: ZbException;
    begin
        if ZbResponseMock.StatusCode <> 0 then
        begin
            ZbResponseMock.UrlCalled := url;
            Result := ZbResponseMock;
        end
        else
        begin
            InitSSLInterface;

            Result.UrlCalled := url;
            Client := TFPHTTPClient.Create(nil);

            Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
            Client.AddHeader('Accept', 'application/json');
            Client.RequestBody := TRawByteStringStream.Create(JsonParam);
            try
                Result.Payload := Client.Post(url);
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                Client.Free;
            end;
        end;

        // check for failure
        if Result.StatusCode > 299 then
        begin
            error := ZbException.FromResponse('Request failed', Result);
            error.MarkHttpError;
            raise error;
        end;
    end;

    function ZBPostRequest(url: String; FormData: TStrings; FileContent: String): TZbRequestResponse;
    const
        FILE_NAME = 'bulk_upload.csv';
    var
        Client: TFPHTTPClient;
        error: ZbException;
        FileStream: TStream;
        ResponseStream: TStream;
    begin
        if ZbResponseMock.StatusCode <> 0 then
        begin
            Result := ZbResponseMock;
            ZbResponseMock.UrlCalled := url;
        end
        else
        begin
            InitSSLInterface;

            Result.UrlCalled := url;
            FileStream := TStringStream.Create(FileContent);
            Client := TFPHTTPClient.Create(nil);
            ResponseStream := TStringStream.Create(FileContent);

            try
                try
                    Client.StreamFormPost(
                        url,
                        FormData,
                        'file',
                        FILE_NAME,
                        FileStream,
                        ResponseStream
                    );
                except
                on e: Exception do
                    begin
                        error := ZbException.FromResponse(e.Message, Result);
                        error.MarkHttpError;
                        raise error;
                    end;
                end;

                Result.Payload := ResponseStream.ReadAnsiString();
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                Client.Free;
                FileStream.Free;
                FormData.Free;
                if ResponseStream <> nil then
                    ResponseStream.Free;
            end;
        end;

            // check for failure
            if Result.StatusCode > 299 then
            begin
                error := ZbException.FromResponse('Request failed', Result);
                error.MarkHttpError;
                raise error;
            end;
    end;

    procedure ZBMockResponse(StatusCode: integer; Payload, ContentType: String);
    begin
        ZbResponseMock.StatusCode := StatusCode;
        ZbResponseMock.Payload := Payload;
        ZbResponseMock.ContentType := ContentType;
    end;

    procedure ZBMockResponse(StatusCode: integer; Payload: String);
    begin
        ZBMockResponse(StatusCode, Payload, JSON_CONTENT_TYPE);
    end;

    procedure Register;
    begin
    end;

end.

