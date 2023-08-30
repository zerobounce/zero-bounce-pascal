unit ZbUtility;

{$I zboptions.inc}{$H+}

interface

uses
    Classes, SysUtils, StrUtils, Types,
    {$IFDEF FPC}
    openssl, opensslsockets, fphttpclient, httpprotocol
    {$ELSE}
    System.Net.HttpClient,
    System.Net.Mime,
    System.NetEncoding
    {$ENDIF}
;

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
    ENDPOINT_EMAIL_FINDER = '/guessformat';
    cDefaultMock: TZbRequestResponse = ();

var
    ZbApiKey: string = '';
    ZbResponseMock: TZbRequestResponse = ();

    procedure ZBSetApiKey ( ApiKey : string );
    function EncodeParam(param: String): String;
    function ZBGetRequest(url: String): TZbRequestResponse;
    // performs a POST request with a raw JSON body
    function ZBPostRequest(url: String; JsonParam: String): TZbRequestResponse; overload;
    // performs a POST request with a multi-part form body
    function ZBPostRequest(url: String; FormData: TStrings; FileContent: String): TZbRequestResponse; overload;
    procedure ZBMockResponse(StatusCode: integer; Payload, ContentType: String); overload;
    procedure ZBMockResponse(StatusCode: integer; Payload: String); overload;
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

    function EncodeParam(param: String): String;
    begin
        {$IFDEF FPC}
        Result := HTTPEncode(param);
        {$ELSE}
        Result := TURLEncoding.Create.EncodePath(param);
        {$ENDIF}
    end;

    function ZBGetRequest(url: String): TZbRequestResponse;
    var
        {$IFDEF FPC}
        Client: TFPHTTPClient;
        {$ELSE}
        Client: THTTPClient;
        Response: IHTTPResponse;
        {$ENDIF}
        error: ZbException;
    begin
        if ZbResponseMock.StatusCode <> 0 then
        begin
            ZbResponseMock.UrlCalled := url;
            Result := ZbResponseMock;
		end
		else
        begin
            Result.UrlCalled := url;

            {$IFDEF FPC}
            InitSSLInterface;

            Client := TFPHTTPClient.Create(nil);
            try
                Result.Payload := Client.Get(url);
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                Client.Free;
            end;

            {$ELSE}
            Client := THTTPClient.Create;
            try
              Response := Client.Get(url);
              Result.Payload := Response.ContentAsString();
              Result.StatusCode := Response.StatusCode;
              Result.ContentType := Response.HeaderValue['Content-Type'];
            finally
              Client.Free;
            end;
            {$ENDIF}

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
        {$IFDEF FPC}
        Client: TFPHTTPClient;
        {$ELSE}
        Client: THTTPClient;
        BodyStream: TStream;
        Response: IHTTPResponse;
        {$ENDIF}
        error: ZbException;
    begin
        if ZbResponseMock.StatusCode <> 0 then
        begin
            ZbResponseMock.UrlCalled := url;
            Result := ZbResponseMock;
        end
        else
        begin
            Result.UrlCalled := url;
            {$IFDEF FPC}
            InitSSLInterface;

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
            {$ELSE}
            Client := THTTPClient.Create;
            BodyStream := TStringStream.Create(JsonParam);
            try
              Client.CustomHeaders['Content-Type'] := 'application/json; charset=UTF-8';
              Client.CustomHeaders['Accept'] := 'application/json';
              Response := Client.Post(url, BodyStream);

              Result.Payload := Response.ContentAsString();
              Result.StatusCode := Response.StatusCode;
              Result.ContentType := Response.HeaderValue['Content-Type'];
            finally
              Client.Free;
              BodyStream.Free;
            end;
            {$ENDIF}

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
        {$IFDEF FPC}
        Client: TFPHTTPClient;
        ResponseStream: TStringStream;
        {$ELSE}
        Client: THTTPClient;
        MPForm: TMultiPartFormData;
        Response: IHTTPResponse;
        {$ENDIF}
        error: ZbException;
        FileStream: TStream;

        {$IFNDEF FPC}
        function FormDataFromTString(): TMultiPartFormData;
        var
          SIterator: TStringsEnumerator;
          ASplit: TStringDynArray;
        begin
          Result := TMultiPartFormData.Create(False);

          SIterator := TStringsEnumerator.Create(FormData);
          while (SIterator.MoveNext) do
          begin
            ASplit := SplitString(SIterator.GetCurrent, FormData.NameValueSeparator);
            Result.AddField(ASplit[0], ASplit[1]);
          end;

        end;
        {$ENDIF}
    begin
        if ZbResponseMock.StatusCode <> 0 then
        begin
            Result := ZbResponseMock;
            ZbResponseMock.UrlCalled := url;
        end
        else
        begin
            Result.UrlCalled := url;
            {$IFDEF FPC}
            InitSSLInterface;

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

                ResponseStream.Seek(0, soBeginning);
                Result.Payload := ResponseStream.DataString;
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                Client.Free;
                FileStream.Free;
                if ResponseStream <> nil then
                    ResponseStream.Free;
            end;
            {$ELSE}
            Client := THTTPClient.Create;
            FileStream := TStringStream.Create(FileContent);
            MPForm := FormDataFromTString;
            try
              MPForm.AddStream('file', FileStream, FILE_NAME, 'text/csv');
              Response := Client.Post(url, MPForm);

              Result.Payload := Response.ContentAsString();
              Result.StatusCode := Response.StatusCode;
              Result.ContentType := Response.HeaderValue['Content-Type'];
            finally
              MPForm.Free;
              Client.Free;
              FileStream.Free;
            end;
            {$ENDIF}
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

