unit ZbUtility;

{$I zboptions.inc}{$H+}

interface

uses
    Classes, SysUtils, StrUtils, Types,
    {$IFDEF FPC}
    openssl, opensslsockets, fphttpclient, httpprotocol, ssockets, sslsockets, dynlibs
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

    ZbApiURL = (ZbApiURLDefault, ZbApiURLUSA, ZbApiURLEU);

const
    JSON_CONTENT_TYPE = 'application/json';
    BULK_URI = 'https://bulkapi.zerobounce.net/v2';
    { Bulk getfile query parameter download_type (validation and scoring). }
    ZB_DOWNLOAD_TYPE_PHASE_1 = 'phase_1';
    ZB_DOWNLOAD_TYPE_PHASE_2 = 'phase_2';
    ZB_DOWNLOAD_TYPE_COMBINED = 'combined';
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
    cDefaultMock: TZbRequestResponse = (
        StatusCode: 0; Payload: ''; ContentType: ''; UrlCalled: ''
    );

var
    ZbApiURLValue: array[ZbApiURL] of string = (
        'https://api.zerobounce.net/v2',
        'https://api-us.zerobounce.net/v2',
        'https://api-eu.zerobounce.net/v2'
    );
    ZbApiBaseURL: string = 'https://api.zerobounce.net/v2';
    ZbApiKey: string = '';
    { FPC only: PEM CA bundle for TLS verification. When empty, SSL_CERT_FILE, system paths, and cacert.pem beside the exe are tried. }
    ZbCACertFile: string = '';
    ZbResponseMock: TZbRequestResponse = (
        StatusCode: 0; Payload: ''; ContentType: ''; UrlCalled: ''
    );

    function ZBApiURLToString(url: ZbApiURL): string;
    procedure ZBSetApiKey ( ApiKey : string );  deprecated 'Use ZBInitialize procedures';
    procedure ZBInitialize(ApiKey: string); overload;
    procedure ZBInitialize(ApiKey: string; ApiBaseURL: ZbApiURL); overload;
    function EncodeParam(param: String): String;
    function RedactSecrets(const S: string): string;
    function ZBGetRequest(url: String): TZbRequestResponse;
    // performs a POST request with a raw JSON body
    function ZBPostRequest(url: String; JsonParam: String): TZbRequestResponse; overload;
    // performs a POST request with a multi-part form body
    function ZBPostRequest(url: String; FormData: TStrings; FileContent: String): TZbRequestResponse; overload;
    procedure ZBMockResponse(StatusCode: integer; Payload, ContentType: String); overload;
    procedure ZBMockResponse(StatusCode: integer; Payload: String); overload;
    procedure Register;
implementation

{$IFDEF FPC}
type
    { fphttpclient does not verify server certificates by default; api_key in URLs would be exposed to MITM. }
    TZbTlsVerifier = class
        procedure GetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
        procedure VerifyCertificate(Sender: TObject; var Allow: Boolean);
    end;

    TZbSslGetPeerCert = function(ssl: PSSL): PX509; cdecl;
    TZbX509CheckHost = function(x: PX509; chk: PAnsiChar; chklen: PtrUInt; flags: Cardinal; peername: Pointer): Integer; cdecl;

var
    ZbTlsVerifier: TZbTlsVerifier = nil;

function ZbFindCACertFile: string;
const
    Candidates: array[0..3] of string = (
        '/etc/ssl/certs/ca-certificates.crt',
        '/etc/pki/tls/certs/ca-bundle.crt',
        '/etc/ssl/ca-bundle.pem',
        '/etc/ssl/cert.pem'
    );
var
    I: Integer;
begin
    Result := ZbCACertFile;
    if Result = '' then
        Result := GetEnvironmentVariable('SSL_CERT_FILE');
    if Result <> '' then
        Exit;
    for I := Low(Candidates) to High(Candidates) do
        if FileExists(Candidates[I]) then
            Exit(Candidates[I]);
    Result := ExtractFilePath(ParamStr(0)) + 'cacert.pem';
    if not FileExists(Result) then
        Result := '';
end;

procedure TZbTlsVerifier.GetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
var
    Handler: TSSLSocketHandler;
    CAFile: string;
begin
    AHandler := nil;
    if not UseSSL then
        Exit;
    CAFile := ZbFindCACertFile;
    if CAFile = '' then
        raise ZbException.Create('No CA certificate bundle found to verify the TLS connection. ' +
            'Set ZbCACertFile or the SSL_CERT_FILE environment variable.');
    Handler := TSSLSocketHandler.GetDefaultHandler;
    Handler.VerifyPeerCert := True;
    Handler.CertificateData.CertCA.FileName := CAFile;
    Handler.OnVerifyCertificate := VerifyCertificate;
    AHandler := Handler;
end;

procedure TZbTlsVerifier.VerifyCertificate(Sender: TObject; var Allow: Boolean);
var
    Handler: TOpenSSLSocketHandler;
    GetPeer: TZbSslGetPeerCert;
    CheckHost: TZbX509CheckHost;
    Cert: PX509;
    Host: AnsiString;
begin
    Allow := False;
    if not (Sender is TOpenSSLSocketHandler) then
        Exit;
    Handler := TOpenSSLSocketHandler(Sender);
    if (Handler.SSL = nil) or not (Handler.Socket is TInetSocket) then
        Exit;
    if Handler.SSL.VerifyResult <> 0 then
        Exit;
    GetPeer := TZbSslGetPeerCert(GetProcedureAddress(SSLLibHandle, 'SSL_get1_peer_certificate'));
    if not Assigned(GetPeer) then
        GetPeer := TZbSslGetPeerCert(GetProcedureAddress(SSLLibHandle, 'SSL_get_peer_certificate'));
    CheckHost := TZbX509CheckHost(GetProcedureAddress(SSLUtilHandle, 'X509_check_host'));
    if not Assigned(GetPeer) or not Assigned(CheckHost) then
        Exit;
    Cert := GetPeer(Handler.SSL.SSL);
    if Cert = nil then
        Exit;
    try
        Host := AnsiString(TInetSocket(Handler.Socket).Host);
        Allow := CheckHost(Cert, PAnsiChar(Host), Length(Host), 0, nil) = 1;
    finally
        X509Free(Cert);
    end;
end;

procedure ZbUseVerifiedTls(Client: TFPHTTPClient);
begin
    if ZbTlsVerifier = nil then
        ZbTlsVerifier := TZbTlsVerifier.Create;
    Client.OnGetSocketHandler := ZbTlsVerifier.GetSocketHandler;
end;
{$ENDIF}

    constructor ZbException.FromResponse(AMessage: String; response: TZbRequestResponse);
    var
        NewMessage: String;
    begin
        Payload := response.Payload;
        StatusCode := response.StatusCode;
        NewMessage := Concat(AMessage, sLineBreak, 'Url:', sLineBreak, RedactSecrets(response.UrlCalled));
        NewMessage := Concat(NewMessage, sLineBreak, 'Status code: ', format('%d', [StatusCode]));
        NewMessage := Concat(NewMessage, sLineBreak, 'Payload:', sLineBreak, RedactSecrets(Payload));
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

    function ZBApiURLToString(url: ZbApiURL): string;
    begin
        ZBApiURLToString := ZbApiURLValue[url];
    end;

    procedure ZBSetApiKey ( ApiKey : string );
    begin
        ZbApiKey := ApiKey;
    end;

    procedure ZBInitialize(ApiKey: string);
    begin
        ZbApiKey := ApiKey;
        ZbApiBaseURL := ZBApiURLToString(ZbApiURLDefault);
    end;

    procedure ZBInitialize(ApiKey: string; ApiBaseURL: ZbApiURL); overload;
    begin
        ZbApiKey := ApiKey;
        ZbApiBaseURL := ZBApiURLToString(ApiBaseURL);
    end;

    function EncodeParam(param: String): String;
    {$IFNDEF FPC}
    var
        Encoding: TURLEncoding;
    {$ENDIF}
    begin
        {$IFDEF FPC}
        Result := HTTPEncode(param);
        {$ELSE}
        Encoding := TURLEncoding.Create;
        try
            Result := Encoding.EncodeQuery(param);
        finally
            Encoding.Free;
        end;
        {$ENDIF}
    end;

    function RedactSecrets(const S: string): string;
    begin
        if ZbApiKey <> '' then
            Result := StringReplace(S, ZbApiKey, 'REDACTED', [rfReplaceAll])
        else
            Result := S;
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
            ZbUseVerifiedTls(Client);
            Client.ConnectTimeout := 30000;
            Client.IOTimeout := 120000;
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
            ZbUseVerifiedTls(Client);
            Client.ConnectTimeout := 30000;
            Client.IOTimeout := 120000;

            Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
            Client.AddHeader('Accept', 'application/json');
            Client.RequestBody := TRawByteStringStream.Create(JsonParam);
            try
                Result.Payload := Client.Post(url);
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                Client.RequestBody.Free;
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
            ZbUseVerifiedTls(Client);
            Client.ConnectTimeout := 30000;
            Client.IOTimeout := 120000;
            ResponseStream := TStringStream.Create('');

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

{$IFDEF FPC}
finalization
    FreeAndNil(ZbTlsVerifier);
{$ENDIF}

end.

