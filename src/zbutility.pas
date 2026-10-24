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
    BULK_HOST = 'bulkapi.zerobounce.net';
    { Bulk getfile query parameter download_type (validation and scoring). }
    ZB_DOWNLOAD_TYPE_PHASE_1 = 'phase_1';
    ZB_DOWNLOAD_TYPE_PHASE_2 = 'phase_2';
    ZB_DOWNLOAD_TYPE_COMBINED = 'combined';
    ENDPOINT_CREDITS = '/getcredits';
    ENDPOINT_ACTIVITY_DATA = '/activity';
    ENDPOINT_SCORE_EMAIL = '/scoring';
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
    { Bulk hosts. Tests may point these at a local stub. }
    BULK_URI: string = 'https://bulkapi.zerobounce.net/v2';
    ZbBulkRootURL: string = 'https://bulkapi.zerobounce.net';
    { The list evaluator and the Inbox Placement Tester have their own base
      URLs, not paths built from BULK_URI, so overriding that alone leaves them
      pointing at the live API. }
    ZbListEvaluatorURL: string = 'https://bulkapi.zerobounce.net/v2/listevaluator';
    ZbInboxPlacementURL: string = 'https://bulkapi.zerobounce.net/api/ipt';
    { What the last request actually sent. Offline tests assert on these. }
    ZbLastRequestMethod: string = '';
    ZbLastRequestBody: string = '';
    { 'Name: Value' of the one extra header the last request carried, or ''. }
    ZbLastRequestHeader: string = '';
    { FPC only: PEM CA bundle for TLS verification. When empty, SSL_CERT_FILE, system paths, and cacert.pem beside the exe are tried. }
    ZbCACertFile: string = '';
    ZbResponseMock: TZbRequestResponse = (
        StatusCode: 0; Payload: ''; ContentType: ''; UrlCalled: ''
    );
    { BULK_HOST has no documented regional variant, so pairing it with a
      regional base URL is worth one warning and no more. }
    ZbBulkRegionWarned: Boolean = False;

{ Per-thread. One thread must not call ZBInitialize for a second key while another request is in flight on that same thread. }
threadvar
    ZbApiBaseURL: string;
    ZbApiKey: string;

    function ZBApiURLToString(url: ZbApiURL): string;
    procedure ZBSetApiKey ( ApiKey : string );  deprecated 'Use ZBInitialize procedures';
    procedure ZBInitialize(ApiKey: string); overload;
    procedure ZBInitialize(ApiKey: string; ApiBaseURL: ZbApiURL); overload;
    function EncodeParam(param: String): String;
    function RedactSecrets(const S: string): string;
    { The key, or a clear error. A key with surrounding whitespace or control
      characters would otherwise be quoted, unredacted, in a transport error. }
    function ZbCheckedApiKey(const ApiKey: String): String;
    { Raises unless the URL is https. Loopback is allowed so an offline test
      can point the SDK at a local stub; nothing leaves the machine there. }
    procedure ZbRequireHttpsUrl(const Url, Name: String);
    { One path segment, url-encoded, or a clear error. A segment of '.' or '..'
      would walk the request out of the endpoint it was meant for. }
    function ZbPathSegment(const Value, Name: String): String;
    { The URL, after checking it is https and warning once when a regional base
      URL is paired with the single global bulk host. }
    function ZbCheckedUrl(const Url: String): String;
    function ZbBodyLooksLikeHtml(const Body: String): Boolean;
    { Whether a JSON body says the call failed although the HTTP status was a
      success. The API sends "success" as a JSON false, "false" or "False". }
    function ZbSuccessMeansFailure(const Body: String): Boolean;
    function ZbMessageFromBody(const Body: String; StatusCode: Integer): String;
    function ZBGetRequest(url: String): TZbRequestResponse;
    // performs a POST request with a raw JSON body
    function ZBPostRequest(url: String; JsonParam: String): TZbRequestResponse; overload;
    // performs a POST request with application/x-www-form-urlencoded body
    { Encodes a value as a single path segment: EncodeParam is query encoding,
      which may turn a space into '+'. }
    function EncodePathSegment(param: String): String;
    { A request with an explicit method and an optional body, for the Inbox
      Placement Tester's PUT and DELETE. }
    function ZBSendRequest(Method, url, Body, ContentType: String): TZbRequestResponse;
    { As ZBSendRequest, plus one extra request header. The Inbox Placement
      Tester needs Authorization: Bearer. }
    function ZBSendRequestWithHeader(Method, url, Body, ContentType,
        HeaderName, HeaderValue: String): TZbRequestResponse;
    { As ZBSendRequestWithHeader, for a client that carries its own key: the
      validation api_key is neither required nor sent. The Warmup API is a
      separate product on its own host with an API-Key header. }
    function ZBSendForeignKeyRequest(Method, url, Body, ContentType,
        HeaderName, HeaderValue: String): TZbRequestResponse;
    { Escapes a string for a JSON double-quoted value. }
    function ZbJsonEscape(const Value: String): String;
    function ZBFormPostRequest(url: String; FormBody: String): TZbRequestResponse;
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

    function ZbLowerPrefix(const Value: String): String;
    var
        I: Integer;
    begin
        Result := Copy(Value, 1, 2000);
        for I := 1 to Length(Result) do
            if Result[I] in ['A'..'Z'] then
                Result[I] := Chr(Ord(Result[I]) + 32);
    end;

    function ZbBodyLooksLikeHtml(const Body: String): Boolean;
    var
        Lower: String;
    begin
        Lower := ZbLowerPrefix(Body);
        Result := (Pos('<!doctype html', Lower) > 0) or (Pos('<html', Lower) > 0) or
            (Pos('<head', Lower) > 0) or (Pos('<body', Lower) > 0);
    end;

    function ZbHtmlTitle(const Body: String): String;
    var
        Lower: String;
        StartAt, OpenEnd, CloseAt: Integer;
    begin
        Result := '';
        Lower := ZbLowerPrefix(Body);
        StartAt := Pos('<title', Lower);
        if StartAt = 0 then Exit;
        OpenEnd := Pos('>', Copy(Lower, StartAt, MaxInt));
        if OpenEnd = 0 then Exit;
        OpenEnd := StartAt + OpenEnd - 1;
        CloseAt := Pos('</title>', Copy(Lower, OpenEnd, MaxInt));
        if CloseAt = 0 then Exit;
        Result := Trim(Copy(Body, OpenEnd + 1, CloseAt - 2));
    end;

    function ZbStripErrorPrefix(const Value: String): String;
    begin
        Result := Trim(Value);
        while True do
        begin
            if Pos('Error:', Result) = 1 then
                Result := Trim(Copy(Result, 7, MaxInt))
            else if Pos('[Error]:', Result) = 1 then
                Result := Trim(Copy(Result, 9, MaxInt))
            else
                Break;
        end;
    end;

    function ZbSuccessMeansFailure(const Body: String): Boolean;
    var
        Trimmed, Lowered: String;
        Marker: Integer;
    begin
        Result := False;
        Trimmed := Trim(Body);
        if (Trimmed = '') or (Trimmed[1] <> '{') then
            Exit;
        Lowered := LowerCase(Trimmed);
        Marker := Pos('"success"', Lowered);
        if Marker = 0 then
            Exit;
        { Only the value that follows the key matters, and it is either a JSON
          false or the string "false". }
        Lowered := Copy(Lowered, Marker + 9, 20);
        Marker := Pos(':', Lowered);
        if Marker = 0 then
            Exit;
        Lowered := Trim(Copy(Lowered, Marker + 1, Length(Lowered) - Marker));
        Result := (Copy(Lowered, 1, 5) = 'false') or (Copy(Lowered, 1, 7) = '"false"');
    end;

    function ZbMessageFromBody(const Body: String; StatusCode: Integer): String;
    var
        Trimmed: String;
    begin
        Trimmed := Trim(Body);
        if Trimmed = '' then
            Exit(Format('Empty response from ZeroBounce (HTTP %d)', [StatusCode]));
        if ZbBodyLooksLikeHtml(Body) then
        begin
            Result := ZbHtmlTitle(Body);
            if Result = '' then
                Exit('ZeroBounce returned an HTML error page instead of an API response.');
            Exit('ZeroBounce returned an HTML error page ("' + Result + '") instead of an API response.');
        end;
        if (Length(Trimmed) > 0) and ((Trimmed[1] = '{') or (Trimmed[1] = '[')) then
        begin
            if (Pos('Access denied', Trimmed) > 0) and (Pos('cloudflare_error', Trimmed) > 0) then
                Exit('Error 1020: Access denied');
            if Length(Trimmed) > 1000 then
                Exit('ZeroBounce returned a response that is not valid JSON.');
            Exit(Trimmed);
        end;
        if Length(Trimmed) > 1000 then
            Exit('ZeroBounce returned a response that is not valid JSON.');
        Result := ZbStripErrorPrefix(Trimmed);
        if Result = '' then
            Result := Format('Empty response from ZeroBounce (HTTP %d)', [StatusCode]);
    end;

    procedure ZbRequireApiKey;
    begin
        if ZbApiKey = '' then
            raise ZbException.Create('ZeroBounce is not initialized. Please call ZBInitialize(api_key) first');
    end;

    procedure ZbFinishResponse(const Response: TZbRequestResponse);
    var
        Error: ZbException;
    begin
        { LIVE: a "success": false field on an HTTP 200 is a failure, not a
          result. The API answers an unknown bulk file id that way. }
        if (Response.StatusCode >= 300) and (Response.StatusCode < 400) then
        begin
            Error := ZbException.FromResponse(
                'ZeroBounce returned a redirect, which this client does not follow.', Response
            );
            Error.MarkHttpError;
            raise Error;
        end;
        if ZbBodyLooksLikeHtml(Response.Payload) or (Response.StatusCode > 299) or
           ZbSuccessMeansFailure(Response.Payload) then
        begin
            Error := ZbException.FromResponse('', Response);
            Error.MarkHttpError;
            raise Error;
        end;
    end;

    constructor ZbException.FromResponse(AMessage: String; response: TZbRequestResponse);
    var
        NewMessage: String;
    begin
        Payload := response.Payload;
        StatusCode := response.StatusCode;
        if (AMessage <> '') and (Length(AMessage) < 300) and
           (Pos('Access violation', AMessage) = 0) and
           (Pos('<', AMessage) = 0) and (Pos('<<<', AMessage) = 0) and
           (Pos(Copy(Trim(response.Payload), 1, 24), AMessage) = 0) then
            NewMessage := AMessage
        else
            NewMessage := ZbMessageFromBody(response.Payload, response.StatusCode);
        { Transport errors quote the URL, which carries api_key. }
        Create(RedactSecrets(NewMessage));
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
        ZBInitialize(ApiKey, ZbApiURLDefault);
    end;

    procedure ZBInitialize(ApiKey: string; ApiBaseURL: ZbApiURL); overload;
    begin
        ZbApiKey := ZbCheckedApiKey(ApiKey);
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
        Result := S;
        if ZbApiKey = '' then
            Exit;
        Result := StringReplace(Result, ZbApiKey, 'REDACTED', [rfReplaceAll]);
        { The key also travels url-encoded, on the query string. }
        if EncodeParam(ZbApiKey) <> ZbApiKey then
            Result := StringReplace(Result, EncodeParam(ZbApiKey), 'REDACTED', [rfReplaceAll]);
    end;

    function ZbCheckedApiKey(const ApiKey: String): String;
    var
        I: Integer;
    begin
        { An empty key stays deferred to ZbRequireApiKey, which names the call
          that initializes the SDK. }
        if Trim(ApiKey) = '' then
            Exit(ApiKey);
        if ApiKey <> Trim(ApiKey) then
            raise ZbException.Create('api_key has surrounding whitespace or control characters');
        for I := 1 to Length(ApiKey) do
            if (ApiKey[I] < #32) or (ApiKey[I] = #127) then
                raise ZbException.Create('api_key has surrounding whitespace or control characters');
        Result := ApiKey;
    end;

    procedure ZbRequireHttpsUrl(const Url, Name: String);
    var
        Lower: String;
    begin
        Lower := LowerCase(Trim(Url));
        if Pos('https://', Lower) = 1 then
            Exit;
        { A local stub never leaves the machine, so plain http is allowed there
          and nowhere else. }
        if (Pos('http://127.0.0.1', Lower) = 1) or (Pos('http://localhost', Lower) = 1) or
           (Pos('http://[::1]', Lower) = 1) then
            Exit;
        raise ZbException.Create(Concat(Name, ' must be an https:// URL'));
    end;

    function ZbPathSegment(const Value, Name: String): String;
    var
        Trimmed: String;
    begin
        Trimmed := Trim(Value);
        if Trimmed = '' then
            raise ZbException.Create(Concat('Empty parameter: ', Name));
        { Everything else, slashes included, is percent-encoded and stays
          inside the segment; '.' and '..' would walk the request elsewhere. }
        if (Trimmed = '.') or (Trimmed = '..') then
            raise ZbException.Create(Concat('Invalid parameter: ', Name));
        Result := EncodePathSegment(Trimmed);
    end;

    function ZbHostOf(const Url: String): String;
    var
        Rest: String;
        Slash: Integer;
    begin
        Rest := LowerCase(Trim(Url));
        Slash := Pos('://', Rest);
        if Slash > 0 then
            Rest := Copy(Rest, Slash + 3, MaxInt);
        Slash := Pos('/', Rest);
        if Slash > 0 then
            Rest := Copy(Rest, 1, Slash - 1);
        Result := Rest;
    end;

    function ZbCheckedUrl(const Url: String): String;
    var
        Region: String;
    begin
        ZbRequireHttpsUrl(Url, 'url');
        Result := Url;
        if ZbBulkRegionWarned or (ZbHostOf(Url) <> BULK_HOST) then
            Exit;
        Region := '';
        if Pos('api-eu.zerobounce.net', LowerCase(ZbApiBaseURL)) > 0 then
            Region := 'EU'
        else if Pos('api-us.zerobounce.net', LowerCase(ZbApiBaseURL)) > 0 then
            Region := 'US';
        if Region = '' then
            Exit;
        ZbBulkRegionWarned := True;
        WriteLn(StdErr, Concat(
            'ZeroBounce warning: the base URL is the ', Region, ' API, but this request goes to ',
            BULK_HOST, '. ZeroBounce documents no regional host for bulk files, scoring files,',
            ' email finder and domain search files, the list evaluator, S3/SFTP or the Inbox',
            ' Placement Tester. Override BULK_URI, ZbBulkRootURL, ZbListEvaluatorURL and',
            ' ZbInboxPlacementURL if ZeroBounce gave you regional ones.'
        ));
    end;

    function ZBGetRequest(url: String): TZbRequestResponse;
    var
        {$IFDEF FPC}
        Client: TFPHTTPClient;
        ResponseStream: TStringStream;
        {$ELSE}
        Client: THTTPClient;
        Response: IHTTPResponse;
        {$ENDIF}
        error: ZbException;
    begin
        ZbRequireApiKey;
        url := ZbCheckedUrl(url);
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
            { A redirect could send the key to another host. }
            Client.MaxRedirects := 0;
            ResponseStream := TStringStream.Create('');
            try
                Client.HTTPMethod('GET', url, ResponseStream, [200, 201, 204, 400, 401, 403, 404, 409, 422, 429, 500, 502, 503]);
                Result.Payload := ResponseStream.DataString;
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                ResponseStream.Free;
                Client.Free;
            end;

            {$ELSE}
            Client := THTTPClient.Create;
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
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

        ZbFinishResponse(Result);
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
        ZbRequireApiKey;
        url := ZbCheckedUrl(url);
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
            { A redirect could send the key to another host. }
            Client.MaxRedirects := 0;

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
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
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

        ZbFinishResponse(Result);
    end;

    function EncodePathSegment(param: String): String;
    begin
        { HTTPEncode / EncodeQuery are query encoders and turn a space into '+',
          which is a literal plus inside a path. }
        Result := StringReplace(EncodeParam(param), '+', '%20', [rfReplaceAll]);
    end;

    function ZBSendRequest(Method, url, Body, ContentType: String): TZbRequestResponse;
    begin
        Result := ZBSendRequestWithHeader(Method, url, Body, ContentType, '', '');
    end;

    function ZbJsonEscape(const Value: String): String;
    var
        I: Integer;
        Ch: Char;
    begin
        Result := '';
        for I := 1 to Length(Value) do
        begin
            Ch := Value[I];
            case Ch of
                '\': Result := Result + '\\';
                '"': Result := Result + '\"';
                #8: Result := Result + '\b';
                #9: Result := Result + '\t';
                #10: Result := Result + '\n';
                #12: Result := Result + '\f';
                #13: Result := Result + '\r';
            else
                if Ch < #32 then
                    Result := Result + Format('\u%.4x', [Ord(Ch)])
                else
                    Result := Result + Ch;
            end;
        end;
    end;

    function ZBSendRequestWithHeader(Method, url, Body, ContentType,
        HeaderName, HeaderValue: String): TZbRequestResponse;
    begin
        ZbRequireApiKey;
        Result := ZBSendForeignKeyRequest(
            Method, url, Body, ContentType, HeaderName, HeaderValue
        );
    end;

    function ZBSendForeignKeyRequest(Method, url, Body, ContentType,
        HeaderName, HeaderValue: String): TZbRequestResponse;
    var
        {$IFDEF FPC}
        Client: TFPHTTPClient;
        ResponseStream: TStringStream;
        {$ELSE}
        Client: THTTPClient;
        BodyStream: TStream;
        Response: IHTTPResponse;
        {$ENDIF}
    begin
        url := ZbCheckedUrl(url);
        ZbLastRequestMethod := Method;
        ZbLastRequestBody := Body;
        if HeaderName <> '' then
            ZbLastRequestHeader := Concat(HeaderName, ': ', HeaderValue)
        else
            ZbLastRequestHeader := '';

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
            { A redirect could send the key to another host. }
            Client.MaxRedirects := 0;
            Client.AddHeader('Accept', 'application/json');
            if HeaderName <> '' then
                Client.AddHeader(HeaderName, HeaderValue);
            if Body <> '' then
            begin
                Client.AddHeader('Content-Type', ContentType);
                Client.RequestBody := TRawByteStringStream.Create(Body);
            end;
            ResponseStream := TStringStream.Create('');
            try
                Client.HTTPMethod(Method, url, ResponseStream, []);
                Result.Payload := ResponseStream.DataString;
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                ResponseStream.Free;
                if Body <> '' then
                    Client.RequestBody.Free;
                Client.Free;
            end;

            {$ELSE}
            Client := THTTPClient.Create;
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
            BodyStream := TStringStream.Create(Body);
            try
                Client.CustomHeaders['Accept'] := 'application/json';
                if HeaderName <> '' then
                    Client.CustomHeaders[HeaderName] := HeaderValue;
                if Body <> '' then
                    Client.CustomHeaders['Content-Type'] := ContentType;
                Response := Client.Execute(Method, url, BodyStream);
                Result.Payload := Response.ContentAsString();
                Result.StatusCode := Response.StatusCode;
                Result.ContentType := Response.HeaderValue['Content-Type'];
            finally
                Client.Free;
                BodyStream.Free;
            end;
            {$ENDIF}
        end;

        ZbFinishResponse(Result);
    end;

    function ZBFormPostRequest(url: String; FormBody: String): TZbRequestResponse;
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
        ZbRequireApiKey;
        url := ZbCheckedUrl(url);
        ZbLastRequestMethod := 'POST';
        ZbLastRequestBody := FormBody;
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
            { A redirect could send the key to another host. }
            Client.MaxRedirects := 0;

            Client.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
            Client.AddHeader('Accept', 'application/json');
            Client.RequestBody := TRawByteStringStream.Create(FormBody);
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
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
            BodyStream := TStringStream.Create(FormBody);
            try
              Client.CustomHeaders['Content-Type'] := 'application/x-www-form-urlencoded';
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
        ZbRequireApiKey;
        url := ZbCheckedUrl(url);
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
            { A redirect could send the key to another host. }
            Client.MaxRedirects := 0;
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
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
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

        ZbFinishResponse(Result);
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

initialization
    ZbApiKey := '';
    ZbApiBaseURL := 'https://api.zerobounce.net/v2';

{$IFDEF FPC}
finalization
    FreeAndNil(ZbTlsVerifier);
{$ENDIF}

end.

