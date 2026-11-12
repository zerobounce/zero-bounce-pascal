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

    { DER-encoded X.509 certificates. }
    TZbDerCertificates = array of RawByteString;

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
    { FPC only: full path of a PEM CA bundle for TLS verification. When empty,
      the SSL_CERT_FILE environment variable, then (not on Windows) the usual
      Unix bundle paths, then cacert.pem beside the executable, and finally
      (Windows only) the certificates of the Windows "ROOT" system store are
      used. A path that does not exist raises rather than falling back.
      Delphi uses the operating system's TLS stack and ignores this. }
    ZbCACertFile: string = '';
    ZbResponseMock: TZbRequestResponse = (
        StatusCode: 0; Payload: ''; ContentType: ''; UrlCalled: ''
    );
    { BULK_HOST has no documented regional variant, so pairing it with a
      regional base URL is worth one warning and no more. }
    ZbBulkRegionWarned: Boolean = False;
    { Limits on every request, in milliseconds: connecting, and waiting for
      the response. Neither is ever switched off. }
    ZbConnectTimeoutMs: Integer = 30000;
    ZbReadTimeoutMs: Integer = 120000;

{ Per-thread. One thread must not call ZBInitialize for a second key while another request is in flight on that same thread. }
threadvar
    ZbApiBaseURL: string;
    ZbApiKey: string;

    function ZBApiURLToString(url: ZbApiURL): string;
    procedure ZBSetApiKey ( ApiKey : string );  deprecated 'Use ZBInitialize procedures';
    procedure ZBInitialize(ApiKey: string); overload;
    procedure ZBInitialize(ApiKey: string; ApiBaseURL: ZbApiURL); overload;
    { An explicit Main API base URL, for a region the three-value enum does not
      name or for a local stub. BULK_URI, ZbBulkRootURL, ZbListEvaluatorURL and
      ZbInboxPlacementURL are already plain overridable vars; this is the Main
      API's equivalent, so pointing it elsewhere no longer means assigning the
      ZbApiBaseURL threadvar by hand. A trailing slash is dropped, since every
      endpoint constant already starts with one. }
    procedure ZBInitialize(ApiKey: string; ApiBaseURL: String); overload;
    { The base URL with any trailing slashes dropped. Every endpoint path
      starts with one, so 'https://host/v2/' would otherwise build
      'https://host/v2//validate'. Applied wherever a base URL variable is
      used, since a caller may set any of them directly. }
    function ZbBaseUrl(const Url: String): String;
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
    {$IFDEF FPC}
    { The DER certificates of the Windows "ROOT" system store; empty when it
      cannot be read, and on every other platform. With no CA bundle found,
      FPC builds on Windows verify the server against these. }
    function ZbWindowsRootCertificates: TZbDerCertificates;
    { Adds the certificates to the trust store of an OpenSSL connection that
      has not yet shaken hands; returns how many were added. Ones OpenSSL
      cannot parse are skipped. }
    function ZbTrustDerCertificates(Ssl: PSSL; const DerCerts: TZbDerCertificates): Integer;
    {$ENDIF}
    procedure Register;
implementation

uses
    { ZbStructures holds the JSON wrapper. Using it here, in the
      implementation section, keeps the two units' interfaces independent. }
    ZbStructures{$IFDEF FPC}, ctypes{$ENDIF};

{$IFDEF FPC}
type
    { fphttpclient does not verify server certificates by default; api_key in URLs would be exposed to MITM. }
    TZbTlsVerifier = class
        procedure GetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
        procedure VerifyCertificate(Sender: TObject; var Allow: Boolean);
    end;

    { An OpenSSL handler whose trust anchors are DER certificates handed to
      it - on Windows, the system "ROOT" store - instead of a PEM file. Peer
      and host name verification are exactly those of the file-based path. }
    TZbSystemStoreSocketHandler = class(TOpenSSLSocketHandler)
    public
        DerCerts: TZbDerCertificates;
    protected
        function InitContext(NeedCertificate: Boolean): Boolean; override;
    end;

    TZbSslGetPeerCert = function(ssl: PSSL): PX509; cdecl;
    TZbX509CheckHost = function(x: PX509; chk: PAnsiChar; chklen: PtrUInt; flags: Cardinal; peername: Pointer): Integer; cdecl;
    TZbSslGetSslCtx = function(ssl: PSSL): PSSL_CTX; cdecl;
    TZbSslCtxGetCertStore = function(ctx: PSSL_CTX): Pointer; cdecl;
    TZbD2iX509 = function(px: PPX509; pin: PPByte; len: clong): PX509; cdecl;
    TZbX509StoreAddCert = function(store: Pointer; x: PX509): cint; cdecl;

{$PUSH}{$PACKRECORDS C}
    { CERT_CONTEXT, from wincrypt.h. }
    PZbCertContext = ^TZbCertContext;
    TZbCertContext = record
        dwCertEncodingType: Cardinal;
        pbCertEncoded: PByte;
        cbCertEncoded: Cardinal;
        pCertInfo: Pointer;
        hCertStore: Pointer;
    end;
{$POP}
    TZbCertOpenSystemStoreW = function(hProv: PtrUInt; szSubsystemProtocol: PWideChar): Pointer;
        {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    TZbCertEnumCertificatesInStore = function(hCertStore: Pointer; pPrevCertContext: PZbCertContext): PZbCertContext;
        {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    TZbCertCloseStore = function(hCertStore: Pointer; dwFlags: Cardinal): LongBool;
        {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

const
    ZB_X509_ASN_ENCODING = 1;

var
    ZbTlsVerifier: TZbTlsVerifier = nil;

function ZbNoCABundleMessage: string;
begin
    Result := 'No CA certificate bundle found to verify the TLS connection. ' +
        'Download a PEM bundle (for example https://curl.se/ca/cacert.pem) and either ' +
        'save it as cacert.pem beside the executable, assign its full path to ' +
        'ZbCACertFile (unit ZbUtility) before the first request, or set the ' +
        'SSL_CERT_FILE environment variable to that path.';
    {$IFDEF MSWINDOWS}
    Result := Result + ' The Windows "ROOT" certificate store was tried as well and ' +
        'yielded no certificates.';
    {$ENDIF}
end;

function ZbFindCACertFile: string;
{$IFNDEF MSWINDOWS}
const
    Candidates: array[0..3] of string = (
        '/etc/ssl/certs/ca-certificates.crt',
        '/etc/pki/tls/certs/ca-bundle.crt',
        '/etc/ssl/ca-bundle.pem',
        '/etc/ssl/cert.pem'
    );
var
    I: Integer;
{$ENDIF}
begin
    { An explicit choice that points nowhere is an error, not a cue to fall
      back to something else. }
    Result := ZbCACertFile;
    if (Result <> '') and not FileExists(Result) then
        raise ZbException.Create('ZbCACertFile names a CA bundle that does not exist: ' + Result);
    if Result = '' then
    begin
        Result := GetEnvironmentVariable('SSL_CERT_FILE');
        if (Result <> '') and not FileExists(Result) then
            raise ZbException.Create('SSL_CERT_FILE names a CA bundle that does not exist: ' + Result);
    end;
    if Result <> '' then
        Exit;
    {$IFNDEF MSWINDOWS}
    for I := Low(Candidates) to High(Candidates) do
        if FileExists(Candidates[I]) then
            Exit(Candidates[I]);
    {$ENDIF}
    Result := ExtractFilePath(ParamStr(0)) + 'cacert.pem';
    if not FileExists(Result) then
        Result := '';
end;

function ZbWindowsRootCertificates: TZbDerCertificates;
var
    Lib: TLibHandle;
    OpenStore: TZbCertOpenSystemStoreW;
    EnumCerts: TZbCertEnumCertificatesInStore;
    CloseStore: TZbCertCloseStore;
    Store: Pointer;
    Ctx: PZbCertContext;
    StoreName: UnicodeString;
    Der: RawByteString;
    Count: Integer;
begin
    Result := nil;
    { crypt32 is a Windows KnownDLL, so it is always the system copy. Loaded
      dynamically, nothing here is linked on other platforms, where the load
      simply fails. }
    Lib := LoadLibrary('crypt32.dll');
    if Lib = NilHandle then
        Exit;
    try
        OpenStore := TZbCertOpenSystemStoreW(GetProcedureAddress(Lib, 'CertOpenSystemStoreW'));
        EnumCerts := TZbCertEnumCertificatesInStore(GetProcedureAddress(Lib, 'CertEnumCertificatesInStore'));
        CloseStore := TZbCertCloseStore(GetProcedureAddress(Lib, 'CertCloseStore'));
        if not Assigned(OpenStore) or not Assigned(EnumCerts) or not Assigned(CloseStore) then
            Exit;
        StoreName := 'ROOT';
        Store := OpenStore(0, PWideChar(StoreName));
        if Store = nil then
            Exit;
        try
            Count := 0;
            { Each call frees the context it is given, so the loop leaks
              nothing when it runs to the end. }
            Ctx := EnumCerts(Store, nil);
            while Ctx <> nil do
            begin
                if ((Ctx^.dwCertEncodingType and ZB_X509_ASN_ENCODING) <> 0) and
                   (Ctx^.pbCertEncoded <> nil) and (Ctx^.cbCertEncoded > 0) then
                begin
                    Der := '';
                    SetLength(Der, Ctx^.cbCertEncoded);
                    Move(Ctx^.pbCertEncoded^, Der[1], Ctx^.cbCertEncoded);
                    SetLength(Result, Count + 1);
                    Result[Count] := Der;
                    Inc(Count);
                end;
                Ctx := EnumCerts(Store, Ctx);
            end;
        finally
            CloseStore(Store, 0);
        end;
    finally
        UnloadLibrary(Lib);
    end;
end;

function ZbTrustDerCertificates(Ssl: PSSL; const DerCerts: TZbDerCertificates): Integer;
var
    GetCtx: TZbSslGetSslCtx;
    GetStore: TZbSslCtxGetCertStore;
    D2i: TZbD2iX509;
    AddCert: TZbX509StoreAddCert;
    Store: Pointer;
    Cert: PX509;
    Cursor: PByte;
    I: Integer;
begin
    Result := 0;
    if (Ssl = nil) or (Length(DerCerts) = 0) then
        Exit;
    GetCtx := TZbSslGetSslCtx(GetProcedureAddress(SSLLibHandle, 'SSL_get_SSL_CTX'));
    GetStore := TZbSslCtxGetCertStore(GetProcedureAddress(SSLLibHandle, 'SSL_CTX_get_cert_store'));
    D2i := TZbD2iX509(GetProcedureAddress(SSLUtilHandle, 'd2i_X509'));
    AddCert := TZbX509StoreAddCert(GetProcedureAddress(SSLUtilHandle, 'X509_STORE_add_cert'));
    if not Assigned(GetCtx) or not Assigned(GetStore) or not Assigned(D2i) or not Assigned(AddCert) then
        Exit;
    { The SSL object verifies against its context's store, so certificates
      added there before the handshake are the trust anchors it uses. }
    Store := GetStore(GetCtx(Ssl));
    if Store = nil then
        Exit;
    for I := 0 to High(DerCerts) do
    begin
        if DerCerts[I] = '' then
            Continue;
        Cursor := PByte(PAnsiChar(DerCerts[I]));
        Cert := D2i(nil, @Cursor, Length(DerCerts[I]));
        if Cert = nil then
            Continue;
        try
            { The store takes its own reference. A duplicate is not an error
              worth failing the request over. }
            if AddCert(Store, Cert) = 1 then
                Inc(Result);
        finally
            X509Free(Cert);
        end;
    end;
    { Whatever a rejected certificate left on the error queue must not be
      read later as the cause of an unrelated failure. }
    ErrClearError;
end;

function TZbSystemStoreSocketHandler.InitContext(NeedCertificate: Boolean): Boolean;
begin
    Result := inherited InitContext(NeedCertificate);
    if not Result then
        Exit;
    if ZbTrustDerCertificates(SSL.SSL, DerCerts) = 0 then
        raise ZbException.Create(ZbNoCABundleMessage);
end;

procedure TZbTlsVerifier.GetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
var
    Handler: TSSLSocketHandler;
    CAFile: string;
    {$IFDEF MSWINDOWS}
    SystemRoots: TZbDerCertificates;
    {$ENDIF}
begin
    AHandler := nil;
    if not UseSSL then
        Exit;
    CAFile := ZbFindCACertFile;
    if CAFile <> '' then
    begin
        Handler := TSSLSocketHandler.GetDefaultHandler;
        Handler.CertificateData.CertCA.FileName := CAFile;
    end
    else
    begin
        {$IFDEF MSWINDOWS}
        { No bundle was configured: trust what Windows trusts, as Delphi's
          THTTPClient does. Verification itself is unchanged. }
        SystemRoots := ZbWindowsRootCertificates;
        if Length(SystemRoots) = 0 then
            raise ZbException.Create(ZbNoCABundleMessage);
        Handler := TZbSystemStoreSocketHandler.Create;
        TZbSystemStoreSocketHandler(Handler).DerCerts := SystemRoots;
        {$ELSE}
        raise ZbException.Create(ZbNoCABundleMessage);
        {$ENDIF}
    end;
    Handler.VerifyPeerCert := True;
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
        ErrorText: String;
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
        (* LIVE: so is an error with no "success" field at all. An invalid or
          drained key answers validate, activity and getapiusage with
          {"error": ...}, and the filters with {"Error": ...}, on an HTTP 200;
          parsed, that used to come back as an empty result. *)
        ErrorText := ZbErrorOnlyMessage(Response.Payload);
        if ErrorText <> '' then
        begin
            Error := ZbException.FromResponse(ErrorText, Response);
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
        { The same check as ZBInitialize: a dirty key is refused and the key
          already set stays. }
        ZbApiKey := ZbCheckedApiKey(ApiKey);
    end;

    function ZbBaseUrl(const Url: String): String;
    begin
        Result := Trim(Url);
        while (Result <> '') and (Result[Length(Result)] = '/') do
            Delete(Result, Length(Result), 1);
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

    procedure ZBInitialize(ApiKey: string; ApiBaseURL: String); overload;
    var
        Normalized: String;
    begin
        Normalized := Trim(ApiBaseURL);
        if Normalized = '' then
            raise ZbException.Create('Empty parameter: api_base_url');
        { The same rail every request URL goes through: https, or loopback so
          an offline test can point the SDK at a local stub. }
        ZbRequireHttpsUrl(Normalized, 'api_base_url');
        { Every endpoint constant starts with a slash, so a trailing one here
          would build '//getcredits'. }
        Normalized := ZbBaseUrl(Normalized);
        ZbApiKey := ZbCheckedApiKey(ApiKey);
        ZbApiBaseURL := Normalized;
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
        Lower, Scheme, Authority, Host, Port: String;
        Marker, I: Integer;
    begin
        Lower := LowerCase(Trim(Url));
        Marker := Pos('://', Lower);
        if Marker < 2 then
            raise ZbException.Create(Concat(Name, ' must be an https:// URL'));
        Scheme := Copy(Lower, 1, Marker - 1);
        if (Scheme <> 'https') and (Scheme <> 'http') then
            raise ZbException.Create(Concat(Name, ' must be an https:// URL'));

        { The authority is taken to run to the first '/'. URL parsers disagree
          about where '?', '#' and '\' end it - FPC's ParseURI cuts the query
          at the last '?', so in 'http://localhost?a@evil.example/x?b' it
          sees user 'localhost?a' at host evil.example - so none of them is
          accepted before the path, and every parser then agrees on the host. }
        Authority := Copy(Lower, Marker + 3, MaxInt);
        I := Pos('/', Authority);
        if I > 0 then
            Authority := Copy(Authority, 1, I - 1);
        if Authority = '' then
            raise ZbException.Create(Concat(Name, ' must be an https:// URL with a host'));
        { 'http://127.0.0.1@evil.example' is a request to evil.example, and
          no ZeroBounce URL carries credentials. }
        if Pos('@', Authority) > 0 then
            raise ZbException.Create(Concat(Name, ' must not contain user information (user@host)'));
        for I := 1 to Length(Authority) do
            if (Authority[I] <= ' ') or (Authority[I] = #127) or
               (Authority[I] in ['?', '#', '\']) then
                raise ZbException.Create(Concat(Name, ' has an invalid host'));

        if Authority[1] = '[' then
        begin
            Marker := Pos(']', Authority);
            if Marker = 0 then
                raise ZbException.Create(Concat(Name, ' has an invalid host'));
            Host := Copy(Authority, 1, Marker);
            Port := Copy(Authority, Marker + 1, MaxInt);
            if Port <> '' then
            begin
                if Port[1] <> ':' then
                    raise ZbException.Create(Concat(Name, ' has an invalid host'));
                Delete(Port, 1, 1);
            end;
        end
        else
        begin
            Marker := Pos(':', Authority);
            if Marker = 0 then
            begin
                Host := Authority;
                Port := '';
            end
            else
            begin
                Host := Copy(Authority, 1, Marker - 1);
                Port := Copy(Authority, Marker + 1, MaxInt);
            end;
        end;
        if (Host = '') or (Host = '[]') then
            raise ZbException.Create(Concat(Name, ' must be an https:// URL with a host'));
        if (Length(Port) > 5) then
            raise ZbException.Create(Concat(Name, ' has an invalid port'));
        for I := 1 to Length(Port) do
            if not (Port[I] in ['0'..'9']) then
                raise ZbException.Create(Concat(Name, ' has an invalid port'));
        if (Port <> '') and (StrToInt(Port) > 65535) then
            raise ZbException.Create(Concat(Name, ' has an invalid port'));

        if Scheme = 'https' then
            Exit;
        { A local stub never leaves the machine, so plain http is allowed there
          and nowhere else: the host has to be exactly a loopback name, not
          merely start with one ('http://localhost.evil.example'). }
        if (Host = '127.0.0.1') or (Host = 'localhost') or (Host = '[::1]') then
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

    {$IFDEF FPC}
    { A body shorter than its Content-Length was cut off: the connection closed
      part-way through. fphttpclient hands back whatever arrived without
      complaint, and a file download used to return it as the whole file.
      (A read that times out part-way raises in fphttpclient itself.) }
    procedure ZbRequireCompleteBody(Client: TFPHTTPClient; Received: Int64);
    var
        Expected: Int64;
    begin
        if Client.GetHeader(Client.ResponseHeaders, 'Content-Encoding') <> '' then
            Exit;
        Expected := StrToInt64Def(Trim(Client.GetHeader(Client.ResponseHeaders, 'Content-Length')), -1);
        if (Expected < 0) or (Received >= Expected) then
            Exit;
        raise ZbException.Create(Format(
            'The response from ZeroBounce was cut short: %d of %d bytes received', [Received, Expected]));
    end;
    {$ENDIF}

    { A request that got no HTTP answer at all - the connection refused or
      timed out, a TLS or protocol failure - raises ZbException with
      StatusCode 0, as the README promises and as uploads and Warmup already
      did. The network library's own message is kept, with the key masked:
      it can quote the URL. }
    function ZbTransportError(E: Exception): ZbException;
    begin
        Result := ZbException.Create(RedactSecrets(E.Message));
        Result.StatusCode := 0;
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
            Client.ConnectTimeout := ZbConnectTimeoutMs;
            Client.IOTimeout := ZbReadTimeoutMs;
            { A redirect could send the key to another host. }
            Client.MaxRedirects := 0;
            ResponseStream := TStringStream.Create('');
            try
                { Every status is let through to ZbFinishResponse, which turns a
                  redirect or an error into a ZbException carrying the status.
                  A list here made fphttpclient raise its own exception, with
                  no status, for anything not on it - a 302 included. }
                try
                    Client.HTTPMethod('GET', url, ResponseStream, []);
                except
                    { Raised by this SDK (no CA bundle, say): already one. }
                    on E: ZbException do
                        raise;
                    on E: Exception do
                        raise ZbTransportError(E);
                end;
                ZbRequireCompleteBody(Client, ResponseStream.Size);
                Result.Payload := ResponseStream.DataString;
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                ResponseStream.Free;
                Client.Free;
            end;

            {$ELSE}
            Client := THTTPClient.Create;
            { The same limits as the FPC branch: ZbConnectTimeoutMs to
              connect, ZbReadTimeoutMs for the response. }
            Client.ConnectionTimeout := ZbConnectTimeoutMs;
            Client.ResponseTimeout := ZbReadTimeoutMs;
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
            try
              { Not compiled by CI, which has no Delphi: the same wrapping
                as the FPC branch. }
              try
                Response := Client.Get(url);
              except
                on E: ZbException do
                  raise;
                on E: Exception do
                  raise ZbTransportError(E);
              end;
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
            Client.ConnectTimeout := ZbConnectTimeoutMs;
            Client.IOTimeout := ZbReadTimeoutMs;
            { A redirect could send the key to another host. }
            Client.MaxRedirects := 0;

            Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
            Client.AddHeader('Accept', 'application/json');
            Client.RequestBody := TRawByteStringStream.Create(JsonParam);
            try
                try
                    Result.Payload := Client.Post(url);
                except
                    { Raised by this SDK (no CA bundle, say): already one. }
                    on E: ZbException do
                        raise;
                    on E: Exception do
                        raise ZbTransportError(E);
                end;
                ZbRequireCompleteBody(Client, Length(Result.Payload));
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                Client.RequestBody.Free;
                Client.Free;
            end;
            {$ELSE}
            Client := THTTPClient.Create;
            { The same limits as the FPC branch: ZbConnectTimeoutMs to
              connect, ZbReadTimeoutMs for the response. }
            Client.ConnectionTimeout := ZbConnectTimeoutMs;
            Client.ResponseTimeout := ZbReadTimeoutMs;
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
            BodyStream := TStringStream.Create(JsonParam);
            try
              Client.CustomHeaders['Content-Type'] := 'application/json; charset=UTF-8';
              Client.CustomHeaders['Accept'] := 'application/json';
              try
                Response := Client.Post(url, BodyStream);
              except
                on E: ZbException do
                  raise;
                on E: Exception do
                  raise ZbTransportError(E);
              end;

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
            Client.ConnectTimeout := ZbConnectTimeoutMs;
            Client.IOTimeout := ZbReadTimeoutMs;
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
                try
                    Client.HTTPMethod(Method, url, ResponseStream, []);
                except
                    { Raised by this SDK (no CA bundle, say): already one. }
                    on E: ZbException do
                        raise;
                    on E: Exception do
                        raise ZbTransportError(E);
                end;
                ZbRequireCompleteBody(Client, ResponseStream.Size);
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
            { The same limits as the FPC branch: ZbConnectTimeoutMs to
              connect, ZbReadTimeoutMs for the response. }
            Client.ConnectionTimeout := ZbConnectTimeoutMs;
            Client.ResponseTimeout := ZbReadTimeoutMs;
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
            BodyStream := TStringStream.Create(Body);
            try
                Client.CustomHeaders['Accept'] := 'application/json';
                if HeaderName <> '' then
                    Client.CustomHeaders[HeaderName] := HeaderValue;
                if Body <> '' then
                    Client.CustomHeaders['Content-Type'] := ContentType;
                try
                    Response := Client.Execute(Method, url, BodyStream);
                except
                    on E: ZbException do
                        raise;
                    on E: Exception do
                        raise ZbTransportError(E);
                end;
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
            Client.ConnectTimeout := ZbConnectTimeoutMs;
            Client.IOTimeout := ZbReadTimeoutMs;
            { A redirect could send the key to another host. }
            Client.MaxRedirects := 0;

            Client.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
            Client.AddHeader('Accept', 'application/json');
            Client.RequestBody := TRawByteStringStream.Create(FormBody);
            try
                try
                    Result.Payload := Client.Post(url);
                except
                    { Raised by this SDK (no CA bundle, say): already one. }
                    on E: ZbException do
                        raise;
                    on E: Exception do
                        raise ZbTransportError(E);
                end;
                ZbRequireCompleteBody(Client, Length(Result.Payload));
                Result.StatusCode := Client.ResponseStatusCode;
                Result.ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
            finally
                Client.RequestBody.Free;
                Client.Free;
            end;
            {$ELSE}
            Client := THTTPClient.Create;
            { The same limits as the FPC branch: ZbConnectTimeoutMs to
              connect, ZbReadTimeoutMs for the response. }
            Client.ConnectionTimeout := ZbConnectTimeoutMs;
            Client.ResponseTimeout := ZbReadTimeoutMs;
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
            BodyStream := TStringStream.Create(FormBody);
            try
              Client.CustomHeaders['Content-Type'] := 'application/x-www-form-urlencoded';
              Client.CustomHeaders['Accept'] := 'application/json';
              try
                Response := Client.Post(url, BodyStream);
              except
                on E: ZbException do
                  raise;
                on E: Exception do
                  raise ZbTransportError(E);
              end;

              Result.Payload := Response.ContentAsString();
              Result.StatusCode := Response.StatusCode;
              Result.ContentType := Response.HeaderValue['Content-Type'];
            finally
              Client.Free;
              BodyStream.Free;
            end;
            {$ENDIF}
        end;

        (* The same checks as every other request: an HTTP error carries the
           API's message, and an HTML page, a "success": false body or, LIVE,
           an error-only body such as the {"error": ...} / {"Error": ...} an
           invalid or drained key gets from validate or the filters on an
           HTTP 200, raises instead of being returned as a result. *)
        ZbFinishResponse(Result);
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
            { No answer, no status: a refused connection is reported from
              here, before any of these is read. }
            Result.StatusCode := 0;
            Result.Payload := '';
            Result.ContentType := '';
            {$IFDEF FPC}
            InitSSLInterface;

            FileStream := TStringStream.Create(FileContent);
            Client := TFPHTTPClient.Create(nil);
            ZbUseVerifiedTls(Client);
            Client.ConnectTimeout := ZbConnectTimeoutMs;
            Client.IOTimeout := ZbReadTimeoutMs;
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

                ZbRequireCompleteBody(Client, ResponseStream.Size);
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
            { The same limits as the FPC branch: ZbConnectTimeoutMs to
              connect, ZbReadTimeoutMs for the response. }
            Client.ConnectionTimeout := ZbConnectTimeoutMs;
            Client.ResponseTimeout := ZbReadTimeoutMs;
            { A redirect could send the key to another host. }
            Client.HandleRedirects := False;
            FileStream := TStringStream.Create(FileContent);
            MPForm := FormDataFromTString;
            try
              MPForm.AddStream('file', FileStream, FILE_NAME, 'text/csv');
              try
                Response := Client.Post(url, MPForm);
              except
                on e: Exception do
                  begin
                    error := ZbException.FromResponse(e.Message, Result);
                    error.MarkHttpError;
                    raise error;
                  end;
              end;

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

