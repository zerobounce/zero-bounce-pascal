unit ZbUtility;

{$I zboptions.inc}{$H+}

interface

uses
    {$IFDEF FPC}
    {$IFDEF UNIX}
    { Gives every program that links the SDK a widestring manager. Without it
      FPC converts text between UTF-8 and UTF-16 by dropping everything past
      ASCII to '?', so the message of an error body that quotes a key with an
      accent in it comes back from the JSON reader with the accent gone and
      the key unmasked. }
    cwstring,
    {$ENDIF}
    {$ENDIF}
    Classes, SysUtils, StrUtils, Types,
    {$IFDEF FPC}
    Character,
    openssl, opensslsockets, fphttpclient, httpprotocol, ssockets, sslsockets, dynlibs
    {$ELSE}
    System.Character,
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
        { Runs after every constructor: the message and the Payload a caller
          can read never carry the key, whatever built them (a body quoting
          the key or the request URL, a batch error, a transport error). }
        procedure AfterConstruction; override;
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
    { What an error says in place of a body that still held a secret after
      masking (ZbMaskSecret). }
    ZB_SECRET_OMITTED_MESSAGE = 'The response body was omitted because it contained a credential.';
    { How many times ZbMaskSecret decodes an error before giving up: the SDK's
      own request encoding plus three layers a server or proxy may add when
      it echoes the request back. }
    ZB_SECRET_DECODE_LAYERS = 4;
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
    { The method the last request used, set once its URL passed the check.
      Offline tests assert on this and on the two threadvars below. }
    ZbLastRequestMethod: string = '';
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
    { What the last request on this thread sent: its body, and 'Name: Value'
      of the one extra header it carried (or ''). They can hold a key, so
      like ZbApiKey they belong to the thread whose request wrote them. }
    ZbLastRequestBody: string;
    ZbLastRequestHeader: string;

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
    { The text with the API key, raw, url-encoded and JSON-escaped, and every
      secret a registered redactor masks, replaced by REDACTED. Every
      ZbException runs its Message and Payload through it. }
    function RedactSecrets(const S: string): string;
    { S with every form of Secret an answer can quote replaced by REDACTED:
      JSON-escaped ('/' escaped or not, as a request body echoed back),
      url-encoded (as a request URL or form, a space as %20 or '+'), the
      url encoding of each JSON-escaped form, and raw. Redaction is closed
      under encoding: when a view of the result decoded up to four times
      (URL-decoding, '+' as a space, JSON-unescaping, in any order) still
      holds Secret, the whole of S is replaced by ZB_SECRET_OMITTED_MESSAGE. }
    function ZbMaskSecret(const S, Secret: string): string;
type
    { Masks a secret ZbUtility does not hold itself, such as the Warmup key. }
    TZbSecretRedactor = function(const S: string): string;
    { Adds a redactor to RedactSecrets, and so to every ZbException. For units
      that hold their own secrets; ZbWarmup registers one for its key and the
      SMTP credentials of the call in progress. }
    procedure ZbRegisterSecretRedactor(Redactor: TZbSecretRedactor);
    { The key, or a clear error. A key with surrounding whitespace or control
      characters would otherwise be quoted, unredacted, in a transport error. }
    function ZbCheckedApiKey(const ApiKey: String): String;
    { Raises unless the URL is https. Loopback is allowed so an offline test
      can point the SDK at a local stub; nothing leaves the machine there. }
    procedure ZbRequireHttpsUrl(const Url, Name: String);
    { Overwrites the text of S in place when the SDK holds the only reference
      to its buffer (not a literal, not a string another variable shares), then
      empties S. Used on a replaced or finalized key. }
    procedure ZbWipeString(var S: String);
    { Raises unless the URL is free of controls, white space, invisible
      characters, backslashes, invalid UTF-8 and (in the host) non-ASCII. }
    procedure ZbRequireCleanUrl(const Url, Name: String);
    { One path segment, url-encoded, or a clear error. A segment of '.' or '..'
      would walk the request out of the endpoint it was meant for. }
    function ZbPathSegment(const Value, Name: String): String;
    { The URL, after checking it is https and warning once when a regional base
      URL is paired with the single global bulk host. }
    function ZbCheckedUrl(const Url: String): String;
    { Where the content of a body starts: the index of the first character
      that is not padding (space, tab, CR, LF, VT, FF, a UTF-8 BOM or a
      no-break space), Length(Body) + 1 for a body of nothing else. Only for
      looking at a body: a file is saved with its padding. }
    function ZbContentStart(const Body: String): Integer;
    { Whether the body is an HTML page: after the padding, the first 2000
      characters hold <!doctype html, <html, <head or <body, each as a whole
      tag (<head does not match <header>), in any case. }
    function ZbBodyLooksLikeHtml(const Body: String): Boolean;
    { Whether a Content-Type names an HTML page (text/html), in any case. }
    function ZbContentTypeIsHtml(const ContentType: String): Boolean;
    { Whether a Content-Type names JSON, in any case and with or without
      parameters: application/json, text/json or anything ending in +json. }
    function ZbContentTypeIsJson(const ContentType: String): Boolean;
type
    { What a body's "success" field says; see ZbSuccessFlagOfBody. }
    TZbSuccessFlag = (zsfAbsent, zsfNotFlag, zsfTrue, zsfFalse);
    { What a file download answered with. }
    TZbDownloadKind = (zdkFile, zdkEmpty, zdkHtml, zdkJson);
    { zdkEmpty: no body, or one of padding only. zdkHtml: an HTML content
      type or body. zdkJson: a JSON content type, or a body that starts with
      { or [ after its padding. Anything else is the file. }
    function ZbDownloadKind(const Payload, ContentType: String): TZbDownloadKind;
    { The body without its leading padding, for reading the JSON of an error. }
    function ZbStripPadding(const Body: String): String;
    { Whether a JSON body says the call failed although the HTTP status was a
      success. The API sends "success" as a JSON false, "false" or "False". }
    function ZbSuccessMeansFailure(const Body: String): Boolean;
    { What a body's own "success" field is. A flag is a JSON boolean, the
      number 0 or 1 (1.0 too), or the text true, false, 0 or 1 (any case,
      trimmed of ASCII whitespace). Anything else (null, 2, 0.4, "maybe", an
      object, a list) is no flag: zsfNotFlag, to be read as if the field were
      absent. }
    function ZbSuccessFlagOfBody(const Body: String): TZbSuccessFlag;
    { The same decision for one value: Token is the raw text of a bare JSON
      value, or the unescaped text of a JSON string (IsString). }
    function ZbClassifySuccess(const Token: String; IsString: Boolean): TZbSuccessFlag;
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
    { The connect and read timeouts, in ms, that the last request on this
      thread actually ran with, read back from its TFPHTTPClient. For tests:
      it proves the defaults reach the client every transport builds. }
    procedure ZbLastRequestTimeouts(out ConnectMs, ReadMs: Integer);
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

    { Socket handlers that note when the server closed the connection. A
      chunked body that ends that way, before its last (empty) chunk, was cut
      short: fphttpclient hands back what arrived without complaint. }
    TZbPlainSocketHandler = class(TSocketHandler)
    public
        function Recv(Const Buffer; Count: Integer): Integer; override;
    end;

    TZbOpenSSLSocketHandler = class(TOpenSSLSocketHandler)
    public
        function Recv(Const Buffer; Count: Integer): Integer; override;
    end;

    { An OpenSSL handler whose trust anchors are DER certificates handed to
      it - on Windows, the system "ROOT" store - instead of a PEM file. Peer
      and host name verification are exactly those of the file-based path. }
    TZbSystemStoreSocketHandler = class(TZbOpenSSLSocketHandler)
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
    { Guards the one-time creation of ZbTlsVerifier: requests run on any thread. }
    ZbTlsVerifierLock: TRTLCriticalSection;

threadvar
    { Whether the server closed the connection of this thread's request. }
    ZbConnectionClosed: Boolean;
    ZbLastConnectTimeoutMs: Integer;
    ZbLastReadTimeoutMs: Integer;

function TZbPlainSocketHandler.Recv(Const Buffer; Count: Integer): Integer;
begin
    Result := inherited Recv(Buffer, Count);
    if Result <= 0 then
        ZbConnectionClosed := True;
end;

function TZbOpenSSLSocketHandler.Recv(Const Buffer; Count: Integer): Integer;
begin
    Result := inherited Recv(Buffer, Count);
    if Result <= 0 then
        ZbConnectionClosed := True;
end;

procedure ZbLastRequestTimeouts(out ConnectMs, ReadMs: Integer);
begin
    ConnectMs := ZbLastConnectTimeoutMs;
    ReadMs := ZbLastReadTimeoutMs;
end;

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
    begin
        AHandler := TZbPlainSocketHandler.Create;
        Exit;
    end;
    CAFile := ZbFindCACertFile;
    if CAFile <> '' then
    begin
        Handler := TZbOpenSSLSocketHandler.Create;
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
    { Created once: two first requests on two threads must not each make one. }
    EnterCriticalSection(ZbTlsVerifierLock);
    try
        if ZbTlsVerifier = nil then
            ZbTlsVerifier := TZbTlsVerifier.Create;
    finally
        LeaveCriticalSection(ZbTlsVerifierLock);
    end;
    Client.OnGetSocketHandler := ZbTlsVerifier.GetSocketHandler;
    ZbConnectionClosed := False;
end;
{$ENDIF}

    { How many characters of padding start at Text[At]: 1 for a space, tab,
      CR, LF, VT or FF; 3 for a UTF-8 BOM and 2 for a UTF-8 no-break space on
      FPC, 1 for either on Delphi; 0 for anything else. }
    function ZbPaddingAt(const Text: String; At: Integer): Integer;
    begin
        Result := 0;
        if (At < 1) or (At > Length(Text)) then
            Exit;
        if Text[At] in [' ', #9, #10, #11, #12, #13] then
            Exit(1);
        {$IFDEF FPC}
        if (Text[At] = #$EF) and (At + 2 <= Length(Text)) and (Text[At + 1] = #$BB) and
           (Text[At + 2] = #$BF) then
            Exit(3);
        if (Text[At] = #$C2) and (At + 1 <= Length(Text)) and (Text[At + 1] = #$A0) then
            Exit(2);
        {$ELSE}
        if (Text[At] = #$FEFF) or (Text[At] = #$00A0) then
            Exit(1);
        {$ENDIF}
    end;

    function ZbContentStart(const Body: String): Integer;
    var
        Step: Integer;
    begin
        Result := 1;
        Step := ZbPaddingAt(Body, Result);
        while Step > 0 do
        begin
            Inc(Result, Step);
            Step := ZbPaddingAt(Body, Result);
        end;
    end;

    function ZbStripPadding(const Body: String): String;
    var
        Start: Integer;
    begin
        Start := ZbContentStart(Body);
        if Start = 1 then
            Result := Body
        else
            Result := Copy(Body, Start, MaxInt);
    end;

    { The first 2000 characters after the padding, ASCII letters lowercased. }
    function ZbLowerPrefix(const Value: String): String;
    var
        I: Integer;
    begin
        Result := Copy(Value, ZbContentStart(Value), 2000);
        for I := 1 to Length(Result) do
            if Result[I] in ['A'..'Z'] then
                Result[I] := Chr(Ord(Result[I]) + 32);
    end;

    { Whether Tag (lowercase, as '<head') is in Lower as a whole tag: the next
      character ends it ('>', '/' or white space), or the text does. }
    function ZbHasTag(const Lower, Tag: String): Boolean;
    var
        At, After: Integer;
    begin
        Result := False;
        At := Pos(Tag, Lower);
        while At > 0 do
        begin
            After := At + Length(Tag);
            if (After > Length(Lower)) or (Lower[After] in ['>', '/', ' ', #9, #10, #11, #12, #13]) then
                Exit(True);
            At := PosEx(Tag, Lower, At + 1);
        end;
    end;

    function ZbBodyLooksLikeHtml(const Body: String): Boolean;
    var
        Lower: String;
    begin
        Lower := ZbLowerPrefix(Body);
        Result := ZbHasTag(Lower, '<!doctype html') or ZbHasTag(Lower, '<html') or
            ZbHasTag(Lower, '<head') or ZbHasTag(Lower, '<body');
    end;

    function ZbContentTypeIsHtml(const ContentType: String): Boolean;
    begin
        Result := Pos('text/html', LowerCase(ContentType)) > 0;
    end;

    function ZbContentTypeIsJson(const ContentType: String): Boolean;
    var
        Media: String;
        Cut: Integer;
    begin
        Media := LowerCase(Trim(ContentType));
        Cut := Pos(';', Media);
        if Cut > 0 then
            Media := Trim(Copy(Media, 1, Cut - 1));
        Result := (Media = 'application/json') or (Media = 'text/json') or
            ((Length(Media) > 5) and (Copy(Media, Length(Media) - 4, 5) = '+json'));
    end;

    function ZbDownloadKind(const Payload, ContentType: String): TZbDownloadKind;
    var
        Start: Integer;
    begin
        Start := ZbContentStart(Payload);
        if Start > Length(Payload) then
            Exit(zdkEmpty);
        if ZbContentTypeIsHtml(ContentType) or ZbBodyLooksLikeHtml(Payload) then
            Exit(zdkHtml);
        if ZbContentTypeIsJson(ContentType) or (Payload[Start] = '{') or (Payload[Start] = '[') then
            Exit(zdkJson);
        Result := zdkFile;
    end;

    function ZbHtmlTitle(const Body: String): String;
    var
        Lower, Content: String;
        StartAt, OpenEnd, CloseAt: Integer;
    begin
        Result := '';
        { The offsets below are into the body without its padding. }
        Lower := ZbLowerPrefix(Body);
        Content := ZbStripPadding(Body);
        StartAt := Pos('<title', Lower);
        if StartAt = 0 then Exit;
        OpenEnd := Pos('>', Copy(Lower, StartAt, MaxInt));
        if OpenEnd = 0 then Exit;
        OpenEnd := StartAt + OpenEnd - 1;
        CloseAt := Pos('</title>', Copy(Lower, OpenEnd, MaxInt));
        if CloseAt = 0 then Exit;
        Result := Trim(Copy(Content, OpenEnd + 1, CloseAt - 2));
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

    function ZbJsonUnescapeView(const Text: String): String; forward;

    { Where the JSON string that opens at Text[At] ('"') ends: the index of its
      closing quote, or Length(Text) + 1 for one that never closes. }
    function ZbJsonStringEnd(const Text: String; At: Integer): Integer;
    begin
        Result := At + 1;
        while Result <= Length(Text) do
        begin
            if Text[Result] = '\' then
                Inc(Result, 2)
            else if Text[Result] = '"' then
                Exit
            else
                Inc(Result);
        end;
        Result := Length(Text) + 1;
    end;

    { Whether Token is a number of exactly the value 0 or 1, read from its
      decimal digits (so 1.0, 1e0 and 0.0 count, 0.4 and 1.0000000001 do not). }
    function ZbNumberIsZeroOrOne(const Token: String; out IsOne: Boolean): Boolean;
    var
        P, Len, Exp10, ExpAdd: Integer;
        Digits: String;
        Negative, ExpNegative, SawDigit: Boolean;
    begin
        Result := False;
        IsOne := False;
        Len := Length(Token);
        P := 1;
        Negative := False;
        Digits := '';
        Exp10 := 0;
        if (P <= Len) and (Token[P] = '-') then
        begin
            Negative := True;
            Inc(P);
        end;
        SawDigit := False;
        while (P <= Len) and (Token[P] >= '0') and (Token[P] <= '9') do
        begin
            Digits := Digits + Token[P];
            SawDigit := True;
            Inc(P);
        end;
        if not SawDigit then
            Exit;
        if (P <= Len) and (Token[P] = '.') then
        begin
            Inc(P);
            SawDigit := False;
            while (P <= Len) and (Token[P] >= '0') and (Token[P] <= '9') do
            begin
                Digits := Digits + Token[P];
                Dec(Exp10);
                SawDigit := True;
                Inc(P);
            end;
            if not SawDigit then
                Exit;
        end;
        ExpAdd := 0;
        if (P <= Len) and ((Token[P] = 'e') or (Token[P] = 'E')) then
        begin
            Inc(P);
            ExpNegative := False;
            if (P <= Len) and ((Token[P] = '+') or (Token[P] = '-')) then
            begin
                ExpNegative := Token[P] = '-';
                Inc(P);
            end;
            SawDigit := False;
            while (P <= Len) and (Token[P] >= '0') and (Token[P] <= '9') do
            begin
                SawDigit := True;
                { Far beyond any exponent that could bring a mantissa to 1. }
                if ExpAdd < 100000 then
                    ExpAdd := ExpAdd * 10 + (Ord(Token[P]) - Ord('0'));
                Inc(P);
            end;
            if not SawDigit then
                Exit;
            if ExpNegative then
                ExpAdd := -ExpAdd;
        end;
        if P <= Len then
            Exit;
        Inc(Exp10, ExpAdd);
        { It is a number: now whether it is 0 or 1. }
        while (Digits <> '') and (Digits[1] = '0') do
            Delete(Digits, 1, 1);
        if Digits = '' then
        begin
            Result := True;
            Exit;
        end;
        while (Digits <> '') and (Digits[Length(Digits)] = '0') do
        begin
            Delete(Digits, Length(Digits), 1);
            Inc(Exp10);
        end;
        if (Digits = '1') and (Exp10 = 0) and not Negative then
        begin
            Result := True;
            IsOne := True;
        end;
    end;

    function ZbClassifySuccess(const Token: String; IsString: Boolean): TZbSuccessFlag;
    var
        Lower: String;
        Start, Stop: Integer;
        IsOne: Boolean;
    begin
        Result := zsfNotFlag;
        Start := 1;
        Stop := Length(Token);
        { ASCII whitespace only: Trim would also take other control bytes. }
        while (Start <= Stop) and ((Token[Start] = ' ') or ((Token[Start] >= #9) and (Token[Start] <= #13))) do
            Inc(Start);
        while (Stop >= Start) and ((Token[Stop] = ' ') or ((Token[Stop] >= #9) and (Token[Stop] <= #13))) do
            Dec(Stop);
        Lower := LowerCase(Copy(Token, Start, Stop - Start + 1));
        if (Lower = 'true') or (IsString and (Lower = '1')) then
            Exit(zsfTrue);
        if (Lower = 'false') or (IsString and (Lower = '0')) then
            Exit(zsfFalse);
        if (not IsString) and ZbNumberIsZeroOrOne(Lower, IsOne) then
        begin
            if IsOne then
                Result := zsfTrue
            else
                Result := zsfFalse;
        end;
    end;

    function ZbSuccessFlagOfBody(const Body: String): TZbSuccessFlag;
    var
        Trimmed, Key, Token: String;
        I, Close, Depth, After, Finish: Integer;
        IsString: Boolean;
        One: TZbSuccessFlag;
    begin
        Result := zsfAbsent;
        Trimmed := Trim(Body);
        if (Trimmed = '') or (Trimmed[1] <> '{') then
            Exit;
        { Only the "success" field of the object itself counts: a nested object
          (a "meta" block, say) may carry one of its own. Read the text as the
          scanner of a JSON reader would, string by string, so that a
          "success" inside a string or a deeper level is never taken for it. }
        Depth := 0;
        I := 1;
        while I <= Length(Trimmed) do
        begin
            case Trimmed[I] of
                '{', '[':
                    Inc(Depth);
                '}', ']':
                    Dec(Depth);
                '"':
                begin
                    Close := ZbJsonStringEnd(Trimmed, I);
                    After := Close + 1;
                    while (After <= Length(Trimmed)) and (Trimmed[After] in [' ', #9, #10, #13]) do
                        Inc(After);
                    if (Depth = 1) and (After <= Length(Trimmed)) and (Trimmed[After] = ':') then
                    begin
                        Key := LowerCase(ZbJsonUnescapeView(Copy(Trimmed, I + 1, Close - I - 1)));
                        if Key = 'success' then
                        begin
                            Inc(After);
                            while (After <= Length(Trimmed)) and (Trimmed[After] in [' ', #9, #10, #13]) do
                                Inc(After);
                            IsString := (After <= Length(Trimmed)) and (Trimmed[After] = '"');
                            if IsString then
                            begin
                                Finish := ZbJsonStringEnd(Trimmed, After);
                                Token := ZbJsonUnescapeView(Copy(Trimmed, After + 1, Finish - After - 1));
                            end
                            else
                            begin
                                Finish := After;
                                while (Finish <= Length(Trimmed)) and
                                      not (Trimmed[Finish] in [',', '}', ']', ' ', #9, #10, #13]) do
                                    Inc(Finish);
                                Token := Copy(Trimmed, After, Finish - After);
                            end;
                            One := ZbClassifySuccess(Token, IsString);
                            if One = zsfFalse then
                                Exit(zsfFalse);
                            if (One = zsfTrue) or (Result = zsfAbsent) then
                                Result := One;
                        end;
                    end;
                    I := Close;
                end;
            end;
            Inc(I);
        end;
    end;

    function ZbSuccessMeansFailure(const Body: String): Boolean;
    begin
        Result := ZbSuccessFlagOfBody(Body) = zsfFalse;
    end;

    { The text of a JSON object's "message", "error" or "error_message" when it
      is a non-empty string (the first of them in that order), else ''. A
      number, an object or a list there is not a message. }
    function ZbJsonErrorText(const Body: String): String;
    const
        KEYS: array[0..2] of String = ('message', 'error', 'error_message');
    var
        J: TZbJson;
        I: Integer;
    begin
        Result := '';
        try
            J := TZbJson.Create(Body);
        except
            Exit;
        end;
        try
            for I := Low(KEYS) to High(KEYS) do
            begin
                Result := J.GetText(KEYS[I]);
                if Result <> '' then
                    Exit;
            end;
        finally
            J.Free;
        end;
    end;

    { Whether a body is rows of comma-separated values: a text of at least two
      lines, the first two of which each hold a comma. A one-line message with
      a comma in it is not. }
    function ZbBodyLooksLikeCsv(const Body: String): Boolean;
    var
        Lines: TStringList;
    begin
        Result := False;
        Lines := TStringList.Create;
        try
            Lines.Text := Trim(Copy(Body, 1, 4000));
            Result := (Lines.Count >= 2) and (Pos(',', Lines[0]) > 0) and (Pos(',', Lines[1]) > 0);
        finally
            Lines.Free;
        end;
    end;

    function ZbContentTypeIsCsv(const ContentType: String): Boolean;
    var
        Lower: String;
    begin
        Lower := LowerCase(ContentType);
        Result := (Pos('text/csv', Lower) > 0) or (Pos('application/csv', Lower) > 0);
    end;

    function ZbMessageFromBody(const Body: String; StatusCode: Integer): String;
    var
        Trimmed: String;
    begin
        { A BOM or a no-break space is padding, not text: Trim leaves both. }
        Trimmed := Trim(ZbStripPadding(Body));
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
            { Too long to quote; it may well be valid JSON, so it is not
              called invalid. A list is never a message either: it is what a
              file download answers with in place of the file. }
            { The API's own words, when the body says them in a field, instead
              of the whole object: however long the rest of it is. }
            if Trimmed[1] = '{' then
            begin
                Result := ZbJsonErrorText(Trimmed);
                if Result <> '' then
                begin
                    { A field too long to quote is the body's size again. }
                    if Length(Result) > 1000 then
                        Result := 'ZeroBounce returned an error response.';
                    Exit;
                end;
                { An object with no such field, too long to quote. }
                if Length(Trimmed) > 1000 then
                    Exit('ZeroBounce returned an error response.');
                Exit(Trimmed);
            end;
            { A list. On a success status it is what a file download answers
              with in place of the file, never a message. On an error status
              it is the API's own text (["Invalid key"]), as it is. }
            if StatusCode < 300 then
                Exit('ZeroBounce returned an error response.');
            Exit(Copy(Trimmed, 1, 1000));
        end;
        { Rows of a file are not a message. }
        if ZbBodyLooksLikeCsv(Trimmed) then
            Exit('ZeroBounce returned an error response.');
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
        { A text/html page is a proxy's or a portal's, not the API's answer,
          even when its body happens to parse as JSON. }
        if ZbBodyLooksLikeHtml(Response.Payload) or ZbContentTypeIsHtml(Response.ContentType) or
           (Response.StatusCode > 299) or ZbSuccessMeansFailure(Response.Payload) then
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
        PayloadEmpty: Boolean;
    begin
        Payload := response.Payload;
        StatusCode := response.StatusCode;
        { With no payload there is nothing for the message to copy, so its
          length says nothing: the instructions for a missing CA bundle are
          322 characters and are the whole of what went wrong. With one, a
          message of 300 characters or more, or one that repeats its start,
          is the body again. }
        PayloadEmpty := Trim(response.Payload) = '';
        if (AMessage <> '') and (PayloadEmpty or (Length(AMessage) < 300)) and
           (Pos('Access violation', AMessage) = 0) and
           (PayloadEmpty or (Pos(Copy(Trim(response.Payload), 1, 24), AMessage) = 0)) then
            NewMessage := AMessage
        else if ZbContentTypeIsCsv(response.ContentType) and
                (ZbDownloadKind(response.Payload, response.ContentType) = zdkFile) then
            { A file, not a message, whatever its first line looks like. }
            NewMessage := 'ZeroBounce returned an error response.'
        else
            NewMessage := ZbMessageFromBody(response.Payload, response.StatusCode);
        { Transport errors quote the URL, which carries api_key. }
        Create(RedactSecrets(NewMessage));
    end;

    procedure ZbException.AfterConstruction;
    begin
        inherited AfterConstruction;
        Message := RedactSecrets(Message);
        Payload := RedactSecrets(Payload);
    end;

    procedure ZbException.MarkHttpError;
    begin
        Self.Message := 'Http Error: ' + Self.Message;
	end;

    procedure ZbException.MarkJsonError;
    begin
         Self.Message := 'Json Error: ' + Self.Message;
	end;

    procedure ZbWipeString(var S: String);
    begin
        { StringRefCount is -1 for a literal and 1 for a buffer nobody else
          shares; only the latter is ours to overwrite. }
        if (S <> '') and (StringRefCount(S) = 1) then
            FillChar(Pointer(S)^, Length(S) * SizeOf(Char), 0);
        S := '';
    end;

    procedure ZbReplaceApiKey(const NewKey: String);
    begin
        { The old buffer is wiped unless it is the new key itself. A buffer
          another variable shares is left alone (ZbWipeString checks). }
        if Pointer(ZbApiKey) <> Pointer(NewKey) then
            ZbWipeString(ZbApiKey);
        ZbApiKey := NewKey;
    end;

    function ZBApiURLToString(url: ZbApiURL): string;
    begin
        ZBApiURLToString := ZbApiURLValue[url];
    end;

    procedure ZBSetApiKey ( ApiKey : string );
    begin
        { The same check as ZBInitialize: a dirty key is refused and the key
          already set stays. }
        ZbReplaceApiKey(ZbCheckedApiKey(ApiKey));
    end;

    function ZbBaseUrl(const Url: String): String;
    begin
        ZbRequireCleanUrl(Url, 'A base URL');
        Result := Url;
        { Every endpoint path is appended, so a query or a fragment here would
          swallow it ('https://host/v2?x=1' + '/validate') and send the rest
          of the request, key included, to a different place than meant. The
          URL is not quoted: it may carry a credential. }
        if (Pos('?', Result) > 0) or (Pos('#', Result) > 0) then
            raise ZbException.Create('A base URL must not contain a query string or a fragment ("?" or "#")');
        while (Result <> '') and (Result[Length(Result)] = '/') do
            Delete(Result, Length(Result), 1);
    end;

    procedure ZBInitialize(ApiKey: string);
    begin
        ZBInitialize(ApiKey, ZbApiURLDefault);
    end;

    procedure ZBInitialize(ApiKey: string; ApiBaseURL: ZbApiURL); overload;
    begin
        ZbReplaceApiKey(ZbCheckedApiKey(ApiKey));
        ZbApiBaseURL := ZBApiURLToString(ApiBaseURL);
    end;

    procedure ZBInitialize(ApiKey: string; ApiBaseURL: String); overload;
    var
        Normalized: String;
    begin
        Normalized := ApiBaseURL;
        if Normalized = '' then
            raise ZbException.Create('Empty parameter: api_base_url');
        { The same rail every request URL goes through: https, or loopback so
          an offline test can point the SDK at a local stub. }
        ZbRequireHttpsUrl(Normalized, 'api_base_url');
        { Every endpoint constant starts with a slash, so a trailing one here
          would build '//getcredits'. }
        Normalized := ZbBaseUrl(Normalized);
        ZbReplaceApiKey(ZbCheckedApiKey(ApiKey));
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

    var
        ZbSecretRedactors: array of TZbSecretRedactor;

    procedure ZbRegisterSecretRedactor(Redactor: TZbSecretRedactor);
    begin
        SetLength(ZbSecretRedactors, Length(ZbSecretRedactors) + 1);
        ZbSecretRedactors[High(ZbSecretRedactors)] := Redactor;
    end;

    function ZbHexDigit(C: Char): Integer;
    begin
        if (C >= '0') and (C <= '9') then
            Result := Ord(C) - Ord('0')
        else if (C >= 'a') and (C <= 'f') then
            Result := Ord(C) - Ord('a') + 10
        else if (C >= 'A') and (C <= 'F') then
            Result := Ord(C) - Ord('A') + 10
        else
            Result := -1;
    end;

    { Output buffers of the decoders below. A decoder writes into Buf and keeps
      the used length in Len; Buf doubles when it is full and the decoder cuts
      it to Len once at the end, so a decoder is linear in its input (Result :=
      Result + X copies the whole result each time). }
    procedure ZbBufReserve(var Buf: String; Len, More: Integer);
    var
        Need, Size: Integer;
    begin
        Need := Len + More;
        if Need <= Length(Buf) then
            Exit;
        Size := Length(Buf) * 2;
        if Size < Need then
            Size := Need + 16;
        SetLength(Buf, Size);
    end;

    procedure ZbBufAddChar(var Buf: String; var Len: Integer; C: Char);
    begin
        ZbBufReserve(Buf, Len, 1);
        Inc(Len);
        Buf[Len] := C;
    end;

    procedure ZbBufAddText(var Buf: String; var Len: Integer; const S: String);
    var
        K: Integer;
    begin
        if S = '' then
            Exit;
        ZbBufReserve(Buf, Len, Length(S));
        for K := 1 to Length(S) do
            Buf[Len + K] := S[K];
        Inc(Len, Length(S));
    end;

    {$IFNDEF FPC}
    { Delphi: the bytes a %XX or a \xHH wrote, in a row, are UTF-8 text, as
      they are on FPC where the string holds bytes; they are not one UTF-16
      unit each. Pending collects them (Count of them, doubling as it fills);
      ZbFlushBytes appends what they spell to Buf. }
    procedure ZbFlushBytes(var Pending: TBytes; var Count: Integer; var Buf: String; var Len: Integer);
    begin
        if Count = 0 then
            Exit;
        ZbBufAddText(Buf, Len, TEncoding.UTF8.GetString(Pending, 0, Count));
        Count := 0;
    end;

    procedure ZbPendByte(var Pending: TBytes; var Count: Integer; B: Integer);
    begin
        if Count >= Length(Pending) then
            SetLength(Pending, Length(Pending) * 2 + 16);
        Pending[Count] := Byte(B);
        Inc(Count);
    end;
    {$ENDIF}

    { Text URL-decoded: each %XX as its byte, and '+' as a space when
      PlusAsSpace is set (a form body). }
    function ZbUrlDecodeView(const Text: String; PlusAsSpace: Boolean): String;
    var
        I, N, Hi, Lo: Integer;
        {$IFNDEF FPC}
        Pending: TBytes;
        Count: Integer;
        {$ENDIF}
    begin
        { Nothing to decode: the view is the text. }
        if (Pos('%', Text) = 0) and not (PlusAsSpace and (Pos('+', Text) > 0)) then
        begin
            Result := Text;
            Exit;
        end;
        {$IFNDEF FPC}
        { Delphi: the bytes of a run of %XX are assembled and read as UTF-8; a
          character of the text itself is kept as it is. }
        N := 0;
        Result := '';
        SetLength(Result, Length(Text));
        Pending := nil;
        Count := 0;
        I := 1;
        while I <= Length(Text) do
        begin
            Hi := -1;
            Lo := -1;
            if (Text[I] = '%') and (I + 2 <= Length(Text)) then
            begin
                Hi := ZbHexDigit(Text[I + 1]);
                Lo := ZbHexDigit(Text[I + 2]);
            end;
            if (Hi >= 0) and (Lo >= 0) then
            begin
                ZbPendByte(Pending, Count, Hi * 16 + Lo);
                Inc(I, 3);
                Continue;
            end;
            ZbFlushBytes(Pending, Count, Result, N);
            if PlusAsSpace and (Text[I] = '+') then
                ZbBufAddChar(Result, N, ' ')
            else
                ZbBufAddChar(Result, N, Text[I]);
            Inc(I);
        end;
        ZbFlushBytes(Pending, Count, Result, N);
        SetLength(Result, N);
        {$ELSE}
        SetLength(Result, Length(Text));
        I := 1;
        N := 0;
        while I <= Length(Text) do
        begin
            Hi := -1;
            Lo := -1;
            if (Text[I] = '%') and (I + 2 <= Length(Text)) then
            begin
                Hi := ZbHexDigit(Text[I + 1]);
                Lo := ZbHexDigit(Text[I + 2]);
            end;
            Inc(N);
            if (Hi >= 0) and (Lo >= 0) then
            begin
                Result[N] := Chr(Hi * 16 + Lo);
                Inc(I, 3);
            end
            else
            begin
                if PlusAsSpace and (Text[I] = '+') then
                    Result[N] := ' '
                else
                    Result[N] := Text[I];
                Inc(I);
            end;
        end;
        SetLength(Result, N);
        {$ENDIF}
    end;

    function ZbHex4(const Text: String; At: Integer): Integer;
    var
        K, D: Integer;
    begin
        Result := -1;
        if At + 3 > Length(Text) then
            Exit;
        Result := 0;
        for K := At to At + 3 do
        begin
            D := ZbHexDigit(Text[K]);
            if D < 0 then
            begin
                Result := -1;
                Exit;
            end;
            Result := Result * 16 + D;
        end;
    end;

    { One code point as the string type holds it: UTF-8 bytes on FPC, a UTF-16
      unit on Delphi. }
    function ZbCodePointText(CodePoint: Integer): String;
    begin
        {$IFDEF FPC}
        if CodePoint < $80 then
            Result := Chr(CodePoint)
        else if CodePoint < $800 then
            Result := Chr($C0 or (CodePoint shr 6)) + Chr($80 or (CodePoint and $3F))
        else if CodePoint < $10000 then
            Result := Chr($E0 or (CodePoint shr 12)) + Chr($80 or ((CodePoint shr 6) and $3F)) +
                Chr($80 or (CodePoint and $3F))
        else
            Result := Chr($F0 or (CodePoint shr 18)) + Chr($80 or ((CodePoint shr 12) and $3F)) +
                Chr($80 or ((CodePoint shr 6) and $3F)) + Chr($80 or (CodePoint and $3F));
        {$ELSE}
        { Above U+FFFF it is a surrogate pair, not one truncated unit. }
        if CodePoint >= $10000 then
            Result := Char($D800 + ((CodePoint - $10000) shr 10)) +
                Char($DC00 + ((CodePoint - $10000) and $3FF))
        else
            Result := Char(CodePoint);
        {$ENDIF}
    end;

    { The hex digits at Text[At..], at most MaxDigits of them, as one number;
      Count is how many there were. Stops at the first character that is not
      a hex digit; a value past U+10FFFF stops growing. }
    function ZbHexRun(const Text: String; At, MaxDigits: Integer; out Count: Integer): Integer;
    var
        D: Integer;
    begin
        Result := 0;
        Count := 0;
        while (At + Count <= Length(Text)) and (Count < MaxDigits) do
        begin
            D := ZbHexDigit(Text[At + Count]);
            if D < 0 then
                Break;
            if Result <= $10FFFF then
                Result := Result * 16 + D;
            Inc(Count);
        end;
    end;

    (* A braced code point, hex digits (1 to 8, value at most U+10FFFF) in curly
      brackets, starting at the opening bracket at Text[At]. Len is how many
      characters it takes, brackets included. -1 when it is not one. *)
    function ZbBracedCodePoint(const Text: String; At: Integer; out Len: Integer): Integer;
    var
        Count: Integer;
    begin
        Result := -1;
        Len := 0;
        if (At > Length(Text)) or (Text[At] <> '{') then
            Exit;
        Result := ZbHexRun(Text, At + 1, 8, Count);
        if (Count = 0) or (At + Count + 1 > Length(Text)) or (Text[At + Count + 1] <> '}') or
           (Result > $10FFFF) then
        begin
            Result := -1;
            Exit;
        end;
        Len := Count + 2;
    end;

    (* Text with string escapes undone: the JSON ones (\" \\ \/ \b \f \n \r \t
      \uXXXX, surrogate pairs included on FPC) and the other spellings a
      server or a language may write: \' \xHH (one byte), \u and \U with
      up to 8 hex digits in curly brackets (at most U+10FFFF), \UXXXXXXXX,
      \UXXXX, \x{...} (a code point, as \u{...} is), and the octal byte \NNN
      or \NN. Anything else is kept. *)
    function ZbJsonUnescapeView(const Text: String): String;
    const
        ESCAPES = '"\/bfnrt''';
    var
        I, K, CodePoint, Low, Len, Count, Value: Integer;
        Decoded: String;
        N: Integer;
        {$IFNDEF FPC}
        Pending: TBytes;
        Count: Integer;
        {$ENDIF}

        { One byte of an escape. FPC strings hold bytes. Delphi's hold UTF-16
          units, so the bytes wait in Pending until what follows them is not
          a byte, and are then read as UTF-8. }
        procedure AddByte(B: Integer);
        begin
            {$IFDEF FPC}
            ZbBufAddChar(Result, N, Char(B));
            {$ELSE}
            ZbPendByte(Pending, Count, B);
            {$ENDIF}
        end;

        procedure FlushBytes;
        begin
            {$IFNDEF FPC}
            ZbFlushBytes(Pending, Count, Result, N);
            {$ENDIF}
        end;

    begin
        { No backslash: nothing to undo, the view is the text. }
        if Pos('\', Text) = 0 then
        begin
            Result := Text;
            Exit;
        end;
        Decoded := '"\/'#8#12#10#13#9#39;
        Result := '';
        SetLength(Result, Length(Text) + 16);
        N := 0;
        {$IFNDEF FPC}
        Pending := nil;
        Count := 0;
        {$ENDIF}
        I := 1;
        while I <= Length(Text) do
        begin
            if (Text[I] <> '\') or (I = Length(Text)) then
            begin
                FlushBytes;
                ZbBufAddChar(Result, N, Text[I]);
                Inc(I);
                Continue;
            end;
            K := Pos(Text[I + 1], ESCAPES);
            if K > 0 then
            begin
                FlushBytes;
                ZbBufAddChar(Result, N, Decoded[K]);
                Inc(I, 2);
                Continue;
            end;
            CodePoint := -1;
            Len := 0;
            case Text[I + 1] of
                'u':
                    if (I + 2 <= Length(Text)) and (Text[I + 2] = '{') then
                    begin
                        CodePoint := ZbBracedCodePoint(Text, I + 2, Len);
                        Inc(Len, 2);
                    end
                    else
                    begin
                        CodePoint := ZbHex4(Text, I + 2);
                        Len := 6;
                    end;
                'U':
                    if (I + 2 <= Length(Text)) and (Text[I + 2] = '{') then
                    begin
                        CodePoint := ZbBracedCodePoint(Text, I + 2, Len);
                        Inc(Len, 2);
                    end
                    else
                    begin
                        { Eight digits first, as \U00000027; else four. }
                        CodePoint := ZbHexRun(Text, I + 2, 8, Count);
                        if (Count = 8) and (CodePoint <= $10FFFF) then
                            Len := 10
                        else
                        begin
                            CodePoint := ZbHex4(Text, I + 2);
                            Len := 6;
                        end;
                    end;
                'x':
                    if (I + 2 <= Length(Text)) and (Text[I + 2] = '{') then
                    begin
                        (* \x{27}: a code point, as \u{27} is. *)
                        CodePoint := ZbBracedCodePoint(Text, I + 2, Len);
                        Inc(Len, 2);
                    end
                    else if (I + 3 <= Length(Text)) and (ZbHexDigit(Text[I + 2]) >= 0) and
                       (ZbHexDigit(Text[I + 3]) >= 0) then
                    begin
                        { One byte, not a code point: \xC3\xA9 is the two bytes
                          of one UTF-8 character. }
                        AddByte(ZbHexDigit(Text[I + 2]) * 16 + ZbHexDigit(Text[I + 3]));
                        Inc(I, 4);
                        Continue;
                    end;
                '0'..'7':
                    if (I + 2 <= Length(Text)) and (Text[I + 2] in ['0'..'7']) then
                    begin
                        { Three digits when the first is 0 to 3 and a third
                          follows (\047), else two (\47). }
                        Value := (Ord(Text[I + 1]) - 48) * 8 + (Ord(Text[I + 2]) - 48);
                        Len := 3;
                        if (Text[I + 1] in ['0'..'3']) and (I + 3 <= Length(Text)) and
                           (Text[I + 3] in ['0'..'7']) then
                        begin
                            Value := Value * 8 + (Ord(Text[I + 3]) - 48);
                            Len := 4;
                        end;
                        AddByte(Value);
                        Inc(I, Len);
                        Continue;
                    end;
            end;
            FlushBytes;
            if (CodePoint < 0) or (Len = 0) then
            begin
                ZbBufAddChar(Result, N, Text[I]);
                Inc(I);
                Continue;
            end;
            Inc(I, Len);
            {$IFDEF FPC}
            if (Len = 6) and (CodePoint >= $D800) and (CodePoint <= $DBFF) and (I + 1 <= Length(Text)) and
               (Text[I] = '\') and (Text[I + 1] = 'u') then
            begin
                Low := ZbHex4(Text, I + 2);
                if (Low >= $DC00) and (Low <= $DFFF) then
                begin
                    CodePoint := $10000 + ((CodePoint - $D800) shl 10) + (Low - $DC00);
                    Inc(I, 6);
                end;
            end;
            {$ENDIF}
            ZbBufAddText(Result, N, ZbCodePointText(CodePoint));
        end;
        FlushBytes;
        SetLength(Result, N);
    end;

    const
        { The HTML 4 Latin-1 names, U+00A0 to U+00FF in order. }
        ZB_HTML_LATIN1_NAMES: array[0..95] of String = (
            'nbsp', 'iexcl', 'cent', 'pound', 'curren', 'yen', 'brvbar', 'sect',
            'uml', 'copy', 'ordf', 'laquo', 'not', 'shy', 'reg', 'macr',
            'deg', 'plusmn', 'sup2', 'sup3', 'acute', 'micro', 'para', 'middot',
            'cedil', 'sup1', 'ordm', 'raquo', 'frac14', 'frac12', 'frac34', 'iquest',
            'Agrave', 'Aacute', 'Acirc', 'Atilde', 'Auml', 'Aring', 'AElig', 'Ccedil',
            'Egrave', 'Eacute', 'Ecirc', 'Euml', 'Igrave', 'Iacute', 'Icirc', 'Iuml',
            'ETH', 'Ntilde', 'Ograve', 'Oacute', 'Ocirc', 'Otilde', 'Ouml', 'times',
            'Oslash', 'Ugrave', 'Uacute', 'Ucirc', 'Uuml', 'Yacute', 'THORN', 'szlig',
            'agrave', 'aacute', 'acirc', 'atilde', 'auml', 'aring', 'aelig', 'ccedil',
            'egrave', 'eacute', 'ecirc', 'euml', 'igrave', 'iacute', 'icirc', 'iuml',
            'eth', 'ntilde', 'ograve', 'oacute', 'ocirc', 'otilde', 'ouml', 'divide',
            'oslash', 'ugrave', 'uacute', 'ucirc', 'uuml', 'yacute', 'thorn', 'yuml'
        );
        { The other names: the ones that make up an HTML-escaped quote or tag,
          and the ASCII punctuation (HTML 5) an echo may spell out. }
        ZB_HTML_OTHER_NAMES: array[0..30] of String = (
            'quot', 'gt', 'lt', 'amp', 'apos', 'lpar', 'rpar', 'period', 'NewLine', 'Tab',
            'excl', 'num', 'dollar', 'percnt', 'ast', 'plus', 'comma', 'sol', 'colon',
            'semi', 'equals', 'quest', 'commat', 'lsqb', 'lbrack', 'rsqb', 'rbrack',
            'bsol', 'lowbar', 'grave', 'verbar'
        );
        ZB_HTML_OTHER_CODES: array[0..30] of Integer = (
            $22, $3E, $3C, $26, $27, $28, $29, $2E, $0A, $09,
            $21, $23, $24, $25, $2A, $2B, $2C, $2F, $3A,
            $3B, $3D, $3F, $40, $5B, $5B, $5D, $5D,
            $5C, $5F, $60, $7C
        );

    { The code point a character reference name stands for (the name without
      its '&' and ';'), or -1. A name is matched exactly first, then in any
      letter case: Eacute and eacute are different letters, but the view is
      for searching and the search ignores case. }
    function ZbHtmlNamedCodePoint(const Name: String): Integer;
    var
        K: Integer;
    begin
        Result := -1;
        for K := Low(ZB_HTML_LATIN1_NAMES) to High(ZB_HTML_LATIN1_NAMES) do
            if ZB_HTML_LATIN1_NAMES[K] = Name then
                Exit($A0 + K);
        for K := Low(ZB_HTML_OTHER_NAMES) to High(ZB_HTML_OTHER_NAMES) do
            if ZB_HTML_OTHER_NAMES[K] = Name then
                Exit(ZB_HTML_OTHER_CODES[K]);
        for K := Low(ZB_HTML_LATIN1_NAMES) to High(ZB_HTML_LATIN1_NAMES) do
            if SameText(ZB_HTML_LATIN1_NAMES[K], Name) then
                Exit($A0 + K);
        for K := Low(ZB_HTML_OTHER_NAMES) to High(ZB_HTML_OTHER_NAMES) do
            if SameText(ZB_HTML_OTHER_NAMES[K], Name) then
                Exit(ZB_HTML_OTHER_CODES[K]);
    end;

    { Text with the HTML character references of a quoted or escaped value
      undone, in any letter case: the numeric ones, decimal or hexadecimal,
      with any number of leading zeros (&#34; &#039; &#233; &#x22; &#X27;),
      and the names of ZbHtmlNamedCodePoint: the HTML 4 Latin-1 ones (&eacute;
      &nbsp;), &quot; &gt; &lt; &amp; &apos; &lpar; &rpar; &period; &NewLine;
      &Tab; and some more punctuation. A numeric reference may leave out its
      semicolon when the character after it is not a digit (a browser reads
      &#34x as a quote and an x); a name may not. Used to search for a secret
      only: a server may HTML-escape the request it echoes back. }
    function ZbHtmlEntityView(const Text: String): String;
    const
        MAX_NAME = 8;
    var
        I, K, J, Digit, Radix, Code: Integer;
        CodePoint: Int64;
        Matched: Boolean;
        N: Integer;
    begin
        { No ampersand: no reference, the view is the text. }
        if Pos('&', Text) = 0 then
        begin
            Result := Text;
            Exit;
        end;
        Result := '';
        SetLength(Result, Length(Text) + 16);
        N := 0;
        I := 1;
        while I <= Length(Text) do
        begin
            Matched := False;
            if Text[I] = '&' then
            begin
                if (I + 2 <= Length(Text)) and (Text[I + 1] = '#') then
                begin
                    { A numeric reference: &#, an optional x, digits, and a
                      ';' or a character that is not a digit. }
                    J := I + 2;
                    Radix := 10;
                    if (Text[J] = 'x') or (Text[J] = 'X') then
                    begin
                        Radix := 16;
                        Inc(J);
                    end;
                    CodePoint := 0;
                    K := J;
                    while K <= Length(Text) do
                    begin
                        Digit := ZbHexDigit(Text[K]);
                        if (Digit < 0) or (Digit >= Radix) then
                            Break;
                        { Past U+10FFFF it stops growing. }
                        if CodePoint <= $10FFFF then
                            CodePoint := CodePoint * Radix + Digit;
                        Inc(K);
                    end;
                    if (K > J) and (CodePoint <= $10FFFF) and
                       not ((CodePoint >= $D800) and (CodePoint <= $DFFF)) then
                    begin
                        ZbBufAddText(Result, N, ZbCodePointText(Integer(CodePoint)));
                        if (K <= Length(Text)) and (Text[K] = ';') then
                            Inc(K);
                        I := K;
                        Matched := True;
                    end;
                end
                else
                begin
                    { A name: letters and digits, then ';'. }
                    K := I + 1;
                    while (K <= Length(Text)) and (K - I <= MAX_NAME + 1) and
                          (Text[K] in ['A'..'Z', 'a'..'z', '0'..'9']) do
                        Inc(K);
                    if (K > I + 1) and (K <= Length(Text)) and (Text[K] = ';') then
                    begin
                        Code := ZbHtmlNamedCodePoint(Copy(Text, I + 1, K - I - 1));
                        if Code >= 0 then
                        begin
                            ZbBufAddText(Result, N, ZbCodePointText(Code));
                            I := K + 1;
                            Matched := True;
                        end;
                    end;
                end;
            end;
            if not Matched then
            begin
                ZbBufAddChar(Result, N, Text[I]);
                Inc(I);
            end;
        end;
        SetLength(Result, N);
    end;

    { Unicode 16.0.0 full case folding (statuses C and F), generated by
      tools/gen_casefold.py: ZB_CASEFOLD_KEYS ascending, three values for each. }
    {$I zbcasefold.inc}

    { What one code point folds to under full case folding, written to
      Folded[0..2] (0 for none); the count. The binary search is over the
      generated table. }
    function ZbCaseFoldCodePoint(CodePoint: Integer; var Folded: array of LongInt): Integer;
    var
        Low, High, Mid: Integer;
    begin
        Result := 1;
        Folded[0] := CodePoint;
        Low := 0;
        High := ZB_CASEFOLD_COUNT - 1;
        while Low <= High do
        begin
            Mid := (Low + High) shr 1;
            if ZB_CASEFOLD_KEYS[Mid] = CodePoint then
            begin
                Folded[0] := ZB_CASEFOLD_VALUES[Mid * 3];
                Folded[1] := ZB_CASEFOLD_VALUES[Mid * 3 + 1];
                Folded[2] := ZB_CASEFOLD_VALUES[Mid * 3 + 2];
                Result := 1;
                if Folded[1] <> 0 then
                    Result := 2;
                if Folded[2] <> 0 then
                    Result := 3;
                Exit;
            end;
            if ZB_CASEFOLD_KEYS[Mid] < CodePoint then
                Low := Mid + 1
            else
                High := Mid - 1;
        end;
    end;

    const
        { The letter a precomposed Latin character is built on, for U+00C0 to
          U+024F and for U+1E00 to U+1EFF: its first letter in canonical
          decomposition (a + U+0301 for U+00E1), lower case. '-' where there is
          none. Together with the dropping of combining marks (ZbIsCombiningMark)
          this makes 'e' + U+0301 and U+00E9 one and the same text. }
        ZB_BASE_LATIN_1 =
            'aaaaaa-ceeeeiiii-nooooo--uuuuy--aaaaaa-ceeeeiiii-nooooo--uuuuy-y' +
            'aaaaaaccccccccdd--eeeeeeeeeegggggggghh--iiiiiiiii---jjkk-llllll-' +
            '---nnnnnn---oooooo--rrrrrrsssssssstttt--uuuuuuuuuuuuwwyyyzzzzzz-' +
            '--------------------------------oo-------------uu---------------' +
            '-------------aaiioouuuuuuuuuu-aaaa----ggkkoooo--j---gg--nnaa----' +
            'aaaaeeeeiiiioooorrrruuuusstt--hh------aaeeooooooooyy------------' +
            '----------------';
        ZB_BASE_LATIN_ADDITIONAL =
            'aabbbbbbccddddddddddeeeeeeeeeeffgghhhhhhhhhhiiiikkkkkkllllllllmm' +
            'mmmmnnnnnnnnoooooooopppprrrrrrrrssssssssssttttttttuuuuuuuuuuvvvv' +
            'wwwwwwwwwwxxxxyyzzzzzzhtwy------aaaaaaaaaaaaaaaaaaaaaaaaeeeeeeee' +
            'eeeeeeeeiiiioooooooooooooooooooooooouuuuuuuuuuuuuuyyyyyyyy------';

    { The letter a precomposed character is built on (the character itself when
      it is not one): the Latin tables above, and the Greek and Cyrillic
      letters with a tonos, a diaeresis or a breve, as their lower-case
      forms. }
    function ZbBaseLetter(CodePoint: Integer): Integer;
    var
        Base: Char;
    begin
        Result := CodePoint;
        Base := '-';
        if (CodePoint >= $C0) and (CodePoint <= $24F) then
            Base := ZB_BASE_LATIN_1[CodePoint - $C0 + 1]
        else if (CodePoint >= $1E00) and (CodePoint <= $1EFF) then
            Base := ZB_BASE_LATIN_ADDITIONAL[CodePoint - $1E00 + 1]
        else
            case CodePoint of
                $390, $3AF, $3CA: Result := $3B9;
                $3AC: Result := $3B1;
                $3AD: Result := $3B5;
                $3AE: Result := $3B7;
                $3B0, $3CB, $3CD: Result := $3C5;
                $3CC: Result := $3BF;
                $3CE: Result := $3C9;
                $439, $45D: Result := $438;
                $450, $451: Result := $435;
                $453: Result := $433;
                $457: Result := $456;
                $45C: Result := $43A;
                $45E: Result := $443;
            end;
        if Base <> '-' then
            Result := Ord(Base);
    end;

    { Whether a code point is a combining mark: the diacritical blocks (U+0300
      to U+036F and its extensions) and the combining marks for symbols and
      half marks. They are not part of what a person reads as the letter, and
      an echo may write the same letter with or without one. }
    function ZbIsCombiningMark(CodePoint: Integer): Boolean;
    begin
        case CodePoint of
            { U+0345, the Greek ypogegrammeni, is left out: upper-casing makes
              it a letter (a capital iota), so it folds to one (ZbFoldUnit)
              and is no mark an echo may leave off. }
            $300..$344, $346..$36F, $1AB0..$1AFF, $1DC0..$1DFF, $20D0..$20FF, $FE20..$FE2F:
                Result := True;
        else
            Result := False;
        end;
    end;

    { The folded text of one character: its full case folding (the table
      above), with the letter a precomposed one is built on (ZbBaseLetter) and
      without combining marks, with these additions:
      - the dotless i folds to i, as the capital dotted I does;
      - a Hangul syllable becomes its two or three jamo, as NFD has it.
      So the sharp s (and its capital) is 'ss', the ligatures U+FB00 to
      U+FB06 are their letters, U+0587 is two Armenian letters and U+1F80 a
      Greek alpha with psili and an iota, as an echo that upper-cases the
      secret writes them. }
    function ZbFoldUnit(CodePoint: Integer): String;
    var
        Folded: array[0..2] of LongInt;
        Count, K, Syllable: Integer;
    begin
        if (CodePoint >= $AC00) and (CodePoint <= $D7A3) then
        begin
            Syllable := CodePoint - $AC00;
            Result := ZbCodePointText($1100 + Syllable div 588) +
                ZbCodePointText($1161 + (Syllable mod 588) div 28);
            if Syllable mod 28 <> 0 then
                Result := Result + ZbCodePointText($11A7 + Syllable mod 28);
            Exit;
        end;
        if CodePoint = $131 then
            Exit('i');
        Count := ZbCaseFoldCodePoint(CodePoint, Folded);
        Result := '';
        for K := 0 to Count - 1 do
            if not ZbIsCombiningMark(Folded[K]) then
                Result := Result + ZbCodePointText(ZbBaseLetter(Folded[K]));
    end;

    { One character at Text[At]: its code point and how many string units it
      takes. A byte that is not valid UTF-8 (FPC) or a lone surrogate (Delphi)
      is a character of its own with the code point -1. }
    procedure ZbNextCharacter(const Text: String; At: Integer; out CodePoint, Len: Integer);
    {$IFDEF FPC}
    var
        B, Need, K, C: Integer;
    {$ELSE}
    var
        B: Integer;
    {$ENDIF}
    begin
        Len := 1;
        CodePoint := -1;
        {$IFDEF FPC}
        B := Ord(Text[At]);
        if B < $80 then
        begin
            CodePoint := B;
            Exit;
        end;
        if (B >= $C2) and (B <= $DF) then
        begin
            Need := 1;
            CodePoint := B and $1F;
        end
        else if (B >= $E0) and (B <= $EF) then
        begin
            Need := 2;
            CodePoint := B and $0F;
        end
        else if (B >= $F0) and (B <= $F4) then
        begin
            Need := 3;
            CodePoint := B and $07;
        end
        else
            Exit;
        if At + Need > Length(Text) then
        begin
            CodePoint := -1;
            Exit;
        end;
        for K := 1 to Need do
        begin
            C := Ord(Text[At + K]);
            if (C < $80) or (C > $BF) then
            begin
                CodePoint := -1;
                Exit;
            end;
            CodePoint := (CodePoint shl 6) or (C and $3F);
        end;
        { Overlong forms, surrogates and what lies past U+10FFFF are not text. }
        if ((Need = 2) and (CodePoint < $800)) or ((Need = 3) and (CodePoint < $10000)) or
           ((CodePoint >= $D800) and (CodePoint <= $DFFF)) or (CodePoint > $10FFFF) then
        begin
            CodePoint := -1;
            Exit;
        end;
        Len := Need + 1;
        {$ELSE}
        B := Ord(Text[At]);
        if (B >= $D800) and (B <= $DBFF) and (At < Length(Text)) and
           (Ord(Text[At + 1]) >= $DC00) and (Ord(Text[At + 1]) <= $DFFF) then
        begin
            CodePoint := $10000 + ((B - $D800) shl 10) + (Ord(Text[At + 1]) - $DC00);
            Len := 2;
        end
        else if (B < $D800) or (B > $DFFF) then
            CodePoint := B;
        {$ENDIF}
    end;

    type
        TZbSpans = array of Integer;

    { Text with every character folded (see ZbFoldUnit). Spans, filled only
      when Mapped is set, holds for the K-th character of the result the
      first and the last character of Text it was made from, at 2K-1 and 2K
      (K from 1, as Spans[2K-2] and Spans[2K-1]); without Mapped the result
      lines up with Text one for one. }
    procedure ZbFold(const Text: String; out Folded: String; out Spans: TZbSpans; out Mapped: Boolean);
    var
        I, Len, CodePoint, Used, J: Integer;
        Piece: String;
    begin
        Spans := nil;
        Mapped := False;
        for I := 1 to Length(Text) do
            if Ord(Text[I]) >= $80 then
            begin
                Mapped := True;
                Break;
            end;
        if not Mapped then
        begin
            Folded := Text;
            UniqueString(Folded);
            for I := 1 to Length(Folded) do
                if Folded[I] in ['A'..'Z'] then
                    Folded[I] := Chr(Ord(Folded[I]) + 32);
            Exit;
        end;
        { No character makes more than three units for each one it takes: a
          Hangul syllable on Delphi, one UTF-16 unit, is three jamo. }
        SetLength(Folded, Length(Text) * 3);
        SetLength(Spans, Length(Text) * 6);
        Used := 0;
        I := 1;
        while I <= Length(Text) do
        begin
            ZbNextCharacter(Text, I, CodePoint, Len);
            { A combining mark is dropped, from the secret and from an echo
              alike, so a letter with an accent and the same letter followed by
              the mark read as one text: U+0130 folds to a plain i and so does
              'i' + U+0307 (what its full lower-case is), and U+00E9 and
              'e' + U+0301 both fold to e. The character before it takes the
              mark into its span, so an echo is masked whole. A mark with
              nothing before it is covered by no span and stays. }
            if ZbIsCombiningMark(CodePoint) then
            begin
                if Used > 0 then
                    Spans[2 * Used - 1] := I + Len - 1;
                Inc(I, Len);
                Continue;
            end;
            if CodePoint < 0 then
                Piece := Copy(Text, I, Len)
            else if CodePoint < $80 then
            begin
                if (CodePoint >= 65) and (CodePoint <= 90) then
                    Piece := Chr(CodePoint + 32)
                else
                    Piece := Chr(CodePoint);
            end
            else
                Piece := ZbFoldUnit(CodePoint);
            for J := 1 to Length(Piece) do
            begin
                Inc(Used);
                Folded[Used] := Piece[J];
                Spans[2 * Used - 2] := I;
                Spans[2 * Used - 1] := I + Len - 1;
            end;
            Inc(I, Len);
        end;
        SetLength(Folded, Used);
    end;

    function ZbFoldText(const Text: String): String;
    var
        Spans: TZbSpans;
        Mapped: Boolean;
    begin
        ZbFold(Text, Result, Spans, Mapped);
    end;

    type
        { A view already looked at: its length, two 64-bit hashes and the
          number of layers it was decoded further with. }
        TZbSeenView = record
            Size: Integer;
            HashA, HashB: QWord;
            Layers: Integer;
        end;
        TZbSeenViews = array of TZbSeenView;

    procedure ZbHashView(const Text: String; out HashA, HashB: QWord);
    var
        I: Integer;
        C: QWord;
    begin
        HashA := QWord($CBF29CE484222325);
        HashB := QWord($84222325CBF29CE4);
        for I := 1 to Length(Text) do
        begin
            C := QWord(Ord(Text[I]));
            HashA := (HashA xor C) * QWord($100000001B3);
            HashB := (HashB + C + 1) * QWord($9E3779B97F4A7C15);
            HashB := HashB xor (HashB shr 29);
        end;
    end;

    { Whether the folded secret can be read in Text, or in any view of it
      decoded up to Layers more times: URL-decoded, form-decoded ('+' as a
      space too), unescaped as a string literal and with its HTML entities
      undone, in any order. Case is ignored. A secret that folds to nothing
      (only combining marks) is looked for as that exact sequence, RawSecret,
      in the text as it is. The whole text is looked at: every decoder is
      linear, a layer that cannot change the text costs nothing, and a view
      reached by two orders of decoding is looked at once (Seen). }
    function ZbSecretInViews(const Text, FoldedSecret, RawSecret: String; Layers: Integer;
        var Seen: TZbSeenViews): Boolean;
    var
        Views: array[0..3] of String;
        I, K: Integer;
        HashA, HashB: QWord;
        Known: Boolean;
    begin
        if FoldedSecret = '' then
            Result := (RawSecret <> '') and (Pos(RawSecret, Text) > 0)
        else
            Result := Pos(FoldedSecret, ZbFoldText(Text)) > 0;
        if Result or (Layers = 0) then
            Exit;
        { Nothing to decode: every view is Text itself. }
        if (Pos('%', Text) = 0) and (Pos('+', Text) = 0) and (Pos('\', Text) = 0) and
           (Pos('&', Text) = 0) then
            Exit;
        { A decoder whose trigger character is missing returns Text itself
          (same string, no copy), so such a layer costs nothing here. The form
          view differs from the URL one only when there is a '+'. }
        Views[0] := ZbUrlDecodeView(Text, False);
        if Pos('+', Text) > 0 then
            Views[1] := ZbUrlDecodeView(Text, True)
        else
            Views[1] := Text;
        Views[2] := ZbJsonUnescapeView(Text);
        Views[3] := ZbHtmlEntityView(Text);
        for I := 0 to 3 do
        begin
            if Views[I] = Text then
                Continue;
            ZbHashView(Views[I], HashA, HashB);
            Known := False;
            for K := 0 to High(Seen) do
                if (Seen[K].Size = Length(Views[I])) and (Seen[K].HashA = HashA) and
                   (Seen[K].HashB = HashB) and (Seen[K].Layers >= Layers - 1) then
                begin
                    Known := True;
                    Break;
                end;
            if Known then
                Continue;
            K := Length(Seen);
            SetLength(Seen, K + 1);
            Seen[K].Size := Length(Views[I]);
            Seen[K].HashA := HashA;
            Seen[K].HashB := HashB;
            Seen[K].Layers := Layers - 1;
            if ZbSecretInViews(Views[I], FoldedSecret, RawSecret, Layers - 1, Seen) then
            begin
                Result := True;
                Exit;
            end;
        end;
    end;

    function ZbSecretInDecodedViews(const Text, FoldedSecret, RawSecret: String; Layers: Integer): Boolean;
    var
        Seen: TZbSeenViews;
    begin
        Seen := nil;
        Result := ZbSecretInViews(Text, FoldedSecret, RawSecret, Layers, Seen);
    end;

    function ZbMaskSecret(const S, Secret: string): string;
    const
        HEX = '0123456789ABCDEF';
    var
        Forms: array[0..11] of String;
        JsonSlash, JsonPlain, FoldedForm, FoldedSecret, Folded, Scan, Out: String;
        Spans: TZbSpans;
        Mapped: Boolean;
        MaxEnd, Cover: array of Integer;
        F, At, I, J, HitEnd, Last, Kept: Integer;
        Seen: array of String;
        Duplicate, AnyHit, AnyProtected: Boolean;

        { The text a mask or an earlier pass already wrote is not looked at
          again: a secret such as 'ted' is in 'REDACTED', and masking that
          would grow the text at every pass. A match wholly inside one of them
          is left; one that only touches it is still masked. }
        procedure Protect(const Literal: String);
        var
            P, K: Integer;
        begin
            P := Pos(Literal, S);
            while P > 0 do
            begin
                if not AnyProtected then
                begin
                    SetLength(Cover, Length(S) + 2);
                    AnyProtected := True;
                end;
                for K := P to P + Length(Literal) - 1 do
                    Cover[K] := P + Length(Literal);
                P := PosEx(Literal, S, P + Length(Literal));
            end;
        end;

        procedure AddHit(OB, OE: Integer);
        begin
            if AnyProtected and (Cover[OB] >= OE) then
                Exit;
            if not AnyHit then
                SetLength(MaxEnd, Length(S) + 2);
            AnyHit := True;
            if OE > MaxEnd[OB] then
                MaxEnd[OB] := OE;
        end;

        { Everything but the unreserved characters percent-encoded. }
        function PercentEncoded(const Value: String): String;
        var
            J: Integer;
        begin
            Result := '';
            for J := 1 to Length(Value) do
                if Value[J] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~'] then
                    Result := Result + Value[J]
                else
                    Result := Result + '%' + HEX[(Ord(Value[J]) shr 4) + 1] + HEX[(Ord(Value[J]) and 15) + 1];
        end;

        { As a form body has it: every %20 written as '+'. }
        function PlusForSpaces(const Value: String): String;
        begin
            Result := StringReplace(Value, '%20', '+', [rfReplaceAll]);
        end;

        { As inside a JSON string: '"' and '\' escaped, and '/' too when
          EscapeSlash is set (fpjson does, other encoders do not). }
        function JsonEscaped(const Value: String; EscapeSlash: Boolean): String;
        var
            J: Integer;
        begin
            Result := '';
            for J := 1 to Length(Value) do
                if (Value[J] = '"') or (Value[J] = '\') or (EscapeSlash and (Value[J] = '/')) then
                    Result := Result + '\' + Value[J]
                else
                    Result := Result + Value[J];
        end;

    begin
        Result := S;
        if Secret = '' then
            Exit;
        JsonSlash := JsonEscaped(Secret, True);
        JsonPlain := JsonEscaped(Secret, False);
        { 0 and 1: JSON-escaped twice, an echoed body quoted inside another
          JSON string; 2 to 5: each JSON-escaped form percent-encoded, a space
          as %20 and as '+', as a server putting an echoed JSON body into a
          URL or form has it; 6 and 7: JSON-escaped; 8 and 9: the secret
          percent-encoded, %20 and '+'; 10: as EncodeParam sends it on a query
          string; 11: raw. The longer, escaped forms go first, as they can
          contain the raw one. }
        Forms[0] := JsonEscaped(JsonSlash, True);
        Forms[1] := JsonEscaped(JsonPlain, False);
        Forms[2] := PercentEncoded(JsonSlash);
        Forms[3] := PlusForSpaces(Forms[2]);
        Forms[4] := PercentEncoded(JsonPlain);
        Forms[5] := PlusForSpaces(Forms[4]);
        Forms[6] := JsonSlash;
        Forms[7] := JsonPlain;
        Forms[8] := PercentEncoded(Secret);
        Forms[9] := PlusForSpaces(Forms[8]);
        Forms[10] := EncodeParam(Secret);
        Forms[11] := Secret;
        { One pass: every form is looked for in the folded text once (case is
          ignored: an echo may upper-case or lower-case the key, and the hex of
          a percent-encoding comes in either case), the spans it covers in S
          are collected and merged, and the text is built once, so a mask is
          never read again as text. }
        AnyProtected := False;
        AnyHit := False;
        Protect('REDACTED');
        Protect(ZB_SECRET_OMITTED_MESSAGE);
        ZbFold(S, Folded, Spans, Mapped);
        SetLength(Seen, 0);
        for F := 0 to High(Forms) do
            if Forms[F] <> '' then
            begin
                FoldedForm := ZbFoldText(Forms[F]);
                Duplicate := False;
                for I := 0 to High(Seen) do
                    if Seen[I] = FoldedForm then
                        Duplicate := True;
                if Duplicate then
                    Continue;
                SetLength(Seen, Length(Seen) + 1);
                Seen[High(Seen)] := FoldedForm;
                if FoldedForm = '' then
                begin
                    { Only combining marks, which the fold drops: nothing
                      to compare folded, so the exact sequence is masked. }
                    At := Pos(Forms[F], S);
                    while At > 0 do
                    begin
                        AddHit(At, At + Length(Forms[F]));
                        At := PosEx(Forms[F], S, At + 1);
                    end;
                    Continue;
                end;
                At := Pos(FoldedForm, Folded);
                while At > 0 do
                begin
                    if Mapped then
                        AddHit(Spans[2 * At - 2], Spans[2 * (At + Length(FoldedForm) - 1) - 1] + 1)
                    else
                        AddHit(At, At + Length(FoldedForm));
                    At := PosEx(FoldedForm, Folded, At + 1);
                end;
            end;
        Folded := '';
        Spans := nil;
        Scan := S;
        if AnyHit or AnyProtected then
        begin
            { Out: S with each merged span a mask. Scan: what is left of S
              outside the spans and outside the literals above, the part a
              decoded view is looked at in. }
            Out := '';
            Scan := '';
            Kept := 1;
            I := 1;
            Last := 1;
            while I <= Length(S) do
            begin
                if AnyHit and (MaxEnd[I] > 0) then
                begin
                    HitEnd := MaxEnd[I];
                    J := I + 1;
                    while J < HitEnd do
                    begin
                        if MaxEnd[J] > HitEnd then
                            HitEnd := MaxEnd[J];
                        Inc(J);
                    end;
                    Out := Out + Copy(S, Last, I - Last) + 'REDACTED';
                    Scan := Scan + Copy(S, Kept, I - Kept) + #1;
                    Last := HitEnd;
                    Kept := HitEnd;
                    I := HitEnd;
                end
                else if AnyProtected and (Cover[I] > 0) then
                begin
                    Scan := Scan + Copy(S, Kept, I - Kept) + #1;
                    Kept := Cover[I];
                    I := Cover[I];
                end
                else
                    Inc(I);
            end;
            Out := Out + Copy(S, Last, MaxInt);
            Scan := Scan + Copy(S, Kept, MaxInt);
            Result := Out;
        end;
        MaxEnd := nil;
        Cover := nil;
        { A spelling the forms above miss (three URL layers, URL of double
          JSON, an HTML-escaped quote, ...) still decodes to the secret: then
          none of S is kept. }
        FoldedSecret := ZbFoldText(Secret);
        { Every decoder is linear, so the views cover the whole text, however
          long. }
        if ZbSecretInDecodedViews(Scan, FoldedSecret, Secret, ZB_SECRET_DECODE_LAYERS) then
            Result := ZB_SECRET_OMITTED_MESSAGE;
    end;

    function RedactSecrets(const S: string): string;
    var
        I: Integer;
    begin
        Result := ZbMaskSecret(S, ZbApiKey);
        for I := 0 to High(ZbSecretRedactors) do
            Result := ZbSecretRedactors[I](Result);
    end;

    { Whether a non-ASCII code point is white space, a control character or one
      of the invisible ones (zero-width, bidirectional marks, variation
      selectors, tags, the byte order mark, ...) that a pasted key can carry
      without showing it. }
    function ZbIsHiddenCodePoint(CodePoint: Integer): Boolean;
    begin
        case CodePoint of
            $7F..$A0, $AD, $34F, $600..$605, $61C, $6DD, $70F, $890, $891, $8E2, $115F, $1160, $1680, $17B4, $17B5, $180B..$180F,
            $2000..$200F, $2028..$202F, $205F..$206F, $3000, $3164, $FE00..$FE0F, $FEFF,
            $FFA0, $FFF9..$FFFB, $110BD, $110CD, $13430..$1343F, $13447..$13455,
            $1BCA0..$1BCA3, $1D173..$1D17A, $E0000..$E0FFF:
                Result := True;
        else
            Result := False;
        end;
    end;

    function ZbCheckedApiKey(const ApiKey: String): String;
    const
        REFUSED = 'api_key has surrounding whitespace or control characters';
    var
        I, Len, CodePoint: Integer;
    begin
        { An empty key stays deferred to ZbRequireApiKey, which names the call
          that initializes the SDK. }
        if Trim(ApiKey) = '' then
            Exit(ApiKey);
        if ApiKey <> Trim(ApiKey) then
            raise ZbException.Create(REFUSED);
        I := 1;
        while I <= Length(ApiKey) do
        begin
            if (ApiKey[I] < #32) or (ApiKey[I] = #127) then
                raise ZbException.Create(REFUSED);
            if Ord(ApiKey[I]) >= $80 then
            begin
                { Text that is not valid UTF-8 (a lone surrogate on Delphi),
                  and the Unicode white space and invisible characters that
                  Trim leaves in place, are refused wherever they sit. A
                  plain space in the middle of a key is still sent. }
                ZbNextCharacter(ApiKey, I, CodePoint, Len);
                if (CodePoint < 0) or ZbIsHiddenCodePoint(CodePoint) then
                    raise ZbException.Create(REFUSED);
                Inc(I, Len);
            end
            else
                Inc(I);
        end;
        { A key of nothing but combining marks folds to no text at all, so
          the redaction of a transport error would have nothing to compare. }
        if ZbFoldText(ApiKey) = '' then
            raise ZbException.Create(REFUSED);
        Result := ApiKey;
    end;

    const
        ZB_DIRTY_URL_MESSAGE = ' must not contain a backslash, whitespace, control or invisible characters, invalid UTF-8, or a non-ASCII character in the host';

    { Refuses the characters that let a URL smuggle a header or hide where it
      goes. The URL is not quoted: it may carry a credential. }
    procedure ZbRequireCleanUrl(const Url, Name: String);
    var
        I, Len, CodePoint, HostEnd: Integer;
        Scheme: Integer;
    begin
        Scheme := Pos('://', Url);
        HostEnd := 0;
        if Scheme > 0 then
        begin
            HostEnd := Scheme + 3;
            while (HostEnd <= Length(Url)) and (Url[HostEnd] <> '/') do
                Inc(HostEnd);
        end;
        I := 1;
        while I <= Length(Url) do
        begin
            ZbNextCharacter(Url, I, CodePoint, Len);
            if (CodePoint < 0) or (CodePoint <= 32) or (CodePoint = 92) or
               ZbIsHiddenCodePoint(CodePoint) or ((CodePoint >= 128) and (I < HostEnd)) then
                raise ZbException.Create(Concat(Name, ZB_DIRTY_URL_MESSAGE));
            Inc(I, Len);
        end;
    end;

    procedure ZbRequireHttpsUrl(const Url, Name: String);
    var
        Lower, Scheme, Authority, Host, Port: String;
        Marker, I: Integer;
    begin
        ZbRequireCleanUrl(Url, Name);
        Lower := LowerCase(Url);
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

    procedure ZbRequireValidTimeouts;
    begin
        if ZbConnectTimeoutMs <= 0 then
            raise ZbException.Create('ZbConnectTimeoutMs must be greater than 0');
        if ZbReadTimeoutMs <= 0 then
            raise ZbException.Create('ZbReadTimeoutMs must be greater than 0');
    end;

    function ZbCheckedUrl(const Url: String): String;
    var
        Region: String;
    begin
        ZbRequireValidTimeouts;
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

    { What a request with no mock installed leaves in ZbLastRequestMethod,
      ZbLastRequestBody and ZbLastRequestHeader: nothing, so a mock's request
      does not linger. }
    procedure ZbClearLastRequest;
    begin
        ZbLastRequestMethod := '';
        ZbLastRequestBody := '';
        ZbLastRequestHeader := '';
    end;

    { The body and the header of the last request, which only a mock may keep;
      the method, set once the URL passed its check, stays. }
    procedure ZbClearLastBodyAndHeader;
    begin
        ZbLastRequestBody := '';
        ZbLastRequestHeader := '';
    end;

    const
        { What a body in a content encoding this client does not read says. }
        ZB_ENCODING_MESSAGE = 'The response from ZeroBounce used a content encoding this client does not read.';

    {$IFDEF FPC}
    { A body shorter than its Content-Length was cut off: the connection closed
      part-way through. fphttpclient hands back whatever arrived without
      complaint, and a file download used to return it as the whole file.
      (A read that times out part-way raises in fphttpclient itself.) }
    procedure ZbRequireCompleteBody(Client: TFPHTTPClient; Received: Int64);
    var
        Expected: Int64;
        Encoding: String;
    begin
        ZbLastConnectTimeoutMs := Client.ConnectTimeout;
        ZbLastReadTimeoutMs := Client.IOTimeout;
        { fphttpclient does not decode a Content-Encoding, so a gzip body would
          be taken for text (or saved as the file). Only "identity" is read. }
        Encoding := Trim(Client.GetHeader(Client.ResponseHeaders, 'Content-Encoding'));
        if (Encoding <> '') and (CompareText(Encoding, 'identity') <> 0) then
            raise ZbException.Create(ZB_ENCODING_MESSAGE);
        { A chunked body ends with an empty chunk, after which fphttpclient
          reads no more. Reaching the end of the connection first means it was
          cut short mid-stream. }
        if (CompareText(Trim(Client.GetHeader(Client.ResponseHeaders, 'Transfer-Encoding')), 'chunked') = 0)
            and ZbConnectionClosed then
            raise ZbException.Create(Format(
                'The response from ZeroBounce was cut short: the connection closed before the last chunk (%d bytes received)',
                [Received]));
        Expected := StrToInt64Def(Trim(Client.GetHeader(Client.ResponseHeaders, 'Content-Length')), -1);
        if (Expected < 0) or (Received >= Expected) then
            Exit;
        raise ZbException.Create(Format(
            'The response from ZeroBounce was cut short: %d of %d bytes received', [Received, Expected]));
    end;
    {$ENDIF}

    {$IFNDEF FPC}
    { Delphi's HTTP client may already have inflated a gzip body, so a
      Content-Encoding header says nothing here. What matters is whether the
      body it hands over is still gzip: it starts with the gzip magic bytes
      1F 8B. Text it has turned into text is kept. }
    procedure ZbRequireNotGzip(const Response: IHTTPResponse);
    var
        Magic: array[0..1] of Byte;
        Stream: TStream;
        Gzip: Boolean;
    begin
        Gzip := False;
        Stream := Response.ContentStream;
        if (Stream <> nil) and (Stream.Size >= 2) then
        begin
            Stream.Position := 0;
            Gzip := (Stream.Read(Magic, 2) = 2) and (Magic[0] = $1F) and (Magic[1] = $8B);
            Stream.Position := 0;
        end;
        if Gzip then
            raise ZbException.Create(ZB_ENCODING_MESSAGE);
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
        { Before anything can refuse the request: a URL that is not https
          must not leave the body and header of an earlier request behind. }
        ZbClearLastRequest;
        ZbRequireApiKey;
        url := ZbCheckedUrl(url);
        ZbLastRequestMethod := 'GET';
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
              ZbRequireNotGzip(Response);
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
        { Before anything can refuse the request: a URL that is not https
          must not leave the body and header of an earlier request behind. }
        ZbClearLastRequest;
        ZbRequireApiKey;
        url := ZbCheckedUrl(url);
        ZbLastRequestMethod := 'POST';
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
            { The string is UTF-16 here: the body goes out as UTF-8, which is
              what the charset above says. }
            BodyStream := TStringStream.Create(JsonParam, TEncoding.UTF8);
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

              ZbRequireNotGzip(Response);

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
        ZbClearLastRequest;
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
        ZbClearLastRequest;
        url := ZbCheckedUrl(url);
        ZbLastRequestMethod := Method;
        { The body and the header can carry a key: kept only for a test that
          installed a mock, never left over from an earlier one. }
        if ZbResponseMock.StatusCode <> 0 then
        begin
            ZbLastRequestBody := Body;
            if HeaderName <> '' then
                ZbLastRequestHeader := Concat(HeaderName, ': ', HeaderValue)
            else
                ZbLastRequestHeader := '';
        end
        else
            ZbClearLastBodyAndHeader;

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
            { UTF-16 string, UTF-8 body. }
            BodyStream := TStringStream.Create(Body, TEncoding.UTF8);
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
                ZbRequireNotGzip(Response);
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
        { Before anything can refuse the request: a URL that is not https
          must not leave the body and header of an earlier request behind. }
        ZbClearLastRequest;
        ZbRequireApiKey;
        url := ZbCheckedUrl(url);
        ZbLastRequestMethod := 'POST';
        { Kept only for a test that installed a mock (it carries the key). }
        if ZbResponseMock.StatusCode <> 0 then
        begin
            ZbLastRequestBody := FormBody;
            ZbLastRequestHeader := '';
        end
        else
            ZbClearLastBodyAndHeader;
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

              ZbRequireNotGzip(Response);

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
          Line: String;
          At: Integer;
        begin
          Result := TMultiPartFormData.Create(False);
          try
            SIterator := TStringsEnumerator.Create(FormData);
            try
              while (SIterator.MoveNext) do
              begin
                { The name is what comes before the first separator and the
                  value is the rest of the line, further '=' included: a
                  return_url of https://cb.example/x?a=1&b=2 is sent whole. A
                  line with no separator is a name with an empty value. }
                Line := SIterator.GetCurrent;
                At := Pos(FormData.NameValueSeparator, Line);
                if At > 0 then
                  Result.AddField(Copy(Line, 1, At - 1), Copy(Line, At + 1, MaxInt))
                else
                  Result.AddField(Line, '');
              end;
            finally
              SIterator.Free;
            end;
          except
            Result.Free;
            raise;
          end;
        end;
        {$ENDIF}
    begin
        { Before anything can refuse the request: a URL that is not https
          must not leave the body and header of an earlier request behind. }
        ZbClearLastRequest;
        ZbRequireApiKey;
        url := ZbCheckedUrl(url);
        ZbLastRequestMethod := 'POST';
        if ZbResponseMock.StatusCode <> 0 then
        begin
            ZbResponseMock.UrlCalled := url;
            Result := ZbResponseMock;
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
            { UTF-16 string, UTF-8 file. }
            FileStream := TStringStream.Create(FileContent, TEncoding.UTF8);
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

              ZbRequireNotGzip(Response);

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
    InitCriticalSection(ZbTlsVerifierLock);
    {$ENDIF}

finalization
    ZbWipeString(ZbApiKey);
{$IFDEF FPC}
    FreeAndNil(ZbTlsVerifier);
    DoneCriticalSection(ZbTlsVerifierLock);
{$ENDIF}

end.

