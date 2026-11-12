unit TestOffline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ZbUtility, ZbGeneric, ZbValidation, ZbBulk, ZbPriority3, ZbStructures, ZbWarmup;

type
  TTestOffline = class(TTestCase)
  published
    procedure TestEndpointsAndOptions;
    procedure TestRejectsBeforeRequest;
    procedure TestErrorBodies;
    procedure TestBatchAccountAndPartial;
    procedure TestConcurrentKeys;
    procedure TestRedirectsAreNotFollowed;
    procedure TestReadTimeoutIsApplied;
    procedure TestConnectTimeoutIsApplied;
    procedure TestADownloadCutShortIsAnError;
    procedure TestAChunkedBodyCutShortIsAnError;
    procedure TestAnHtmlOrEmptyAnswerInPlaceOfAFileIsAnError;
    procedure TestTheDefaultTimeoutsReachEveryTransport;
    procedure TestEveryRequestFunctionRefusesPlainHttp;
    procedure TestBaseUrlsWithATrailingSlash;
    procedure TestAJsonAnswerCutShortIsAnErrorOnEveryTransport;
    procedure TestARefusedUploadCarriesNoStatus;
    procedure TestAWarmupTransportErrorIsRedacted;
    procedure TestEveryPublicCallRaisesZbExceptionWhenRefused;
    procedure TestAProtocolErrorIsAZbExceptionWithoutTheKey;
    procedure TestEveryPublicRequestFunctionIsInTheSweep;
    procedure TestNoErrorCarriesTheKey;
    procedure TestAnEmptyBodyIsASuccessOnlyForAnInboxPlacementDelete;
    procedure TestTheContentTypeAloneMakesA200AnError;
    procedure TestANonJson200IsAnErrorOnEveryJsonCall;
    procedure TestEveryJsonCallTakesOnlyStrictJson;
    procedure TestRedactionIsClosedUnderEncoding;
    procedure TestABodyStillHoldingTheKeyAfterMaskingIsDropped;
  end;

implementation

uses
  Sockets, SyncObjs;

type
  TRecorded = record
    Method, Host, Path, Query, Body, ApiKey: String;
  end;

  TStubServer = class(TThread)
  public
    Port: Integer;
    ListenSock: LongInt;
    constructor Create;
    procedure Execute; override;
  end;

const
  SLOW_REPLY_MS = 1500;

var
  Stub: TStubServer;
  Gate: TCriticalSection;
  Recorded: array of TRecorded;

procedure Remember(const Item: TRecorded);
begin
  Gate.Enter;
  try
    SetLength(Recorded, Length(Recorded) + 1);
    Recorded[High(Recorded)] := Item;
  finally
    Gate.Leave;
  end;
end;

function UrlDecode(const Value: String): String;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(Value) do
  begin
    if (Value[I] = '%') and (I + 2 <= Length(Value)) then
    begin
      Result := Result + Chr(StrToInt('$' + Copy(Value, I + 1, 2)));
      Inc(I, 3);
    end
    else if Value[I] = '+' then
    begin
      Result := Result + ' ';
      Inc(I);
    end
    else
    begin
      Result := Result + Value[I];
      Inc(I);
    end;
  end;
end;

function QueryValue(const Query, Name: String): String;
var
  Parts: TStringArray;
  Part: String;
  Eq: Integer;
begin
  Result := '';
  Parts := Query.Split(['&']);
  for Part in Parts do
  begin
    Eq := Pos('=', Part);
    if (Eq > 0) and (Copy(Part, 1, Eq - 1) = Name) then
      Exit(UrlDecode(Copy(Part, Eq + 1, MaxInt)));
  end;
end;

function HeaderValue(const Headers, Name: String): String;
var
  Lines: TStringArray;
  Line, LowerName: String;
begin
  Result := '';
  LowerName := LowerCase(Name) + ':';
  Lines := Headers.Split([#10]);
  for Line in Lines do
  begin
    if Pos(LowerName, LowerCase(Trim(Line))) = 1 then
      Exit(Trim(Copy(Trim(Line), Length(Name) + 2, MaxInt)));
  end;
end;

function HtmlPage: String;
var
  I: Integer;
begin
  Result := '<!DOCTYPE html><html><head><title>Access denied | zerobounce.net | Zero Bounce</title></head><body>';
  for I := 1 to 20000 do
    Result := Result + '<p>blocked</p>';
  Result := Result + '</body></html>';
end;

function AnswerFor(const Item: TRecorded): String;
var
  Code, ContentType, Body: String;
  Length_: Integer;
begin
  Code := '200';
  ContentType := 'application/json';
  Body := '{"success":true,"message":"ok","file_id":"' + Item.ApiKey + '","file_name":"f.csv","email":"' +
    Item.ApiKey + '","domain":"' + Item.ApiKey + '","Credits":"7","total":1,"found":true,"active_in_days":"3","score":10,' +
    '"address":"' + Item.ApiKey + '","status":"valid","processed_at":"2018-01-01 00:00:00.000","other_domain_formats":[],' +
    '"start_date":"1/1/2018","end_date":"1/31/2018","upload_date":"2018-01-01T00:00:00","complete_percentage":"100%","file_status":"Complete"}';
  if Pos('redirect-', Item.ApiKey) = 1 then
  begin
    { Same host, so a client that followed it would be seen here. }
    Result := 'HTTP/1.1 302 Found' + #13#10 +
      'Location: http://127.0.0.1:' + IntToStr(Stub.Port) + '/followed?api_key=' + Item.ApiKey + #13#10 +
      'Content-Length: 0' + #13#10 +
      'Connection: close' + #13#10 + #13#10;
    Exit;
  end;
  if Pos('html-403-', Item.ApiKey) = 1 then
  begin
    Code := '403';
    ContentType := 'text/html; charset=UTF-8';
    Body := HtmlPage;
  end
  else if Pos('html-200-', Item.ApiKey) = 1 then
  begin
    ContentType := 'text/html; charset=UTF-8';
    Body := HtmlPage;
  end
  else if Pos('empty-', Item.ApiKey) = 1 then
    Body := ''
  else if Pos('malformed-', Item.ApiKey) = 1 then
    Body := '<<<not-json>>>' + StringOfChar('x', 5000)
  else if Pos('cf-json-', Item.ApiKey) = 1 then
  begin
    Code := '403';
    Body := '{"type":"https://developers.cloudflare.com/support/troubleshooting/http-status-codes/cloudflare-1xxx-errors/error-1020/",' +
      '"title":"Error 1020: Access denied","status":403,"error_code":1020,"cloudflare_error":true}';
  end
  else if Pos('double-error-', Item.ApiKey) = 1 then
  begin
    Code := '400';
    ContentType := 'text/plain';
    Body := 'Error: Error: api_key is invalid';
  end
  else if (Pos('validatebatch', Item.Path) > 0) and (Pos('batch-account-', Item.ApiKey) = 1) then
    Body := '{"email_batch":[],"errors":[{"email_address":"all","error":"Invalid API Key or your account ran out of credits"}]}'
  else if (Pos('validatebatch', Item.Path) > 0) and (Pos('batch-partial-', Item.ApiKey) = 1) then
    Body := '{"email_batch":[{"address":"good@example.com","status":"valid","processed_at":"2018-01-01 00:00:00.000"}],' +
      '"errors":[{"email_address":"bad@","error":"Invalid email address"}]}'
  else if Pos('getfile', Item.Path) > 0 then
  begin
    ContentType := 'application/octet-stream';
    Body := 'email' + LineEnding + Item.ApiKey + '@example.com' + LineEnding;
  end
  else if Pos('validatebatch', Item.Path) > 0 then
    Body := '{"email_batch":[{"address":"' + Item.ApiKey + '","status":"valid","processed_at":"2018-01-01 00:00:00.000"}],"errors":[]}'
  else if Pos('/validate', Item.Path) > 0 then
    Body := '{"address":"' + Item.ApiKey + '","status":"valid","processed_at":"2018-01-01 00:00:00.000"}'
  else if Pos('getcredits', Item.Path) > 0 then
    Body := '{"Credits":"7"}';
  { 'cutjson-': the whole, parsable answer, under a Content-Length 500 bytes
    longer, then the end of the connection. }
  Length_ := Length(Body);
  if Pos('cutjson-', Item.ApiKey) = 1 then
    Length_ := Length_ + 500;
  { 'badstatus-': a status line whose code is the key, which fphttpclient
    quotes in its own exception. }
  if Pos('badstatus-', Item.ApiKey) = 1 then
    Code := Item.ApiKey;
  Result := 'HTTP/1.1 ' + Code + ' OK' + #13#10 +
    'Content-Type: ' + ContentType + #13#10 +
    'Content-Length: ' + IntToStr(Length_) + #13#10 +
    'Connection: close' + #13#10 + #13#10 + Body;
end;

function ReadSome(Sock: LongInt): String;
var
  Buf: array[0..8191] of Byte;
  N: Integer;
begin
  N := fpRecv(Sock, @Buf, SizeOf(Buf), 0);
  if N <= 0 then
    Exit('');
  SetLength(Result, N);
  Move(Buf[0], Result[1], N);
end;

procedure HandleClient(Sock: LongInt);
var
  Raw, Headers, Body, Line, Target, Query: String;
  HeaderEnd, ContentLength, Space1, Space2, Q: Integer;
  Item: TRecorded;
  Reply: String;
begin
  Raw := '';
  repeat
    Raw := Raw + ReadSome(Sock);
    HeaderEnd := Pos(#13#10#13#10, Raw);
  until (HeaderEnd > 0) or (Raw = '') or (Length(Raw) > 2000000);
  if HeaderEnd = 0 then
  begin
    CloseSocket(Sock);
    Exit;
  end;
  Headers := Copy(Raw, 1, HeaderEnd - 1);
  Body := Copy(Raw, HeaderEnd + 4, MaxInt);
  ContentLength := StrToIntDef(HeaderValue(Headers, 'Content-Length'), 0);
  while Length(Body) < ContentLength do
    Body := Body + ReadSome(Sock);
  Line := Copy(Headers, 1, Pos(#13#10, Headers + #13#10) - 1);
  Space1 := Pos(' ', Line);
  Space2 := Pos(' ', Copy(Line, Space1 + 1, MaxInt));
  Target := Copy(Line, Space1 + 1, Space2 - 1);
  Q := Pos('?', Target);
  if Q = 0 then
  begin
    Item.Path := Target;
    Query := '';
  end
  else
  begin
    Item.Path := Copy(Target, 1, Q - 1);
    Query := Copy(Target, Q + 1, MaxInt);
  end;
  Item.Method := Copy(Line, 1, Space1 - 1);
  Item.Host := HeaderValue(Headers, 'Host');
  Item.Query := Query;
  Item.Body := Body;
  Item.ApiKey := QueryValue(Query, 'api_key');
  if Item.ApiKey = '' then
    Item.ApiKey := QueryValue(Body, 'api_key');
  if Item.ApiKey = '' then
  begin
    Q := Pos('"api_key"', Body);
    if Q > 0 then
    begin
      Q := Q + 9;
      while (Q <= Length(Body)) and (Body[Q] in [' ', ':', #9]) do
        Inc(Q);
      if (Q <= Length(Body)) and (Body[Q] = '"') then
      begin
        Inc(Q);
        Item.ApiKey := Copy(Body, Q, Pos('"', Copy(Body, Q, MaxInt)) - 1);
      end;
    end;
  end;
  { The Warmup API and the Inbox Placement Tester carry the key in a header. }
  if Item.ApiKey = '' then
    Item.ApiKey := HeaderValue(Headers, 'API-Key');
  if (Item.ApiKey = '') and (Pos('Bearer ', HeaderValue(Headers, 'Authorization')) = 1) then
    Item.ApiKey := Copy(HeaderValue(Headers, 'Authorization'), 8, MaxInt);
  if (Item.ApiKey = '') and (Pos('name="api_key"', Body) > 0) then
  begin
    Q := Pos('name="api_key"', Body);
    Q := Pos(#13#10#13#10, Copy(Body, Q, MaxInt));
    if Q > 0 then
    begin
      Item.ApiKey := Copy(Body, Pos('name="api_key"', Body) + Q + 3, 200);
      Item.ApiKey := Trim(Copy(Item.ApiKey, 1, Pos(#13#10, Item.ApiKey + #13#10) - 1));
    end;
  end;
  Sleep(Random(21));
  Remember(Item);
  if Pos('slow-', Item.ApiKey) = 1 then
  begin
    { Longer than the read timeout the test sets, then hang up without an
      answer: a client with no read timeout waits the whole time. }
    Sleep(SLOW_REPLY_MS);
    CloseSocket(Sock);
    Exit;
  end;
  if (Pos('truncated-', Item.ApiKey) = 1) or (Pos('truncstall-', Item.ApiKey) = 1) then
  begin
    { A 200 header promising 1000 bytes, 26 of them, then the end of the
      connection - or, for 'truncstall-', silence past the read timeout. }
    Reply := 'HTTP/1.1 200 OK' + #13#10 + 'Content-Type: text/csv' + #13#10 +
      'Content-Length: 1000' + #13#10 + 'Connection: close' + #13#10 + #13#10 +
      'email' + #10 + 'partial@example.com' + #10;
    fpSend(Sock, PChar(Reply), Length(Reply), 0);
    if Pos('truncstall-', Item.ApiKey) = 1 then
      Sleep(SLOW_REPLY_MS);
    CloseSocket(Sock);
    Exit;
  end;
  if (Pos('chunkcut-', Item.ApiKey) = 1) or (Pos('chunkend-', Item.ApiKey) = 1) or
     (Pos('chunkok-', Item.ApiKey) = 1) then
  begin
    { A chunked 200: 'chunkcut-' closes in the middle of a chunk that
      promised 64 bytes, 'chunkend-' after a whole chunk but before the last
      (empty) one, 'chunkok-' sends the whole body. }
    Reply := 'HTTP/1.1 200 OK' + #13#10 + 'Content-Type: text/csv' + #13#10 +
      'Transfer-Encoding: chunked' + #13#10 + 'Connection: close' + #13#10 + #13#10;
    if Pos('chunkcut-', Item.ApiKey) = 1 then
      Reply := Reply + '40' + #13#10 + 'email' + #10 + 'partial@example.com' + #10
    else
      Reply := Reply + '1a' + #13#10 + 'email' + #10 + 'partial@example.com' + #10 + #13#10;
    if Pos('chunkok-', Item.ApiKey) = 1 then
      Reply := Reply + '0' + #13#10 + #13#10;
    fpSend(Sock, PChar(Reply), Length(Reply), 0);
    CloseSocket(Sock);
    Exit;
  end;
  Reply := AnswerFor(Item);
  fpSend(Sock, PChar(Reply), Length(Reply), 0);
  CloseSocket(Sock);
end;

constructor TStubServer.Create;
var
  Addr: sockaddr_in;
  NameLen: TSockLen;
  Yes: LongInt;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  ListenSock := fpSocket(AF_INET, SOCK_STREAM, 0);
  Yes := 1;
  fpSetSockOpt(ListenSock, SOL_SOCKET, SO_REUSEADDR, @Yes, SizeOf(Yes));
  Addr := Default(sockaddr_in);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(0);
  Addr.sin_addr.s_addr := htonl($7F000001);
  if fpBind(ListenSock, @Addr, SizeOf(Addr)) <> 0 then
    raise Exception.Create('bind failed');
  fpListen(ListenSock, 32);
  NameLen := SizeOf(Addr);
  fpGetSockName(ListenSock, @Addr, @NameLen);
  Port := NToHS(Addr.sin_port);
end;

procedure TStubServer.Execute;
var
  Client: LongInt;
  Addr: sockaddr_in;
  NameLen: TSockLen;
begin
  while not Terminated do
  begin
    NameLen := SizeOf(Addr);
    Client := fpAccept(ListenSock, @Addr, @NameLen);
    if (Client >= 0) and not Terminated then
      HandleClient(Client)
    else if Client >= 0 then
      CloseSocket(Client);
  end;
  CloseSocket(ListenSock);
end;

procedure WakeStubAccept(APort: Integer);
{ Closing the listening socket does NOT reliably wake a thread blocked in
  accept() on Linux: POSIX leaves the behaviour undefined and there the call
  stays blocked until a connection actually arrives. That is why this suite
  finished on a macOS host but hung forever in Docker and in CI, where the
  job ran to GitHub's 6 hour limit and was cancelled. One throwaway loopback
  connection makes accept() return so the loop can observe Terminated. }
var
  Sock: LongInt;
  Addr: sockaddr_in;
begin
  if APort <= 0 then
    Exit;
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sock < 0 then
    Exit;
  Addr := Default(sockaddr_in);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Addr.sin_addr.s_addr := htonl($7F000001);
  fpConnect(Sock, @Addr, SizeOf(Addr));
  CloseSocket(Sock);
end;

procedure ClearRecorded;
begin
  Gate.Enter;
  try
    SetLength(Recorded, 0);
  finally
    Gate.Leave;
  end;
end;

function OnlyRequest: TRecorded;
begin
  Gate.Enter;
  try
    if Length(Recorded) <> 1 then
      raise Exception.CreateFmt('expected 1 request, got %d', [Length(Recorded)]);
    Result := Recorded[0];
  finally
    Gate.Leave;
  end;
end;

procedure EnsureStub;
begin
  if Stub <> nil then Exit;
  Gate := TCriticalSection.Create;
  Stub := TStubServer.Create;
  Stub.Start;
  Sleep(20);
end;

procedure UseStub(const Key: String);
begin
  EnsureStub;
  ZBInitialize(Key);
  ZbApiBaseURL := Format('http://127.0.0.1:%d/v2', [Stub.Port]);
  BULK_URI := ZbApiBaseURL;
  ZbBulkRootURL := Format('http://127.0.0.1:%d', [Stub.Port]);
  { The list evaluator and the Inbox Placement Tester are separate base URLs,
    not paths built from BULK_URI, so they have to be pointed at the stub too -
    otherwise these tests reach the live API. }
  ZbListEvaluatorURL := Format('http://127.0.0.1:%d/v2/listevaluator', [Stub.Port]);
  ZbInboxPlacementURL := Format('http://127.0.0.1:%d/api/ipt', [Stub.Port]);
  ZbWarmupBaseURL := Format('http://127.0.0.1:%d', [Stub.Port]);
  ZbWarmupInitialize(Key);
  ZbResponseMock := cDefaultMock;
end;

procedure UseDefaultUrls;
begin
  ZBInitialize('mock_api_key');
  BULK_URI := 'https://bulkapi.zerobounce.net/v2';
  ZbBulkRootURL := 'https://bulkapi.zerobounce.net';
  ZbListEvaluatorURL := 'https://bulkapi.zerobounce.net/v2/listevaluator';
  ZbInboxPlacementURL := 'https://bulkapi.zerobounce.net/api/ipt';
  ZbWarmupBaseURL := 'https://warmup-api.zerobounce.net';
  ZbWarmupInitialize('mock_warmup_key');
  ZbResponseMock := cDefaultMock;
end;

function CountRecorded(const Prefix: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  Gate.Enter;
  try
    for I := 0 to High(Recorded) do
      if Pos(Prefix, Recorded[I].ApiKey) = 1 then
        Inc(Result);
  finally
    Gate.Leave;
  end;
end;

type
  { One call per transport: GET, JSON POST, form POST, multipart upload, and
    the method-and-header request the Warmup API and the Inbox Placement
    Tester use. }
  TTransport = (tpGet, tpJsonPost, tpFormPost, tpMultipart, tpWithHeader);

const
  TRANSPORT_NAMES: array[TTransport] of String = (
    'GET', 'JSON POST', 'form POST', 'multipart upload', 'request with header'
  );

procedure CallTransport(Transport: TTransport);
var
  Params: TZbBulkParams;
  Options: TZbValidateOptions;
begin
  case Transport of
    tpGet: ZbGetCredits;
    tpJsonPost: ZbBatchValidateEmails(['a@example.com']);
    tpFormPost:
      begin
        Options := ZbDefaultValidateOptions;
        Options.UsePost := 1;
        ZbValidateEmail('a@example.com', '', Options);
      end;
    tpMultipart:
      begin
        FillChar(Params, SizeOf(Params), 0);
        Params.EmailAddressColumn := 1;
        ZbBulkValidationFileSubmit('email' + LineEnding + 'a@example.com' + LineEnding, Params);
      end;
    tpWithHeader: ZbWarmupGetSeeds;
  end;
end;

procedure ExpectShort(const Msg: String);
begin
  if Length(Msg) >= 500 then
    raise Exception.Create('error is too long: ' + Copy(Msg, 1, 80));
  if (Pos('<html', LowerCase(Msg)) > 0) or (Pos('<!doctype', LowerCase(Msg)) > 0) or
     (Pos('<p>blocked</p>', Msg) > 0) or (Pos('Error: Error:', Msg) > 0) then
    raise Exception.Create(Msg);
end;

procedure TTestOffline.TestEndpointsAndOptions;
var
  Options: TZbValidateOptions;
  Batch: array[0..1] of TZbEmailAndIp;
  One: array[0..0] of TZbEmailAndIp;
  FileOpts: TZbBulkParams;
  GetOpts: TZbGetFileOptions;
  Finder: TZbEmailFinderSendFileOptions;
  DomainOpts: TZbDomainSearchSendFileOptions;
  S3: TZbValidateS3FileOptions;
  Score: TZbScoreS3FileOptions;
  Request: TRecorded;
  StartDate, EndDate: TDateTime;
begin
  EnsureStub;
  ClearRecorded;
  UseStub('credits-key');
  AssertEquals(7, ZbGetCredits);
  Request := OnlyRequest;
  AssertEquals('GET', Request.Method);
  AssertEquals('/v2/getcredits', Request.Path);
  AssertEquals('credits-key', Request.ApiKey);
  AssertTrue(Pos('127.0.0.1', Request.Host) > 0);

  ClearRecorded;
  UseStub('validate-key');
  Options := ZbDefaultValidateOptions;
  Options.ActivityData := 0;
  Options.VerifyPlus := 1;
  AssertEquals('validate-key', ZbValidateEmail('person@example.com', '', Options).Address);
  Request := OnlyRequest;
  AssertEquals('/v2/validate', Request.Path);
  AssertEquals('false', QueryValue(Request.Query, 'activity_data'));
  AssertEquals('true', QueryValue(Request.Query, 'verify_plus'));
  AssertEquals('', QueryValue(Request.Query, 'timeout'));

  ClearRecorded;
  Options := ZbDefaultValidateOptions;
  Options.UsePost := 1;
  Options.Timeout := 15;
  ZbValidateEmail('person@example.com', '203.0.113.5', Options);
  Request := OnlyRequest;
  AssertEquals('POST', Request.Method);
  AssertTrue(Pos('timeout=15', Request.Body) > 0);
  AssertTrue(Pos('activity_data', Request.Body) = 0);

  ClearRecorded;
  UseStub('batch-key');
  Batch[0].Email := 'a@example.com';
  Batch[0].Ip := '';
  Batch[1].Email := 'b@example.com';
  Batch[1].Ip := '203.0.113.9';
  AssertEquals('batch-key', ZbBatchValidateEmails(Batch).EmailBatch[0].Address);
  Request := OnlyRequest;
  AssertEquals('/v2/validatebatch', Request.Path);
  AssertTrue(Pos('"ip_address": null', Request.Body) > 0);
  AssertTrue(Pos('203.0.113.9', Request.Body) > 0);
  AssertTrue(Pos('timeout', Request.Body) = 0);

  ClearRecorded;
  One[0].Email := 'a@example.com';
  One[0].Ip := '0.0.0.0';
  Options := ZbDefaultValidateOptions;
  Options.Timeout := 30;
  Options.ActivityData := 0;
  Options.VerifyPlus := 1;
  ZbBatchValidateEmails(One, Options);
  Request := OnlyRequest;
  AssertTrue(Pos('30', Request.Body) > 0);
  AssertTrue(Pos('false', Request.Body) > 0);
  AssertTrue(Pos('0.0.0.0', Request.Body) > 0);

  ClearRecorded;
  UseStub('file-key');
  FillChar(FileOpts, SizeOf(FileOpts), 0);
  FileOpts.EmailAddressColumn := 0;
  FileOpts.HasHeaderRow := True;
  FileOpts.RemoveDuplicate := False;
  FileOpts.AllowPhase2Specified := True;
  FileOpts.AllowPhase2 := False;
  ZbBulkValidationFileSubmit('email,name', FileOpts);
  Request := OnlyRequest;
  AssertEquals('/v2/sendfile', Request.Path);
  AssertTrue(Pos('email_address_column', Request.Body) > 0);
  AssertTrue(Pos('has_header_row', Request.Body) > 0);
  AssertTrue(Pos('false', Request.Body) > 0);

  ClearRecorded;
  StartDate := EncodeDate(2018, 1, 1);
  EndDate := EncodeDate(2018, 1, 31);
  ZbGetApiUsage(StartDate, EndDate);
  AssertEquals('/v2/getapiusage', OnlyRequest.Path);
  ClearRecorded;
  ZbActivityData('person@example.com');
  AssertEquals('/v2/activity', OnlyRequest.Path);
  ClearRecorded;
  AssertEquals('file-key', ZbScoreEmail('person@example.com').Email);
  AssertEquals('GET', OnlyRequest.Method);
  AssertEquals('/v2/scoring', OnlyRequest.Path);
  ClearRecorded;
  ZbScoreEmailPost('person@example.com');
  AssertEquals('POST', OnlyRequest.Method);

  ClearRecorded;
  ZbFindEmailByDomain('example.com', 'Ada', 'M', 'Lovelace');
  Request := OnlyRequest;
  AssertEquals('/v2/guessformat', Request.Path);
  AssertEquals('GET', Request.Method);
  AssertEquals('example.com', QueryValue(Request.Query, 'domain'));
  AssertEquals('', QueryValue(Request.Query, 'company_name'));
  AssertEquals('Ada', QueryValue(Request.Query, 'first_name'));
  ClearRecorded;
  { The two-argument overload passes '' for the names it does not take, and a
    blank first_name= is a blank field rather than an omitted one. }
  ZbFindEmailByDomain('example.com', '');
  Request := OnlyRequest;
  AssertEquals('/v2/guessformat', Request.Path);
  AssertEquals('example.com', QueryValue(Request.Query, 'domain'));
  AssertTrue(
    'first_name should be absent: ' + Request.Query,
    Pos('first_name', Request.Query) = 0
  );
  ClearRecorded;
  ZbDomainSearchByCompanyName('ZeroBounce');
  AssertEquals('ZeroBounce', QueryValue(OnlyRequest.Query, 'company_name'));

  ClearRecorded;
  FillChar(Finder, SizeOf(Finder), 0);
  Finder.HasHeaderRowSpecified := True;
  Finder.HasHeaderRow := False;
  { One of the two name columns is required. }
  Finder.FirstNameColumn := 2;
  ZbSendEmailFinderFile('domain', 1, @Finder);
  Request := OnlyRequest;
  AssertEquals('POST', Request.Method);
  AssertEquals('/email-finder/sendfile', Request.Path);
  AssertTrue(Pos('first_name_column', Request.Body) > 0);
  AssertTrue(Pos('has_header_row', Request.Body) > 0);
  AssertTrue(Pos('false', Request.Body) > 0);
  ClearRecorded;
  ZbEmailFinderFileStatus('fid');
  AssertEquals('/email-finder/filestatus', OnlyRequest.Path);
  ClearRecorded;
  ZbEmailFinderResultFetch('fid');
  AssertEquals('/email-finder/getfile', OnlyRequest.Path);
  ClearRecorded;
  ZbEmailFinderResultDelete('fid');
  AssertEquals('/email-finder/deletefile', OnlyRequest.Path);
  ClearRecorded;
  FillChar(DomainOpts, SizeOf(DomainOpts), 0);
  DomainOpts.HasHeaderRowSpecified := True;
  DomainOpts.HasHeaderRow := False;
  ZbSendDomainSearchFile('company', 2, @DomainOpts);
  AssertEquals('/domain-search/sendfile', OnlyRequest.Path);
  ClearRecorded;
  ZbSendListEvaluatorFile('emails', 1);
  AssertEquals('/v2/listevaluator/', OnlyRequest.Path);
  ClearRecorded;
  AssertTrue(Pos('file-key', ZbGetListEvaluatorResult('fid')) > 0);
  ClearRecorded;
  ZbAddFilter('exclude', 'domain', 'example.com');
  AssertEquals('/v2/filters/add', OnlyRequest.Path);
  ClearRecorded;
  ZbListFilters(False);
  AssertEquals('GET', OnlyRequest.Method);
  ClearRecorded;
  ZbListFilters(True);
  AssertEquals('POST', OnlyRequest.Method);
  ClearRecorded;
  FillChar(S3, SizeOf(S3), 0);
  S3.FileName := 'inbox.csv';
  S3.ReturnUrl := 'https://example.com/done';
  S3.EmailAddressColumnSpecified := True;
  S3.EmailAddressColumn := 0;
  S3.HasHeaderRowSpecified := True;
  S3.HasHeaderRow := False;
  ZbValidateS3File(S3);
  Request := OnlyRequest;
  AssertEquals('/validateS3file', Request.Path);
  AssertTrue(Pos('email_address_column', Request.Body) > 0);
  AssertTrue(Pos('=0', Request.Body) > 0);
  ClearRecorded;
  FillChar(Score, SizeOf(Score), 0);
  Score.FileName := 'inbox.csv';
  Score.ReturnUrl := 'https://example.com/done';
  ZbScoreS3File(Score);
  AssertEquals('/scoreS3file', OnlyRequest.Path);
  ClearRecorded;
  ZbExportS3File('fid', 'bucket-out');
  AssertEquals('/exportS3file', OnlyRequest.Path);
  ClearRecorded;
  ZbListS3Files('', '');
  Request := OnlyRequest;
  AssertEquals('/listS3files', Request.Path);
  AssertEquals('', QueryValue(Request.Query, 'bucket'));
  FillChar(GetOpts, SizeOf(GetOpts), 0);
  GetOpts.DownloadType := ZB_DOWNLOAD_TYPE_PHASE_2;
  GetOpts.ActivityDataSpecified := True;
  GetOpts.ActivityData := True;
  ClearRecorded;
  ZbBulkValidationResultFetch('fid', @GetOpts);
  Request := OnlyRequest;
  AssertEquals('/v2/getfile', Request.Path);
  AssertEquals('phase_2', QueryValue(Request.Query, 'download_type'));
  AssertEquals('true', QueryValue(Request.Query, 'activity_data'));
end;

procedure TTestOffline.TestRejectsBeforeRequest;
var
  S3: TZbValidateS3FileOptions;
begin
  EnsureStub;
  ClearRecorded;
  ZBInitialize('');
  ZbApiBaseURL := Format('http://127.0.0.1:%d/v2', [Stub.Port]);
  try
    ZbGetCredits;
    Fail('missing key was sent');
  except
    on E: ZbException do
      AssertTrue(Pos('ZBInitialize', E.Message) > 0);
  end;
  try
    ZbFindEmailByDomain('example.com', 'Ada');
    Fail('missing key was sent');
  except
    on E: ZbException do ;
  end;
  FillChar(S3, SizeOf(S3), 0);
  S3.FileName := 'inbox.csv';
  { Both are required, and this case is about the key, not the arguments. }
  S3.ReturnUrl := 'https://example.com/done';
  try
    ZbValidateS3File(S3);
    Fail('missing key was sent');
  except
    on E: ZbException do ;
  end;
  AssertEquals(0, Length(Recorded));

  UseStub('keyed');
  try
    ZbValidateEmail('a@example.com', '', ZbDefaultValidateOptions);
  except
    on E: Exception do
      Fail(E.Message);
  end;
end;

procedure TTestOffline.TestErrorBodies;
begin
  EnsureStub;
  ClearRecorded;
  UseStub('html-403-user');
  try
    ZbGetCredits;
    Fail('html 403 succeeded');
  except
    on E: ZbException do
    begin
      ExpectShort(E.Message);
      AssertTrue(E.Message, Pos('Access denied', E.Message) > 0);
    end;
  end;

  UseStub('html-200-user');
  try
    ZbValidateEmail('a@example.com', '');
    Fail('html 200 succeeded');
  except
    on E: ZbException do
    begin
      ExpectShort(E.Message);
      AssertTrue(E.Message, Pos('Access denied', E.Message) > 0);
    end;
  end;

  UseStub('html-200-file');
  try
    if ZbBulkValidationResultFetch('fid').HasContent then
      Fail('html was saved as a file');
    Fail('html getfile succeeded');
  except
    on E: ZbException do
    begin
      ExpectShort(E.Message);
      AssertTrue(E.Message, Pos('Access denied', E.Message) > 0);
    end;
  end;

  UseStub('cf-json-user');
  try
    ZbGetCredits;
    Fail('cloudflare succeeded');
  except
    on E: ZbException do
    begin
      ExpectShort(E.Message);
      AssertTrue(E.Message, Pos('developers.cloudflare.com', E.Message) = 0);
      AssertTrue(E.Message, Pos('Access denied', E.Message) > 0);
    end;
  end;

  UseStub('empty-user');
  try
    ZbValidateEmail('a@example.com', '');
    Fail('empty succeeded');
  except
    on E: ZbException do
    begin
      ExpectShort(E.Message);
      AssertTrue(E.Message, Pos('Empty response', E.Message) > 0);
    end;
  end;

  UseStub('malformed-user');
  try
    ZbValidateEmail('a@example.com', '');
    Fail('malformed succeeded');
  except
    on E: ZbException do
    begin
      ExpectShort(E.Message);
      AssertTrue(E.Message, Pos('not valid JSON', E.Message) > 0);
      AssertTrue(E.Message, Pos('<<<not-json>>>', E.Message) = 0);
    end;
  end;

  UseStub('double-error-user');
  try
    ZbActivityData('a@example.com');
    Fail('double error succeeded');
  except
    on E: ZbException do
    begin
      ExpectShort(E.Message);
      AssertTrue(E.Message, Pos('api_key is invalid', E.Message) > 0);
      AssertTrue(E.Message, Pos('Error: Error:', E.Message) = 0);
    end;
  end;
end;

procedure TTestOffline.TestBatchAccountAndPartial;
var
  Emails: array[0..1] of TZbEmailAndIp;
  Batch: TZBBatchValidation;
begin
  EnsureStub;
  UseStub('batch-account-user');
  Emails[0].Email := 'a@example.com';
  Emails[0].Ip := '';
  try
    ZbBatchValidateEmails(Emails);
    Fail('account error succeeded');
  except
    on E: ZbException do
    begin
      ExpectShort(E.Message);
      AssertTrue(Pos('Invalid API Key', E.Message) > 0);
      AssertTrue(Pos('ran out of credits', E.Message) > 0);
    end;
  end;

  UseStub('batch-partial-user');
  Emails[0].Email := 'good@example.com';
  Emails[1].Email := 'bad@';
  Batch := ZbBatchValidateEmails(Emails);
  AssertEquals('good@example.com', Batch.EmailBatch[0].Address);
  AssertEquals('bad@', Batch.Errors[0].EmailAddress);
end;

type
  TConc = class(TThread)
  public
    Index: Integer;
    Echo: String;
    procedure Execute; override;
  end;

procedure TConc.Execute;
begin
  UseStub('conc-' + IntToStr(Index));
  Echo := ZbValidateEmail('person@example.com', '').Address;
end;

procedure TTestOffline.TestConcurrentKeys;
var
  Threads: array[0..11] of TConc;
  I, J, Seen: Integer;
begin
  EnsureStub;
  ClearRecorded;
  for I := 0 to High(Threads) do
  begin
    Threads[I] := TConc.Create(True);
    Threads[I].FreeOnTerminate := False;
    Threads[I].Index := I;
    Threads[I].Start;
  end;
  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor;
    AssertEquals('conc-' + IntToStr(I), Threads[I].Echo);
    Threads[I].Free;
  end;
  AssertEquals(Length(Threads), Length(Recorded));
  for I := 0 to High(Threads) do
  begin
    Seen := 0;
    for J := 0 to High(Recorded) do
      if Recorded[J].ApiKey = 'conc-' + IntToStr(I) then
        Inc(Seen);
    AssertEquals(1, Seen);
  end;
end;

procedure TTestOffline.TestRedirectsAreNotFollowed;
var
  Transport: TTransport;
  Key: String;
begin
  { A redirect could carry the key to another host. Every transport must
    stop at the 302, and report it as an error with its status. }
  EnsureStub;
  try
    for Transport := Low(TTransport) to High(TTransport) do
    begin
      Key := 'redirect-' + IntToStr(Ord(Transport));
      UseStub(Key);
      ClearRecorded;
      try
        CallTransport(Transport);
        Fail(TRANSPORT_NAMES[Transport] + ': a redirect was returned as a result');
      except
        on E: ZbException do
          AssertEquals(TRANSPORT_NAMES[Transport] + ': ' + E.Message, 302, E.StatusCode);
      end;
      { Only the first request reached the stub; nothing went to /followed. }
      AssertEquals(TRANSPORT_NAMES[Transport] + ' followed the redirect', 1, Length(Recorded));
      AssertTrue(TRANSPORT_NAMES[Transport], Pos('/followed', OnlyRequest.Path) = 0);
    end;
  finally
    UseDefaultUrls;
  end;
end;

procedure TTestOffline.TestReadTimeoutIsApplied;
const
  TEST_READ_TIMEOUT_MS = 200;
var
  Transport: TTransport;
  Started, Elapsed: QWord;
  SavedRead: Integer;
  Raised: Boolean;
  Calls: Integer;
begin
  { The stub holds every 'slow-' request longer than the read timeout and
    then hangs up. A transport that sets no read timeout waits it out. }
  EnsureStub;
  SavedRead := ZbReadTimeoutMs;
  Calls := 0;
  ClearRecorded;
  try
    ZbReadTimeoutMs := TEST_READ_TIMEOUT_MS;
    for Transport := Low(TTransport) to High(TTransport) do
    begin
      UseStub('slow-' + IntToStr(Ord(Transport)));
      Raised := False;
      Started := GetTickCount64;
      try
        CallTransport(Transport);
      except
        on E: Exception do
        begin
          Raised := True;
          AssertTrue(TRANSPORT_NAMES[Transport] + ' leaked the key: ' + E.Message,
            Pos('slow-', E.Message) = 0);
        end;
      end;
      Elapsed := GetTickCount64 - Started;
      Inc(Calls);
      AssertTrue(TRANSPORT_NAMES[Transport] + ': a stalled response should time out', Raised);
      AssertTrue(Format('%s: took %d ms with a %d ms read timeout',
        [TRANSPORT_NAMES[Transport], Elapsed, TEST_READ_TIMEOUT_MS]),
        Elapsed < SLOW_REPLY_MS - 300);
    end;
  finally
    ZbReadTimeoutMs := SavedRead;
    { Let the stub work through the stalled connections, so they do not land
      in a later test's recorded requests. }
    Started := GetTickCount64;
    while (CountRecorded('slow-') < Calls) and (GetTickCount64 - Started < 15000) do
      Sleep(50);
    Sleep(SLOW_REPLY_MS + 200);
    ClearRecorded;
    UseDefaultUrls;
  end;
end;

procedure TTestOffline.TestConnectTimeoutIsApplied;
const
  { fphttpclient counts the connect timeout in whole seconds; its own default
    is 3000 ms, so a transport that does not set ZbConnectTimeoutMs takes 3 s. }
  TEST_CONNECT_TIMEOUT_MS = 1000;
  { TEST-NET-1 (RFC 5737): never routed, so a connection attempt gets no answer. }
  UNANSWERED = 'https://192.0.2.1';
var
  Transport: TTransport;
  Started, Elapsed: QWord;
  SavedConnect, SavedRead: Integer;
  Raised: Boolean;
begin
  SavedConnect := ZbConnectTimeoutMs;
  SavedRead := ZbReadTimeoutMs;
  try
    ZbConnectTimeoutMs := TEST_CONNECT_TIMEOUT_MS;
    ZbReadTimeoutMs := 20000;
    for Transport := Low(TTransport) to High(TTransport) do
    begin
      ZBInitialize('connect-key');
      ZbApiBaseURL := UNANSWERED + '/v2';
      BULK_URI := ZbApiBaseURL;
      ZbBulkRootURL := UNANSWERED;
      ZbWarmupBaseURL := UNANSWERED;
      ZbWarmupInitialize('connect-key');
      ZbResponseMock := cDefaultMock;
      Raised := False;
      Started := GetTickCount64;
      try
        CallTransport(Transport);
      except
        on E: Exception do
        begin
          Raised := True;
          AssertTrue(TRANSPORT_NAMES[Transport] + ' leaked the key: ' + E.Message,
            Pos('connect-key', E.Message) = 0);
        end;
      end;
      Elapsed := GetTickCount64 - Started;
      { With no route at all (a container without a network) the attempt fails
        at once, and the timeout is never reached: nothing to measure. }
      if Elapsed < TEST_CONNECT_TIMEOUT_MS div 2 then
        Ignore(Format('192.0.2.1 is refused at once here (%d ms), so the connect timeout cannot be measured', [Elapsed]));
      AssertTrue(TRANSPORT_NAMES[Transport] + ': an unanswered connection should time out', Raised);
      AssertTrue(Format('%s: took %d ms with a %d ms connect timeout',
        [TRANSPORT_NAMES[Transport], Elapsed, TEST_CONNECT_TIMEOUT_MS]),
        Elapsed < 2500);
    end;
  finally
    ZbConnectTimeoutMs := SavedConnect;
    ZbReadTimeoutMs := SavedRead;
    UseDefaultUrls;
  end;
end;

procedure TTestOffline.TestAChunkedBodyCutShortIsAnError;
type
  TDownload = (dlValidation, dlScoring, dlEmailFinder, dlDomainSearch, dlWarmupSeeds);
const
  DOWNLOAD_NAMES: array[TDownload] of String = (
    'ZbBulkValidationResultFetch', 'ZbAiScoringResultFetch', 'ZbEmailFinderResultFetch',
    'ZbDomainSearchResultFetch', 'ZbWarmupDownloadSeeds'
  );
  CUTS: array[0..1] of String = ('chunkcut-', 'chunkend-');
var
  Download: TDownload;
  Cut: Integer;
  Content, Message: String;
  Raised: Boolean;

  function Run(D: TDownload): String;
  begin
    case D of
      dlValidation: Result := ZbBulkValidationResultFetch('fid').Content;
      dlScoring: Result := ZbAiScoringResultFetch('fid').Content;
      dlEmailFinder: Result := ZbEmailFinderResultFetch('fid').Content;
      dlDomainSearch: Result := ZbDomainSearchResultFetch('fid').Content;
      dlWarmupSeeds: Result := ZbWarmupDownloadSeeds('');
    end;
  end;

begin
  { fphttpclient reads a chunked body until its last, empty chunk - or until
    the connection closes, which it does not report. A body cut there used to
    come back as the whole file. }
  EnsureStub;
  try
    for Download := Low(TDownload) to High(TDownload) do
    begin
      for Cut := Low(CUTS) to High(CUTS) do
      begin
        UseStub(CUTS[Cut] + IntToStr(Ord(Download)));
        Raised := False;
        Message := '';
        try
          Run(Download);
        except
          on E: Exception do
          begin
            Raised := True;
            Message := E.Message;
          end;
        end;
        AssertTrue(DOWNLOAD_NAMES[Download] + ' ' + CUTS[Cut] + ': a chunked body cut short should be an error',
          Raised);
        AssertTrue(DOWNLOAD_NAMES[Download] + ': ' + Message, Pos('cut short', Message) > 0);
        AssertTrue(DOWNLOAD_NAMES[Download] + ' leaked the key: ' + Message, Pos(CUTS[Cut], Message) = 0);
      end;
      { The same body with its last chunk is the whole file. }
      UseStub('chunkok-' + IntToStr(Ord(Download)));
      Content := Run(Download);
      AssertEquals(DOWNLOAD_NAMES[Download] + ': a whole chunked body',
        'email' + #10 + 'partial@example.com' + #10, Content);
    end;
  finally
    UseDefaultUrls;
  end;
end;

procedure TTestOffline.TestAnHtmlOrEmptyAnswerInPlaceOfAFileIsAnError;
type
  TDownload = (dlValidation, dlScoring, dlEmailFinder, dlDomainSearch);
const
  DOWNLOAD_NAMES: array[TDownload] of String = (
    'ZbBulkValidationResultFetch', 'ZbAiScoringResultFetch', 'ZbEmailFinderResultFetch',
    'ZbDomainSearchResultFetch'
  );
  { A proxy page that does not look like HTML, under an HTML content type in
    any case, and an empty body under file content types. }
  BODIES: array[0..3] of String = ('<p>Access denied by proxy</p>', 'Access denied', '', '');
  TYPES: array[0..3] of String = ('text/html', 'TEXT/HTML; charset=UTF-8', 'text/csv',
    'application/octet-stream');
var
  Download: TDownload;
  I: Integer;
  Raised: Boolean;
  Status: Integer;
  Label_: String;
begin
  try
    for I := Low(BODIES) to High(BODIES) do
      for Download := Low(TDownload) to High(TDownload) do
      begin
        UseDefaultUrls;
        ZBMockResponse(200, BODIES[I], TYPES[I]);
        Label_ := Format('%s: %s under %s', [DOWNLOAD_NAMES[Download], QuotedStr(BODIES[I]), TYPES[I]]);
        Raised := False;
        Status := -1;
        try
          case Download of
            dlValidation: ZbBulkValidationResultFetch('fid');
            dlScoring: ZbAiScoringResultFetch('fid');
            dlEmailFinder: ZbEmailFinderResultFetch('fid');
            dlDomainSearch: ZbDomainSearchResultFetch('fid');
          end;
        except
          on E: ZbException do
          begin
            Raised := True;
            Status := E.StatusCode;
          end;
        end;
        AssertTrue(Label_ + ' should be an error', Raised);
        AssertEquals(Label_, 200, Status);
      end;
  finally
    UseDefaultUrls;
  end;
end;

procedure TTestOffline.TestTheDefaultTimeoutsReachEveryTransport;
var
  Transport: TTransport;
  ConnectMs, ReadMs: Integer;
begin
  { Nothing here sets a timeout: what each transport's client ran with is the
    SDK's default, 30 s to connect and 120 s to read. }
  EnsureStub;
  AssertEquals('ZbConnectTimeoutMs default', 30000, ZbConnectTimeoutMs);
  AssertEquals('ZbReadTimeoutMs default', 120000, ZbReadTimeoutMs);
  try
    for Transport := Low(TTransport) to High(TTransport) do
    begin
      UseStub('timeouts-' + IntToStr(Ord(Transport)));
      try
        CallTransport(Transport);
      except
        on E: Exception do
          ;
      end;
      ZbLastRequestTimeouts(ConnectMs, ReadMs);
      AssertEquals(TRANSPORT_NAMES[Transport] + ' connect timeout', 30000, ConnectMs);
      AssertEquals(TRANSPORT_NAMES[Transport] + ' read timeout', 120000, ReadMs);
    end;
  finally
    UseDefaultUrls;
  end;
end;

procedure TTestOffline.TestADownloadCutShortIsAnError;
type
  TDownload = (dlValidation, dlValidationWithOptions, dlScoring, dlScoringWithOptions,
    dlEmailFinder, dlDomainSearch, dlWarmupSeeds);
const
  DOWNLOAD_NAMES: array[TDownload] of String = (
    'ZbBulkValidationResultFetch', 'ZbBulkValidationResultFetch(options)', 'ZbAiScoringResultFetch',
    'ZbAiScoringResultFetch(options)', 'ZbEmailFinderResultFetch', 'ZbDomainSearchResultFetch',
    'ZbWarmupDownloadSeeds'
  );
var
  Download: TDownload;
  Options: TZbGetFileOptions;
  Saved, Message: String;
  Raised: Boolean;
  SavedRead: Integer;
  Started: QWord;

  procedure Run(D: TDownload);
  begin
    case D of
      dlValidation: ZbBulkValidationResultFetch('fid');
      dlValidationWithOptions: ZbBulkValidationResultFetch('fid', @Options);
      dlScoring: ZbAiScoringResultFetch('fid');
      dlScoringWithOptions: ZbAiScoringResultFetch('fid', @Options);
      dlEmailFinder: ZbEmailFinderResultFetch('fid');
      dlDomainSearch: ZbDomainSearchResultFetch('fid');
      dlWarmupSeeds: ZbWarmupDownloadSeeds(Saved);
    end;
  end;

begin
  { The server promises 1000 bytes, sends 26 and closes the connection. Every
    download used to hand back the 26 bytes as the whole file. }
  EnsureStub;
  Options := Default(TZbGetFileOptions);
  Options.DownloadType := 'combined';
  Saved := GetTempDir + 'zb-pascal-cut-short-' + IntToStr(GetProcessID) + '.csv';
  DeleteFile(Saved);
  for Download := Low(TDownload) to High(TDownload) do
  begin
    UseStub('truncated-' + IntToStr(Ord(Download)));
    Raised := False;
    Message := '';
    try
      Run(Download);
    except
      on E: Exception do
      begin
        Raised := True;
        Message := E.Message;
      end;
    end;
    AssertTrue(DOWNLOAD_NAMES[Download] + ': a body cut short should be an error', Raised);
    AssertTrue(DOWNLOAD_NAMES[Download] + ': ' + Message, Pos('cut short', Message) > 0);
    AssertTrue(DOWNLOAD_NAMES[Download] + ' leaked the key: ' + Message, Pos('truncated-', Message) = 0);
  end;
  AssertFalse('ZbWarmupDownloadSeeds saved the partial file', FileExists(Saved));

  { 26 bytes, then silence past the read timeout. }
  SavedRead := ZbReadTimeoutMs;
  try
    ZbReadTimeoutMs := 200;
    UseStub('truncstall-1');
    Raised := False;
    try
      ZbBulkValidationResultFetch('fid');
    except
      on E: Exception do
        Raised := True;
    end;
    AssertTrue('a body that stalls part-way should be an error', Raised);
  finally
    ZbReadTimeoutMs := SavedRead;
    { Let the stub finish the stalled connection before the next test. }
    Started := GetTickCount64;
    while (CountRecorded('truncstall-') < 1) and (GetTickCount64 - Started < 15000) do
      Sleep(50);
    Sleep(SLOW_REPLY_MS + 200);
    ClearRecorded;
    UseDefaultUrls;
  end;
end;

type
  { What a sweep expects of one call: E is what it raised, nil if it returned. }
  TSweepCheck = procedure(const Name: String; E: Exception);

var
  { The functions the last sweep called, by name without the overload label. }
  SweepNames: TStringList;
  { The same, once per overload the sweep called: a label starting with '#'
    calls an overload already listed, with other arguments. }
  SweepOverloads: TStringList;
  { Every call the check did not accept, with why. }
  SweepMissed: String;

{ Calls every public request function once, overloads included, and hands
  each outcome to Check. TestEveryPublicRequestFunctionIsInTheSweep keeps the
  list complete against the units' interfaces. }
procedure SweepEveryPublicCall(Check: TSweepCheck);
var
  Params: TZbBulkParams;
  Finder: TZbEmailFinderSendFileOptions;
  S3: TZbValidateS3FileOptions;
  Score: TZbScoreS3FileOptions;
  Job: TZbBulkFileJobOptions;
  Emails: array[0..0] of TZbEmailAndIp;
  Warmup: TZbWarmupAddSendingAccountOptions;
  Form: TStringList;
  Url: String;

  procedure Expect(const Name: String; E: Exception);
  var
    Base, OverloadLabel: String;
  begin
    Base := Name;
    OverloadLabel := '';
    if Pos('(', Base) > 0 then
    begin
      OverloadLabel := Copy(Base, Pos('(', Base) + 1, MaxInt);
      Base := Copy(Base, 1, Pos('(', Base) - 1);
    end;
    if SweepNames.IndexOf(Base) < 0 then
      SweepNames.Add(Base);
    if Copy(OverloadLabel, 1, 1) <> '#' then
      SweepOverloads.Add(Base);
    Check(Name, E);
  end;

begin
  SweepNames.Clear;
  SweepOverloads.Clear;
  SweepMissed := '';
  FillChar(Params, SizeOf(Params), 0);
  Params.EmailAddressColumn := 1;
  FillChar(Finder, SizeOf(Finder), 0);
  Finder.FirstNameColumn := 2;
  FillChar(S3, SizeOf(S3), 0);
  S3.FileName := 'f.csv';
  S3.ReturnUrl := 'https://example.com/done';
  FillChar(Score, SizeOf(Score), 0);
  Score.FileName := 'f.csv';
  Score.ReturnUrl := 'https://example.com/done';
  Job := ZbDefaultBulkFileJobOptions;
  Job.FileName := 'f.csv';
  Job.ReturnUrl := 'https://example.com/done';
  Emails[0].Email := 'a@example.com';
  Emails[0].Ip := '';
  Warmup := ZbWarmupDefaultAddSendingAccountOptions;
  Warmup.EngagementRuleIdSpecified := True;
  Warmup.EngagementRuleId := 2;
  Form := TStringList.Create;
  try
    { ZbGeneric }
    try ZbGetCredits; Expect('ZbGetCredits', nil); except on E: Exception do Expect('ZbGetCredits', E); end;
    try ZbGetApiUsage; Expect('ZbGetApiUsage', nil); except on E: Exception do Expect('ZbGetApiUsage', E); end;
    try ZbGetApiUsage(EncodeDate(2024, 1, 1), EncodeDate(2024, 1, 2)); Expect('ZbGetApiUsage(dates)', nil); except on E: Exception do Expect('ZbGetApiUsage(dates)', E); end;
    try ZbActivityData('a@example.com'); Expect('ZbActivityData', nil); except on E: Exception do Expect('ZbActivityData', E); end;
    try ZbScoreEmail('a@example.com'); Expect('ZbScoreEmail', nil); except on E: Exception do Expect('ZbScoreEmail', E); end;
    try ZbScoreEmailPost('a@example.com'); Expect('ZbScoreEmailPost', nil); except on E: Exception do Expect('ZbScoreEmailPost', E); end;
    try ZbFindEmailByDomain('example.com', 'Ada', 'B', 'Lovelace'); Expect('ZbFindEmailByDomain', nil); except on E: Exception do Expect('ZbFindEmailByDomain', E); end;
    try ZbFindEmailByCompanyName('Example', 'Ada', 'B', 'Lovelace'); Expect('ZbFindEmailByCompanyName', nil); except on E: Exception do Expect('ZbFindEmailByCompanyName', E); end;
    try ZbDomainSearchByDomain('example.com'); Expect('ZbDomainSearchByDomain', nil); except on E: Exception do Expect('ZbDomainSearchByDomain', E); end;
    try ZbDomainSearchByCompanyName('Example'); Expect('ZbDomainSearchByCompanyName', nil); except on E: Exception do Expect('ZbDomainSearchByCompanyName', E); end;
    {$WARN SYMBOL_DEPRECATED OFF}
    try ZbFindEmail('example.com', 'Ada', 'B', 'Lovelace'); Expect('ZbFindEmail', nil); except on E: Exception do Expect('ZbFindEmail', E); end;
    try ZbFindEmail('example.com', 'Ada', 'Lovelace'); Expect('ZbFindEmail(3)', nil); except on E: Exception do Expect('ZbFindEmail(3)', E); end;
    try ZbDomainSearch('example.com'); Expect('ZbDomainSearch', nil); except on E: Exception do Expect('ZbDomainSearch', E); end;
    {$WARN SYMBOL_DEPRECATED ON}
    { ZbValidation }
    try ZbValidateEmail('a@example.com'); Expect('ZbValidateEmail', nil); except on E: Exception do Expect('ZbValidateEmail', E); end;
    try CallTransport(tpFormPost); Expect('ZbValidateEmail(UsePost)', nil); except on E: Exception do Expect('ZbValidateEmail(UsePost)', E); end;
    try ZbBatchValidateEmails(Emails); Expect('ZbBatchValidateEmails', nil); except on E: Exception do Expect('ZbBatchValidateEmails', E); end;
    { ZbBulk }
    try ZbBulkValidationFileSubmit('email', Params); Expect('ZbBulkValidationFileSubmit', nil); except on E: Exception do Expect('ZbBulkValidationFileSubmit', E); end;
    try ZbBulkValidationFileStatusCheck('fid'); Expect('ZbBulkValidationFileStatusCheck', nil); except on E: Exception do Expect('ZbBulkValidationFileStatusCheck', E); end;
    try ZbBulkValidationResultFetch('fid'); Expect('ZbBulkValidationResultFetch', nil); except on E: Exception do Expect('ZbBulkValidationResultFetch', E); end;
    try ZbBulkValidationResultDelete('fid'); Expect('ZbBulkValidationResultDelete', nil); except on E: Exception do Expect('ZbBulkValidationResultDelete', E); end;
    try ZbAiScoringFileSubmit('email', Params); Expect('ZbAiScoringFileSubmit', nil); except on E: Exception do Expect('ZbAiScoringFileSubmit', E); end;
    try ZbAiScoringFileStatusCheck('fid'); Expect('ZbAiScoringFileStatusCheck', nil); except on E: Exception do Expect('ZbAiScoringFileStatusCheck', E); end;
    try ZbAiScoringResultFetch('fid'); Expect('ZbAiScoringResultFetch', nil); except on E: Exception do Expect('ZbAiScoringResultFetch', E); end;
    try ZbAiScoringResultDelete('fid'); Expect('ZbAiScoringResultDelete', nil); except on E: Exception do Expect('ZbAiScoringResultDelete', E); end;
    { ZbPriority3 }
    try ZbSendEmailFinderFile('a', 1, @Finder); Expect('ZbSendEmailFinderFile', nil); except on E: Exception do Expect('ZbSendEmailFinderFile', E); end;
    try ZbEmailFinderFileStatus('fid'); Expect('ZbEmailFinderFileStatus', nil); except on E: Exception do Expect('ZbEmailFinderFileStatus', E); end;
    try ZbEmailFinderResultFetch('fid'); Expect('ZbEmailFinderResultFetch', nil); except on E: Exception do Expect('ZbEmailFinderResultFetch', E); end;
    try ZbEmailFinderResultDelete('fid'); Expect('ZbEmailFinderResultDelete', nil); except on E: Exception do Expect('ZbEmailFinderResultDelete', E); end;
    try ZbSendDomainSearchFile('a', 1, nil); Expect('ZbSendDomainSearchFile', nil); except on E: Exception do Expect('ZbSendDomainSearchFile', E); end;
    try ZbDomainSearchFileStatus('fid'); Expect('ZbDomainSearchFileStatus', nil); except on E: Exception do Expect('ZbDomainSearchFileStatus', E); end;
    try ZbDomainSearchResultFetch('fid'); Expect('ZbDomainSearchResultFetch', nil); except on E: Exception do Expect('ZbDomainSearchResultFetch', E); end;
    try ZbDomainSearchResultDelete('fid'); Expect('ZbDomainSearchResultDelete', nil); except on E: Exception do Expect('ZbDomainSearchResultDelete', E); end;
    try ZbSendListEvaluatorFile('a', 1); Expect('ZbSendListEvaluatorFile', nil); except on E: Exception do Expect('ZbSendListEvaluatorFile', E); end;
    try ZbGetListEvaluatorResult('fid'); Expect('ZbGetListEvaluatorResult', nil); except on E: Exception do Expect('ZbGetListEvaluatorResult', E); end;
    try ZbSendEvaluatedFile('fid'); Expect('ZbSendEvaluatedFile', nil); except on E: Exception do Expect('ZbSendEvaluatedFile', E); end;
    try ZbAddFilter('allow', 'email', 'a@example.com'); Expect('ZbAddFilter', nil); except on E: Exception do Expect('ZbAddFilter', E); end;
    try ZbListFilters(False); Expect('ZbListFilters(GET)', nil); except on E: Exception do Expect('ZbListFilters(GET)', E); end;
    try ZbListFilters(True); Expect('ZbListFilters(#POST)', nil); except on E: Exception do Expect('ZbListFilters(#POST)', E); end;
    try ZbDeleteFilter('allow', 'email', 'a@example.com'); Expect('ZbDeleteFilter', nil); except on E: Exception do Expect('ZbDeleteFilter', E); end;
    try ZbListS3Files('bucket', ''); Expect('ZbListS3Files', nil); except on E: Exception do Expect('ZbListS3Files', E); end;
    try ZbValidateS3File(S3); Expect('ZbValidateS3File', nil); except on E: Exception do Expect('ZbValidateS3File', E); end;
    try ZbScoreS3File(Score); Expect('ZbScoreS3File', nil); except on E: Exception do Expect('ZbScoreS3File', E); end;
    try ZbExportS3File('fid', ''); Expect('ZbExportS3File', nil); except on E: Exception do Expect('ZbExportS3File', E); end;
    try ZbEmailFinderS3File(Job); Expect('ZbEmailFinderS3File', nil); except on E: Exception do Expect('ZbEmailFinderS3File', E); end;
    try ZbDomainSearchS3File(Job); Expect('ZbDomainSearchS3File', nil); except on E: Exception do Expect('ZbDomainSearchS3File', E); end;
    try ZbValidateSFTPFile(Job); Expect('ZbValidateSFTPFile', nil); except on E: Exception do Expect('ZbValidateSFTPFile', E); end;
    try ZbScoreSFTPFile(Job); Expect('ZbScoreSFTPFile', nil); except on E: Exception do Expect('ZbScoreSFTPFile', E); end;
    try ZbEmailFinderSFTPFile(Job); Expect('ZbEmailFinderSFTPFile', nil); except on E: Exception do Expect('ZbEmailFinderSFTPFile', E); end;
    try ZbDomainSearchSFTPFile(Job); Expect('ZbDomainSearchSFTPFile', nil); except on E: Exception do Expect('ZbDomainSearchSFTPFile', E); end;
    try ZbExportSFTPFile('fid', ''); Expect('ZbExportSFTPFile', nil); except on E: Exception do Expect('ZbExportSFTPFile', E); end;
    try ZbInboxPlacementProviders; Expect('ZbInboxPlacementProviders', nil); except on E: Exception do Expect('ZbInboxPlacementProviders', E); end;
    try ZbInboxPlacementTestTypes; Expect('ZbInboxPlacementTestTypes', nil); except on E: Exception do Expect('ZbInboxPlacementTestTypes', E); end;
    try ZbCreateInboxPlacementTest('Launch', nil); Expect('ZbCreateInboxPlacementTest', nil); except on E: Exception do Expect('ZbCreateInboxPlacementTest', E); end;
    try ZbInboxPlacementTest('tid'); Expect('ZbInboxPlacementTest', nil); except on E: Exception do Expect('ZbInboxPlacementTest', E); end;
    try ZbListInboxPlacementTests(nil); Expect('ZbListInboxPlacementTests', nil); except on E: Exception do Expect('ZbListInboxPlacementTests', E); end;
    try ZbDeleteInboxPlacementTest('tid'); Expect('ZbDeleteInboxPlacementTest', nil); except on E: Exception do Expect('ZbDeleteInboxPlacementTest', E); end;
    try ZbInboxPlacementWebhook; Expect('ZbInboxPlacementWebhook', nil); except on E: Exception do Expect('ZbInboxPlacementWebhook', E); end;
    try ZbSetInboxPlacementWebhook('https://example.com/ipt'); Expect('ZbSetInboxPlacementWebhook', nil); except on E: Exception do Expect('ZbSetInboxPlacementWebhook', E); end;
    { ZbWarmup }
    try ZbWarmupListSendingAccounts; Expect('ZbWarmupListSendingAccounts', nil); except on E: Exception do Expect('ZbWarmupListSendingAccounts', E); end;
    try ZbWarmupGetSendingAccount('s@example.com'); Expect('ZbWarmupGetSendingAccount', nil); except on E: Exception do Expect('ZbWarmupGetSendingAccount', E); end;
    try ZbWarmupAddSendingAccount('s@example.com', ZB_WARMUP_SELF_MANAGED, @Warmup); Expect('ZbWarmupAddSendingAccount', nil); except on E: Exception do Expect('ZbWarmupAddSendingAccount', E); end;
    try ZbWarmupUpdateSendingAccount('s@example.com', 1, 4, True); Expect('ZbWarmupUpdateSendingAccount', nil); except on E: Exception do Expect('ZbWarmupUpdateSendingAccount', E); end;
    try ZbWarmupDeleteSendingAccount('s@example.com'); Expect('ZbWarmupDeleteSendingAccount', nil); except on E: Exception do Expect('ZbWarmupDeleteSendingAccount', E); end;
    try ZbWarmupChangeFromAddress('old@example.com', 'new@example.com'); Expect('ZbWarmupChangeFromAddress', nil); except on E: Exception do Expect('ZbWarmupChangeFromAddress', E); end;
    try ZbWarmupListFromAddresses(nil); Expect('ZbWarmupListFromAddresses', nil); except on E: Exception do Expect('ZbWarmupListFromAddresses', E); end;
    try ZbWarmupGetEngagementRules; Expect('ZbWarmupGetEngagementRules', nil); except on E: Exception do Expect('ZbWarmupGetEngagementRules', E); end;
    try ZbWarmupSetEngagementRule('s@example.com', 3); Expect('ZbWarmupSetEngagementRule', nil); except on E: Exception do Expect('ZbWarmupSetEngagementRule', E); end;
    try ZbWarmupGetCustomEngagement('s@example.com'); Expect('ZbWarmupGetCustomEngagement', nil); except on E: Exception do Expect('ZbWarmupGetCustomEngagement', E); end;
    try ZbWarmupCreateCustomEngagement('s@example.com', 10, 20, 30); Expect('ZbWarmupCreateCustomEngagement', nil); except on E: Exception do Expect('ZbWarmupCreateCustomEngagement', E); end;
    try ZbWarmupUpdateCustomEngagement('s@example.com', 10, 20, 30); Expect('ZbWarmupUpdateCustomEngagement', nil); except on E: Exception do Expect('ZbWarmupUpdateCustomEngagement', E); end;
    try ZbWarmupDeleteCustomEngagement('s@example.com', 2); Expect('ZbWarmupDeleteCustomEngagement', nil); except on E: Exception do Expect('ZbWarmupDeleteCustomEngagement', E); end;
    try ZbWarmupGetSeeds; Expect('ZbWarmupGetSeeds', nil); except on E: Exception do Expect('ZbWarmupGetSeeds', E); end;
    try ZbWarmupDownloadSeeds; Expect('ZbWarmupDownloadSeeds', nil); except on E: Exception do Expect('ZbWarmupDownloadSeeds', E); end;
    try ZbWarmupGetSeedCount; Expect('ZbWarmupGetSeedCount', nil); except on E: Exception do Expect('ZbWarmupGetSeedCount', E); end;
    try ZbWarmupGetProgressToday; Expect('ZbWarmupGetProgressToday', nil); except on E: Exception do Expect('ZbWarmupGetProgressToday', E); end;
    try ZbWarmupGetSentToday; Expect('ZbWarmupGetSentToday', nil); except on E: Exception do Expect('ZbWarmupGetSentToday', E); end;
    try ZbWarmupGetEmailsPerDay; Expect('ZbWarmupGetEmailsPerDay', nil); except on E: Exception do Expect('ZbWarmupGetEmailsPerDay', E); end;
    try ZbWarmupGetStatistics('s@example.com'); Expect('ZbWarmupGetStatistics', nil); except on E: Exception do Expect('ZbWarmupGetStatistics', E); end;
    try ZbWarmupGetStatisticsPerDay('s@example.com', '2026-10-01', '2026-10-07'); Expect('ZbWarmupGetStatisticsPerDay', nil); except on E: Exception do Expect('ZbWarmupGetStatisticsPerDay', E); end;
    try ZbWarmupGetStatisticsDetailed('s@example.com'); Expect('ZbWarmupGetStatisticsDetailed', nil); except on E: Exception do Expect('ZbWarmupGetStatisticsDetailed', E); end;
    try ZbWarmupDownloadStatistics('s@example.com'); Expect('ZbWarmupDownloadStatistics', nil); except on E: Exception do Expect('ZbWarmupDownloadStatistics', E); end;
    try ZbWarmupGetFromAddressStatistics('s@example.com'); Expect('ZbWarmupGetFromAddressStatistics', nil); except on E: Exception do Expect('ZbWarmupGetFromAddressStatistics', E); end;
    try ZbWarmupCheckDomainDmarc('s@example.com'); Expect('ZbWarmupCheckDomainDmarc', nil); except on E: Exception do Expect('ZbWarmupCheckDomainDmarc', E); end;
    try ZbWarmupTestSmtpConfiguration('s@example.com', 's@example.com', 'pw', 'smtp.example.com', 587); Expect('ZbWarmupTestSmtpConfiguration', nil); except on E: Exception do Expect('ZbWarmupTestSmtpConfiguration', E); end;
    { ZbGeneric overloads }
    try ZbFindEmailByDomain('example.com', 'Ada', 'Lovelace'); Expect('ZbFindEmailByDomain(3)', nil); except on E: Exception do Expect('ZbFindEmailByDomain(3)', E); end;
    try ZbFindEmailByDomain('example.com', 'Ada'); Expect('ZbFindEmailByDomain(2)', nil); except on E: Exception do Expect('ZbFindEmailByDomain(2)', E); end;
    try ZbFindEmailByCompanyName('Example', 'Ada', 'Lovelace'); Expect('ZbFindEmailByCompanyName(3)', nil); except on E: Exception do Expect('ZbFindEmailByCompanyName(3)', E); end;
    try ZbFindEmailByCompanyName('Example', 'Ada'); Expect('ZbFindEmailByCompanyName(2)', nil); except on E: Exception do Expect('ZbFindEmailByCompanyName(2)', E); end;
    { ZbValidation overloads }
    try ZbValidateEmail('a@example.com', '1.2.3.4'); Expect('ZbValidateEmail(ip)', nil); except on E: Exception do Expect('ZbValidateEmail(ip)', E); end;
    try ZbBatchValidateEmails(['a@example.com']); Expect('ZbBatchValidateEmails(strings)', nil); except on E: Exception do Expect('ZbBatchValidateEmails(strings)', E); end;
    try ZbBatchValidateEmails(Emails, ZbDefaultValidateOptions); Expect('ZbBatchValidateEmails(options)', nil); except on E: Exception do Expect('ZbBatchValidateEmails(options)', E); end;
    { ZbWarmup overloads }
    try ZbWarmupDownloadSeeds(''); Expect('ZbWarmupDownloadSeeds(path)', nil); except on E: Exception do Expect('ZbWarmupDownloadSeeds(path)', E); end;
    try ZbWarmupDownloadStatistics('s@example.com', ''); Expect('ZbWarmupDownloadStatistics(path)', nil); except on E: Exception do Expect('ZbWarmupDownloadStatistics(path)', E); end;
    try ZbWarmupCheckDomainDmarc('s@example.com', 'sel'); Expect('ZbWarmupCheckDomainDmarc(selector)', nil); except on E: Exception do Expect('ZbWarmupCheckDomainDmarc(selector)', E); end;
    { ZbUtility: the transports themselves, on the main base URL. }
    Url := ZbBaseUrl(ZbApiBaseURL) + '/getcredits?api_key=' + EncodeParam(ZbApiKey);
    try ZBGetRequest(Url); Expect('ZBGetRequest', nil); except on E: Exception do Expect('ZBGetRequest', E); end;
    try ZBPostRequest(Url, '{}'); Expect('ZBPostRequest(json)', nil); except on E: Exception do Expect('ZBPostRequest(json)', E); end;
    try ZBFormPostRequest(Url, 'a=b'); Expect('ZBFormPostRequest', nil); except on E: Exception do Expect('ZBFormPostRequest', E); end;
    try ZBPostRequest(Url, Form, 'email'); Expect('ZBPostRequest(multipart)', nil); except on E: Exception do Expect('ZBPostRequest(multipart)', E); end;
    try ZBSendRequest('PUT', Url, '{}', JSON_CONTENT_TYPE); Expect('ZBSendRequest', nil); except on E: Exception do Expect('ZBSendRequest', E); end;
    try ZBSendRequestWithHeader('DELETE', Url, '', '', 'X-Test', 'x'); Expect('ZBSendRequestWithHeader', nil); except on E: Exception do Expect('ZBSendRequestWithHeader', E); end;
    try ZBSendForeignKeyRequest('GET', Url, '', '', 'API-Key', 'x'); Expect('ZBSendForeignKeyRequest', nil); except on E: Exception do Expect('ZBSendForeignKeyRequest', E); end;
  finally
    Form.Free;
  end;
end;

procedure ExpectHttpsRefusal(const Name: String; E: Exception);
begin
  if (E = nil) or not (E is ZbException) or (Pos('https://', E.Message) = 0) then
  begin
    if E <> nil then
      SweepMissed := SweepMissed + Name + ' (' + E.Message + '); '
    else
      SweepMissed := SweepMissed + Name + ' (sent); ';
  end;
end;

procedure TTestOffline.TestEveryRequestFunctionRefusesPlainHttp;
begin
  { Every base URL is plain http on a real host. The mock would answer any
    request that got past the URL check, so a function that skips the check
    shows up as "sent". }
  ZBInitialize('mock_api_key');
  ZbWarmupInitialize('mock_warmup_key');
  ZbApiBaseURL := 'http://api.zerobounce.net/v2';
  BULK_URI := 'http://bulkapi.zerobounce.net/v2';
  ZbBulkRootURL := 'http://bulkapi.zerobounce.net';
  ZbListEvaluatorURL := 'http://bulkapi.zerobounce.net/v2/listevaluator';
  ZbInboxPlacementURL := 'http://bulkapi.zerobounce.net/api/ipt';
  ZbWarmupBaseURL := 'http://warmup-api.zerobounce.net';
  ZBMockResponse(200, '{"success":true,"message":"ok","file_id":"fid","Credits":"7"}', 'application/octet-stream');
  try
    SweepEveryPublicCall(@ExpectHttpsRefusal);
  finally
    UseDefaultUrls;
  end;
  AssertEquals('functions that did not refuse plain http', '', SweepMissed);
end;

procedure TTestOffline.TestBaseUrlsWithATrailingSlash;
var
  Failures: String;

  procedure Check(const Name: String);
  var
    Url: String;
  begin
    Url := ZbResponseMock.UrlCalled;
    if Url = '' then
      Failures := Failures + Name + ': nothing sent; '
    else if Pos('//', Copy(Url, Pos('://', Url) + 3, MaxInt)) > 0 then
      Failures := Failures + Name + ': ' + Url + '; ';
    ZbResponseMock.UrlCalled := '';
  end;

begin
  { Every base URL is a plain variable a caller can set. A trailing slash on
    any of them must not turn into '//' in the request path. }
  Failures := '';
  ZBInitialize('mock_api_key');
  ZbWarmupInitialize('mock_warmup_key');
  ZbApiBaseURL := 'https://api.zerobounce.net/v2/';
  BULK_URI := 'https://bulkapi.zerobounce.net/v2//';
  ZbBulkRootURL := 'https://bulkapi.zerobounce.net/';
  ZbListEvaluatorURL := 'https://bulkapi.zerobounce.net/v2/listevaluator/';
  ZbInboxPlacementURL := 'https://bulkapi.zerobounce.net/api/ipt/';
  ZbWarmupBaseURL := 'https://warmup-api.zerobounce.net/';
  ZBMockResponse(200, '{"success":true,"message":"ok","file_id":"fid","Credits":"7","ok":true}');
  ZbResponseMock.UrlCalled := '';
  try
    { Only the URL matters here; the answer may not parse as each result. }
    try ZbGetCredits; except end; Check('ZbApiBaseURL');
    try ZbBulkValidationResultDelete('fid'); except end; Check('BULK_URI');
    try ZbEmailFinderResultDelete('fid'); except end; Check('ZbBulkRootURL');
    try ZbGetListEvaluatorResult('fid'); except end; Check('ZbListEvaluatorURL');
    try ZbSendEvaluatedFile('fid'); except end; Check('ZbListEvaluatorURL (send-evaluated-file)');
    try ZbInboxPlacementProviders; except end; Check('ZbInboxPlacementURL');
    try ZbWarmupGetSeedCount; except end; Check('ZbWarmupBaseURL');
  finally
    UseDefaultUrls;
  end;
  AssertEquals('requests built with a double slash', '', Failures);
end;

procedure TTestOffline.TestAJsonAnswerCutShortIsAnErrorOnEveryTransport;
var
  Transport: TTransport;
  Key, Missed: String;
begin
  { The server sends a whole, parsable answer, but promised 500 bytes more and
    closes the connection. The JSON POST, form POST and multipart transports
    used to return what arrived as the result. }
  EnsureStub;
  Missed := '';
  try
    for Transport := Low(TTransport) to High(TTransport) do
    begin
      Key := 'cutjson-' + IntToStr(Ord(Transport));
      UseStub(Key);
      try
        CallTransport(Transport);
        Missed := Missed + TRANSPORT_NAMES[Transport] + ' (returned a result); ';
      except
        on E: ZbException do
          if (Pos('cut short', E.Message) = 0) or (E.StatusCode <> 0) or (Pos(Key, E.Message) > 0) then
            Missed := Missed + Format('%s (status %d: %s); ', [TRANSPORT_NAMES[Transport], E.StatusCode, E.Message]);
      end;
    end;
  finally
    ClearRecorded;
    UseDefaultUrls;
  end;
  { Each transport raises "cut short", with status 0 and without the key. }
  AssertEquals('transports that accepted an answer cut short', '', Missed);
end;

{ A loopback port with nothing listening: bound, read back, closed. }
function ClosedPort: Integer;
var
  Sock: LongInt;
  Addr: sockaddr_in;
  NameLen: TSockLen;
begin
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  Addr := Default(sockaddr_in);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(0);
  Addr.sin_addr.s_addr := htonl($7F000001);
  fpBind(Sock, @Addr, SizeOf(Addr));
  NameLen := SizeOf(Addr);
  fpGetSockName(Sock, @Addr, @NameLen);
  Result := NToHS(Addr.sin_port);
  CloseSocket(Sock);
end;

procedure TTestOffline.TestARefusedUploadCarriesNoStatus;
var
  Params: TZbBulkParams;
  Form: TStringList;
  Response: TZbRequestResponse;
  Url: String;
  Attempt: Integer;
  Raised: Boolean;
begin
  { No HTTP answer means no status: 0, as for every other transport error,
    not whatever happened to be in memory. }
  FillChar(Params, SizeOf(Params), 0);
  Params.EmailAddressColumn := 1;
  Form := TStringList.Create;
  try
    for Attempt := 1 to 3 do
    begin
      UseStub('refused-key');
      Url := Format('http://127.0.0.1:%d', [ClosedPort]);
      BULK_URI := Url + '/v2';
      Raised := False;
      try
        ZbBulkValidationFileSubmit('email' + LineEnding + 'a@example.com' + LineEnding, Params);
      except
        on E: ZbException do
        begin
          Raised := True;
          AssertEquals('ZbBulkValidationFileSubmit: ' + E.Message, 0, E.StatusCode);
          AssertTrue('leaked the key: ' + E.Message, Pos('refused-key', E.Message) = 0);
        end;
      end;
      AssertTrue('a refused upload should raise', Raised);

      { The same, where the result variable already holds an answer. }
      Response := Default(TZbRequestResponse);
      Response.StatusCode := 200 + Attempt;
      Raised := False;
      try
        Response := ZBPostRequest(Url + '/v2/sendfile', Form, 'email');
      except
        on E: ZbException do
        begin
          Raised := True;
          AssertEquals('ZBPostRequest(multipart): ' + E.Message, 0, E.StatusCode);
        end;
      end;
      AssertTrue('a refused multipart POST should raise', Raised);
    end;
  finally
    Form.Free;
    UseDefaultUrls;
  end;
end;

procedure TTestOffline.TestAWarmupTransportErrorIsRedacted;
const
  KEY = 'badstatus-warmup';
var
  Raised: Boolean;
begin
  { fphttpclient raises its own exception, quoting the status line, which
    here carries the key. Warmup turns it into a ZbException without it. }
  EnsureStub;
  UseStub(KEY);
  Raised := False;
  try
    try
      ZbWarmupGetSeeds;
    except
      on E: Exception do
      begin
        Raised := True;
        AssertTrue('not a ZbException: ' + E.ClassName, E is ZbException);
        AssertTrue('the key leaked: ' + E.Message, Pos(KEY, E.Message) = 0);
        AssertTrue(E.Message, Pos('REDACTED', E.Message) > 0);
      end;
    end;
    AssertTrue('a bad status line should raise', Raised);
  finally
    ClearRecorded;
    UseDefaultUrls;
  end;
end;

const
  REFUSED_KEY = 'refusedsweepkey';
  REFUSED_WARMUP_KEY = 'refusedsweepwarmupkey';

var
  RefusedPort: Integer;

procedure ExpectRefusedAsZbException(const Name: String; E: Exception);
begin
  if E = nil then
    SweepMissed := SweepMissed + Name + ' (returned); '
  else if not (E is ZbException) then
    SweepMissed := SweepMissed + Format('%s (%s: %s); ', [Name, E.ClassName, E.Message])
  else if ZbException(E).StatusCode <> 0 then
    SweepMissed := SweepMissed + Format('%s (status %d: %s); ', [Name, ZbException(E).StatusCode, E.Message])
  else if (Pos(REFUSED_KEY, E.Message) > 0) or (Pos(REFUSED_KEY, ZbException(E).Payload) > 0) then
    SweepMissed := SweepMissed + Name + ' (the key leaked: ' + E.Message + '); '
  else if Pos(IntToStr(RefusedPort), E.Message) = 0 then
    { The network library's own message names the address it could not
      reach; anything else did not come from the refused connection. }
    SweepMissed := SweepMissed + Name + ' (not the connection error: ' + E.Message + '); ';
end;

procedure TTestOffline.TestEveryPublicCallRaisesZbExceptionWhenRefused;
var
  Base: String;
begin
  { Nothing listens on the port, so every request fails before any HTTP
    answer. The README promises a ZbException; GET, the JSON and form POSTs
    and ZBSendRequest let fphttpclient's ESocketError through, with no
    StatusCode, while uploads and Warmup wrapped it. }
  ZBInitialize(REFUSED_KEY);
  ZbWarmupInitialize(REFUSED_WARMUP_KEY);
  RefusedPort := ClosedPort;
  Base := Format('http://127.0.0.1:%d', [RefusedPort]);
  ZbApiBaseURL := Base + '/v2';
  BULK_URI := Base + '/v2';
  ZbBulkRootURL := Base;
  ZbListEvaluatorURL := Base + '/v2/listevaluator';
  ZbInboxPlacementURL := Base + '/api/ipt';
  ZbWarmupBaseURL := Base;
  ZbResponseMock := cDefaultMock;
  try
    SweepEveryPublicCall(@ExpectRefusedAsZbException);
  finally
    UseDefaultUrls;
  end;
  AssertEquals('calls that did not raise ZbException (StatusCode 0, key masked)', '', SweepMissed);
end;

procedure TTestOffline.TestAProtocolErrorIsAZbExceptionWithoutTheKey;
var
  Transport: TTransport;
  Key, Missed: String;
begin
  { The status line carries the key, and fphttpclient quotes it in its own
    exception. Every transport raises a ZbException with StatusCode 0 and the
    key masked in that text. }
  EnsureStub;
  Missed := '';
  try
    for Transport := Low(TTransport) to High(TTransport) do
    begin
      Key := 'badstatus-' + IntToStr(Ord(Transport));
      UseStub(Key);
      try
        CallTransport(Transport);
        Missed := Missed + TRANSPORT_NAMES[Transport] + ' (returned a result); ';
      except
        on E: Exception do
          if not (E is ZbException) then
            Missed := Missed + Format('%s (%s); ', [TRANSPORT_NAMES[Transport], E.ClassName])
          else if (ZbException(E).StatusCode <> 0) or (Pos(Key, E.Message) > 0) or
                  (Pos('REDACTED', E.Message) = 0) then
            Missed := Missed + Format('%s (status %d: %s); ',
              [TRANSPORT_NAMES[Transport], ZbException(E).StatusCode, E.Message]);
      end;
    end;
  finally
    ClearRecorded;
    UseDefaultUrls;
  end;
  AssertEquals('transports whose protocol error was not a ZbException without the key', '', Missed);
end;

procedure IgnoreOutcome(const Name: String; E: Exception);
begin
end;

{ The routines a unit's interface declares, outside class and record
  declarations, by name, once per overload. }
procedure AddInterfaceRoutines(const FileName: String; Names: TStrings);
var
  Text, Clean, Line, Lower, Name: String;
  Lines: TStringList;
  I, J, Depth: Integer;
  InType: Boolean;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    Text := Lines.Text;
  finally
    Lines.Free;
  end;
  { Comments and directives can hold anything, so they go first. }
  Clean := '';
  I := 1;
  while I <= Length(Text) do
  begin
    if Text[I] = '{' then
    begin
      while (I <= Length(Text)) and (Text[I] <> '}') do
        Inc(I);
      Inc(I);
    end
    else if (Text[I] = '(') and (I < Length(Text)) and (Text[I + 1] = '*') then
    begin
      J := Pos('*)', Copy(Text, I + 2, MaxInt));
      if J = 0 then
        Break;
      I := I + 2 + J + 1;
    end
    else if (Text[I] = '/') and (I < Length(Text)) and (Text[I + 1] = '/') then
    begin
      while (I <= Length(Text)) and (Text[I] <> #10) do
        Inc(I);
    end
    else
    begin
      Clean := Clean + Text[I];
      Inc(I);
    end;
  end;
  Lines := TStringList.Create;
  try
    Lines.Text := Clean;
    Depth := 0;
    InType := False;
    for I := 0 to Lines.Count - 1 do
    begin
      Lower := LowerCase(Trim(Lines[I]));
      if Lower = 'interface' then
        Depth := 1
      else if Lower = 'implementation' then
        Break
      else if Depth = 0 then
        Continue
      else if InType then
      begin
        if (Lower = 'end;') or (Copy(Lower, 1, 4) = 'end;') then
          InType := False;
      end
      else if ((Pos('= class', Lower) > 0) or (Pos('= record', Lower) > 0) or
               (Pos('= packed record', Lower) > 0)) and (Lower[Length(Lower)] <> ';') then
        InType := True
      else if (Copy(Lower, 1, 9) = 'function ') or (Copy(Lower, 1, 10) = 'procedure ') then
      begin
        Line := Trim(Lines[I]);
        Line := Trim(Copy(Line, Pos(' ', Line) + 1, MaxInt));
        Name := '';
        J := 1;
        while (J <= Length(Line)) and (Line[J] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
        begin
          Name := Name + Line[J];
          Inc(J);
        end;
        { Once per declaration, so overloads are counted. }
        if Name <> '' then
          Names.Add(Name);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

{ How many times Name is in List. }
function CountOf(List: TStrings; const Name: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to List.Count - 1 do
    if List[I] = Name then
      Inc(Result);
end;

procedure TTestOffline.TestEveryPublicRequestFunctionIsInTheSweep;
const
  UNITS: array[0..5] of String = (
    'zbutility.pas', 'zbgeneric.pas', 'zbvalidation.pas', 'zbbulk.pas', 'zbpriority3.pas', 'zbwarmup.pas'
  );
  { Public routines that send nothing. Any other routine in these interfaces
    is a request and must be in SweepEveryPublicCall. }
  NOT_REQUESTS: array[0..29] of String = (
    'Register', 'ZBApiURLToString', 'ZBSetApiKey', 'ZBInitialize', 'ZbBaseUrl', 'EncodeParam',
    'RedactSecrets', 'ZbMaskSecret', 'ZbContentTypeIsHtml', 'ZbLastRequestTimeouts', 'ZbRegisterSecretRedactor', 'ZbCheckedApiKey', 'ZbRequireHttpsUrl', 'ZbPathSegment', 'ZbCheckedUrl',
    'ZbBodyLooksLikeHtml', 'ZbSuccessMeansFailure', 'ZbMessageFromBody', 'EncodePathSegment',
    'ZbJsonEscape', 'ZBMockResponse', 'ZbWindowsRootCertificates', 'ZbTrustDerCertificates',
    'ZbDefaultValidateOptions', 'ZbBatchRequestBodyFromEmails', 'ZbFromDataFromFileSubmitRecord',
    'ZbDefaultBulkFileJobOptions', 'ZbWarmupInitialize', 'ZbWarmupDefaultAddSendingAccountOptions',
    'ZbWarmupDefaultListFromAddressesOptions'
  );
var
  SrcDir, Missing, Overloads, Extra: String;
  Declared: TStringList;
  I: Integer;
begin
  { The CI and Docker runners build the executable in the repository root. }
  SrcDir := ExtractFilePath(ParamStr(0)) + 'src' + PathDelim;
  if not FileExists(SrcDir + UNITS[0]) then
    SrcDir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'src' + PathDelim;
  AssertTrue('src/ not found beside the test runner or in the working directory',
    FileExists(SrcDir + UNITS[0]));

  Declared := TStringList.Create;
  try
    for I := Low(UNITS) to High(UNITS) do
      AddInterfaceRoutines(SrcDir + UNITS[I], Declared);
    for I := Low(NOT_REQUESTS) to High(NOT_REQUESTS) do
    begin
      AssertTrue(NOT_REQUESTS[I] + ' is no longer declared', Declared.IndexOf(NOT_REQUESTS[I]) >= 0);
      while Declared.IndexOf(NOT_REQUESTS[I]) >= 0 do
        Declared.Delete(Declared.IndexOf(NOT_REQUESTS[I]));
    end;
    AssertTrue(Format('only %d request functions found', [Declared.Count]), Declared.Count > 80);

    UseDefaultUrls;
    ZBMockResponse(200, '{"success":true,"message":"ok","file_id":"fid","Credits":"7"}', 'application/octet-stream');
    try
      SweepEveryPublicCall(@IgnoreOutcome);
    finally
      UseDefaultUrls;
    end;

    Missing := '';
    Overloads := '';
    for I := 0 to Declared.Count - 1 do
      if Declared.IndexOf(Declared[I]) = I then
      begin
        if SweepNames.IndexOf(Declared[I]) < 0 then
          Missing := Missing + Declared[I] + ' '
        else if CountOf(Declared, Declared[I]) <> CountOf(SweepOverloads, Declared[I]) then
          { An overload left out of the sweep, or one listed twice. }
          Overloads := Overloads + Format('%s (%d declared, %d swept) ',
            [Declared[I], CountOf(Declared, Declared[I]), CountOf(SweepOverloads, Declared[I])]);
      end;
    Extra := '';
    for I := 0 to SweepNames.Count - 1 do
      if Declared.IndexOf(SweepNames[I]) < 0 then
        Extra := Extra + SweepNames[I] + ' ';
  finally
    Declared.Free;
  end;
  AssertEquals('public request functions missing from SweepEveryPublicCall', '', Missing);
  AssertEquals('overloads missing from SweepEveryPublicCall', '', Overloads);
  AssertEquals('swept names that are not public request functions', '', Extra);
end;

const
  { Keys that need escaping in a URL and in JSON, so their raw, URL-encoded
    and JSON-escaped forms ('/' escaped or not) all differ. }
  LEAK_KEY = 'leak-K3y/x+y"q%z';
  LEAK_KEY_ESCAPED = 'leak-K3y%2Fx%2By%22q%25z';
  LEAK_KEY_JSON = 'leak-K3y\/x+y\"q%z';
  LEAK_KEY_JSON_PLAIN = 'leak-K3y/x+y\"q%z';
  LEAK_WARMUP_KEY = 'leak-W4rm/x+y"q%z';
  LEAK_WARMUP_KEY_ESCAPED = 'leak-W4rm%2Fx%2By%22q%25z';
  LEAK_WARMUP_KEY_JSON = 'leak-W4rm\/x+y\"q%z';
  LEAK_WARMUP_KEY_JSON_PLAIN = 'leak-W4rm/x+y\"q%z';

function HasValidationKey(const Text: String): Boolean;
begin
  Result := (Pos(LEAK_KEY, Text) > 0) or (Pos(LEAK_KEY_ESCAPED, Text) > 0) or
    (Pos(LEAK_KEY_JSON, Text) > 0) or (Pos(LEAK_KEY_JSON_PLAIN, Text) > 0);
end;

function HasWarmupKey(const Text: String): Boolean;
begin
  Result := (Pos(LEAK_WARMUP_KEY, Text) > 0) or (Pos(LEAK_WARMUP_KEY_ESCAPED, Text) > 0) or
    (Pos(LEAK_WARMUP_KEY_JSON, Text) > 0) or (Pos(LEAK_WARMUP_KEY_JSON_PLAIN, Text) > 0);
end;

var
  { The HTTP status the mocked answer of the current sweep carries. }
  SweepStatus: Integer;

procedure ExpectKeyMasked(const Name: String; E: Exception);
var
  Text: String;
begin
  if E = nil then
  begin
    SweepMissed := SweepMissed + Name + ' (returned); ';
    Exit;
  end;
  if not (E is ZbException) then
  begin
    SweepMissed := SweepMissed + Format('%s (%s: %s); ', [Name, E.ClassName, E.Message]);
    Exit;
  end;
  { Every field a caller can read. A Warmup call sends the Warmup key, every
    other call the validation key; each body quotes both. }
  Text := E.Message + ' | ' + ZbException(E).Payload;
  if HasValidationKey(Text) or ((Pos('ZbWarmup', Name) = 1) and HasWarmupKey(Text)) then
    SweepMissed := SweepMissed + Name + ' (the key leaked: ' + Text + '); '
  else if ZbException(E).StatusCode <> SweepStatus then
    SweepMissed := SweepMissed + Format('%s (status %d: %s); ', [Name, ZbException(E).StatusCode, E.Message]);
end;

procedure ExpectKeyMaskedIfRaised(const Name: String; E: Exception);
begin
  { A message-only or detail-only 200 is a result for most calls; where it is
    an error it must not carry the key either. A Warmup CSV download always
    raises: a JSON answer is not a file. }
  if E <> nil then
    ExpectKeyMasked(Name, E)
  else if Pos('ZbWarmupDownload', Name) = 1 then
    SweepMissed := SweepMissed + Name + ' (returned); ';
end;

procedure TTestOffline.TestNoErrorCarriesTheKey;
const
  { A dead-key error and "success": false on a 200, a proxy quoting the
    request URL, a JSON error, and a text one quoting the URL-encoded keys. }
  STATUSES: array[0..5] of Integer = (200, 200, 400, 400, 400, 400);
  TYPES: array[0..5] of String = ('application/json', 'application/json', 'text/plain',
    'application/json', 'text/plain', 'text/plain');
var
  Bodies: array[0..5] of String;
  I: Integer;
  Missed: String;
begin
  { Standard section 3: the key is never in an error. Every call gets each
    body; neither the message nor the Payload may carry the key it sent, raw
    or URL-encoded. }
  { JSON bodies carry the keys JSON-escaped (a JSON string cannot hold a raw
    '"'); the payload keeps them in that form. }
  Bodies[0] := '{"error":"Invalid API key ' + LEAK_KEY_JSON + ' ' + LEAK_WARMUP_KEY_JSON + '"}';
  Bodies[1] := '{"success":false,"message":"Invalid API key ' + LEAK_KEY_JSON_PLAIN + ' ' +
    LEAK_WARMUP_KEY_JSON_PLAIN + '"}';
  Bodies[2] := 'Bad Request: GET https://api.zerobounce.net/v2/validate?api_key=' + LEAK_KEY_ESCAPED +
    '&key=' + LEAK_WARMUP_KEY_ESCAPED;
  Bodies[3] := '{"error":"Bad key ' + LEAK_KEY_JSON + ' ' + LEAK_WARMUP_KEY_JSON + '"}';
  Bodies[4] := 'Rejected key ' + LEAK_KEY_ESCAPED + ' ' + LEAK_WARMUP_KEY_ESCAPED;
  { A proxy quoting the JSON it was sent, and the raw keys. }
  Bodies[5] := 'Bad Request: {"api_key":"' + LEAK_KEY_JSON + '","k":"' + LEAK_KEY_JSON_PLAIN + '","w":"' +
    LEAK_WARMUP_KEY_JSON + '","x":"' + LEAK_WARMUP_KEY_JSON_PLAIN + '"} ' + LEAK_KEY + ' ' + LEAK_WARMUP_KEY;
  Missed := '';
  try
    for I := Low(Bodies) to High(Bodies) do
    begin
      UseDefaultUrls;
      ZBInitialize(LEAK_KEY);
      ZbWarmupInitialize(LEAK_WARMUP_KEY);
      SweepStatus := STATUSES[I];
      ZBMockResponse(STATUSES[I], Bodies[I], TYPES[I]);
      SweepEveryPublicCall(@ExpectKeyMasked);
      if SweepMissed <> '' then
        Missed := Missed + Format('[body %d] %s', [I, SweepMissed]);
    end;
    { A 200 JSON body with only a message or a detail quoting both keys, as
      the Warmup API answers an unknown key. The Warmup CSV downloads raise
      on it outside the request itself. }
    Bodies[0] := '{"message":"Invalid API-Key ' + LEAK_KEY_JSON + ' ' + LEAK_WARMUP_KEY_JSON + ' ' +
      LEAK_WARMUP_KEY_ESCAPED + '"}';
    Bodies[1] := '{"detail":"Invalid API-Key ' + LEAK_KEY_JSON_PLAIN + ' ' + LEAK_WARMUP_KEY_JSON_PLAIN + ' ' +
      LEAK_WARMUP_KEY_ESCAPED + '"}';
    for I := 0 to 1 do
    begin
      UseDefaultUrls;
      ZBInitialize(LEAK_KEY);
      ZbWarmupInitialize(LEAK_WARMUP_KEY);
      SweepStatus := 200;
      ZBMockResponse(200, Bodies[I], 'application/json');
      SweepEveryPublicCall(@ExpectKeyMaskedIfRaised);
      if SweepMissed <> '' then
        Missed := Missed + Format('[200 %s-only body] %s', [Copy('messagedetail', 1 + 7 * I, 7 - I), SweepMissed]);
    end;
    { The account-level batch error is raised from the parsed result. }
    UseDefaultUrls;
    ZBInitialize(LEAK_KEY);
    ZBMockResponse(200, '{"email_batch":[],"errors":[{"email_address":"all","error":"Invalid API key ' +
      LEAK_KEY_JSON + '"}]}', 'application/json');
    try
      ZbBatchValidateEmails(['a@example.com']);
      Missed := Missed + '[batch all] returned; ';
    except
      on E: Exception do
        if HasValidationKey(E.Message) or (Pos('REDACTED', E.Message) = 0) then
          Missed := Missed + '[batch all] ' + E.Message + '; ';
    end;
  finally
    UseDefaultUrls;
  end;
  AssertEquals('calls whose error carried the key, or not the status', '', Missed);
end;

procedure ExpectEmptyBody(const Name: String; E: Exception);
begin
  if Pos('ZbWarmup', Name) = 1 then
    { The Warmup API documents an empty 200 as an empty success. }
    Exit;
  if (SweepStatus < 300) and (Name = 'ZbDeleteInboxPlacementTest') then
  begin
    { DELETE answers with an empty body: a success with an empty result. }
    if E <> nil then
      SweepMissed := SweepMissed + Format('%s (%s: %s); ', [Name, E.ClassName, E.Message]);
  end
  else if Pos('InboxPlacement', Name) > 0 then
  begin
    if E = nil then
      SweepMissed := SweepMissed + Name + ' (returned); '
    else if not (E is ZbException) or (ZbException(E).StatusCode <> SweepStatus) or
            (Pos('Empty response', E.Message) = 0) then
      SweepMissed := SweepMissed + Format('%s (%s: %s); ', [Name, E.ClassName, E.Message]);
  end
  else if (SweepStatus >= 300) and ((E = nil) or not (E is ZbException) or
          (ZbException(E).StatusCode <> SweepStatus)) then
    { An empty non-2xx is an error with its status everywhere. }
    SweepMissed := SweepMissed + Name + ' (not an error with its status); ';
end;

procedure TTestOffline.TestAnEmptyBodyIsASuccessOnlyForAnInboxPlacementDelete;
const
  STATUSES: array[0..2] of Integer = (200, 204, 404);
var
  I: Integer;
  Missed: String;
begin
  { Only a DELETE of an Inbox Placement test answers with an empty body. An
    empty 2xx from the other Inbox Placement calls is an error with its
    status, and an empty non-2xx is an error from every call. }
  Missed := '';
  try
    for I := Low(STATUSES) to High(STATUSES) do
    begin
      UseDefaultUrls;
      SweepStatus := STATUSES[I];
      ZBMockResponse(STATUSES[I], '', 'application/json');
      SweepEveryPublicCall(@ExpectEmptyBody);
      if SweepMissed <> '' then
        Missed := Missed + Format('[HTTP %d] %s', [STATUSES[I], SweepMissed]);
    end;
    UseDefaultUrls;
    ZBMockResponse(200, '', 'application/json');
    AssertEquals('the empty result of a delete', '', ZbDeleteInboxPlacementTest('tid'));
  finally
    UseDefaultUrls;
  end;
  AssertEquals('calls that did not treat an empty body as expected', '', Missed);
end;

procedure ExpectDownloadsToRaise(const Name: String; E: Exception);
begin
  { Only a download takes a file-looking body labelled as JSON for the API's
    error; the raw-JSON calls hand their body over as it is. }
  if (Pos('ResultFetch', Name) > 0) or (Pos('ZbWarmupDownload', Name) = 1) then
    ExpectKeyMasked(Name, E);
end;

procedure TTestOffline.TestTheContentTypeAloneMakesA200AnError;
var
  Missed: String;
begin
  { The content-type half of each guard is what catches these bodies. A 200
    labelled text/html whose body parses as JSON is a proxy's or a portal's
    page: an error with the status on every call. A file-looking body
    labelled application/json is the API's error in place of a file: an
    error with the status on every download (bulk getfile, the Warmup CSVs). }
  Missed := '';
  try
    UseDefaultUrls;
    ZBInitialize('content-type-key');
    ZbWarmupInitialize('content-type-warmup-key');
    SweepStatus := 200;
    ZBMockResponse(200, '{"a":1}', 'text/html; charset=UTF-8');
    SweepEveryPublicCall(@ExpectKeyMasked);
    if SweepMissed <> '' then
      Missed := Missed + '[text/html] ' + SweepMissed;
    UseDefaultUrls;
    ZBInitialize('content-type-key');
    ZbWarmupInitialize('content-type-warmup-key');
    ZBMockResponse(200, 'email'#10'labelled-json@example.com'#10, 'application/json');
    SweepEveryPublicCall(@ExpectDownloadsToRaise);
    if SweepMissed <> '' then
      Missed := Missed + '[application/json] ' + SweepMissed;
  finally
    UseDefaultUrls;
  end;
  AssertEquals('calls that took the answer as a result, or lost the status', '', Missed);
end;

var
  { The content type of the current non-JSON sweep. }
  SweepContentType: String;

procedure ExpectNonJsonRejected(const Name: String; E: Exception);
begin
  { The low-level ZB* request functions hand back the response itself. A
    download takes a body that is not JSON as the file, unless it is
    labelled application/json. Every other call hands over JSON, so a body
    that is not JSON is an error with the status. }
  if Copy(Name, 1, 2) = 'ZB' then
    Exit;
  if ((Pos('ResultFetch', Name) > 0) or (Pos('ZbWarmupDownload', Name) = 1)) and
     (SweepContentType <> 'application/json') then
    Exit;
  ExpectKeyMasked(Name, E);
end;

procedure TTestOffline.TestANonJson200IsAnErrorOnEveryJsonCall;
const
  BODIES: array[0..4] of String = ('not json', 'hello', 'email,status'#10'a@example.com,valid'#10,
    '{"a":1} <- proxy note', '{"a":');
  TYPES: array[0..2] of String = ('application/json', 'text/plain', 'text/csv');
var
  B, T: Integer;
  Missed: String;
begin
  { E7: a 2xx whose body is not JSON (a proxy's text, a CSV labelled
    application/json, a JSON value with text after it) did not come from the
    API: an error with status 200 on every JSON call, the raw-JSON calls
    (filters, list evaluator, ZbListS3Files, Inbox Placement) and the Warmup
    JSON calls included. }
  Missed := '';
  try
    for T := Low(TYPES) to High(TYPES) do
      for B := Low(BODIES) to High(BODIES) do
      begin
        UseDefaultUrls;
        ZBInitialize('non-json-key');
        ZbWarmupInitialize('non-json-warmup-key');
        SweepStatus := 200;
        SweepContentType := TYPES[T];
        ZBMockResponse(200, BODIES[B], TYPES[T]);
        SweepEveryPublicCall(@ExpectNonJsonRejected);
        if SweepMissed <> '' then
          Missed := Missed + Format('[%s %s] %s', [TYPES[T], BODIES[B], SweepMissed]);
      end;
  finally
    UseDefaultUrls;
  end;
  AssertEquals('calls that took a non-JSON 200 as a result, or lost the status', '', Missed);
end;

{ -- E7: every JSON call against the strict-JSON corpus ------------------- }

var
  { The corpus body of the current sweep, and whether it is an ACCEPT one. }
  CorpusBody: String;
  CorpusAccept: Boolean;

function IsDownloadCall(const Name: String): Boolean;
begin
  Result := (Pos('ResultFetch', Name) > 0) or (Pos('ZbWarmupDownload', Name) = 1);
end;

function IsBlankJson(const Text: String): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(Text) do
    if not (Text[I] in [' ', #9, #10, #13]) then
      Exit(False);
  Result := True;
end;

procedure ExpectCorpusOutcome(const Name: String; E: Exception);
var
  Success: Boolean;
begin
  { The low-level ZB* request functions hand back the response itself. }
  if Copy(Name, 1, 2) = 'ZB' then
    Exit;
  { A download takes any application/json answer as the API's error. An
    empty or blank 2xx is a Warmup JSON call's documented empty success, and
    ZbDeleteInboxPlacementTest's (E8). }
  if IsDownloadCall(Name) then
    Success := False
  else if CorpusAccept then
    Success := True
  else
    Success := IsBlankJson(CorpusBody) and
      ((Pos('ZbWarmup', Name) = 1) or (Name = 'ZbDeleteInboxPlacementTest'));
  if Success then
  begin
    { A typed call whose result needs fields an ACCEPT body lacks (a batch's
      email_batch, a status's or the usage's dates) reports that, with status
      200; it read the body as JSON. When the parser's message quotes the
      body (a date of "0"), FromResponse names the body instead. }
    if (E <> nil) and not ((E is ZbException) and (ZbException(E).StatusCode = 200) and
       (Pos('not valid JSON', E.Message) = 0) and
       ((Pos('" not found while parsing', E.Message) > 0) or (Pos('Invalid date/timestamp', E.Message) > 0) or
        (((Pos('ZbGetApiUsage', Name) = 1) or (Pos('Status', Name) > 0)) and
         (E.Message = 'Json Error: ' + Trim(CorpusBody))))) then
      SweepMissed := SweepMissed + Format('%s (%s: %s); ', [Name, E.ClassName, E.Message]);
  end
  else
    ExpectKeyMasked(Name, E);
end;

{ The bodies of tests/fixtures/strict-json-corpus.tsv, a byte-identical copy
  of sdk-docs/skills/zerobounce-sdk/references/strict-json-corpus.tsv in
  zerobounce/sdk-docs (standard section 3, check E7); update it from there.
  Each line is REJECT or ACCEPT, a tab, and the body as a JSON-encoded string. }
function FuzzDecode(const Text: String; How: Integer): String; forward;

procedure ReadStrictJsonCorpus(Kinds, Bodies, Lines: TStringList);
var
  Path, Line, Kind, Field: String;
  Source: TStringList;
  I, Tab: Integer;
begin
  Path := ExtractFilePath(ParamStr(0)) + 'tests' + PathDelim + 'fixtures' + PathDelim + 'strict-json-corpus.tsv';
  if not FileExists(Path) then
    Path := IncludeTrailingPathDelimiter(GetCurrentDir) + 'tests' + PathDelim + 'fixtures' + PathDelim +
      'strict-json-corpus.tsv';
  if not FileExists(Path) then
    raise Exception.Create('tests/fixtures/strict-json-corpus.tsv not found');
  Source := TStringList.Create;
  try
    Source.LoadFromFile(Path);
    for I := 0 to Source.Count - 1 do
    begin
      Line := Source[I];
      if (Line = '') or (Line[1] = '#') then
        Continue;
      Tab := Pos(#9, Line);
      if Tab = 0 then
        raise Exception.Create('no tab in corpus line: ' + Line);
      Kind := Copy(Line, 1, Tab - 1);
      if (Kind <> 'ACCEPT') and (Kind <> 'REJECT') then
        raise Exception.Create('bad corpus line: ' + Line);
      { The field is one JSON string. fpjson's GetJSON ends a string at an
        escaped NUL, so it is unescaped here instead. }
      Field := Copy(Line, Tab + 1, MaxInt);
      if (Length(Field) < 2) or (Field[1] <> '"') or (Field[Length(Field)] <> '"') then
        raise Exception.Create('bad corpus field: ' + Line);
      Kinds.Add(Kind);
      Bodies.Add(FuzzDecode(Copy(Field, 2, Length(Field) - 2), 2));
      Lines.Add(Line);
    end;
  finally
    Source.Free;
  end;
end;

procedure TTestOffline.TestEveryJsonCallTakesOnlyStrictJson;
var
  Kinds, Bodies, Lines: TStringList;
  I: Integer;
  Missed: String;
begin
  { Every call against every corpus body on an HTTP 200 application/json: a
    REJECT body is an error with status 200, an ACCEPT body the result. }
  Kinds := TStringList.Create;
  Bodies := TStringList.Create;
  Lines := TStringList.Create;
  Missed := '';
  try
    ReadStrictJsonCorpus(Kinds, Bodies, Lines);
    { 42 REJECT and 9 ACCEPT lines when this was written. }
    AssertTrue(Format('only %d corpus lines', [Bodies.Count]), Bodies.Count >= 51);
    { The NUL line decodes whole: the body is the object and a NUL byte. }
    AssertTrue('the corpus has the NUL body', Bodies.IndexOf('{"a":1}'#0) >= 0);
    for I := 0 to Bodies.Count - 1 do
    begin
      UseDefaultUrls;
      ZBInitialize('corpus-key');
      ZbWarmupInitialize('corpus-warmup-key');
      SweepStatus := 200;
      CorpusBody := Bodies[I];
      CorpusAccept := Kinds[I] = 'ACCEPT';
      ZBMockResponse(200, Bodies[I], 'application/json');
      SweepEveryPublicCall(@ExpectCorpusOutcome);
      if SweepMissed <> '' then
        Missed := Missed + Format('[%s] %s', [Lines[I], SweepMissed]);
    end;
  finally
    UseDefaultUrls;
    Kinds.Free;
    Bodies.Free;
    Lines.Free;
  end;
  AssertEquals('calls that took a REJECT body or refused an ACCEPT one', '', Missed);
end;

{ -- S4/S7: redaction is closed under encoding ---------------------------- }

const
  { Every character JSON or URL encoding changes, and a space. }
  FUZZ_KEY = 'fz a"b/c\d+e%f&g=h<i>j''k';
  FUZZ_WARMUP_KEY = 'wz a"b/c\d+e%f&g=h<i>j''k';
  FUZZ_SMTP_USER = 'us a"b/c\d+e%f&g=h<i>j''k';
  FUZZ_SMTP_PASSWORD = 'pw a"b/c\d+e%f&g=h<i>j''k';

var
  FuzzSeed: Cardinal;
  FuzzDesc: String;
  { The secret the current fuzz sweep checks, besides FUZZ_KEY. }
  FuzzSecret: String;

function FuzzNext: Cardinal;
begin
  { A fixed linear congruential generator, so a failure repeats. }
  FuzzSeed := FuzzSeed * 1664525 + 1013904223;
  Result := FuzzSeed shr 8;
end;

{ One encoding a server or proxy may apply: URL-encoding with a space as %20
  (0) or '+' (1), JSON escaping (2), JSON escaping with '/' as '\/' (3). }
function FuzzEncode(const Text: String; How: Integer): String;
const
  HEX = '0123456789ABCDEF';
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(Text) do
  begin
    C := Text[I];
    if How >= 2 then
    begin
      if (C = '"') or (C = '\') or ((How = 3) and (C = '/')) then
        Result := Result + '\';
      Result := Result + C;
    end
    else if C in ['A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~'] then
      Result := Result + C
    else if (C = ' ') and (How = 1) then
      Result := Result + '+'
    else
      Result := Result + '%' + HEX[(Ord(C) shr 4) + 1] + HEX[(Ord(C) and 15) + 1];
  end;
end;

function FuzzHex(C: Char): Integer;
begin
  case C of
    '0'..'9': Result := Ord(C) - Ord('0');
    'a'..'f': Result := Ord(C) - Ord('a') + 10;
    'A'..'F': Result := Ord(C) - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

{ One decoding: %XX (0), %XX and '+' (1), JSON unescaping (2), written apart
  from the SDK's own. }
function FuzzDecode(const Text: String; How: Integer): String;
const
  FROM_CHARS = '"\/bfnrt';
var
  I, K, CodePoint: Integer;
  ToChars: String;
begin
  ToChars := '"\/'#8#12#10#13#9;
  Result := '';
  I := 1;
  while I <= Length(Text) do
  begin
    if (How < 2) and (Text[I] = '%') and (I + 2 <= Length(Text)) and (FuzzHex(Text[I + 1]) >= 0) and
       (FuzzHex(Text[I + 2]) >= 0) then
    begin
      Result := Result + Chr(FuzzHex(Text[I + 1]) * 16 + FuzzHex(Text[I + 2]));
      Inc(I, 3);
    end
    else if (How = 1) and (Text[I] = '+') then
    begin
      Result := Result + ' ';
      Inc(I);
    end
    else if (How = 2) and (Text[I] = '\') and (I < Length(Text)) and (Pos(Text[I + 1], FROM_CHARS) > 0) then
    begin
      Result := Result + ToChars[Pos(Text[I + 1], FROM_CHARS)];
      Inc(I, 2);
    end
    else if (How = 2) and (Text[I] = '\') and (I + 5 <= Length(Text)) and (Text[I + 1] = 'u') and
            (FuzzHex(Text[I + 2]) >= 0) and (FuzzHex(Text[I + 3]) >= 0) and (FuzzHex(Text[I + 4]) >= 0) and
            (FuzzHex(Text[I + 5]) >= 0) then
    begin
      CodePoint := 0;
      for K := 2 to 5 do
        CodePoint := CodePoint * 16 + FuzzHex(Text[I + K]);
      if CodePoint < $80 then
        Result := Result + Chr(CodePoint)
      else if CodePoint < $800 then
        Result := Result + Chr($C0 or (CodePoint shr 6)) + Chr($80 or (CodePoint and $3F))
      else
        Result := Result + Chr($E0 or (CodePoint shr 12)) + Chr($80 or ((CodePoint shr 6) and $3F)) +
          Chr($80 or (CodePoint and $3F));
      Inc(I, 6);
    end
    else
    begin
      Result := Result + Text[I];
      Inc(I);
    end;
  end;
end;

{ Whether Secret is in Text or in any view of it decoded up to Layers times,
  in any order. }
function FuzzSecretReadable(const Text, Secret: String; Layers: Integer): Boolean;
var
  How: Integer;
begin
  Result := Pos(Secret, Text) > 0;
  if Result or (Layers = 0) then
    Exit;
  for How := 0 to 2 do
    if FuzzSecretReadable(FuzzDecode(Text, How), Secret, Layers - 1) then
      Exit(True);
end;

{ A random composition of up to three encodings of Secret; FuzzDesc gets the
  sequence, e.g. "2,0". }
function FuzzSpelling(const Secret: String): String;
var
  Layers, I, How: Integer;
begin
  Result := Secret;
  FuzzDesc := '';
  Layers := FuzzNext mod 4;
  for I := 1 to Layers do
  begin
    How := FuzzNext mod 4;
    Result := FuzzEncode(Result, How);
    if FuzzDesc <> '' then
      FuzzDesc := FuzzDesc + ',';
    FuzzDesc := FuzzDesc + IntToStr(How);
  end;
end;

{ A body quoting a spelling with a 4xx: a JSON error (one more JSON escape,
  which the SDK's parser undoes) or a text one. }
procedure MockFuzzedEcho(const Spelling: String; Round: Integer);
begin
  if Odd(Round) then
    ZBMockResponse(400, '{"error":"Invalid API key ' + FuzzEncode(Spelling, 2) + '"}', 'application/json')
  else
    ZBMockResponse(400, 'Bad Request: [' + Spelling + '] end', 'text/plain');
end;

procedure ExpectNoDecodableSecret(const Name: String; E: Exception);
var
  Text: String;
begin
  if Copy(Name, 1, 2) = 'ZB' then
    Exit;
  if E = nil then
  begin
    SweepMissed := SweepMissed + Name + ' (returned); ';
    Exit;
  end;
  Text := E.Message;
  if E is ZbException then
    Text := Text + ' | ' + ZbException(E).Payload;
  if FuzzSecretReadable(Text, FUZZ_KEY, 3) or
     ((Pos('ZbWarmup', Name) = 1) and FuzzSecretReadable(Text, FuzzSecret, 3)) then
    SweepMissed := SweepMissed + Format('%s, encodings [%s]: %s; ', [Name, FuzzDesc, Text]);
end;

procedure TTestOffline.TestRedactionIsClosedUnderEncoding;
var
  Round: Integer;
  Spelling, Text, Missed: String;
begin
  { Standard section 3: whatever composition of up to three URL and JSON
    encodings a body uses to quote the API key, the Warmup key or the SMTP
    credentials, no decoded view of the message or the Payload of any error
    holds it. }
  Missed := '';
  FuzzSeed := 20261112;
  try
    for Round := 0 to 39 do
    begin
      UseDefaultUrls;
      ZBInitialize(FUZZ_KEY);
      ZbWarmupInitialize(FUZZ_WARMUP_KEY);
      FuzzSecret := FUZZ_WARMUP_KEY;
      { Each body quotes the key a non-Warmup call sends, or the Warmup key. }
      if Odd(Round div 2) then
        Spelling := FuzzSpelling(FUZZ_WARMUP_KEY)
      else
        Spelling := FuzzSpelling(FUZZ_KEY);
      MockFuzzedEcho(Spelling, Round);
      SweepEveryPublicCall(@ExpectNoDecodableSecret);
      if SweepMissed <> '' then
        Missed := Missed + SweepMissed;
    end;
    for Round := 0 to 199 do
    begin
      UseDefaultUrls;
      ZbWarmupInitialize('fuzz-smtp-warmup-key');
      if Round mod 3 = 0 then
        FuzzSecret := FUZZ_SMTP_USER
      else
        FuzzSecret := FUZZ_SMTP_PASSWORD;
      Spelling := FuzzSpelling(FuzzSecret);
      MockFuzzedEcho(Spelling, Round);
      try
        ZbWarmupTestSmtpConfiguration('s@example.com', FUZZ_SMTP_USER, FUZZ_SMTP_PASSWORD,
          'smtp.example.com', 587);
        Missed := Missed + 'ZbWarmupTestSmtpConfiguration (returned); ';
      except
        on E: Exception do
        begin
          Text := E.Message;
          if E is ZbException then
            Text := Text + ' | ' + ZbException(E).Payload;
          if FuzzSecretReadable(Text, FuzzSecret, 3) then
            Missed := Missed + Format('ZbWarmupTestSmtpConfiguration, encodings [%s]: %s; ', [FuzzDesc, Text]);
        end;
      end;
    end;
  finally
    UseDefaultUrls;
  end;
  AssertEquals('errors with a secret readable in a decoded view', '', Missed);
end;

procedure TTestOffline.TestABodyStillHoldingTheKeyAfterMaskingIsDropped;
var
  Thrice: String;
begin
  { A spelling no list of generated forms covers (three URL layers) is
    dropped whole, with a message that says why; text without the secret is
    kept as it is. }
  Thrice := FuzzEncode(FuzzEncode(FuzzEncode(FUZZ_KEY, 0), 0), 1);
  AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('echo [' + Thrice + '] end', FUZZ_KEY));
  AssertEquals('nothing secret here %41 A', ZbMaskSecret('nothing secret here %41 A', FUZZ_KEY));
end;

initialization
  Randomize;
  SweepNames := TStringList.Create;
  SweepOverloads := TStringList.Create;
  RegisterTest(TTestOffline);

finalization
  if Stub <> nil then
  begin
    Stub.Terminate;
    WakeStubAccept(Stub.Port);
    Stub.WaitFor;
    Stub.Free;
  end;
  Gate.Free;
  SweepNames.Free;
  SweepOverloads.Free;

end.
