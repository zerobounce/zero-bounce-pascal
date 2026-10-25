unit TestOffline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ZbUtility, ZbGeneric, ZbValidation, ZbBulk, ZbPriority3, ZbStructures;

type
  TTestOffline = class(TTestCase)
  published
    procedure TestEndpointsAndOptions;
    procedure TestRejectsBeforeRequest;
    procedure TestErrorBodies;
    procedure TestBatchAccountAndPartial;
    procedure TestConcurrentKeys;
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
begin
  Code := '200';
  ContentType := 'application/json';
  Body := '{"success":true,"message":"ok","file_id":"' + Item.ApiKey + '","file_name":"f.csv","email":"' +
    Item.ApiKey + '","domain":"' + Item.ApiKey + '","Credits":"7","total":1,"found":true,"active_in_days":"3","score":10,' +
    '"address":"' + Item.ApiKey + '","status":"valid","processed_at":"2018-01-01 00:00:00.000","other_domain_formats":[],' +
    '"start_date":"1/1/2018","end_date":"1/31/2018","upload_date":"2018-01-01T00:00:00","complete_percentage":"100%","file_status":"Complete"}';
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
  Result := 'HTTP/1.1 ' + Code + ' OK' + #13#10 +
    'Content-Type: ' + ContentType + #13#10 +
    'Content-Length: ' + IntToStr(Length(Body)) + #13#10 +
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
  ZbResponseMock := cDefaultMock;
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

initialization
  Randomize;
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

end.
