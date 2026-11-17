unit TestBehaviour;

{ Offline tests for the audit 1 behaviour fixes: downloads that start with
  padding, JSON served as a file, message-only and empty answers, the batch
  "all" error, redaction of echoed secrets, refused keys and base URLs, and
  replacing a downloaded file.

  Every request is answered from ZbResponseMock; nothing here reaches the live
  API. }

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry,
    BaseTest, ZbUtility, ZbStructures, ZbGeneric, ZbValidation, ZbBulk, ZbPriority3, ZbWarmup;

type
    TTestBehaviour = class(TBaseTestCase)
    protected
        SavePath: String;
        procedure SetUp; override;
        procedure TearDown; override;
        procedure ResetUrls;
        { Runs download Index (0 validation, 1 scoring, 2 email finder, 3 domain
          search, 4 Warmup seeds, 5 Warmup statistics) against a 200 with the
          body and content type. True when it raised; then Status and Text say
          what with. Otherwise Content is the file as it came back. }
        function Download(Index: Integer; const Body, ContentType: String;
            out Content, Text: String; out Status: Integer): Boolean;
        { Runs call Index of the no-result family. True when it raised. }
        function NoResultCall(Index: Integer; const Body: String; out Text: String;
            out Status: Integer): Boolean;
        function ReadFile(const Path: String): String;
    published
        procedure TestContentStartSkipsEveryKindOfPadding;
        procedure TestHtmlAfterPaddingIsRefusedOnEverySixDownloads;
        procedure TestAPaddingOnlyBodyIsAnErrorNotAFile;
        procedure TestHeadDoesNotMatchHeader;
        procedure TestJsonAfterPaddingIsAnErrorOnEverySixDownloads;
        procedure TestAJsonContentTypeInAnyCaseIsAnError;
        procedure TestAFileThatBeginsWithPaddingIsSavedUnchanged;
        procedure TestMessageOnlyAndEmptyAnswersAreErrorsWithStatus200;
        procedure TestAMessageOnlyValidateAnswerRaisesWithTheMessageText;
        procedure TestAMessageBesideNullFieldsIsAnError;
        procedure TestFinderAndDomainSearchBodies;
        procedure TestCreditsWithASignOrASpaceIsAnError;
        procedure TestAnActivityAnswerWithNoDaysIsMinusOne;
        procedure TestAFileStatusNeedsAKnownField;
        procedure TestResultsThatMustStayResults;
        procedure TestTheBatchAllErrorCarriesTheStatusAndTheBody;
        procedure TestAnUpperCasedEchoOfEverySecretIsMasked;
        procedure TestAnEchoOfTheSharpSIsMasked;
        procedure TestEveryOtherEscapeSpellingOfASecretIsDropped;
        procedure TestHtmlEntitiesOfASecretAreDroppedInAnyCase;
        procedure TestWhatIsNotAnEscapeIsNotDecoded;
        procedure TestKeysWithUnicodeWhiteSpaceOrInvisibleCharactersAreRefused;
        procedure TestKeysThatAreNotValidUtf8AreRefused;
        procedure TestPlainKeysStillPass;
        procedure TestABaseUrlWithAQueryOrFragmentIsRefusedBeforeSending;
        procedure TestAnIpv6BaseUrlStaysAHost;
        procedure TestADownloadReplacesAFileAlreadyThere;
        procedure TestAFailedReplaceKeepsWhatWasThere;
        procedure TestTheFoldCoversWhatAnEchoChanges;
        procedure TestMoreInvisibleCodePointsAreRefused;
        procedure TestAShortMessageWithAnAngleBracketIsKept;
        procedure TestALongJsonBodyIsNotCalledInvalid;
        procedure TestProcessedAtKeepsItsDayAndItsMilliseconds;
        procedure TestActivityDaysAreDecimalTextOnly;
        procedure TestCreditsRaisesOnAMessageAlone;
        procedure TestANullSuccessKeyDoesNotHideAMessage;
        procedure TestCompletePercentageParsesWithADotInACommaLocale;
        procedure TestJsonWrappersFreeWhatTheyCreate;
        procedure TestAnIntegerOutsideIntegerRangeIsAFieldTypeError;
        procedure TestAShortApiUsageDateIsAnErrorNotLeftoverStack;
        procedure TestProcessedAtAcceptsTAsTheSeparator;
        procedure TestAFractionIsADecimalOfSeconds;
        procedure TestABlankCompletePercentageIsMinusOne;
        procedure TestJsonKeysAreLookedUpExactly;
        procedure TestGetFileErrorKeysInAnyCase;
        procedure TestActivityDaysAcceptsAJsonInteger;
        procedure TestTheSuccessFlagSet;
        procedure TestASuccessThatIsNoFlagHidesNothing;
        procedure TestARealFalseFlagInAnySpellingIsAFailure;
        procedure TestErrorTextOfALongObjectAndOfAList;
    end;

implementation

const
    BOM = #$EF#$BB#$BF;
    NBSP = #$C2#$A0;
    CSV_TYPE = 'text/csv';
    HTML_PAGE = '<!DOCTYPE html><html><head><title>Access denied</title></head><body>blocked</body></html>';
    CSV_FILE = 'email,name' + #13#10 + 'a@example.com,Ada' + #13#10;
    DOWNLOAD_NAMES: array[0..5] of String = (
        'ZbBulkValidationResultFetch', 'ZbAiScoringResultFetch', 'ZbEmailFinderResultFetch',
        'ZbDomainSearchResultFetch', 'ZbWarmupDownloadSeeds', 'ZbWarmupDownloadStatistics'
    );
    NO_RESULT_NAMES: array[0..39] of String = (
        'ZbBulkValidationFileSubmit', 'ZbAiScoringFileSubmit', 'ZbBulkValidationResultDelete',
        'ZbAiScoringResultDelete', 'ZbBulkValidationFileStatusCheck', 'ZbAiScoringFileStatusCheck',
        'ZbEmailFinderFileStatus', 'ZbDomainSearchFileStatus', 'ZbValidateS3File', 'ZbScoreS3File',
        'ZbEmailFinderS3File', 'ZbDomainSearchS3File', 'ZbValidateSFTPFile', 'ZbScoreSFTPFile',
        'ZbEmailFinderSFTPFile', 'ZbDomainSearchSFTPFile', 'ZbExportS3File', 'ZbExportSFTPFile',
        'ZbSendEvaluatedFile', 'ZbBatchValidateEmails', 'ZbActivityData', 'ZbScoreEmail',
        'ZbScoreEmailPost', 'ZbGetApiUsage', 'ZbSendEmailFinderFile', 'ZbSendDomainSearchFile',
        'ZbSendListEvaluatorFile', 'ZbEmailFinderResultDelete', 'ZbDomainSearchResultDelete',
        'ZbFindEmailByDomain4', 'ZbFindEmailByDomain3', 'ZbFindEmailByDomain2',
        'ZbFindEmailByCompanyName4', 'ZbFindEmailByCompanyName3', 'ZbFindEmailByCompanyName2',
        'ZbDomainSearchByDomain', 'ZbDomainSearchByCompanyName', 'ZbFindEmail4', 'ZbFindEmail3',
        'ZbDomainSearch'
    );
    { The finder and domain-search calls: a message-only answer is an error, a
      body of nulls with no message is a result. }
    FINDER_FIRST = 29;
    FINDER_LAST = 39;
    { The calls of the family that read a file status. }
    STATUS_FIRST = 4;
    STATUS_LAST = 7;

function Pad(const Unit_: String; Count: Integer): String;
var
    I: Integer;
begin
    Result := '';
    for I := 1 to Count do
        Result := Result + Unit_;
end;

procedure TTestBehaviour.ResetUrls;
begin
    ZBInitialize('mock_api_key');
    BULK_URI := 'https://bulkapi.zerobounce.net/v2';
    ZbBulkRootURL := 'https://bulkapi.zerobounce.net';
    ZbListEvaluatorURL := 'https://bulkapi.zerobounce.net/v2/listevaluator';
    ZbInboxPlacementURL := 'https://bulkapi.zerobounce.net/api/ipt';
    ZbWarmupBaseURL := 'https://warmup-api.zerobounce.net';
    ZbResponseMock := cDefaultMock;
end;

procedure TTestBehaviour.SetUp;
begin
    inherited SetUp;
    ResetUrls;
    ZbWarmupInitialize('mock_warmup_key');
    SavePath := GetTempDir + 'zb-behaviour-' + IntToStr(GetProcessID) + '.csv';
    DeleteFile(SavePath);
end;

procedure TTestBehaviour.TearDown;
begin
    DeleteFile(SavePath);
    ZbWarmupApiKey := '';
    ResetUrls;
    inherited TearDown;
end;

function TTestBehaviour.ReadFile(const Path: String): String;
var
    Stream: TFileStream;
begin
    Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
    try
        SetLength(Result, Stream.Size);
        if Stream.Size > 0 then
            Stream.ReadBuffer(Result[1], Stream.Size);
    finally
        Stream.Free;
    end;
end;

function TTestBehaviour.Download(Index: Integer; const Body, ContentType: String;
    out Content, Text: String; out Status: Integer): Boolean;
var
    Fetched: TZbBulkResponse;
begin
    Result := False;
    Content := '';
    Text := '';
    Status := -1;
    DeleteFile(SavePath);
    ZBMockResponse(200, Body, ContentType);
    try
        case Index of
            0: Fetched := ZbBulkValidationResultFetch('fid');
            1: Fetched := ZbAiScoringResultFetch('fid');
            2: Fetched := ZbEmailFinderResultFetch('fid');
            3: Fetched := ZbDomainSearchResultFetch('fid');
            4: Content := ZbWarmupDownloadSeeds(SavePath);
            5: Content := ZbWarmupDownloadStatistics('s@example.com', SavePath);
        end;
        if Index <= 3 then
        begin
            AssertTrue(DOWNLOAD_NAMES[Index] + ' returned no content', Fetched.HasContent);
            Content := Fetched.Content;
        end;
    except
        on E: ZbException do
        begin
            Result := True;
            Text := E.Message;
            Status := E.StatusCode;
        end;
    end;
end;

procedure TTestBehaviour.TestContentStartSkipsEveryKindOfPadding;
begin
    AssertEquals('no padding', 1, ZbContentStart('abc'));
    AssertEquals('empty', 1, ZbContentStart(''));
    AssertEquals('space tab CR LF VT FF', 7, ZbContentStart(' ' + #9 + #13 + #10 + #11 + #12 + 'x'));
    AssertEquals('BOM', 4, ZbContentStart(BOM + 'x'));
    AssertEquals('NBSP', 3, ZbContentStart(NBSP + 'x'));
    AssertEquals('mixed', 8, ZbContentStart(BOM + ' ' + NBSP + #10 + 'x'));
    AssertEquals('only padding', 6, ZbContentStart('  ' + BOM));
    { A lone byte of a BOM or of a no-break space is not padding. }
    AssertEquals('half a BOM', 1, ZbContentStart(#$EF#$BB + 'x'));
    AssertEquals('half an NBSP', 1, ZbContentStart(#$C2 + 'x'));
    AssertTrue('html after 1999 spaces', ZbBodyLooksLikeHtml(Pad(' ', 1999) + HTML_PAGE));
    AssertTrue('html after 2500 spaces', ZbBodyLooksLikeHtml(Pad(' ', 2500) + HTML_PAGE));
    AssertTrue('html after 2500 CRLF pairs', ZbBodyLooksLikeHtml(Pad(#13#10, 2500) + HTML_PAGE));
    AssertTrue('html after a BOM and 2500 NBSP', ZbBodyLooksLikeHtml(BOM + Pad(NBSP, 2500) + HTML_PAGE));
    AssertFalse('csv after 2500 spaces', ZbBodyLooksLikeHtml(Pad(' ', 2500) + CSV_FILE));
end;

procedure TTestBehaviour.TestHtmlAfterPaddingIsRefusedOnEverySixDownloads;
var
    Pads: array[0..6] of String;
    Index, P: Integer;
    Content, Text: String;
    Status: Integer;
begin
    Pads[0] := Pad(' ', 1999);
    Pads[1] := Pad(' ', 2500);
    Pads[2] := Pad(#13#10, 2500);
    Pads[3] := BOM + Pad(' ', 2500);
    Pads[4] := Pad(NBSP, 2500);
    Pads[5] := Pad(#9#11#12, 1000);
    Pads[6] := Pad(' ', 5000);
    for P := Low(Pads) to High(Pads) do
        for Index := 0 to 5 do
        begin
            AssertTrue(Format('%s after padding %d should be refused', [DOWNLOAD_NAMES[Index], P]),
                Download(Index, Pads[P] + HTML_PAGE, CSV_TYPE, Content, Text, Status));
            AssertEquals(Format('%s status after padding %d', [DOWNLOAD_NAMES[Index], P]), 200, Status);
            AssertFalse('no file is saved', FileExists(SavePath));
        end;
end;

procedure TTestBehaviour.TestAPaddingOnlyBodyIsAnErrorNotAFile;
var
    Bodies: array[0..5] of String;
    B, Index: Integer;
    Content, Text: String;
    Status: Integer;
begin
    Bodies[0] := Pad(' ', 3000);
    Bodies[1] := BOM;
    Bodies[2] := NBSP;
    Bodies[3] := BOM + NBSP + Pad(#13#10, 20) + #9#11#12;
    Bodies[4] := Pad(#13#10, 2500);
    Bodies[5] := ' ';
    for B := Low(Bodies) to High(Bodies) do
        for Index := 0 to 5 do
        begin
            AssertTrue(Format('%s with padding only %d should be an error', [DOWNLOAD_NAMES[Index], B]),
                Download(Index, Bodies[B], CSV_TYPE, Content, Text, Status));
            AssertEquals(Format('%s status for padding only %d', [DOWNLOAD_NAMES[Index], B]), 200, Status);
            AssertTrue(Format('%s message for padding only %d: %s', [DOWNLOAD_NAMES[Index], B, Text]),
                Pos('Empty response', Text) > 0);
            AssertFalse('no file is saved', FileExists(SavePath));
        end;
end;

procedure TTestBehaviour.TestHeadDoesNotMatchHeader;
var
    Index: Integer;
    Content, Text: String;
    Status: Integer;
    Body: String;
begin
    Text := '';
    { A CSV whose text mentions a header tag is not an HTML page. }
    Body := 'email,note' + #10 + 'a@example.com,<header>' + #10;
    for Index := 0 to 5 do
    begin
        AssertFalse(DOWNLOAD_NAMES[Index] + ' refused <header>: ' + Text,
            Download(Index, Body, CSV_TYPE, Content, Text, Status));
        AssertEquals(DOWNLOAD_NAMES[Index] + ' content', Body, Content);
    end;
    AssertFalse('<header>', ZbBodyLooksLikeHtml('x <header>y</header>'));
    AssertFalse('<headline', ZbBodyLooksLikeHtml('<headline>'));
    AssertFalse('<bodyguard', ZbBodyLooksLikeHtml('<bodyguard>'));
    AssertFalse('<htmlx', ZbBodyLooksLikeHtml('<htmlx>'));
    AssertTrue('<head>', ZbBodyLooksLikeHtml('x <head>'));
    AssertTrue('<head with a space', ZbBodyLooksLikeHtml('x <HEAD lang="en">'));
    AssertTrue('<head with a newline', ZbBodyLooksLikeHtml('x <head' + #10 + 'lang="en">'));
    AssertTrue('<head/', ZbBodyLooksLikeHtml('x <head/>'));
    AssertTrue('<body>', ZbBodyLooksLikeHtml('x <body>'));
    AssertTrue('<html>', ZbBodyLooksLikeHtml('x <html>'));
    AssertTrue('doctype', ZbBodyLooksLikeHtml('<!DOCTYPE html>'));
    { The marker at the very end of the sniffed text ends its tag. }
    AssertTrue('<head at the end', ZbBodyLooksLikeHtml('x <head'));
end;

procedure TTestBehaviour.TestJsonAfterPaddingIsAnErrorOnEverySixDownloads;
var
    Bodies: array[0..8] of String;
    Types: array[0..2] of String;
    B, T, Index: Integer;
    Content, Text: String;
    Status: Integer;
begin
    Bodies[0] := BOM + '{"message":"nope"}';
    Bodies[1] := NBSP + '{"message":"nope"}';
    Bodies[2] := '[1,2,3]';
    Bodies[3] := '  ' + BOM + '[{"a":1}]';
    Bodies[4] := Pad(' ', 2500) + '{"a":1}';
    Bodies[5] := Pad(#13#10, 2500) + '[]';
    Bodies[6] := '{"success":true,"file_id":"x"}';
    Bodies[7] := NBSP + Pad(' ', 3000) + '[]';
    Bodies[8] := BOM + BOM + '{}';
    Types[0] := CSV_TYPE;
    Types[1] := 'application/octet-stream';
    Types[2] := 'text/plain; charset=utf-8';
    for B := Low(Bodies) to High(Bodies) do
        for T := Low(Types) to High(Types) do
            for Index := 0 to 5 do
            begin
                AssertTrue(Format('%s took %d under %s as a file', [DOWNLOAD_NAMES[Index], B, Types[T]]),
                    Download(Index, Bodies[B], Types[T], Content, Text, Status));
                AssertEquals(Format('%s status for %d', [DOWNLOAD_NAMES[Index], B]), 200, Status);
                AssertFalse('no file is saved', FileExists(SavePath));
            end;
    { The API's own message comes through, from behind a BOM too. }
    AssertTrue(Download(0, Bodies[0], CSV_TYPE, Content, Text, Status));
    AssertTrue(Text, Pos('nope', Text) > 0);
end;

procedure TTestBehaviour.TestAJsonContentTypeInAnyCaseIsAnError;
const
    TYPES: array[0..8] of String = (
        'Application/JSON', 'APPLICATION/JSON; charset=UTF-8', 'application/problem+json',
        'Application/Vnd.Api+JSON', 'text/json', 'TEXT/JSON', 'application/json',
        ' application/json ', 'application/ld+json; profile="x"'
    );
    BODIES: array[0..2] of String = ('"just a string"', 'email' + #10 + 'a@example.com' + #10, '42');
var
    T, B, Index: Integer;
    Content, Text: String;
    Status: Integer;
begin
    Text := '';
    for T := Low(TYPES) to High(TYPES) do
        for B := Low(BODIES) to High(BODIES) do
            for Index := 0 to 5 do
            begin
                AssertTrue(Format('%s took %s under %s as a file', [DOWNLOAD_NAMES[Index], BODIES[B], TYPES[T]]),
                    Download(Index, BODIES[B], TYPES[T], Content, Text, Status));
                AssertEquals(DOWNLOAD_NAMES[Index] + ' status', 200, Status);
            end;
    AssertTrue('application/json', ZbContentTypeIsJson('application/json'));
    AssertTrue('+json', ZbContentTypeIsJson('application/x.y+JSON'));
    AssertTrue('text/json', ZbContentTypeIsJson('Text/Json; charset=utf-8'));
    AssertFalse('text/csv', ZbContentTypeIsJson('text/csv'));
    AssertFalse('application/jsonl', ZbContentTypeIsJson('application/jsonl'));
    AssertFalse('x-ndjson', ZbContentTypeIsJson('application/x-ndjson'));
    AssertFalse('empty', ZbContentTypeIsJson(''));
    { Other types stay files. }
    for Index := 0 to 5 do
    begin
        AssertFalse(DOWNLOAD_NAMES[Index] + ' refused a csv: ' + Text,
            Download(Index, CSV_FILE, 'application/x-ndjson', Content, Text, Status));
        AssertEquals(DOWNLOAD_NAMES[Index] + ' content', CSV_FILE, Content);
    end;
end;

procedure TTestBehaviour.TestAFileThatBeginsWithPaddingIsSavedUnchanged;
var
    Files: array[0..4] of String;
    F, Index: Integer;
    Content, Text: String;
    Status: Integer;
begin
    Text := '';
    Files[0] := Pad(' ', 3000) + CSV_FILE;
    Files[1] := '   ' + CSV_FILE;
    Files[2] := BOM + CSV_FILE;
    Files[3] := Pad(#13#10, 2500) + CSV_FILE;
    Files[4] := NBSP + 'email' + #10 + 'a@example.com' + #10;
    for F := Low(Files) to High(Files) do
        for Index := 0 to 5 do
        begin
            AssertFalse(Format('%s refused file %d: %s', [DOWNLOAD_NAMES[Index], F, Text]),
                Download(Index, Files[F], CSV_TYPE, Content, Text, Status));
            AssertEquals(Format('%s content %d', [DOWNLOAD_NAMES[Index], F]), Files[F], Content);
            if Index >= 4 then
                AssertEquals(Format('%s saved file %d', [DOWNLOAD_NAMES[Index], F]), Files[F], ReadFile(SavePath));
        end;
end;

function TTestBehaviour.NoResultCall(Index: Integer; const Body: String; out Text: String;
    out Status: Integer): Boolean;
var
    Params: TZbBulkParams;
    Finder: TZbEmailFinderSendFileOptions;
    S3: TZbValidateS3FileOptions;
    Score: TZbScoreS3FileOptions;
    Job: TZbBulkFileJobOptions;
    Emails: array[0..0] of TZbEmailAndIp;
begin
    Result := False;
    Text := '';
    Status := -1;
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
    ZBMockResponse(200, Body, 'application/json');
    try
        case Index of
            0: ZbBulkValidationFileSubmit('email', Params);
            1: ZbAiScoringFileSubmit('email', Params);
            2: ZbBulkValidationResultDelete('fid');
            3: ZbAiScoringResultDelete('fid');
            4: ZbBulkValidationFileStatusCheck('fid');
            5: ZbAiScoringFileStatusCheck('fid');
            6: ZbEmailFinderFileStatus('fid');
            7: ZbDomainSearchFileStatus('fid');
            8: ZbValidateS3File(S3);
            9: ZbScoreS3File(Score);
            10: ZbEmailFinderS3File(Job);
            11: ZbDomainSearchS3File(Job);
            12: ZbValidateSFTPFile(Job);
            13: ZbScoreSFTPFile(Job);
            14: ZbEmailFinderSFTPFile(Job);
            15: ZbDomainSearchSFTPFile(Job);
            16: ZbExportS3File('fid', '');
            17: ZbExportSFTPFile('fid', '');
            18: ZbSendEvaluatedFile('fid');
            19: ZbBatchValidateEmails(Emails);
            20: ZbActivityData('a@example.com');
            21: ZbScoreEmail('a@example.com');
            22: ZbScoreEmailPost('a@example.com');
            23: ZbGetApiUsage;
            24: ZbSendEmailFinderFile('a', 1, @Finder);
            25: ZbSendDomainSearchFile('a', 1, nil);
            26: ZbSendListEvaluatorFile('a', 1);
            27: ZbEmailFinderResultDelete('fid');
            28: ZbDomainSearchResultDelete('fid');
            29: ZbFindEmailByDomain('example.com', 'A', 'B', 'C');
            30: ZbFindEmailByDomain('example.com', 'A', 'C');
            31: ZbFindEmailByDomain('example.com', 'A');
            32: ZbFindEmailByCompanyName('Example', 'A', 'B', 'C');
            33: ZbFindEmailByCompanyName('Example', 'A', 'C');
            34: ZbFindEmailByCompanyName('Example', 'A');
            35: ZbDomainSearchByDomain('example.com');
            36: ZbDomainSearchByCompanyName('Example');
            37: ZbFindEmail('example.com', 'A', 'B', 'C');
            38: ZbFindEmail('example.com', 'A', 'C');
            39: ZbDomainSearch('example.com');
        end;
    except
        on E: ZbException do
        begin
            Result := True;
            Text := E.Message;
            Status := E.StatusCode;
        end;
    end;
end;

{ validate: a message with no result on a 2xx raises with the message's own
  text (key masked) and the status, like the other typed calls - not with the
  text of a field of the wrong type. The greylist answer is a result. }
procedure TTestBehaviour.TestAMessageOnlyValidateAnswerRaisesWithTheMessageText;
const
  BODIES: array[0..5] of String = (
    '{"message":"Invalid API key or your account ran out of credits"}',
    '{"error":"Invalid API key or your account ran out of credits"}',
    '{"error_message":"Invalid API key or your account ran out of credits"}',
    '{"message":"Invalid API key or your account ran out of credits","address":null,"status":null}',
    '{"Error":"Invalid API key or your account ran out of credits","domain":null}',
    '{"detail":"Invalid API key or your account ran out of credits"}');
var
  I: Integer;
  Raised: Boolean;
begin
  ZBInitialize('validate-key-91');
  for I := 0 to High(BODIES) do
  begin
    ZBMockResponse(200, BODIES[I], 'application/json');
    Raised := False;
    try
      ZbValidateEmail('a@example.com');
    except
      on E: ZbException do
      begin
        Raised := True;
        AssertEquals(BODIES[I], 200, E.StatusCode);
        AssertTrue(E.Message, Pos('Invalid API key or your account ran out of credits', E.Message) > 0);
        AssertEquals(E.Message, 0, Pos('wrong JSON type', E.Message));
        AssertEquals(E.Message, 0, Pos('"address"', E.Message));
      end;
    end;
    AssertTrue('raised for ' + BODIES[I], Raised);
  end;
  { The key in the message is masked. }
  ZBMockResponse(200, '{"message":"Bad key validate-key-91"}', 'application/json');
  try
    ZbValidateEmail('a@example.com');
    Fail('a message-only answer was taken as a result');
  except
    on E: ZbException do
    begin
      AssertTrue(E.Message, Pos('Bad key', E.Message) > 0);
      AssertEquals(E.Message, 0, Pos('validate-key-91', E.Message));
    end;
  end;
  { The greylist answer stays a result, beside nulls too. }
  ZBMockResponse(200, '{"Message":"Greylist validation still in progress"}', 'application/json');
  AssertEquals('Greylist validation still in progress', ZbValidateEmail('a@example.com').Message);
  ZBMockResponse(200, '{"Message":"Greylist validation still in progress","address":null}', 'application/json');
  AssertEquals('Greylist validation still in progress', ZbValidateEmail('a@example.com').Message);
end;

procedure TTestBehaviour.TestMessageOnlyAndEmptyAnswersAreErrorsWithStatus200;
const
    KEYS: array[0..5] of String = ('message', 'Message', 'error', 'error_message', 'Error', 'detail');
var
    Index, K, Checked: Integer;
    Text: String;
    Status: Integer;
begin
    Checked := 0;
    for Index := Low(NO_RESULT_NAMES) to High(NO_RESULT_NAMES) do
    begin
        for K := Low(KEYS) to High(KEYS) do
        begin
            AssertTrue(Format('%s took {"%s":"m-text"} as an answer', [NO_RESULT_NAMES[Index], KEYS[K]]),
                NoResultCall(Index, '{"' + KEYS[K] + '":"m-text"}', Text, Status));
            AssertEquals(NO_RESULT_NAMES[Index] + ' status for ' + KEYS[K], 200, Status);
            AssertTrue(NO_RESULT_NAMES[Index] + ' message for ' + KEYS[K] + ': ' + Text, Pos('m-text', Text) > 0);
            Inc(Checked);
        end;
        { No string message: the fixed sentence. }
        AssertTrue(NO_RESULT_NAMES[Index] + ' took {} as an answer', NoResultCall(Index, '{}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' status for {}', 200, Status);
        AssertEquals(NO_RESULT_NAMES[Index] + ' text for {}',
            'Http Error: ZeroBounce returned an unexpected JSON response.', Text);
        AssertTrue(NO_RESULT_NAMES[Index] + ' took a message list', NoResultCall(Index, '{"message":["a-1","b-2"]}', Text, Status));
        AssertTrue(NO_RESULT_NAMES[Index] + ' list text: ' + Text, (Pos('a-1', Text) > 0) and (Pos('b-2', Text) > 0));
        AssertTrue(NO_RESULT_NAMES[Index] + ' took a null message',
            NoResultCall(Index, '{"message":null}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' text for a null message',
            'Http Error: ZeroBounce returned an unexpected JSON response.', Text);
        AssertEquals(NO_RESULT_NAMES[Index] + ' status for a null message', 200, Status);
        AssertTrue(NO_RESULT_NAMES[Index] + ' took an object message',
            NoResultCall(Index, '{"detail":{"code":7}}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' status for an object message', 200, Status);
    end;
    AssertEquals('bodies checked', Length(NO_RESULT_NAMES) * Length(KEYS), Checked);
end;

procedure TTestBehaviour.TestAMessageBesideNullFieldsIsAnError;
var
    Index: Integer;
    Text: String;
    Status: Integer;
begin
    for Index := Low(NO_RESULT_NAMES) to High(NO_RESULT_NAMES) do
    begin
        AssertTrue(NO_RESULT_NAMES[Index] + ' took a message beside a null field',
            NoResultCall(Index, '{"message":"m-text","file_id":null}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' status', 200, Status);
        AssertTrue(NO_RESULT_NAMES[Index] + ': ' + Text, Pos('m-text', Text) > 0);
        if (Index < FINDER_FIRST) or (Index > FINDER_LAST) then
        begin
            AssertTrue(NO_RESULT_NAMES[Index] + ' took nothing but nulls',
                NoResultCall(Index, '{"file_id":null,"file_name":null,"success":null}', Text, Status));
            AssertEquals(NO_RESULT_NAMES[Index] + ' status for nulls', 200, Status);
        end;
    end;
end;

procedure TTestBehaviour.TestFinderAndDomainSearchBodies;
var
    Index: Integer;
    Text: String;
    Status: Integer;
begin
    Text := '';
    for Index := FINDER_FIRST to FINDER_LAST do
    begin
        { A message beside fields that are all null: the message is the text. }
        AssertTrue(NO_RESULT_NAMES[Index] + ' took a message beside a null domain',
            NoResultCall(Index, '{"message":"Invalid domain","domain":null}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' status', 200, Status);
        AssertTrue(NO_RESULT_NAMES[Index] + ' text: ' + Text, Pos('Invalid domain', Text) > 0);
        AssertTrue(NO_RESULT_NAMES[Index] + ' took a message beside several nulls',
            NoResultCall(Index, '{"Message":"Invalid domain","domain":null,"email":null,"format":null}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' status for several nulls', 200, Status);
        { No string message: the fixed sentence. }
        AssertTrue(NO_RESULT_NAMES[Index] + ' took a null message beside a null domain',
            NoResultCall(Index, '{"message":null,"domain":null}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' text for a null message',
            'Http Error: ZeroBounce returned an unexpected JSON response.', Text);
        AssertTrue(NO_RESULT_NAMES[Index] + ' took {}', NoResultCall(Index, '{}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' status for {}', 200, Status);
        AssertEquals(NO_RESULT_NAMES[Index] + ' text for {}',
            'Http Error: ZeroBounce returned an unexpected JSON response.', Text);
        { A null domain with no message, and a body with a non-null finder
          field, stay results. }
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused {"domain":null}: ' + Text,
            NoResultCall(Index, '{"domain":null}', Text, Status));
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused a result with an empty email: ' + Text,
            NoResultCall(Index, '{"email":"","domain":"example.com","failure_reason":"not found"}', Text, Status));
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused a message beside a non-null field: ' + Text,
            NoResultCall(Index, '{"message":"m-text","domain":"example.com"}', Text, Status));
    end;
end;

procedure TTestBehaviour.TestCreditsWithASignOrASpaceIsAnError;
const
    BODIES: array[0..1] of String = ('{"Credits":"+5"}', '{"Credits":" 5"}');
var
    I: Integer;
    Raised: Boolean;
begin
    for I := Low(BODIES) to High(BODIES) do
    begin
        ZBMockResponse(200, BODIES[I], 'application/json');
        Raised := False;
        try
            ZbGetCredits;
        except
            on E: ZbException do
            begin
                Raised := True;
                AssertEquals(BODIES[I] + ' status', 200, E.StatusCode);
            end;
        end;
        AssertTrue(BODIES[I] + ' was taken as a count', Raised);
    end;
end;

procedure TTestBehaviour.TestAnActivityAnswerWithNoDaysIsMinusOne;
begin
    ZBMockResponse(200, '{"found":false,"active_in_days":null}', 'application/json');
    AssertEquals(-1, ZbActivityData('a@example.com'));
    ZBMockResponse(200, '{"found":true,"active_in_days":"12"}', 'application/json');
    AssertEquals(12, ZbActivityData('a@example.com'));
end;

procedure TTestBehaviour.TestAFileStatusNeedsAKnownField;
const
    UNKNOWN: array[0..3] of String = ('{"foo":1}', '{"foo":"x","bar":[1]}', '{"message":"m-text","foo":1}',
        '{"message":"m-text","file_id":null}');
    KNOWN: array[0..8] of String = ('success', 'error_reason', 'file_id', 'file_name', 'upload_date',
        'file_status', 'complete_percentage', 'file_phase_2_status', 'return_url');
var
    Index, I: Integer;
    Text: String;
    Status: Integer;
begin
    for Index := STATUS_FIRST to STATUS_LAST do
    begin
        for I := Low(UNKNOWN) to High(UNKNOWN) do
        begin
            AssertTrue(Format('%s took %s as a status', [NO_RESULT_NAMES[Index], UNKNOWN[I]]),
                NoResultCall(Index, UNKNOWN[I], Text, Status));
            AssertEquals(NO_RESULT_NAMES[Index] + ' status', 200, Status);
        end;
        { An object with an unknown key and no message: the fixed sentence. }
        AssertTrue(NoResultCall(Index, '{"foo":1}', Text, Status));
        AssertEquals('Http Error: ZeroBounce returned an unexpected JSON response.', Text);
        { Each known field, on its own and not null, is a status. }
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused file_status Processing',
            NoResultCall(Index, '{"file_status":"Processing"}', Text, Status));
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused success true: ' + Text,
            NoResultCall(Index, '{"success":true}', Text, Status));
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused a message beside file_id: ' + Text,
            NoResultCall(Index, '{"message":"m-text","file_id":"abc"}', Text, Status));
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused file_id alone: ' + Text,
            NoResultCall(Index, '{"file_id":"abc"}', Text, Status));
        for I := Low(KNOWN) to High(KNOWN) do
            if (KNOWN[I] <> 'upload_date') and (KNOWN[I] <> 'complete_percentage') and (KNOWN[I] <> 'success') then
                AssertFalse(NO_RESULT_NAMES[Index] + ' refused ' + KNOWN[I] + ': ' + Text,
                    NoResultCall(Index, '{"' + KNOWN[I] + '":"v"}', Text, Status));
    end;
end;

procedure TTestBehaviour.TestResultsThatMustStayResults;
var
    Index: Integer;
    Text, Raw: String;
    Status: Integer;
    Greylist: TZbValidationResult;
    Domain: TZbFindEmailResponse;
begin
    Text := '';
    { sendfile and deletefile: success true, with no message. }
    for Index := 0 to 3 do
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused {"success":true}: ' + Text,
            NoResultCall(Index, '{"success":true}', Text, Status));
    AssertFalse('sendfile with a file id', NoResultCall(0, '{"success":true,"message":"File Accepted","file_id":"x","file_name":"f.csv"}', Text, Status));
    { A greylist answer to validate is a result; a whitespace-only Message is not. }
    ZBMockResponse(200, '{"Message":"Greylist validation still in progress"}', 'application/json');
    Greylist := ZbValidateEmail('a@example.com');
    AssertEquals('Greylist validation still in progress', Greylist.Message);
    ZBMockResponse(200, '{"Message":"   "}', 'application/json');
    try
        ZbValidateEmail('a@example.com');
        Fail('a whitespace-only Message was taken as a validate result');
    except
        on E: ZbException do
            AssertEquals(200, E.StatusCode);
    end;
    { A null domain with no message is a result of the finder. }
    ZBMockResponse(200, '{"domain":null}', 'application/json');
    Domain := ZbFindEmailByDomain('example.com', 'A', 'B', 'C');
    AssertEquals('', Domain.Domain);
    { Credits -1. }
    ZBMockResponse(200, '{"Credits":"-1"}', 'application/json');
    AssertEquals(-1, ZbGetCredits);
    { Filters and the list evaluator result are exempt from message-only. }
    ZBMockResponse(200, '{"message":"No filters"}', 'application/json');
    Raw := ZbListFilters(False);
    AssertEquals('{"message":"No filters"}', Raw);
    ZBMockResponse(200, '{}', 'application/json');
    AssertEquals('{}', ZbGetListEvaluatorResult('fid'));
    ZBMockResponse(200, '{"detail":"x"}', 'application/json');
    AssertEquals('{"detail":"x"}', ZbInboxPlacementProviders);
end;

procedure TTestBehaviour.TestTheBatchAllErrorCarriesTheStatusAndTheBody;
var
    Body: String;
    Raised: Boolean;
begin
    ZBInitialize('batch-key-77');
    Body := '{"email_batch":[],"errors":[{"email_address":"all","error":"Invalid API Key batch-key-77 or your account ran out of credits"}]}';
    ZBMockResponse(200, Body, 'application/json');
    Raised := False;
    try
        ZbBatchValidateEmails(['a@example.com']);
    except
        on E: ZbException do
        begin
            Raised := True;
            AssertEquals('status', 200, E.StatusCode);
            AssertTrue('the body is kept: ' + E.Payload, Pos('"email_address"', E.Payload) > 0);
            AssertTrue('the message is the error: ' + E.Message, Pos('ran out of credits', E.Message) > 0);
            AssertEquals('key in message', 0, Pos('batch-key-77', E.Message));
            AssertEquals('key in payload', 0, Pos('batch-key-77', E.Payload));
            AssertTrue('masked', Pos('REDACTED', E.Message) > 0);
        end;
    end;
    AssertTrue('a batch "all" error should raise', Raised);
end;

procedure TTestBehaviour.TestAnUpperCasedEchoOfEverySecretIsMasked;
var
    Raised: Boolean;
begin
    { The API key. }
    ZBInitialize('Api-Key-Mixed-77x');
    ZBMockResponse(400, 'rejected key API-KEY-MIXED-77X and api-key-mixed-77x', 'text/plain');
    Raised := False;
    try
        ZbGetCredits;
    except
        on E: ZbException do
        begin
            Raised := True;
            AssertEquals('upper-cased key in message', 0, Pos('API-KEY-MIXED-77X', UpperCase(E.Message)));
            AssertEquals('upper-cased key in payload', 0, Pos('API-KEY-MIXED-77X', UpperCase(E.Payload)));
            AssertTrue('masked: ' + E.Message, Pos('REDACTED', E.Message) > 0);
        end;
    end;
    AssertTrue('raised', Raised);
    { The percent-encoding of a key, with its hex in the other case. }
    ZBInitialize('Key/With+Chars-88y');
    ZBMockResponse(400, 'rejected ?api_key=key%2fwith%2bchars-88y again KEY%2FWITH%2BCHARS-88Y', 'text/plain');
    Raised := False;
    try
        ZbGetCredits;
    except
        on E: ZbException do
        begin
            Raised := True;
            AssertEquals('lower-case hex in message', 0, Pos('KEY%2FWITH%2BCHARS-88Y', UpperCase(E.Message)));
            AssertEquals('payload', 0, Pos('KEY%2FWITH%2BCHARS-88Y', UpperCase(E.Payload)));
        end;
    end;
    AssertTrue('raised for the percent-encoded key', Raised);
    { The Warmup key. }
    ZbWarmupInitialize('Warmup-Key-Mixed-99z');
    ZBMockResponse(400, 'rejected WARMUP-KEY-MIXED-99Z', 'text/plain');
    Raised := False;
    try
        ZbWarmupGetSeeds;
    except
        on E: ZbException do
        begin
            Raised := True;
            AssertEquals('warmup key in message', 0, Pos('WARMUP-KEY-MIXED-99Z', UpperCase(E.Message)));
            AssertEquals('warmup key in payload', 0, Pos('WARMUP-KEY-MIXED-99Z', UpperCase(E.Payload)));
        end;
    end;
    AssertTrue('raised for the warmup key', Raised);
    { The SMTP user name and password. }
    ZBMockResponse(500, 'login failed for SMTPUSER-Mixed-01 with SM7PPASS-Mixed-02!', 'text/plain');
    Raised := False;
    try
        ZbWarmupTestSmtpConfiguration('s@example.com', 'smtpuser-Mixed-01', 'sm7ppass-Mixed-02!', 'smtp.example.com', 587);
    except
        on E: ZbException do
        begin
            Raised := True;
            AssertEquals('smtp user in message', 0, Pos('SMTPUSER-MIXED-01', UpperCase(E.Message)));
            AssertEquals('smtp user in payload', 0, Pos('SMTPUSER-MIXED-01', UpperCase(E.Payload)));
            AssertEquals('smtp password in message', 0, Pos('SM7PPASS-MIXED-02!', UpperCase(E.Message)));
            AssertEquals('smtp password in payload', 0, Pos('SM7PPASS-MIXED-02!', UpperCase(E.Payload)));
        end;
    end;
    AssertTrue('raised for the smtp credentials', Raised);
    { Masking itself. }
    AssertEquals('x REDACTED y', ZbMaskSecret('x AB-cd-9 y', 'ab-CD-9'));
    AssertEquals('REDACTED REDACTED', ZbMaskSecret('ab-cd-9 AB-CD-9', 'Ab-Cd-9'));
end;

procedure TTestBehaviour.TestAnEchoOfTheSharpSIsMasked;
var
    Key, Masked: String;
begin
    { 'straße' upper-cased by a server is 'STRASSE'. }
    Key := 'k-stra' + #$C3#$9F + 'e-77';
    Masked := ZbMaskSecret('echo K-STRASSE-77 end', Key);
    AssertEquals('echo REDACTED end', Masked);
    Masked := ZbMaskSecret('echo K-STRA' + #$C3#$9F + 'E-77 end', Key);
    AssertEquals('echo REDACTED end', Masked);
    Masked := ZbMaskSecret('echo k-stra' + #$C3#$9F + 'e-77 end', Key);
    AssertEquals('echo REDACTED end', Masked);
    { The capital sharp s, and text after multi-byte characters keeps its place. }
    Masked := ZbMaskSecret(#$C3#$A9#$C3#$A9 + ' K-STRA' + #$E1#$BA#$9E + 'E-77 ' + #$C3#$A9, Key);
    AssertEquals(#$C3#$A9#$C3#$A9 + ' REDACTED ' + #$C3#$A9, Masked);
    { Through an exception. }
    ZBInitialize(Key);
    ZBMockResponse(400, 'rejected K-STRASSE-77', 'text/plain');
    try
        ZbGetCredits;
        Fail('raised');
    except
        on E: ZbException do
        begin
            AssertEquals('key in message', 0, Pos('K-STRASSE-77', E.Message));
            AssertEquals('key in payload', 0, Pos('K-STRASSE-77', E.Payload));
        end;
    end;
end;

procedure TTestBehaviour.TestEveryOtherEscapeSpellingOfASecretIsDropped;
const
    SECRET = 'ab''cd-9z';
    ECHOES: array[0..9] of String = (
        'ab\''cd-9z', 'ab\x27cd-9z', 'ab\u{27}cd-9z', 'ab\u{00000027}cd-9z', 'ab\U{27}cd-9z',
        'ab\U00000027cd-9z', 'ab\U0027cd-9z', 'ab\047cd-9z', 'AB\x27CD-9Z', 'ab\u0027cd-9z');
var
    I: Integer;
begin
    for I := Low(ECHOES) to High(ECHOES) do
        AssertEquals('echo ' + ECHOES[I], ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('echo ' + ECHOES[I] + ' end', SECRET));
    { Through an exception: the body is dropped for the sentence. }
    ZBInitialize(SECRET);
    ZBMockResponse(400, 'rejected ab\x27cd-9z', 'text/plain');
    try
        ZbGetCredits;
        Fail('raised');
    except
        on E: ZbException do
        begin
            AssertTrue('message: ' + E.Message, Pos(ZB_SECRET_OMITTED_MESSAGE, E.Message) > 0);
            AssertEquals('payload', ZB_SECRET_OMITTED_MESSAGE, E.Payload);
        end;
    end;
    { Octal and \x spell one byte: two of them are one UTF-8 character. }
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE,
        ZbMaskSecret('k\xC3\xA9y-5', 'k' + #$C3#$A9 + 'y-5'));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE,
        ZbMaskSecret('k\303\251y-5', 'k' + #$C3#$A9 + 'y-5'));
    { A code point up to U+10FFFF, in braces or as \U plus 8 hex digits. }
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE,
        ZbMaskSecret('k\u{e9}y-5', 'k' + #$C3#$A9 + 'y-5'));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE,
        ZbMaskSecret('k\U000000e9y-5', 'k' + #$C3#$A9 + 'y-5'));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE,
        ZbMaskSecret('k\u{1F600}y-5', 'k' + #$F0#$9F#$98#$80 + 'y-5'));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE,
        ZbMaskSecret('k\U0001F600y-5', 'k' + #$F0#$9F#$98#$80 + 'y-5'));
    { Layered with URL encoding. }
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('ab%5Cx27cd-9z', SECRET));
end;

procedure TTestBehaviour.TestHtmlEntitiesOfASecretAreDroppedInAnyCase;
const
    QUOTE_ECHOES: array[0..8] of String = (
        'ab&#39;cd-9z', 'ab&#x27;cd-9z', 'ab&apos;cd-9z', 'AB&#39;CD-9Z', 'AB&#X27;CD-9Z',
        'AB&APOS;CD-9Z', 'ab&Apos;cd-9z', 'ab&#39;CD-9z', 'x ab&#x27;cd-9z y');
var
    I: Integer;
begin
    for I := Low(QUOTE_ECHOES) to High(QUOTE_ECHOES) do
        AssertEquals(QUOTE_ECHOES[I], ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret(QUOTE_ECHOES[I], 'ab''cd-9z'));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('ab&amp;cd-9z', 'ab&cd-9z'));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('AB&AMP;CD-9Z', 'ab&cd-9z'));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('ab&lt;cd-9z', 'ab<cd-9z'));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('AB&LT;CD-9Z', 'ab<cd-9z'));
    { An escaped ampersand, itself percent-encoded. }
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('ab%26amp%3Bcd-9z', 'ab&cd-9z'));
    { Entities are searched for in a secret only: text is not rewritten. }
    AssertEquals('1 &amp; 2 &lt; 3 &#39;', ZbMaskSecret('1 &amp; 2 &lt; 3 &#39;', 'unrelated-key'));
end;

procedure TTestBehaviour.TestWhatIsNotAnEscapeIsNotDecoded;
const
    SECRET = 'ab''cd-9z';
    NOT_DECODED: array[0..4] of String = (
        'ab%u0027cd-9z', 'ab\u{000000027}cd-9z', 'ab\u{110000}cd-9z', 'ab\U{}cd-9z', 'ab\x2cd-9z');
var
    I: Integer;
begin
    for I := Low(NOT_DECODED) to High(NOT_DECODED) do
        AssertEquals(NOT_DECODED[I], 'echo ' + NOT_DECODED[I], ZbMaskSecret('echo ' + NOT_DECODED[I], SECRET));
    { \47 is the apostrophe, and &#x0027; is the same character with leading zeros. }
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('echo ab\47cd-9z', SECRET));
    AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('echo ab&#x0027;cd-9z', SECRET));
    { A mid-key ASCII space is still sent, in the form the API takes. }
    ZBInitialize('ab cd-9z');
    ZBMockResponse(200, '{"Credits":"5"}', 'application/json');
    AssertEquals(5, ZbGetCredits);
    AssertTrue(ZbResponseMock.UrlCalled, (Pos('api_key=ab%20cd-9z', ZbResponseMock.UrlCalled) > 0) or
        (Pos('api_key=ab+cd-9z', ZbResponseMock.UrlCalled) > 0));
end;

procedure TTestBehaviour.TestKeysWithUnicodeWhiteSpaceOrInvisibleCharactersAreRefused;
const
    REFUSED = 'api_key has surrounding whitespace or control characters';
    HIDDEN: array[0..19] of String = (
        'ab' + #$C2#$A0 + 'cd', #$C2#$A0 + 'abcd', 'abcd' + #$C2#$A0, 'ab' + #$E2#$80#$8B + 'cd',
        BOM + 'abcd', 'abcd' + BOM, 'ab' + #$E2#$80#$8E + 'cd', 'ab' + #$E2#$80#$A8 + 'cd',
        'ab' + #$E3#$80#$80 + 'cd', 'ab' + #$C2#$AD + 'cd', 'ab' + #$EF#$B8#$8F + 'cd',
        'ab' + #$F3#$A0#$81#$81 + 'cd', 'ab' + #$E2#$81#$A0 + 'cd', 'ab' + #$E1#$A0#$8E + 'cd',
        'ab' + #$C2#$85 + 'cd', 'ab' + #$E2#$80#$AF + 'cd', 'ab' + #$E1#$9A#$80 + 'cd',
        'ab' + #$E3#$85#$A4 + 'cd', 'ab' + #$E2#$80#$AE + 'cd', 'ab' + #$E2#$80#$80 + 'cd');
var
    I: Integer;
    Raised: Boolean;
begin
    ZBInitialize('keep-this-key');
    for I := Low(HIDDEN) to High(HIDDEN) do
    begin
        Raised := False;
        try
            ZBInitialize(HIDDEN[I]);
        except
            on E: ZbException do
            begin
                Raised := True;
                AssertEquals(REFUSED, E.Message);
            end;
        end;
        AssertTrue(Format('key %d was accepted', [I]), Raised);
        AssertEquals('the key already set stays', 'keep-this-key', ZbApiKey);
        Raised := False;
        try
            ZbWarmupInitialize(HIDDEN[I]);
        except
            on E: ZbException do
                Raised := True;
        end;
        AssertTrue(Format('warmup key %d was accepted', [I]), Raised);
        Raised := False;
        try
            ZBInitialize(HIDDEN[I], 'https://api.zerobounce.net/v2');
        except
            on E: ZbException do
                Raised := True;
        end;
        AssertTrue(Format('key %d with a base URL was accepted', [I]), Raised);
    end;
end;

procedure TTestBehaviour.TestKeysThatAreNotValidUtf8AreRefused;
const
    BAD: array[0..8] of String = (
        'ab' + #$C3 + 'cd', 'ab' + #$FF + 'cd', 'ab' + #$80 + 'cd', 'ab' + #$C0#$80 + 'cd',
        'ab' + #$ED#$A0#$80 + 'cd', 'ab' + #$F4#$90#$80#$80 + 'cd', 'abcd' + #$E2#$82,
        'ab' + #$C3#$28 + 'cd', 'ab' + #$F8#$88#$80#$80#$80 + 'cd');
var
    I: Integer;
    Raised: Boolean;
begin
    for I := Low(BAD) to High(BAD) do
    begin
        Raised := False;
        try
            ZBInitialize(BAD[I]);
        except
            on E: ZbException do
            begin
                Raised := True;
                AssertEquals('api_key has surrounding whitespace or control characters', E.Message);
            end;
        end;
        AssertTrue(Format('invalid UTF-8 key %d was accepted', [I]), Raised);
    end;
end;

procedure TTestBehaviour.TestPlainKeysStillPass;
begin
    ZBInitialize('plain-key-123');
    AssertEquals('plain-key-123', ZbApiKey);
    ZBInitialize('ab cd-9z');
    AssertEquals('ab cd-9z', ZbApiKey);
    { Letters of other scripts are text, not padding. }
    ZBInitialize('k' + #$C3#$A9 + 'y-' + #$E4#$B8#$AD + '-' + #$F0#$9F#$98#$80);
    AssertTrue(Length(ZbApiKey) > 0);
    ZbWarmupInitialize('warmup-key-123');
    AssertEquals('warmup-key-123', ZbWarmupApiKey);
end;

procedure TTestBehaviour.TestABaseUrlWithAQueryOrFragmentIsRefusedBeforeSending;
const
    SENTENCE = 'A base URL must not contain a query string or a fragment ("?" or "#")';
    BAD: array[0..3] of String = (
        'https://api.example.test/v2?secret=abc123', 'https://api.example.test/v2#frag-xyz123',
        'https://api.example.test/v2/?secret=abc123', 'https://api.example.test?secret=abc123');
var
    B, Which: Integer;
    Raised: Boolean;

    procedure Point(AWhich: Integer; const Url: String);
    begin
        case AWhich of
            0: ZbApiBaseURL := Url;
            1: BULK_URI := Url;
            2: ZbBulkRootURL := Url;
            3: ZbListEvaluatorURL := Url;
            4: ZbInboxPlacementURL := Url;
            5: ZbWarmupBaseURL := Url;
        end;
    end;

    procedure Call(AWhich: Integer);
    begin
        case AWhich of
            0: ZbGetCredits;
            1: ZbBulkValidationFileStatusCheck('fid');
            2: ZbEmailFinderFileStatus('fid');
            3: ZbGetListEvaluatorResult('fid');
            4: ZbInboxPlacementProviders;
            5: ZbWarmupGetSeeds;
        end;
    end;

begin
    for B := Low(BAD) to High(BAD) do
        for Which := 0 to 5 do
        begin
            ResetUrls;
            ZbWarmupInitialize('mock_warmup_key');
            ZBMockResponse(200, '{"Credits":"5","success":true,"file_id":"x"}', 'application/json');
            Point(Which, BAD[B]);
            Raised := False;
            try
                Call(Which);
            except
                on E: ZbException do
                begin
                    Raised := True;
                    AssertEquals(SENTENCE, E.Message);
                    AssertEquals('the URL is not quoted', 0, Pos('abc123', E.Message));
                    AssertEquals('the URL is not quoted', 0, Pos('xyz123', E.Message));
                end;
            end;
            AssertTrue(Format('base URL %d of call %d was accepted', [B, Which]), Raised);
            AssertEquals('nothing was sent', '', ZbResponseMock.UrlCalled);
        end;
    { ZBInitialize with a base URL takes the same rail. }
    for B := Low(BAD) to High(BAD) do
    begin
        Raised := False;
        try
            ZBInitialize('some-key-1', BAD[B]);
        except
            on E: ZbException do
                Raised := True;
        end;
        AssertTrue('ZBInitialize accepted ' + BAD[B], Raised);
    end;
end;

procedure TTestBehaviour.TestAnIpv6BaseUrlStaysAHost;
var
    Raised: Boolean;
begin
    ZBInitialize('some-key-2', 'https://[::1]/v2');
    ZBMockResponse(200, '{"Credits":"5"}', 'application/json');
    AssertEquals(5, ZbGetCredits);
    AssertEquals('https://[::1]/v2/getcredits?api_key=some-key-2', ZbResponseMock.UrlCalled);
    Raised := False;
    try
        ZBInitialize('some-key-2', 'https://[]/v2');
    except
        on E: ZbException do
            Raised := True;
    end;
    AssertTrue('empty brackets are not a host', Raised);
end;

procedure TTestBehaviour.TestADownloadReplacesAFileAlreadyThere;
var
    Content, Text: String;
    Status: Integer;
    Leftovers: TStringList;
    Found: TSearchRec;
begin
    AssertFalse(Download(4, 'old' + #10 + '1' + #10, CSV_TYPE, Content, Text, Status));
    AssertEquals('old' + #10 + '1' + #10, ReadFile(SavePath));
    ZBMockResponse(200, 'new' + #10 + '2' + #10, CSV_TYPE);
    ZbWarmupDownloadSeeds(SavePath);
    AssertEquals('new' + #10 + '2' + #10, ReadFile(SavePath));
    { No temporary file is left beside it. }
    Leftovers := TStringList.Create;
    try
        if FindFirst(SavePath + '.*.part', faAnyFile, Found) = 0 then
        begin
            repeat
                Leftovers.Add(Found.Name);
            until FindNext(Found) <> 0;
            FindClose(Found);
        end;
        AssertEquals('leftover temporary files: ' + Leftovers.Text, 0, Leftovers.Count);
    finally
        Leftovers.Free;
    end;
end;

procedure TTestBehaviour.TestTheFoldCoversWhatAnEchoChanges;
var
    Password: String;

    { The message and the payload of the exception a failed SMTP login raises
      when the server echoes Echo back. }
    procedure Echoed(const Label_, Secret, Echo: String);
    var
        Raised: Boolean;
    begin
        Password := Secret;
        ZBMockResponse(500, 'login failed for ' + Echo + ' on the server', 'text/plain');
        Raised := False;
        try
            ZbWarmupTestSmtpConfiguration('s@example.com', 'smtpuser', Secret, 'smtp.example.com', 587);
        except
            on E: ZbException do
            begin
                Raised := True;
                AssertEquals(Label_ + ': echo in message', 0, Pos(Echo, E.Message));
                AssertEquals(Label_ + ': echo in payload', 0, Pos(Echo, E.Payload));
            end;
        end;
        AssertTrue(Label_ + ': raised', Raised);
    end;

begin
    ZbWarmupInitialize('warmup-key-fold');
    { The SMTP password cases: dotless i upper-cased, the micro sign upper-cased
      to Greek capital mu, and U+0130 echoed as i plus U+0307. }
    Echoed('dotless i', 'p' + #$C4#$B1 + 'ssword1', 'PISSWORD1');
    Echoed('micro sign', #$C2#$B5 + 'pass-word-2', #$CE#$9C + 'PASS-WORD-2');
    Echoed('U+0130 as i and a dot', 'pass' + #$C4#$B0 + 'word-3', 'PASSi' + #$CC#$87 + 'WORD-3');
    Echoed('U+0130 as i', 'pass' + #$C4#$B0 + 'word-4', 'passiword-4');
    Echoed('U+0130 upper-cased by the server', 'pass' + #$C4#$B0 + 'word-5', 'PASS' + #$C4#$B0 + 'WORD-5');
    Echoed('sharp s as SS', 'stra' + #$C3#$9F + 'e-pass-6', 'STRASSE-PASS-6');
    { Masking itself: the other letters the table now has. }
    { U+0180 (lower) and U+0243 (its capital). }
    AssertEquals('x REDACTED y', ZbMaskSecret('x pw' + #$C9#$83 + 'k y', 'pw' + #$C6#$80 + 'k'));
    { U+01C4 / U+01C5 / U+01C6 are one letter to the fold. }
    AssertEquals('REDACTED', ZbMaskSecret(#$C7#$84 + 'ab', #$C7#$86 + 'ab'));
    AssertEquals('REDACTED', ZbMaskSecret(#$C7#$85 + 'ab', #$C7#$86 + 'ab'));
    AssertEquals('REDACTED', ZbMaskSecret(#$C7#$86 + 'ab', #$C7#$84 + 'ab'));
    { Armenian: capital U+0531 against small U+0561, and U+0556 against U+0586. }
    AssertEquals('a REDACTED b', ZbMaskSecret('a ' + #$D4#$B1 + #$D5#$96 + '-9 b', #$D5#$A1 + #$D6#$86 + '-9'));
    AssertEquals('a REDACTED b', ZbMaskSecret('a ' + #$D5#$A1 + #$D6#$86 + '-9 b', #$D4#$B1 + #$D5#$96 + '-9'));
    { A combining mark is kept: a letter with one is not the plain letter. }
    AssertEquals('x' + #$CC#$87 + #$CC#$87, ZbMaskSecret('x' + #$CC#$87 + #$CC#$87, 'x'));
    AssertEquals('REDACTED', ZbMaskSecret('x' + #$CC#$87 + #$CC#$87, 'x' + #$CC#$87 + #$CC#$87));
    AssertEquals('REDACTED-after', ZbMaskSecret('PASSi' + #$CC#$87 + '-after', 'pass' + #$C4#$B0));
    AssertEquals('REDACTED', ZbMaskSecret('i' + #$CC#$87, 'i'));
    { Text that has no echo stays as it was. }
    AssertEquals('p' + #$C4#$B1 + 'ssword', ZbMaskSecret('p' + #$C4#$B1 + 'ssword', 'unrelated-1'));
end;

procedure TTestBehaviour.TestMoreInvisibleCodePointsAreRefused;
const
    REFUSED = 'api_key has surrounding whitespace or control characters';
    HIDDEN: array[0..9] of String = (
        'ab' + #$D8#$80 + 'cd',          { U+0600 }
        'ab' + #$D8#$85 + 'cd',          { U+0605 }
        'ab' + #$DB#$9D + 'cd',          { U+06DD }
        'ab' + #$DC#$8F + 'cd',          { U+070F }
        'ab' + #$E0#$A2#$90 + 'cd',      { U+0890 }
        'ab' + #$E0#$A2#$91 + 'cd',      { U+0891 }
        'ab' + #$E0#$A3#$A2 + 'cd',      { U+08E2 }
        'ab' + #$F0#$91#$82#$BD + 'cd',  { U+110BD }
        #$E0#$A2#$90 + 'abcd',
        'abcd' + #$D8#$80);
var
    I: Integer;
    Raised: Boolean;
begin
    ZBInitialize('keep-this-key');
    for I := Low(HIDDEN) to High(HIDDEN) do
    begin
        Raised := False;
        try
            ZBInitialize(HIDDEN[I]);
        except
            on E: ZbException do
            begin
                Raised := True;
                AssertEquals(REFUSED, E.Message);
            end;
        end;
        AssertTrue(Format('key %d was accepted', [I]), Raised);
        AssertEquals('the key already set stays', 'keep-this-key', ZbApiKey);
    end;
    { Letters of those scripts are still fine. }
    ZBInitialize('ab' + #$D8#$A7 + 'cd');
    AssertEquals('ab' + #$D8#$A7 + 'cd', ZbApiKey);
end;

procedure TTestBehaviour.TestAShortMessageWithAnAngleBracketIsKept;
var
    Response: TZbRequestResponse;
    Error: ZbException;
begin
    Response := Default(TZbRequestResponse);
    Response.StatusCode := 400;
    Response.Payload := '{"error":"unrelated"}';
    Error := ZbException.FromResponse('Invalid email <a@b.c>', Response);
    try
        AssertEquals('Invalid email <a@b.c>', Error.Message);
    finally
        Error.Free;
    end;
    { Dropped all the same: 300 characters or more, and Access violation. }
    Error := ZbException.FromResponse('<' + StringOfChar('x', 299), Response);
    try
        AssertTrue('a long message is dropped: ' + Error.Message, Pos('xxxxxxxx', Error.Message) = 0);
    finally
        Error.Free;
    end;
    Error := ZbException.FromResponse('Access violation at <0x0>', Response);
    try
        AssertEquals(0, Pos('Access violation', Error.Message));
    finally
        Error.Free;
    end;
end;

procedure TTestBehaviour.TestALongJsonBodyIsNotCalledInvalid;
var
    Response: TZbRequestResponse;
    Error: ZbException;
begin
    Response := Default(TZbRequestResponse);
    Response.StatusCode := 500;
    Response.Payload := '{"detail":"' + StringOfChar('x', 1100) + '"}';
    Error := ZbException.FromResponse('', Response);
    try
        AssertEquals('ZeroBounce returned an error response.', Error.Message);
    finally
        Error.Free;
    end;
    { Only that case: a long body that is not JSON keeps its sentence. }
    Response.Payload := StringOfChar('y', 1100);
    Error := ZbException.FromResponse('', Response);
    try
        AssertEquals('ZeroBounce returned a response that is not valid JSON.', Error.Message);
    finally
        Error.Free;
    end;
end;

procedure TTestBehaviour.TestAFailedReplaceKeepsWhatWasThere;
var
    Raised: Boolean;
    Marker: String;
    Leftovers: TStringList;
    Found: TSearchRec;
begin
    { The target is a directory with a file in it, so the move cannot replace
      it and must not delete it first. }
    Marker := IncludeTrailingPathDelimiter(SavePath) + 'keep.txt';
    ForceDirectories(SavePath);
    try
        with TFileStream.Create(Marker, fmCreate) do
            Free;
        ZBMockResponse(200, CSV_FILE, CSV_TYPE);
        Raised := False;
        try
            ZbWarmupDownloadSeeds(SavePath);
        except
            { Not an EWriteError: every failure of the SDK is a ZbException. }
            on E: ZbException do
            begin
                Raised := True;
                AssertEquals('Could not write ' + SavePath, E.Message);
                AssertEquals('status', 0, E.StatusCode);
            end;
        end;
        AssertTrue('replacing a directory should fail', Raised);
        AssertTrue('what was there is gone', FileExists(Marker));
    finally
        DeleteFile(Marker);
        RemoveDir(SavePath);
    end;
    Leftovers := TStringList.Create;
    try
        if FindFirst(SavePath + '.*.part', faAnyFile, Found) = 0 then
        begin
            repeat
                Leftovers.Add(Found.Name);
            until FindNext(Found) <> 0;
            FindClose(Found);
        end;
        AssertEquals('leftover temporary files: ' + Leftovers.Text, 0, Leftovers.Count);
    finally
        Leftovers.Free;
    end;
end;

procedure TTestBehaviour.TestProcessedAtKeepsItsDayAndItsMilliseconds;
const
    HEAD = '{"address":"a@example.com","status":"valid",';
var
    Parsed: TZbValidationResult;
    Y, Mo, D, H, Mi, S, Ms: Word;
    Round: Integer;
begin
    { Repeated, so a part left unread would show whatever the stack held. }
    for Round := 1 to 5 do
    begin
        { No fraction: the time as given, and .000. }
        Parsed := ZbValidationFromJson(HEAD + '"processed_at":"2023-01-12 22:02:02"}');
        DecodeDateTime(Parsed.ProcessedAt, Y, Mo, D, H, Mi, S, Ms);
        AssertEquals('year', 2023, Y);
        AssertEquals('month', 1, Mo);
        AssertEquals('day is not the month', 12, D);
        AssertEquals('hour', 22, H);
        AssertEquals('minute', 2, Mi);
        AssertEquals('second', 2, S);
        AssertEquals('no fraction is .000', 0, Ms);
        { A fraction stays what it was. }
        Parsed := ZbValidationFromJson(HEAD + '"processed_at":"2023-01-12 22:02:02.592"}');
        DecodeDateTime(Parsed.ProcessedAt, Y, Mo, D, H, Mi, S, Ms);
        AssertEquals('day with a fraction', 12, D);
        AssertEquals('second with a fraction', 2, S);
        AssertEquals('milliseconds', 592, Ms);
    end;
end;

procedure TTestBehaviour.TestActivityDaysAreDecimalTextOnly;
const
    BAD: array[0..3] of String = ('+5', ' 5', '$1F', '0x10');
var
    I: Integer;
    Raised: Boolean;
begin
    for I := Low(BAD) to High(BAD) do
    begin
        ZBMockResponse(200, '{"found":true,"active_in_days":"' + BAD[I] + '"}', 'application/json');
        Raised := False;
        try
            ZbActivityData('a@example.com');
        except
            on E: ZbException do
            begin
                Raised := True;
                AssertEquals(BAD[I] + ' status', 200, E.StatusCode);
            end;
        end;
        AssertTrue('active_in_days ' + BAD[I] + ' was taken as a count', Raised);
    end;
    ZBMockResponse(200, '{"found":false,"active_in_days":null}', 'application/json');
    AssertEquals(-1, ZbActivityData('a@example.com'));
    ZBMockResponse(200, '{"found":true,"active_in_days":"12"}', 'application/json');
    AssertEquals(12, ZbActivityData('a@example.com'));
end;

procedure TTestBehaviour.TestCreditsRaisesOnAMessageAlone;
var
    Raised: Boolean;
begin
    ZBMockResponse(200, '{"message":"Invalid API key"}', 'application/json');
    Raised := False;
    try
        ZbGetCredits;
    except
        on E: ZbException do
        begin
            Raised := True;
            AssertEquals('status', 200, E.StatusCode);
            AssertTrue('the API text is the message: ' + E.Message, Pos('Invalid API key', E.Message) > 0);
        end;
    end;
    AssertTrue('a message alone was taken as credits', Raised);
    ZBMockResponse(200, '{"Credits":"-1"}', 'application/json');
    AssertEquals(-1, ZbGetCredits);
end;

procedure TTestBehaviour.TestANullSuccessKeyDoesNotHideAMessage;
var
    Index: Integer;
    Text: String;
    Status: Integer;
begin
    Text := '';
    for Index := FINDER_FIRST to FINDER_LAST do
    begin
        AssertTrue(NO_RESULT_NAMES[Index] + ' hid a message behind a null success',
            NoResultCall(Index, '{"success":null,"message":"Invalid domain"}', Text, Status));
        AssertEquals(NO_RESULT_NAMES[Index] + ' status', 200, Status);
        AssertTrue(NO_RESULT_NAMES[Index] + ' text: ' + Text, Pos('Invalid domain', Text) > 0);
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused {"domain":null}: ' + Text,
            NoResultCall(Index, '{"domain":null}', Text, Status));
    end;
    { A real success on a file status is still not an error. }
    for Index := STATUS_FIRST to STATUS_LAST do
        AssertFalse(NO_RESULT_NAMES[Index] + ' refused success true: ' + Text,
            NoResultCall(Index, '{"success":true}', Text, Status));
end;

procedure TTestBehaviour.TestCompletePercentageParsesWithADotInACommaLocale;
var
    Saved: Char;
    Status: TZbFileStatus;
begin
    Saved := DefaultFormatSettings.DecimalSeparator;
    DefaultFormatSettings.DecimalSeparator := ',';
    try
        Status := ZbFileStatusFromJson('{"success":true,"file_id":"a","complete_percentage":"45.5%"}');
        AssertEquals('percentage', 45.5, Status.CompletePercentage, 0.0001);
        AssertEquals('the locale is left as it was', ',', DefaultFormatSettings.DecimalSeparator);
    finally
        DefaultFormatSettings.DecimalSeparator := Saved;
    end;
    Status := ZbFileStatusFromJson('{"success":true,"file_id":"a","complete_percentage":"45.5%"}');
    AssertEquals('percentage in the saved locale', 45.5, Status.CompletePercentage, 0.0001);
end;

procedure TTestBehaviour.TestJsonWrappersFreeWhatTheyCreate;
const
    VALID = '{"address":"a@example.com","status":"valid","processed_at":"2023-01-12 22:02:02.592"}';
    BATCH = '{"email_batch":[' + VALID + ',' + VALID + '],"errors":[{"error":"bad","email_address":"x"}]}';
    FINDER = '{"email":"a@example.com","domain":"example.com","email_confidence":"high"}';
    SEARCH = '{"domain":"example.com","other_domain_formats":[{"format":"first","confidence":"high"},' +
        '{"format":"last","confidence":"low"}]}';
    USAGE = '{"total":1,"start_date":"1/1/2023","end_date":"1/2/2023"}';
var
    Before, After: PtrUInt;
    I: Integer;
    Held: TZbJson;
    Parsed: TZbValidationResult;
    procedure RunAll;
    begin
        ZbValidationFromJson(VALID);
        ZbBatchErrorFromJson('{"error":"bad","email_address":"x"}');
        ZbApiUsageFromJson(USAGE);
        TZbFindEmailResponseFromJson(FINDER);
        TZbDomainSearchResponseFromJson(SEARCH);
        ZbBatchValidationFromJson(BATCH);
    end;
begin
    RunAll;
    Before := GetFPCHeapStatus.CurrHeapUsed;
    for I := 1 to 50 do
        RunAll;
    After := GetFPCHeapStatus.CurrHeapUsed;
    AssertEquals('heap grew over 50 rounds of parsing', 0, Int64(After) - Int64(Before));
    { The object a caller passes in is theirs: still readable afterwards. }
    Held := TZbJson.Create(VALID);
    try
        Parsed := ZbValidationFromJson(Held);
        AssertEquals('a@example.com', Parsed.Address);
        AssertEquals('the caller still holds its object', 'valid', Held.GetString('status'));
    finally
        Held.Free;
    end;
end;

function RaisesOnParse(const Kind, Body: String; out Text: String): Boolean;
begin
    Result := False;
    Text := '';
    try
        if Kind = 'usage' then
            ZbApiUsageFromJson(Body)
        else
            ZbValidationFromJson(Body);
    except
        on E: Exception do
        begin
            Result := True;
            Text := E.Message;
        end;
    end;
end;

function ProcessedAtOf(const Value: String): TDateTime;
begin
    Result := ZbValidationFromJson(
        '{"address":"a@example.com","status":"valid","processed_at":"' + Value + '"}').ProcessedAt;
end;

procedure TTestBehaviour.TestAnIntegerOutsideIntegerRangeIsAFieldTypeError;
const
    BAD: array[0..3] of String = ('4294967297', '2147483648', '-2147483649', '9223372036854775807');
var
    I: Integer;
    Text: String;
begin
    for I := Low(BAD) to High(BAD) do
    begin
        AssertTrue('total ' + BAD[I] + ' was cut down to an Integer',
            RaisesOnParse('usage', '{"total":' + BAD[I] + '}', Text));
        AssertTrue(Text, Pos('"total"', Text) > 0);
        AssertTrue(Text, Pos('wrong JSON type', Text) > 0);
    end;
    AssertEquals('total 1', 1, ZbApiUsageFromJson('{"total":1}').Total);
    AssertEquals('total High(Integer)', 2147483647, ZbApiUsageFromJson('{"total":2147483647}').Total);
    AssertEquals('total Low(Integer)', -2147483648, ZbApiUsageFromJson('{"total":-2147483648}').Total);
    { The same through the call, which reports it with the status. }
    ZBMockResponse(200, '{"total":4294967297}', 'application/json');
    try
        ZbGetApiUsage;
        Fail('ZbGetApiUsage took 4294967297 as a count');
    except
        on E: ZbException do
            AssertEquals('status', 200, E.StatusCode);
    end;
end;

procedure TTestBehaviour.TestAShortApiUsageDateIsAnErrorNotLeftoverStack;
const
    SHORT: array[0..3] of String = ('11/17', '11', '/', '1/1');
var
    I, Round: Integer;
    Text: String;
    Usage: TApiUsage;
    Y, Mo, D: Word;
begin
    for Round := 1 to 3 do
    begin
        for I := Low(SHORT) to High(SHORT) do
        begin
            AssertTrue('start_date ' + SHORT[I] + ' became a date',
                RaisesOnParse('usage', '{"total":1,"start_date":"' + SHORT[I] + '"}', Text));
            AssertTrue(Text, Pos('Invalid date', Text) > 0);
            AssertTrue('end_date ' + SHORT[I] + ' became a date',
                RaisesOnParse('usage', '{"total":1,"end_date":"' + SHORT[I] + '"}', Text));
        end;
        Usage := ZbApiUsageFromJson('{"total":1,"start_date":"1/1/2023","end_date":"11/17/2026"}');
        DecodeDate(Usage.StartDate, Y, Mo, D);
        AssertEquals('start year', 2023, Y);
        AssertEquals('start month', 1, Mo);
        AssertEquals('start day', 1, D);
        DecodeDate(Usage.EndDate, Y, Mo, D);
        AssertEquals('end year', 2026, Y);
        AssertEquals('end month', 11, Mo);
        AssertEquals('end day', 17, D);
    end;
end;

procedure TTestBehaviour.TestProcessedAtAcceptsTAsTheSeparator;
var
    Y, Mo, D, H, Mi, S, Ms: Word;
begin
    DecodeDateTime(ProcessedAtOf('2026-11-17T10:30:00'), Y, Mo, D, H, Mi, S, Ms);
    AssertEquals('year', 2026, Y);
    AssertEquals('month', 11, Mo);
    AssertEquals('day', 17, D);
    AssertEquals('hour is not midnight', 10, H);
    AssertEquals('minute', 30, Mi);
    AssertEquals('second', 0, S);
    AssertEquals('milliseconds', 0, Ms);
    DecodeDateTime(ProcessedAtOf('2026-11-17T10:30:05.250'), Y, Mo, D, H, Mi, S, Ms);
    AssertEquals('hour with a fraction', 10, H);
    AssertEquals('second with a fraction', 5, S);
    AssertEquals('milliseconds with a fraction', 250, Ms);
    { The space form is as it was. }
    DecodeDateTime(ProcessedAtOf('2026-11-17 10:30:00'), Y, Mo, D, H, Mi, S, Ms);
    AssertEquals('space form hour', 10, H);
    AssertEquals('space form minute', 30, Mi);
end;

procedure TTestBehaviour.TestAFractionIsADecimalOfSeconds;
const
    BAD: array[0..2] of String = ('2023-01-12 22:02:02.1234', '2023-01-12 22:02:02.0001', '2023-01-12 22:02:02.9999');
var
    Y, Mo, D, H, Mi, S, Ms: Word;
    I: Integer;
    Text: String;
begin
    DecodeDateTime(ProcessedAtOf('2023-01-12 22:02:02.5'), Y, Mo, D, H, Mi, S, Ms);
    AssertEquals('.5 is 500', 500, Ms);
    AssertEquals('second', 2, S);
    DecodeDateTime(ProcessedAtOf('2023-01-12 22:02:02.12'), Y, Mo, D, H, Mi, S, Ms);
    AssertEquals('.12 is 120', 120, Ms);
    DecodeDateTime(ProcessedAtOf('2023-01-12 22:02:02.592'), Y, Mo, D, H, Mi, S, Ms);
    AssertEquals('.592 is 592', 592, Ms);
    DecodeDateTime(ProcessedAtOf('2023-01-12 22:02:02.007'), Y, Mo, D, H, Mi, S, Ms);
    AssertEquals('.007 is 7', 7, Ms);
    DecodeDateTime(ProcessedAtOf('2023-01-12 22:02:02.05'), Y, Mo, D, H, Mi, S, Ms);
    AssertEquals('.05 is 50', 50, Ms);
    for I := Low(BAD) to High(BAD) do
        AssertTrue(BAD[I] + ' was accepted',
            RaisesOnParse('validation', '{"address":"a@example.com","status":"valid","processed_at":"' + BAD[I] + '"}', Text));
end;

procedure TTestBehaviour.TestABlankCompletePercentageIsMinusOne;
const
    BLANK: array[0..1] of String = ('', '%');
var
    I: Integer;
    Status: TZbFileStatus;
begin
    for I := Low(BLANK) to High(BLANK) do
    begin
        Status := ZbFileStatusFromJson(
            '{"success":true,"file_id":"a","complete_percentage":"' + BLANK[I] + '"}');
        AssertEquals('percentage "' + BLANK[I] + '"', -1.0, Status.CompletePercentage, 0.0001);
    end;
    Status := ZbFileStatusFromJson('{"success":true,"file_id":"a","complete_percentage":"45.5%"}');
    AssertEquals('45.5%', 45.5, Status.CompletePercentage, 0.0001);
    Status := ZbFileStatusFromJson('{"success":true,"file_id":"a","complete_percentage":"0%"}');
    AssertEquals('0%', 0.0, Status.CompletePercentage, 0.0001);
end;

procedure TTestBehaviour.TestJsonKeysAreLookedUpExactly;
var
    J: TZbJson;
begin
    J := TZbJson.Create('{"a":{"b":"nested"},"a.b":"flat","c":"x"}');
    try
        AssertEquals('a.b is the key a.b, not a path', 'flat', J.GetString('a.b'));
        AssertTrue('an empty key is not the object itself', J.GetValue('') = nil);
        AssertEquals('c', 'x', J.GetString('c'));
    finally
        J.Free;
    end;
    J := TZbJson.Create('{"a":{"b":"nested"}}');
    try
        AssertTrue('a.b is missing, not b inside a', J.GetValue('a.b') = nil);
        AssertEquals('a missing key reads as empty', '', J.GetString('a.b'));
    finally
        J.Free;
    end;
end;

procedure TTestBehaviour.TestGetFileErrorKeysInAnyCase;
const
    KEYS: array[0..5] of String = ('message', 'Message', 'error', 'Error', 'error_message', 'detail');
var
    I: Integer;
begin
    for I := Low(KEYS) to High(KEYS) do
    begin
        AssertTrue(KEYS[I] + ' text is an error',
            ZbGetFileJsonIndicatesError('{"' + KEYS[I] + '":"Invalid API key"}'));
        AssertFalse(KEYS[I] + ' null is not an error',
            ZbGetFileJsonIndicatesError('{"' + KEYS[I] + '":null}'));
        AssertFalse(KEYS[I] + ' blank is not an error',
            ZbGetFileJsonIndicatesError('{"' + KEYS[I] + '":"  "}'));
    end;
    AssertFalse(ZbGetFileJsonIndicatesError('{"file_id":"a"}'));
end;

procedure TTestBehaviour.TestActivityDaysAcceptsAJsonInteger;
const
    BAD: array[0..4] of String = ('"+5"', '" 5"', '"$1F"', '"0x10"', '4294967297');
var
    I: Integer;
    Raised: Boolean;
begin
    ZBMockResponse(200, '{"found":true,"active_in_days":180}', 'application/json');
    AssertEquals('a JSON integer', 180, ZbActivityData('a@example.com'));
    ZBMockResponse(200, '{"found":true,"active_in_days":0}', 'application/json');
    AssertEquals('a JSON zero', 0, ZbActivityData('a@example.com'));
    ZBMockResponse(200, '{"found":true,"active_in_days":"12"}', 'application/json');
    AssertEquals('a JSON string', 12, ZbActivityData('a@example.com'));
    ZBMockResponse(200, '{"found":false,"active_in_days":null}', 'application/json');
    AssertEquals('null', -1, ZbActivityData('a@example.com'));
    for I := Low(BAD) to High(BAD) do
    begin
        ZBMockResponse(200, '{"found":true,"active_in_days":' + BAD[I] + '}', 'application/json');
        Raised := False;
        try
            ZbActivityData('a@example.com');
        except
            on E: ZbException do
            begin
                Raised := True;
                AssertEquals(BAD[I] + ' status', 200, E.StatusCode);
            end;
        end;
        AssertTrue('active_in_days ' + BAD[I] + ' was taken as a count', Raised);
    end;
end;

procedure TTestBehaviour.TestTheSuccessFlagSet;
const
    TRUE_BODIES: array[0..9] of String = (
        '{"success":true}', '{"success":1}', '{"success":1.0}', '{"success":1e0}', '{"success":"true"}',
        '{"success":" TRUE\t"}', '{"success":"1"}', '{"Success":true}', '{"success":10e-1}', '{"success":"True"}');
    FALSE_BODIES: array[0..9] of String = (
        '{"success":false}', '{"success":0}', '{"success":0.0}', '{"success":-0}', '{"success":"false"}',
        '{"success":"\nFalse "}', '{"success":"0"}', '{"SUCCESS":false}', '{"success":0e5}', '{"success":False}');
    OTHER_BODIES: array[0..13] of String = (
        '{"success":null}', '{"success":2}', '{"success":0.4}', '{"success":"maybe"}', '{"success":""}',
        '{"success":{}}', '{"success":[]}', '{"success":[false]}', '{"success":-1}', '{"success":1.0000001}',
        '{"success":"2"}', '{"success":"\u00a01"}', '{"success":1e1}', '{"success":"yes"}');
var
    I: Integer;
begin
    for I := Low(TRUE_BODIES) to High(TRUE_BODIES) do
        AssertTrue(TRUE_BODIES[I], ZbSuccessFlagOfBody(TRUE_BODIES[I]) = zsfTrue);
    for I := Low(FALSE_BODIES) to High(FALSE_BODIES) do
    begin
        AssertTrue(FALSE_BODIES[I], ZbSuccessFlagOfBody(FALSE_BODIES[I]) = zsfFalse);
        AssertTrue(FALSE_BODIES[I] + ' failure', ZbSuccessMeansFailure(FALSE_BODIES[I]));
    end;
    for I := Low(OTHER_BODIES) to High(OTHER_BODIES) do
    begin
        AssertTrue(OTHER_BODIES[I], ZbSuccessFlagOfBody(OTHER_BODIES[I]) = zsfNotFlag);
        AssertFalse(OTHER_BODIES[I] + ' failure', ZbSuccessMeansFailure(OTHER_BODIES[I]));
    end;
    AssertTrue('absent', ZbSuccessFlagOfBody('{"a":1}') = zsfAbsent);
    AssertTrue('nested only', ZbSuccessFlagOfBody('{"meta":{"success":false}}') = zsfAbsent);
    AssertTrue('in a string', ZbSuccessFlagOfBody('{"a":"\"success\":false"}') = zsfAbsent);
    AssertTrue('not an object', ZbSuccessFlagOfBody('[{"success":false}]') = zsfAbsent);
end;

procedure TTestBehaviour.TestASuccessThatIsNoFlagHidesNothing;
const
    ODD: array[0..5] of String = ('2', '"maybe"', '0.4', 'null', '{}', '[]');
var
    Index, V: Integer;
    Text: String;
    Status: Integer;
begin
    Text := '';
    for V := Low(ODD) to High(ODD) do
        for Index := 0 to High(NO_RESULT_NAMES) do
        begin
            { Judged as if "success" were not there: an error text alone, with
              the field beside it, is an error. }
            if (Index >= FINDER_FIRST) or (Index in [19..23]) then
            begin
                AssertTrue(NO_RESULT_NAMES[Index] + ' ' + ODD[V] + ' with an error',
                    NoResultCall(Index, '{"success":' + ODD[V] + ',"error":"Invalid API key"}', Text, Status));
                AssertTrue(NO_RESULT_NAMES[Index] + ' ' + ODD[V] + ' with a message: ' + Text,
                    NoResultCall(Index, '{"success":' + ODD[V] + ',"message":"Invalid file id"}', Text, Status));
                if Index < FINDER_FIRST then
                    AssertTrue(NO_RESULT_NAMES[Index] + ' ' + ODD[V] + ' alone',
                        NoResultCall(Index, '{"success":' + ODD[V] + '}', Text, Status));
            end;
        end;
    { A real true beside a message is not an error (as before). }
    AssertFalse('success true beside a message',
        NoResultCall(FINDER_FIRST, '{"success":true,"message":"hello","domain":"example.com"}', Text, Status));
    AssertTrue('a file status of a non-flag only',
        NoResultCall(STATUS_FIRST, '{"success":"maybe"}', Text, Status));
    AssertFalse('a file status with a real 1',
        NoResultCall(STATUS_FIRST, '{"success":1}', Text, Status));
end;

procedure TTestBehaviour.TestARealFalseFlagInAnySpellingIsAFailure;
const
    FLAGS: array[0..4] of String = ('0', '0.0', '"0"', '" FALSE "', 'false');
var
    I, Index: Integer;
    Text: String;
    Status: Integer;
begin
    Text := '';
    for I := Low(FLAGS) to High(FLAGS) do
    begin
        for Index := 0 to High(NO_RESULT_NAMES) do
            if Index in [4, 7, 19..23, 29..39] then
                AssertTrue(NO_RESULT_NAMES[Index] + ' with success ' + FLAGS[I],
                    NoResultCall(Index, '{"success":' + FLAGS[I] + ',"message":"Invalid file id"}', Text, Status));
    end;
    ZBMockResponse(200, '{"success":0,"message":"Invalid file id"}', 'application/json');
    try
        ZbGetApiUsage;
        Fail('no error');
    except
        on E: ZbException do
            AssertTrue(E.Message, Pos('Invalid file id', E.Message) > 0);
    end;
end;

procedure TTestBehaviour.TestErrorTextOfALongObjectAndOfAList;
var
    Long: String;
begin
    { A long object with a short message field says the field. }
    Long := '{"message":"File cannot be found.","pad":"' + StringOfChar('x', 5000) + '"}';
    AssertEquals('File cannot be found.', ZbMessageFromBody(Long, 500));
    AssertEquals('File cannot be found.', ZbMessageFromBody(Long, 200));
    { Without one, the fixed sentence. }
    AssertEquals('ZeroBounce returned an error response.',
        ZbMessageFromBody('{"pad":"' + StringOfChar('x', 5000) + '"}', 500));
    { A list on an error status keeps its text; on a success status it is a
      file's place and has none. }
    AssertEquals('["Invalid key"]', ZbMessageFromBody('["Invalid key"]', 401));
    AssertEquals('ZeroBounce returned an error response.', ZbMessageFromBody('["Invalid key"]', 200));
end;

initialization
    RegisterTest(TTestBehaviour);

end.
