unit TestPriority3;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    BaseTest, ZbPriority3, ZbStructures, ZbUtility;

type
    TTestPriority3 = class(TBaseTestCase)
    published
        procedure TestValidateS3FileOk;
        procedure TestGetFileJsonBodiesAreErrors;
    end;

implementation

procedure TTestPriority3.TestValidateS3FileOk;
var
    Options: TZbValidateS3FileOptions;
    Feedback: TZBFileFeedback;
begin
    ZBMockResponse(200, '{"success":true,"file_id":"s3-1","message":""}');
    FillChar(Options, SizeOf(Options), 0);
    Options.FileName := 'bucket/key.csv';
    { LIVE: the job is refused without a return_url. }
    Options.ReturnUrl := 'https://example.com/done';
    Feedback := ZbValidateS3File(Options);
    AssertTrue(ZbResponseMock.UrlCalled.Contains('/validateS3file'));
    AssertTrue(Feedback.Success);
    AssertEquals('s3-1', Feedback.FileId);
end;

procedure TTestPriority3.TestGetFileJsonBodiesAreErrors;
type
    TCase = record
        Payload, ContentType, Why: String;
    end;
const
    { Each body is caught by one check alone: a JSON content type with a body
      that is not an object, and an object that says nothing about failure
      under a file content type. }
    CASES: array[0..2] of TCase = (
        (Payload: '["File is still processing"]'; ContentType: 'application/json'; Why: 'a JSON content type'),
        (Payload: '{"file_status":"Processing"}'; ContentType: 'application/octet-stream'; Why: 'a JSON object body'),
        (Payload: '{"file_status":"Processing"}'; ContentType: 'application/json; charset=utf-8'; Why: 'both')
    );
var
    I, Call: Integer;
    Raised: Boolean;
    Name: String;
begin
    for Call := 0 to 1 do
        for I := Low(CASES) to High(CASES) do
        begin
            ZBMockResponse(200, CASES[I].Payload, CASES[I].ContentType);
            Raised := False;
            if Call = 0 then
                Name := 'ZbEmailFinderResultFetch'
            else
                Name := 'ZbDomainSearchResultFetch';
            try
                if Call = 0 then
                    ZbEmailFinderResultFetch('fid')
                else
                    ZbDomainSearchResultFetch('fid');
            except on e: ZbException do
                begin
                    Raised := True;
                    AssertEquals(Name + ' (' + CASES[I].Why + ')', 200, e.StatusCode);
                end;
            end;
            AssertTrue(Name + ' returned a file for ' + CASES[I].Why, Raised);
        end;
end;

initialization
    RegisterTest(TTestPriority3);

end.
