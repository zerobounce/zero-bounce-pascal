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
    end;

implementation

procedure TTestPriority3.TestValidateS3FileOk;
var
    Options: TZbValidateS3FileOptions;
    Feedback: TZBFileFeedback;
begin
    ZBMockResponse(200, '{"success":true,"file_id":"s3-1","message":""}');
    Options.FileName := 'bucket/key.csv';
    Feedback := ZbValidateS3File(Options);
    AssertTrue(ZbResponseMock.UrlCalled.Contains('/validateS3file'));
    AssertTrue(Feedback.Success);
    AssertEquals('s3-1', Feedback.FileId);
end;

initialization
    RegisterTest(TTestPriority3);

end.
