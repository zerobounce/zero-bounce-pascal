program ExampleAiScoring;

uses
    SysUtils,
    ZbBulk, // AiScoring methods, TZbBulkParams record
    ZbStructures, // TZBFileFeedback, TZBFileStatus, TZBBulkResponse records
    ZbUtility; // ZBSetApiKey method

procedure PerformAiScoringCheck;
const
    CSV_FILE_CONTENT = '' +
        'valid@example.com,99.110.204.1' + LineEnding +
        'invalid@example.com,99.110.204.1' + LineEnding +
        'donotmail@example.com,' + LineEnding +
        'unknown@example.com,';

    ONE_SECOND = 1000;
var
    SubmitParam: TZbBulkParams;
    FileFeedback: TZBFileFeedback;
    FileStatus: TZBFileStatus;
    FileId: String;
    ProcessingResult: TZBBulkResponse;
    WaitAmount: Integer;
begin
    ZbSetApiKey('YOUR__API__KEY');

    SubmitParam.EmailAddressColumn := 1;
    SubmitParam.IpAddressColumn := 2;
    SubmitParam.HasHeaderRow := FALSE;

    // submit csv file for bulk validation
    FileFeedback := ZbAiScoringFileSubmit(CSV_FILE_CONTENT, SubmitParam);
    FileId := FileFeedback.FileId;

    // wait for the file to be processed
    WaitAmount := ONE_SECOND;
    WriteLn('Waiting for submitted file to be validated.');

    // check for the file status
    FileStatus := ZbAiScoringFileStatusCheck(FileId);
    while FileStatus.CompletePercentage < 100.0 do
    begin
         Sleep(WaitAmount);
         if WaitAmount < 10 then
            WaitAmount += ONE_SECOND;

         FileStatus := ZbAiScoringFileStatusCheck(FileId);
         Write('.');
    end;
    WriteLn;

    // Fetch the resulted file
    ProcessingResult := ZbAiScoringResultFetch(FileId);
    WriteLn('Result file:');
    WriteLn(ProcessingResult.Content);
    WriteLn;

    // delete result file
    WriteLn('Deleting result file from server..');
    FileFeedback := ZbAiScoringResultDelete(FileId);
    WriteLn(FileFeedback.Message);

end;


begin
  try
    PerformAiScoringCheck;
  except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
  end;
end.

