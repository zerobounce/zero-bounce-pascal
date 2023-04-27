unit TestDeserialization;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry,
    MockValues, ZbStructures;

type

    TTestDeserialization = class(TTestCase)

    published
        procedure TestApiUsageParse;
        procedure TestValidationParseValidContent;
        procedure TestValidationParseInvalidContent;
        procedure TestBatchErrorParse;
        procedure TestBatchValidationParseOkContent;
        procedure TestBatchValidationParseErrorContent;
        procedure TestFileFeedbackParse;
        procedure TestFileFeedbackParsePartial;
        procedure TestFileStatusParse;
    end;

implementation

procedure TTestDeserialization.TestApiUsageParse;
var
   ApiUsage: TApiUsage;
begin
    ApiUsage := ZbApiUsageFromJson(API_USAGE_RESPONSE);
    AssertEquals('total', ApiUsage.Total, 7);
    AssertEquals('unknown', ApiUsage.StatusUnknown, 0);
end;

procedure TTestDeserialization.TestValidationParseValidContent;
var
    validation: TZbValidationResult;
begin
    validation := ZbValidationFromJson(VALDATION_RESPONSE_VALID);
    AssertEquals('address', validation.Address, 'valid@example.com');
    AssertEquals('did_you_mean', validation.DidYouMean, '');
    AssertEquals('free_email', validation.FreeEmail, FALSE);

    AssertEquals(
        'processed_at',
        validation.ProcessedAt,
        EncodeDateTime(2023, 04, 25, 13, 08, 24, 269)
    );
end;

procedure TTestDeserialization.TestValidationParseInvalidContent;
var
    validation: TZbValidationResult;
begin
    validation := ZbValidationFromJson(VALDATION_RESPONSE_INVALID);
    AssertEquals('address', validation.Address, 'invalid@example.com');
    AssertEquals('status', validation.Status, 'invalid');
    AssertEquals('sub_status', validation.SubStatus, 'mailbox_not_found');

    AssertEquals(
        'processed_at',
        validation.ProcessedAt,
        EncodeDateTime(2023, 12, 25, 13, 08, 24, 1)
    );
end;

procedure TTestDeserialization.TestBatchErrorParse;
var
    BatchError: TZbBatchError;
begin
    BatchError := ZbBatchErrorFromJson(BATCH_VALIDATE_ERROR_SAMPLE);
    AssertEquals('email_address', BatchError.EmailAddress, 'all');
    AssertTrue('error message parse', BatchError.Error.Contains('Invalid API Key'));
end;

procedure TTestDeserialization.TestBatchValidationParseOkContent;
var
    BatchResult: TZBBatchValidation;
begin
    BatchResult := ZbBatchValidationFromJson(BATCH_VALIDATE_OK);
    AssertEquals('email_batch length', BatchResult.EmailBatchLength, 2);
    AssertEquals('errors length', BatchResult.ErrorsLength, 0);
    AssertEquals('status of first validation', BatchResult.EmailBatch[0].Status, 'valid');
end;

procedure TTestDeserialization.TestBatchValidationParseErrorContent;
var
    BatchResult: TZBBatchValidation;
begin
    BatchResult := ZbBatchValidationFromJson(BATCH_VALIDATE_ERROR);
    AssertEquals('email_batch length', BatchResult.EmailBatchLength, 0);
    AssertEquals('errors length', BatchResult.ErrorsLength, 1);
    AssertEquals('email_address in error', BatchResult.Errors[0].EmailAddress, 'all');
end;


procedure TTestDeserialization.TestFileFeedbackParse;
var
    parsed: TZbFileFeedback;
begin
    parsed := ZbFileFeedbackFromJson(BULK_SUBMIT_OK);
    AssertEquals('success', parsed.Success, True);
    AssertTrue('message does not have content', Length(parsed.Message) > 0);
    AssertTrue('file_name does not have content', Length(parsed.FileName) > 0);
    AssertTrue('file_id does not have content', Length(parsed.FileId) > 0);
end;

procedure TTestDeserialization.TestFileFeedbackParsePartial;
var
    parsed: TZbFileFeedback;
begin
    parsed := ZbFileFeedbackFromJson(BULK_RESULT_DELETED);
    AssertEquals('success', parsed.Success, False);
    AssertTrue('message does not have content', Length(parsed.Message) > 0);
    AssertTrue('file_name should not have content', Length(parsed.FileName) = 0);
    AssertTrue('file_id should not have content', Length(parsed.FileId) = 0);
end;

procedure TTestDeserialization.TestFileStatusParse;
var
    parsed: TZbFileStatus;
begin
    parsed := ZbFileStatusFromJson(BULK_STATUS_OK);
    AssertEquals('complete_percentage', parsed.CompletePercentage, 100.0);
    AssertEquals('success', parsed.Success, True);

    AssertTrue('file_id does not have content', Length(parsed.FileId) > 0);
    AssertTrue('file_name does not have content', Length(parsed.FileName) > 0);
    AssertTrue('file_status does not have content', Length(parsed.FileStatus) > 0);
    AssertTrue('return_url does not have content', Length(parsed.ReturnUrl) > 0);

    AssertTrue('error_reason should not have content', Length(parsed.ErrorReason) = 0);

    AssertEquals(
        'upload_date',
        parsed.UploadDate,
        EncodeDateTime(2023, 4, 26, 17, 52, 23, 0)
    );
end;


initialization
    RegisterTest(TTestDeserialization);
end.

