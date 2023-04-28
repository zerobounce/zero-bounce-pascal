unit ZbBulk;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils,
    ZbStructures, ZbUtility;


type
    // Provide information about the CSV file being uploaded
    // and/or other request-specific parameters
    //
    // COLUMN INDEXES START FROM 1
    TZbBulkParams = record
        EmailAddressColumn: Integer;
        FirstNameColumn: Integer;
        LastNameColumn: Integer;
        GenderColumn: Integer;
        IpAddressColumn: Integer;
        HasHeaderRow: Boolean;
        RemoveDuplicate: Boolean;
    end;

function ZbFromDataFromFileSubmitRecord(SubmitParams: TZbBulkParams): TStrings;

function ZbBulkValidationFileSubmit(FileContent: String; FileParams: TZbBulkParams): TZBFileFeedback;
function ZbBulkValidationFileStatusCheck(FileId: String): TZBFileStatus;
function ZbBulkValidationResultFetch(FileId: String): TZBBulkResponse;
function ZbBulkValidationResultDelete(FileId: String): TZBFileFeedback;

function ZbAiScoringFileSubmit(FileContent: String; FileParams: TZbBulkParams): TZBFileFeedback;
function ZbAiScoringFileStatusCheck(FileId: String): TZBFileStatus;
function ZbAiScoringResultFetch(FileId: String): TZBBulkResponse;
function ZbAiScoringResultDelete(FileId: String): TZBFileFeedback;

procedure Register;

implementation

    function ZbFromDataFromFileSubmitRecord(SubmitParams: TZbBulkParams): TStrings;

        function StrBool(Value: Boolean): String;
        begin
            if Value then Result := 'true' else Result := 'false';
        end;

    begin
        Result := TStringList.Create;
        Result.AddPair('api_key', ZbApiKey);
        if SubmitParams.EmailAddressColumn = 0 then
            Result.AddPair('email_address_column', '1') // index cannot be 0
        else
            Result.AddPair('email_address_column', IntToStr(SubmitParams.EmailAddressColumn));

        if SubmitParams.FirstNameColumn > 0 then
            Result.AddPair('first_name_column', IntToStr(SubmitParams.FirstNameColumn));
        if SubmitParams.LastNameColumn > 0 then
            Result.AddPair('last_name_column', IntToStr(SubmitParams.LastNameColumn));
        if SubmitParams.GenderColumn > 0 then
            Result.AddPair('gender_column', IntToStr(SubmitParams.GenderColumn));
        if SubmitParams.IpAddressColumn > 0 then
            Result.AddPair('ip_address_column', IntToStr(SubmitParams.IpAddressColumn));

        Result.AddPair('has_header_row', StrBool(SubmitParams.HasHeaderRow));
        Result.AddPair('remove_duplicate', StrBool(SubmitParams.RemoveDuplicate));
    end;


function GenericFileSubmit(endpoint, FileContent: String; FileParams: TZbBulkParams): TZBFileFeedback;
var
    UrlToAccess: string;
    response: TZbRequestResponse;
    error: ZbException;
    FormData: TStrings;
begin
    UrlToAccess := Concat(BULK_URI, endpoint);
    FormData := ZbFromDataFromFileSubmitRecord(FileParams);
    try
        response := ZBPostRequest(UrlToAccess, FormData, FileContent);
    finally
      FormData.Free;
    end;

    // check for failure
    if response.StatusCode > 299 then
    begin
        error := ZbException.FromResponse('Request failed', response);
        error.MarkHttpError;
        raise error;
    end;

    // attempt json parsing
    try
        Result := ZbFileFeedbackFromJson(response.Payload);
    except
        on e: Exception do
        begin
            error := ZbException.FromResponse(e.Message, response);
            error.MarkJsonError;
            raise error;
        end;
    end;
end;

function GenericFileStatusCheck(endpoint, FileId: String): TZBFileStatus;
var
    UrlToAccess: string;
    response: TZbRequestResponse;
    error: ZbException;
begin
    UrlToAccess := Concat(BULK_URI, endpoint, '?api_key=', ZbApiKey);
    UrlToAccess := Concat(UrlToAccess, '&file_id=', FileId);
    response := ZBGetRequest(UrlToAccess);

    // attempt json parsing
    try
        Result := ZbFileStatusFromJson(response.Payload);
    except on e: Exception do
        begin
            error := ZbException.FromResponse(e.Message, response);
            error.MarkJsonError;
            raise error;
        end;
    end;
end;

function GenericResultFetch(endpoint, FileId: String): TZBBulkResponse;
var
    UrlToAccess: string;
    response: TZbRequestResponse;
    error: ZbException;
begin
    UrlToAccess := Concat(BULK_URI, endpoint, '?api_key=', ZbApiKey);
    UrlToAccess := Concat(UrlToAccess, '&file_id=', FileId);
    response := ZBGetRequest(UrlToAccess);

    if response.ContentType = '' then
    begin
        error := ZbException.FromResponse('No headers found in response', response);
        error.MarkHttpError;
        raise error;
    end;

    if response.ContentType.Contains(JSON_CONTENT_TYPE) then
    begin
        Result.HasContent := False;
        Result.Feedback := ZbFileFeedbackFromJson(response.Payload);
    end
    else
    begin
        Result.HasContent := True;
        Result.Content := response.Payload;
    end;
end;

function GenericResultDelete(endpoint, FileId: String): TZBFileFeedback;
var
    UrlToAccess: string;
    response: TZbRequestResponse;
    error: ZbException;
begin
    UrlToAccess := Concat(BULK_URI, endpoint, '?api_key=', ZbApiKey);
    UrlToAccess := Concat(UrlToAccess, '&file_id=', FileId);
    response := ZBGetRequest(UrlToAccess);

    // attempt json parsing
    try
        Result := ZbFileFeedbackFromJson(response.Payload);
    except on e: Exception do
        begin
            error := ZbException.FromResponse(e.Message, response);
            error.MarkJsonError;
            raise error;
        end;
    end;
end;

// BULK EMAIL VALIDATION
function ZbBulkValidationFileSubmit(FileContent: String; FileParams: TZbBulkParams): TZBFileFeedback;
begin
    Result := GenericFileSubmit(ENDPOINT_FILE_SEND, FileContent, FileParams);
end;

function ZbBulkValidationFileStatusCheck(FileId: String): TZBFileStatus;
begin
    Result := GenericFileStatusCheck(ENDPOINT_FILE_STATUS, FileId);
end;

function ZbBulkValidationResultFetch(FileId: String): TZBBulkResponse;
begin
    Result := GenericResultFetch(ENDPOINT_FILE_RESULT, FileId);
end;

function ZbBulkValidationResultDelete(FileId: String): TZBFileFeedback;
begin
    Result := GenericResultDelete(ENDPOINT_FILE_DELETE, FileId);
end;

// AI SCORING
function ZbAiScoringFileSubmit(FileContent: String; FileParams: TZbBulkParams): TZBFileFeedback;
begin
    Result := GenericFileSubmit(ENDPOINT_SCORING_SEND, FileContent, FileParams);
end;

function ZbAiScoringFileStatusCheck(FileId: String): TZBFileStatus;
begin
    Result := GenericFileStatusCheck(ENDPOINT_SCORING_STATUS, FileId);
end;

function ZbAiScoringResultFetch(FileId: String): TZBBulkResponse;
begin
    Result := GenericResultFetch(ENDPOINT_SCORING_RESULT, FileId);
end;

function ZbAiScoringResultDelete(FileId: String): TZBFileFeedback;
begin
    Result := GenericResultDelete(ENDPOINT_SCORING_DELETE, FileId);
end;

procedure Register;
begin
end;

end.
