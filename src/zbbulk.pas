unit ZbBulk;

{$I zboptions.inc}

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
        { When AllowPhase2Specified, sends allow_phase_2 (validation bulk sendfile only). }
        AllowPhase2Specified: Boolean;
        AllowPhase2: Boolean;
    end;

    { Optional query parameters for bulk getfile (validation and scoring). }
    TZbGetFileOptions = record
        DownloadType: String;
        ActivityDataSpecified: Boolean;
        ActivityData: Boolean;
    end;

    PTZbGetFileOptions = ^TZbGetFileOptions;

function ZbFromDataFromFileSubmitRecord(SubmitParams: TZbBulkParams; const Endpoint: String): TStrings;

function ZbBulkValidationFileSubmit(FileContent: String; FileParams: TZbBulkParams): TZBFileFeedback;
function ZbBulkValidationFileStatusCheck(FileId: String): TZBFileStatus;
function ZbBulkValidationResultFetch(FileId: String; Options: PTZbGetFileOptions = nil): TZBBulkResponse;
function ZbBulkValidationResultDelete(FileId: String): TZBFileFeedback;

function ZbAiScoringFileSubmit(FileContent: String; FileParams: TZbBulkParams): TZBFileFeedback;
function ZbAiScoringFileStatusCheck(FileId: String): TZBFileStatus;
function ZbAiScoringResultFetch(FileId: String; Options: PTZbGetFileOptions = nil): TZBBulkResponse;
function ZbAiScoringResultDelete(FileId: String): TZBFileFeedback;

procedure Register;

implementation

    function ZbFromDataFromFileSubmitRecord(SubmitParams: TZbBulkParams; const Endpoint: String): TStrings;

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

        if (Endpoint = ENDPOINT_FILE_SEND) and SubmitParams.AllowPhase2Specified then
            Result.AddPair('allow_phase_2', StrBool(SubmitParams.AllowPhase2));
    end;


function GenericFileSubmit(endpoint, FileContent: String; FileParams: TZbBulkParams): TZBFileFeedback;
var
    UrlToAccess: string;
    response: TZbRequestResponse;
    error: ZbException;
    FormData: TStrings;
begin
    UrlToAccess := Concat(BULK_URI, endpoint);
    FormData := ZbFromDataFromFileSubmitRecord(FileParams, endpoint);
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

function GenericResultFetch(const endpoint, FileId: String; Scoring: Boolean;
    const Options: TZbGetFileOptions): TZBBulkResponse;
var
    UrlToAccess: string;
    response: TZbRequestResponse;
    error: ZbException;
    IsJsonCT: Boolean;
begin
    UrlToAccess := Concat(BULK_URI, endpoint, '?api_key=', EncodeParam(ZbApiKey));
    UrlToAccess := Concat(UrlToAccess, '&file_id=', EncodeParam(FileId));
    if Length(Options.DownloadType) > 0 then
        UrlToAccess := Concat(UrlToAccess, '&download_type=', EncodeParam(Options.DownloadType));
    if (not Scoring) and Options.ActivityDataSpecified then
    begin
        if Options.ActivityData then
            UrlToAccess := Concat(UrlToAccess, '&activity_data=true')
        else
            UrlToAccess := Concat(UrlToAccess, '&activity_data=false');
    end;

    response := ZBGetRequest(UrlToAccess);

    if response.ContentType = '' then
    begin
        error := ZbException.FromResponse('No headers found in response', response);
        error.MarkHttpError;
        raise error;
    end;

    IsJsonCT := response.ContentType.Contains(JSON_CONTENT_TYPE);
    if IsJsonCT or ZbGetFileJsonIndicatesError(response.Payload) then
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

function ResolveGetFileOptions(Options: PTZbGetFileOptions): TZbGetFileOptions;
var
    Empty: TZbGetFileOptions;
begin
    if Options <> nil then
        Result := Options^
    else
    begin
        Empty.DownloadType := '';
        Empty.ActivityDataSpecified := False;
        Empty.ActivityData := False;
        Result := Empty;
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

function ZbBulkValidationResultFetch(FileId: String; Options: PTZbGetFileOptions): TZBBulkResponse;
begin
    Result := GenericResultFetch(ENDPOINT_FILE_RESULT, FileId, False, ResolveGetFileOptions(Options));
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

function ZbAiScoringResultFetch(FileId: String; Options: PTZbGetFileOptions): TZBBulkResponse;
begin
    Result := GenericResultFetch(ENDPOINT_SCORING_RESULT, FileId, True, ResolveGetFileOptions(Options));
end;

function ZbAiScoringResultDelete(FileId: String): TZBFileFeedback;
begin
    Result := GenericResultDelete(ENDPOINT_SCORING_DELETE, FileId);
end;

procedure Register;
begin
end;

end.
