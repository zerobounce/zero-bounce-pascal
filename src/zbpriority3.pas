unit ZbPriority3;

{$I zboptions.inc}{$H+}

interface

uses
    Classes, SysUtils,
    ZbStructures, ZbUtility;

type
    TZbEmailFinderSendFileOptions = record
        FirstNameColumn: Integer;
        LastNameColumn: Integer;
        MiddleNameColumn: Integer;
        FullNameColumn: Integer;
        HasHeaderRow: Boolean;
        HasHeaderRowSpecified: Boolean;
    end;

    TZbDomainSearchSendFileOptions = record
        HasHeaderRow: Boolean;
        HasHeaderRowSpecified: Boolean;
    end;

    TZbValidateS3FileOptions = record
        FileName: String;
        HasHeaderRowSpecified: Boolean;
        HasHeaderRow: Boolean;
        RemoveDuplicatesSpecified: Boolean;
        RemoveDuplicates: Boolean;
        RemoveDuplicateSpecified: Boolean;
        RemoveDuplicate: Boolean;
        EmailAddressColumnSpecified: Boolean;
        EmailAddressColumn: Integer;
        ReturnUrl: String;
        FirstNameColumn: Integer;
        LastNameColumn: Integer;
        GenderColumn: Integer;
        IpAddressColumn: Integer;
        AllowPhase2Specified: Boolean;
        AllowPhase2: Boolean;
    end;

    TZbScoreS3FileOptions = record
        FileName: String;
        HasHeaderRowSpecified: Boolean;
        HasHeaderRow: Boolean;
        RemoveDuplicatesSpecified: Boolean;
        RemoveDuplicates: Boolean;
        RemoveDuplicateSpecified: Boolean;
        RemoveDuplicate: Boolean;
        EmailAddressColumnSpecified: Boolean;
        EmailAddressColumn: Integer;
    end;

    PTZbEmailFinderSendFileOptions = ^TZbEmailFinderSendFileOptions;
    PTZbDomainSearchSendFileOptions = ^TZbDomainSearchSendFileOptions;

function ZbSendEmailFinderFile(FileContent: String; DomainColumn: Integer;
    Options: PTZbEmailFinderSendFileOptions = nil): TZBFileFeedback;
function ZbEmailFinderFileStatus(FileId: String): TZBFileStatus;
function ZbEmailFinderResultFetch(FileId: String): TZBBulkResponse;
function ZbEmailFinderResultDelete(FileId: String): TZBFileFeedback;

function ZbSendDomainSearchFile(FileContent: String; DomainColumn: Integer;
    Options: PTZbDomainSearchSendFileOptions = nil): TZBFileFeedback;
function ZbDomainSearchFileStatus(FileId: String): TZBFileStatus;
function ZbDomainSearchResultFetch(FileId: String): TZBBulkResponse;
function ZbDomainSearchResultDelete(FileId: String): TZBFileFeedback;

function ZbSendListEvaluatorFile(FileContent: String; EmailAddressColumn: Integer): TZBFileFeedback;
function ZbGetListEvaluatorResult(FileId: String): String;
function ZbSendEvaluatedFile(FileContent: String): TZBFileFeedback;

function ZbAddFilter(const Rule, Target, Value: String): String;
function ZbListFilters(UsePost: Boolean): String;
function ZbDeleteFilter(const Rule, Target, Value: String): String;

function ZbListS3Files(const Bucket, Search: String): String;
function ZbValidateS3File(Options: TZbValidateS3FileOptions): TZBFileFeedback;
function ZbScoreS3File(Options: TZbScoreS3FileOptions): TZBFileFeedback;
function ZbExportS3File(const FileId, ExportBucket: String): TZBFileFeedback;

procedure Register;

implementation

const
    { ZbBulkRootURL lives in ZbUtility so tests can point bulk calls at a local stub. }
    ENDPOINT_EMAIL_FINDER_SEND = '/email-finder/sendfile';
    ENDPOINT_EMAIL_FINDER_STATUS = '/email-finder/filestatus';
    ENDPOINT_EMAIL_FINDER_RESULT = '/email-finder/getfile';
    ENDPOINT_EMAIL_FINDER_DELETE = '/email-finder/deletefile';
    ENDPOINT_DOMAIN_SEARCH_SEND = '/domain-search/sendfile';
    ENDPOINT_DOMAIN_SEARCH_STATUS = '/domain-search/filestatus';
    ENDPOINT_DOMAIN_SEARCH_RESULT = '/domain-search/getfile';
    ENDPOINT_DOMAIN_SEARCH_DELETE = '/domain-search/deletefile';
    ENDPOINT_LIST_EVALUATOR_SEND = '/listevaluator/';
    ENDPOINT_SEND_EVALUATED_FILE = '/send-evaluated-file/';
    ENDPOINT_VALIDATE_S3 = '/validateS3file';
    ENDPOINT_SCORE_S3 = '/scoreS3file';
    ENDPOINT_EXPORT_S3 = '/exportS3file';
    ENDPOINT_LIST_S3 = '/listS3files';
    ENDPOINT_FILTERS_ADD = '/filters/add';
    ENDPOINT_FILTERS_LIST = '/filters/list';
    ENDPOINT_FILTERS_DELETE = '/filters/delete';

    function StrBool(Value: Boolean): String;
    begin
        if Value then Result := 'true' else Result := 'false';
    end;

    procedure PutOptionalBool(Form: TStrings; const Key: String; Specified: Boolean; Value: Boolean);
    begin
        if Specified then
            Form.AddPair(Key, StrBool(Value));
    end;

    function P3MultipartSubmit(const Url: String; FileContent: String; FormData: TStrings): TZBFileFeedback;
    var
        response: TZbRequestResponse;
        error: ZbException;
    begin
        try
            response := ZBPostRequest(Url, FormData, FileContent);
        finally
            FormData.Free;
        end;
        if response.StatusCode > 299 then
        begin
            error := ZbException.FromResponse('Request failed', response);
            error.MarkHttpError;
            raise error;
        end;
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

    function P3FormSubmit(const Url: String; Form: TStrings): TZBFileFeedback;
    var
        Body: String;
        response: TZbRequestResponse;
        error: ZbException;
        I: Integer;
    begin
        Body := '';
        for I := 0 to Form.Count - 1 do
        begin
            if Body <> '' then Body := Body + '&';
            Body := Body + EncodeParam(Form.Names[I]) + '=' + EncodeParam(Form.ValueFromIndex[I]);
        end;
        Form.Free;
        response := ZBFormPostRequest(Url, Body);
        if response.StatusCode > 299 then
        begin
            error := ZbException.FromResponse('Request failed', response);
            error.MarkHttpError;
            raise error;
        end;
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

    function P3RawJsonGet(const Url: String): String;
    var
        response: TZbRequestResponse;
    begin
        response := ZBGetRequest(Url);
        Result := response.Payload;
    end;

    function P3RawJsonForm(const Url: String; Form: TStrings): String;
    var
        Body: String;
        response: TZbRequestResponse;
        I: Integer;
    begin
        Body := '';
        for I := 0 to Form.Count - 1 do
        begin
            if Body <> '' then Body := Body + '&';
            Body := Body + EncodeParam(Form.Names[I]) + '=' + EncodeParam(Form.ValueFromIndex[I]);
        end;
        Form.Free;
        response := ZBFormPostRequest(Url, Body);
        Result := response.Payload;
    end;

    function P3BulkRootStatus(const Endpoint, FileId: String): TZBFileStatus;
    var
        UrlToAccess: String;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(ZbBulkRootURL, Endpoint, '?api_key=', EncodeParam(ZbApiKey));
        UrlToAccess := Concat(UrlToAccess, '&file_id=', EncodeParam(FileId));
        response := ZBGetRequest(UrlToAccess);
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

    function P3BulkRootFetch(const Endpoint, FileId: String): TZBBulkResponse;
    var
        UrlToAccess: String;
        response: TZbRequestResponse;
        error: ZbException;
        IsJsonCT: Boolean;
    begin
        UrlToAccess := Concat(ZbBulkRootURL, Endpoint, '?api_key=', EncodeParam(ZbApiKey));
        UrlToAccess := Concat(UrlToAccess, '&file_id=', EncodeParam(FileId));
        response := ZBGetRequest(UrlToAccess);
        if response.ContentType = '' then
        begin
            error := ZbException.FromResponse('No headers found in response', response);
            error.MarkHttpError;
            raise error;
        end;
        if ZbBodyLooksLikeHtml(response.Payload) then
            raise ZbException.FromResponse('', response);
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

    function P3BulkRootDelete(const Endpoint, FileId: String): TZBFileFeedback;
    var
        UrlToAccess: String;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(ZbBulkRootURL, Endpoint, '?api_key=', EncodeParam(ZbApiKey));
        UrlToAccess := Concat(UrlToAccess, '&file_id=', EncodeParam(FileId));
        response := ZBGetRequest(UrlToAccess);
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

    function BuildEmailFinderForm(DomainColumn: Integer; Options: PTZbEmailFinderSendFileOptions): TStrings;
    var
        Form: TStrings;
    begin
        Form := TStringList.Create;
        Form.AddPair('api_key', ZbApiKey);
        Form.AddPair('domain_column', IntToStr(DomainColumn));
        if (Options <> nil) and Options^.HasHeaderRowSpecified then
            Form.AddPair('has_header_row', StrBool(Options^.HasHeaderRow));
        if (Options <> nil) and (Options^.FirstNameColumn > 0) then
            Form.AddPair('first_name_column', IntToStr(Options^.FirstNameColumn));
        if (Options <> nil) and (Options^.LastNameColumn > 0) then
            Form.AddPair('last_name_column', IntToStr(Options^.LastNameColumn));
        if (Options <> nil) and (Options^.MiddleNameColumn > 0) then
            Form.AddPair('middle_name_column', IntToStr(Options^.MiddleNameColumn));
        if (Options <> nil) and (Options^.FullNameColumn > 0) then
            Form.AddPair('full_name_column', IntToStr(Options^.FullNameColumn));
        Result := Form;
    end;

function ZbSendEmailFinderFile(FileContent: String; DomainColumn: Integer;
    Options: PTZbEmailFinderSendFileOptions): TZBFileFeedback;
begin
    if DomainColumn < 1 then
        raise ZbException.Create('Empty parameter: domain_column');
    Result := P3MultipartSubmit(
        Concat(ZbBulkRootURL, ENDPOINT_EMAIL_FINDER_SEND),
        FileContent,
        BuildEmailFinderForm(DomainColumn, Options)
    );
end;

function ZbEmailFinderFileStatus(FileId: String): TZBFileStatus;
begin
    Result := P3BulkRootStatus(ENDPOINT_EMAIL_FINDER_STATUS, FileId);
end;

function ZbEmailFinderResultFetch(FileId: String): TZBBulkResponse;
begin
    Result := P3BulkRootFetch(ENDPOINT_EMAIL_FINDER_RESULT, FileId);
end;

function ZbEmailFinderResultDelete(FileId: String): TZBFileFeedback;
begin
    Result := P3BulkRootDelete(ENDPOINT_EMAIL_FINDER_DELETE, FileId);
end;

function ZbSendDomainSearchFile(FileContent: String; DomainColumn: Integer;
    Options: PTZbDomainSearchSendFileOptions): TZBFileFeedback;
var
    Form: TStrings;
begin
    if DomainColumn < 1 then
        raise ZbException.Create('Empty parameter: domain_column');
    Form := TStringList.Create;
    Form.AddPair('api_key', ZbApiKey);
    Form.AddPair('domain_column', IntToStr(DomainColumn));
    if (Options <> nil) and Options^.HasHeaderRowSpecified then
        Form.AddPair('has_header_row', StrBool(Options^.HasHeaderRow));
    Result := P3MultipartSubmit(Concat(ZbBulkRootURL, ENDPOINT_DOMAIN_SEARCH_SEND), FileContent, Form);
end;

function ZbDomainSearchFileStatus(FileId: String): TZBFileStatus;
begin
    Result := P3BulkRootStatus(ENDPOINT_DOMAIN_SEARCH_STATUS, FileId);
end;

function ZbDomainSearchResultFetch(FileId: String): TZBBulkResponse;
begin
    Result := P3BulkRootFetch(ENDPOINT_DOMAIN_SEARCH_RESULT, FileId);
end;

function ZbDomainSearchResultDelete(FileId: String): TZBFileFeedback;
begin
    Result := P3BulkRootDelete(ENDPOINT_DOMAIN_SEARCH_DELETE, FileId);
end;

function ZbSendListEvaluatorFile(FileContent: String; EmailAddressColumn: Integer): TZBFileFeedback;
var
    Form: TStrings;
begin
    if EmailAddressColumn < 1 then
        raise ZbException.Create('Empty parameter: email_address_column');
    Form := TStringList.Create;
    Form.AddPair('api_key', ZbApiKey);
    Form.AddPair('email_address_column', IntToStr(EmailAddressColumn));
    Result := P3MultipartSubmit(Concat(BULK_URI, ENDPOINT_LIST_EVALUATOR_SEND), FileContent, Form);
end;

function ZbGetListEvaluatorResult(FileId: String): String;
var
    UrlToAccess: String;
begin
    UrlToAccess := Concat(BULK_URI, '/listevaluator/', FileId, '/?api_key=', EncodeParam(ZbApiKey));
    Result := P3RawJsonGet(UrlToAccess);
end;

function ZbSendEvaluatedFile(FileContent: String): TZBFileFeedback;
var
    Form: TStrings;
begin
    Form := TStringList.Create;
    Form.AddPair('api_key', ZbApiKey);
    Result := P3MultipartSubmit(Concat(BULK_URI, ENDPOINT_SEND_EVALUATED_FILE), FileContent, Form);
end;

function ZbAddFilter(const Rule, Target, Value: String): String;
var
    Form: TStrings;
begin
    Form := TStringList.Create;
    Form.AddPair('api_key', ZbApiKey);
    Form.AddPair('rule', Rule);
    Form.AddPair('target', Target);
    Form.AddPair('value', Value);
    Result := P3RawJsonForm(Concat(ZbApiBaseURL, ENDPOINT_FILTERS_ADD), Form);
end;

function ZbListFilters(UsePost: Boolean): String;
var
    Form: TStrings;
begin
    if UsePost then
    begin
        Form := TStringList.Create;
        Form.AddPair('api_key', ZbApiKey);
        Result := P3RawJsonForm(Concat(ZbApiBaseURL, ENDPOINT_FILTERS_LIST), Form);
    end
    else
        Result := P3RawJsonGet(Concat(ZbApiBaseURL, ENDPOINT_FILTERS_LIST, '?api_key=', EncodeParam(ZbApiKey)));
end;

function ZbDeleteFilter(const Rule, Target, Value: String): String;
var
    Form: TStrings;
begin
    Form := TStringList.Create;
    Form.AddPair('api_key', ZbApiKey);
    Form.AddPair('rule', Rule);
    Form.AddPair('target', Target);
    Form.AddPair('value', Value);
    Result := P3RawJsonForm(Concat(ZbApiBaseURL, ENDPOINT_FILTERS_DELETE), Form);
end;

function ZbListS3Files(const Bucket, Search: String): String;
var
    UrlToAccess: String;
    response: TZbRequestResponse;
begin
    UrlToAccess := Concat(ZbBulkRootURL, ENDPOINT_LIST_S3, '?api_key=', EncodeParam(ZbApiKey));
    if Bucket <> '' then
        UrlToAccess := Concat(UrlToAccess, '&bucket=', EncodeParam(Bucket));
    if Search <> '' then
        UrlToAccess := Concat(UrlToAccess, '&search=', EncodeParam(Search));
    response := ZBGetRequest(UrlToAccess);
    Result := response.Payload;
end;

function ZbValidateS3File(Options: TZbValidateS3FileOptions): TZBFileFeedback;
var
    Form: TStrings;
begin
    if Trim(Options.FileName) = '' then
        raise ZbException.Create('Empty parameter: file_name');
    Form := TStringList.Create;
    Form.AddPair('file_name', Options.FileName);
    PutOptionalBool(Form, 'has_header_row', Options.HasHeaderRowSpecified, Options.HasHeaderRow);
    PutOptionalBool(Form, 'remove_duplicates', Options.RemoveDuplicatesSpecified, Options.RemoveDuplicates);
    PutOptionalBool(Form, 'remove_duplicate', Options.RemoveDuplicateSpecified, Options.RemoveDuplicate);
    if Options.EmailAddressColumnSpecified then
        Form.AddPair('email_address_column', IntToStr(Options.EmailAddressColumn));
    if Options.ReturnUrl <> '' then
        Form.AddPair('return_url', Options.ReturnUrl);
    if Options.FirstNameColumn > 0 then
        Form.AddPair('first_name_column', IntToStr(Options.FirstNameColumn));
    if Options.LastNameColumn > 0 then
        Form.AddPair('last_name_column', IntToStr(Options.LastNameColumn));
    if Options.GenderColumn > 0 then
        Form.AddPair('gender_column', IntToStr(Options.GenderColumn));
    if Options.IpAddressColumn > 0 then
        Form.AddPair('ip_address_column', IntToStr(Options.IpAddressColumn));
    if Options.AllowPhase2Specified then
        Form.AddPair('allow_phase_2', StrBool(Options.AllowPhase2));
    Form.AddPair('api_key', ZbApiKey);
    Result := P3FormSubmit(Concat(ZbBulkRootURL, ENDPOINT_VALIDATE_S3), Form);
end;

function ZbScoreS3File(Options: TZbScoreS3FileOptions): TZBFileFeedback;
var
    Form: TStrings;
begin
    if Trim(Options.FileName) = '' then
        raise ZbException.Create('Empty parameter: file_name');
    Form := TStringList.Create;
    Form.AddPair('file_name', Options.FileName);
    PutOptionalBool(Form, 'has_header_row', Options.HasHeaderRowSpecified, Options.HasHeaderRow);
    PutOptionalBool(Form, 'remove_duplicates', Options.RemoveDuplicatesSpecified, Options.RemoveDuplicates);
    PutOptionalBool(Form, 'remove_duplicate', Options.RemoveDuplicateSpecified, Options.RemoveDuplicate);
    if Options.EmailAddressColumnSpecified then
        Form.AddPair('email_address_column', IntToStr(Options.EmailAddressColumn));
    Form.AddPair('api_key', ZbApiKey);
    Result := P3FormSubmit(Concat(ZbBulkRootURL, ENDPOINT_SCORE_S3), Form);
end;

function ZbExportS3File(const FileId, ExportBucket: String): TZBFileFeedback;
var
    Form: TStrings;
begin
    if Trim(FileId) = '' then
        raise ZbException.Create('Empty parameter: file_id');
    if Trim(ExportBucket) = '' then
        raise ZbException.Create('Empty parameter: export_bucket');
    Form := TStringList.Create;
    Form.AddPair('file_id', FileId);
    Form.AddPair('export_bucket', ExportBucket);
    Form.AddPair('api_key', ZbApiKey);
    Result := P3FormSubmit(Concat(ZbBulkRootURL, ENDPOINT_EXPORT_S3), Form);
end;

procedure Register;
begin
end;

end.
