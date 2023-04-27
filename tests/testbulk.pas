unit TestBulk;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fgl,
    BaseTest, MockValues, ZbBulk, ZbStructures;

type

    TStringMapping = specialize TFPGMap < String, String >;

    TTestBulk= class(TBaseTestCase)
    protected
        // if does not find and match values, it issues assertion errors
        procedure ExpectKeyInParamsMap(ParamsMap: TStringMapping; Key, ExpectedData: String);
        // if does find, it issues assertion errors
        procedure NotExpectKeyInParamsMap(ParamsMap: TStringMapping; Key: String);
    published
        procedure TestSubmitParamProviderEmpty;
        procedure TestSubmitParamProviderPartial;
        procedure TestSubmitParamProviderFull;
    end;


const
    FILE_PARAM_EMPTY: TZbBulkParams = ();
    FILE_PARAM_PARTIAL: TZbBulkParams = (
        EmailAddressColumn: 2;
        FirstNameColumn: 1
    );
    FILE_PARAM_FULL: TZbBulkParams = (
        EmailAddressColumn: 2;
        FirstNameColumn: 1;
        LastNameColumn: 3;
        GenderColumn: 4;
        IpAddressColumn: 5;
        HasHeaderRow: True;
        RemoveDuplicate: True
    );

// TODO: test:
// GenericFileSubmit
// GenericFileStatusCheck
// GenericResultFetch
// GenericResultDelete
// endpoint called for all methods of BulkValidation and AiScoring

implementation


function FileParamsMapFromRecord(FileParams: TZbBulkParams): TStringMapping;
var
    FormData: TStrings;
    Index: Integer;
    Key, Data: String;
begin
    try
        FormData := ZbFromDataFromFileSubmitRecord(FileParams);
        Result := TStringMapping.Create;
        for Index := 0 to FormData.Count -1 do
        begin
            FormData.GetNameValue(Index, Key, Data);
            Result.InsertKeyData(Index, Key, Data);
        end;
    finally
        FormData.Free;
    end;
end;

procedure TTestBulk.ExpectKeyInParamsMap(ParamsMap: TStringMapping; Key, ExpectedData: String);
var
    IndexFound: Integer;
    FoundData: String;
begin
    IndexFound := ParamsMap.IndexOf(Key);
    AssertTrue('No value for field "' + Key + '" in form params', IndexFound <> -1);
    FoundData := ParamsMap.Data[IndexFound];
    AssertEquals('Value for field "' + Key + '" was unexpected', FoundData, ExpectedData);
end;

procedure TTestBulk.NotExpectKeyInParamsMap(ParamsMap: TStringMapping; Key: String);
var
    IndexFound: Integer;
    FoundData: String;
begin
    FoundData := '';
    IndexFound := ParamsMap.IndexOf(Key);
    if IndexFound <> -1 then
    begin
       FoundData := ParamsMap.Data[IndexFound];
       Fail(
            'No value was expected for field "' + Key + '" in form params. ' +
            'Found: ' + FoundData
       );
    end;
end;


procedure TTestBulk.TestSubmitParamProviderEmpty;
var
    ParamsMap: TStringMapping;
begin
    ParamsMap := FileParamsMapFromRecord(FILE_PARAM_EMPTY);
    try
        ExpectKeyInParamsMap(ParamsMap, 'api_key', MOCK_API_KEY);
        ExpectKeyInParamsMap(ParamsMap, 'email_address_column', '1');
        ExpectKeyInParamsMap(ParamsMap, 'has_header_row', 'false');
        ExpectKeyInParamsMap(ParamsMap, 'remove_duplicate', 'false');

        NotExpectKeyInParamsMap(ParamsMap, 'first_name_column');
        NotExpectKeyInParamsMap(ParamsMap, 'last_name_column');
        NotExpectKeyInParamsMap(ParamsMap, 'gender_column');
        NotExpectKeyInParamsMap(ParamsMap, 'ip_address_column');
    finally
        ParamsMap.Free;
    end;
end;

procedure TTestBulk.TestSubmitParamProviderPartial;
var
    ParamsMap: TStringMapping;
begin
    ParamsMap := FileParamsMapFromRecord(FILE_PARAM_PARTIAL);
    try
        ExpectKeyInParamsMap(ParamsMap, 'api_key', MOCK_API_KEY);
        ExpectKeyInParamsMap(ParamsMap, 'email_address_column', '2');
        ExpectKeyInParamsMap(ParamsMap, 'first_name_column', '1');
        ExpectKeyInParamsMap(ParamsMap, 'has_header_row', 'false');
        ExpectKeyInParamsMap(ParamsMap, 'remove_duplicate', 'false');

        NotExpectKeyInParamsMap(ParamsMap, 'last_name_column');
        NotExpectKeyInParamsMap(ParamsMap, 'gender_column');
        NotExpectKeyInParamsMap(ParamsMap, 'ip_address_column');
    finally
        ParamsMap.Free;
    end;
end;

procedure TTestBulk.TestSubmitParamProviderFull;
var
    ParamsMap: TStringMapping;
begin
    ParamsMap := FileParamsMapFromRecord(FILE_PARAM_FULL);
    try
        ExpectKeyInParamsMap(ParamsMap, 'api_key', MOCK_API_KEY);
        ExpectKeyInParamsMap(ParamsMap, 'email_address_column', '2');
        ExpectKeyInParamsMap(ParamsMap, 'first_name_column', '1');
        ExpectKeyInParamsMap(ParamsMap, 'last_name_column', '3');
        ExpectKeyInParamsMap(ParamsMap, 'gender_column', '4');
        ExpectKeyInParamsMap(ParamsMap, 'ip_address_column', '5');
        ExpectKeyInParamsMap(ParamsMap, 'has_header_row', 'true');
        ExpectKeyInParamsMap(ParamsMap, 'remove_duplicate', 'true');
    finally
        ParamsMap.Free;
    end;
end;


initialization

    RegisterTest(TTestBulk);
end.

