program ExampleGetApiUsage;

uses
    DateUtils,
    ZbGeneric, // ZbGetApiUsage method
    ZbStructures, // TApiUsage record
    ZbUtility; // ZBSetApiKey method

var
  ApiUsage: TApiUsage;
begin
    ZBSetApiKey('YOUR__API__KEY');
    ApiUsage := ZbGetApiUsage;

    WriteLn('Total API calls: ', ApiUsage.Total);
    WriteLn('Checked up until: ', DateToISO8601(ApiUsage.EndDate));
end.

