program ExampleGetApiUsage;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DateUtils,
  zbgeneric in '..\..\src\zbgeneric.pas',
  zbstructures in '..\..\src\zbstructures.pas',
  zbutility in '..\..\src\zbutility.pas';


var
  ApiUsage: TApiUsage;
begin
    ZBSetApiKey('YOUR__API__KEY');
    ApiUsage := ZbGetApiUsage;

    WriteLn('Total API calls: ', ApiUsage.Total);
    WriteLn('Checked up until: ', DateToISO8601(ApiUsage.EndDate));
    ReadLn;
end.


