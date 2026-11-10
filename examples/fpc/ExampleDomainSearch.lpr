program ExampleDomainSearch;

{$mode objfpc}{$H+}

uses
    SysUtils,
    ZbGeneric,
    ZbStructures,
    ZbUtility;

var
   Response: TZbDomainSearchResponse;
   IIndex: Integer;
begin
    try
        ZBInitialize('YOUR__API__KEY');
        // or ZbDomainSearchByCompanyName('Example Inc')
        Response := ZbDomainSearchByDomain('example.com');

        WriteLn('Domain: ', Response.Domain);
        WriteLn('Format: ', Response.Format, ' (', Response.Confidence, ')');

        // more info
        if Length(Response.OtherDomainFormats) > 0 then
          WriteLn('Other domain formats:');
        for IIndex := 0 to Length(Response.OtherDomainFormats) - 1 do
        begin
          WriteLn(Response.OtherDomainFormats[IIndex].Format, ' (',
            Response.OtherDomainFormats[IIndex].Confidence, ')');
        end;

    except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
    end;
end.

