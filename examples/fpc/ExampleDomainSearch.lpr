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
        Response := ZbDomainSearchByDomain('example.com', 'John', 'Doe');

        // more info
        if Length(Response.OtherDomainFormats) > 0 then
          WriteLn('Domain formats:');
        for IIndex := 0 to Length(Response.OtherDomainFormats) - 1 do
        begin
          WriteLn(Response.OtherDomainFormats[IIndex].Format);
        end;

    except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
    end;
end.

