program ExampleFindEmail;

{$mode objfpc}{$H+}

uses
    SysUtils,
    ZbGeneric,
    ZbStructures,
    ZbUtility;

var
   Response: TZbFindEmailResponse;
begin
    try
        ZBInitialize('YOUR__API__KEY');
        Response := ZbFindEmailByDomain('example.com', 'John', 'Doe');

        // more info
        if Length(Response.Email) > 0 then
          WriteLn('Recommended email: ', Response.Email);

    except on e: Exception do
        begin
            WriteLn('Exception occured:');
            WriteLn(e.Message);
        end;
    end;
end.

