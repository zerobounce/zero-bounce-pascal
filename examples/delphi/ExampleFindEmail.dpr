program ExampleFindEmail;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ZbGeneric,
  ZbStructures,
  ZbUtility;

var
   Response: TZbFindEmailResponse;
   IIndex: Integer;
begin
    try
        ZBSetApiKey('YOUR__API__KEY');
        Response := ZbFindEmail('example.com', 'John', 'Doe');
        WriteLn('Status: ', Response.Status);

        // more info
        if Length(Response.Email) > 0 then
          WriteLn('Recommended email: ', Response.Email);
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
    ReadLn;
end.
