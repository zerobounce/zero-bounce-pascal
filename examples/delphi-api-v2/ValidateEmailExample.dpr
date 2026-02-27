program ValidateEmailExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  IdHTTP,
  IdSSLOpenSSL,
  ZeroBounceValidate in 'ZeroBounceValidate.pas';

var
  ApiKey: string;
  EmailToValidate: string;
  Status: string;

function ValidateEmail(const ApiKey, EmailAddress: string; var Status: string): Boolean;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  Url, ResponseBody: string;
  JsonResponse: TJsonObject;
  StatusVal, SubStatusVal: TJSONValue;
begin
  Result := False;
  Status := '';
  HTTP := nil;
  SSLHandler := nil;
  JsonResponse := nil;
  try
    try
      SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      SSLHandler.SSLOptions.Method := sslvTLSv1_2;
      SSLHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];

      HTTP := TIdHTTP.Create(nil);
      HTTP.IOHandler := SSLHandler;
      HTTP.ReadTimeout := 15000;
      HTTP.Request.ContentType := 'application/json';

      Url := 'https://api.zerobounce.net/v2/validate?api_key=' + ApiKey +
             '&email=' + UrlEncode(EmailAddress, False) + '&ip_address=';
      ResponseBody := HTTP.Get(Url);

      JsonResponse := TJSONObject.ParseJSONValue(ResponseBody) as TJSONObject;
      if JsonResponse = nil then
        Exit;

      try
        StatusVal := JsonResponse.GetValue('status');
        if (StatusVal <> nil) and (StatusVal is TJSONString) then
          Status := TJSONString(StatusVal).Value;
        SubStatusVal := JsonResponse.GetValue('sub_status');
        if (SubStatusVal <> nil) and (SubStatusVal is TJSONString) and (TJSONString(SubStatusVal).Value <> '') then
          Status := Status + ', ' + TJSONString(SubStatusVal).Value;
        Result := IsValidStatus(Status);
      finally
        JsonResponse.Free;
      end;
    except
      on E: Exception do
      begin
        Status := 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    if Assigned(HTTP) then
      HTTP.Free;
    if Assigned(SSLHandler) then
      SSLHandler.Free;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ValidateEmailExample <email_to_validate> [api_key]');
    WriteLn('  Or set ZEROBOUNCE_API_KEY environment variable.');
    ExitCode := 1;
    Exit;
  end;

  EmailToValidate := ParamStr(1);
  if ParamCount >= 2 then
    ApiKey := ParamStr(2)
  else
    ApiKey := GetEnvironmentVariable('ZEROBOUNCE_API_KEY');

  if ApiKey = '' then
  begin
    WriteLn('Error: Provide API key as second argument or set ZEROBOUNCE_API_KEY');
    ExitCode := 1;
    Exit;
  end;

  if ValidateEmail(ApiKey, EmailToValidate, Status) then
    WriteLn('Valid: ', Status)
  else
    WriteLn('Invalid: ', Status);
end.
