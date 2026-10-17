program ValidateEmailExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,
  System.NetEncoding,
  ZeroBounceValidate in 'ZeroBounceValidate.pas';

var
  ApiKey: string;
  EmailToValidate: string;
  Status: string;

function ValidateEmail(const ApiKey, EmailAddress: string; var Status: string): Boolean;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
  Url, ResponseBody: string;
  JsonResponse: TJsonObject;
  StatusVal, SubStatusVal: TJSONValue;
begin
  Result := False;
  Status := '';
  JsonResponse := nil;
  Client := THTTPClient.Create;
  try
    try
      Url := 'https://api.zerobounce.net/v2/validate?api_key=' +
        TNetEncoding.URL.EncodeQuery(ApiKey) +
        '&email=' + TNetEncoding.URL.EncodeQuery(EmailAddress) + '&ip_address=';
      Response := Client.Get(Url);
      ResponseBody := Response.ContentAsString;

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
    Client.Free;
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
