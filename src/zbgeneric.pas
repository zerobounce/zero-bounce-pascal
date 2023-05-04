unit ZbGeneric;

{$mode objfpc}{$H+}

interface

uses
    openssl, opensslsockets, fpjson, jsonparser, fphttpclient,

    ZbUtility;

function ZbGetCredits : integer;
procedure Register;

implementation

    function ZbGetCredits : integer;
    var
        UrlToAccess: string;
        ResponsePayload: string;
        JObject: TJSONObject;
        Client: TFPHTTPClient;
    begin
        InitSSLInterface;

        UrlToAccess := Concat(BASE_URI, ENDPOINT_CREDITS, '?api_key=', ZbApiKey);
        Client := HTTPClient.Create(nil);
        try
            ResponsePayload := Client.Get(UrlToAccess);
            JObject := TJSONObject(GetJSON(ResponsePayload));
            Result := JObject.Find('Credits').AsInteger;
        finally
            Client.Free;
        end;
    end;

    procedure Register;
    begin
    end;

begin
end.

