unit ZbGeneric;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, DateUtils, openssl, opensslsockets, fpjson, jsonparser,

    ZbStructures, ZbUtility;

function ZbGetCredits : Integer;
function ZbGetApiUsage : TApiUsage;
function ZbGetApiUsage(StartDate, EndDate: TDate) : TApiUsage;
procedure Register;

implementation


    function ZbGetCredits : Integer;
    var
        UrlToAccess: string;
        response: TZbRequestResponse;
        JObject: TJSONObject;
        error: ZbException;
    begin
        UrlToAccess := Concat(BASE_URI, ENDPOINT_CREDITS, '?api_key=', ZbApiKey);
        response := ZBGetRequest(UrlToAccess);

        // attempt json parsing
        try
			JObject := TJSONObject(GetJSON(response.Payload));
            Result := JObject.Find('Credits').AsInteger;
        except on e: Exception do
            begin
               error := ZbException.FromResponse(e.Message, response);
               error.MarkJsonError;
               raise error;
            end;
        end;
	end;

    function ZbGetApiUsage(StartDate, EndDate: TDate) : TApiUsage;
    var
        ApiUsage: TApiUsage;
        DateAuxString: String;
        UrlToAccess: String;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(BASE_URI, ENDPOINT_API_USAGE, '?api_key=', ZbApiKey);
        DateAuxString := Format(
            '%d-%d-%d',
            [
                YearOf(StartDate),
                MonthOf(StartDate),
                DayOf(StartDate)
            ]
        );
        UrlToAccess := Concat(UrlToAccess, '&start_date=', DateAuxString);
        DateAuxString := Format(
            '%d-%d-%d',
            [
                YearOf(EndDate),
                MonthOf(EndDate),
                DayOf(EndDate)
            ]
        );
        UrlToAccess := Concat(UrlToAccess, '&end_date=', DateAuxString);

        response := ZBGetRequest(UrlToAccess);

        try
            ApiUsage := ZbApiUsageFromJson(response.Payload);
		except on e: Exception do
            begin
               error := ZbException.FromResponse(e.Message, response);
               error.MarkJsonError;
               raise error;
            end;
		end;

        Result := ApiUsage;
	end;

    function ZbGetApiUsage : TApiUsage;
    begin
        Result := ZbGetApiUsage(RecodeYear(Today, 2000), Today);
	end;

    procedure Register;
    begin
    end;

begin
end.

