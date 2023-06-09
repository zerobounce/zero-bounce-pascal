unit ZbGeneric;

{$I zboptions.inc}{$H+}

interface

uses
    SysUtils, DateUtils,
    ZbStructures, ZbUtility;

function ZbGetCredits : Integer;
function ZbGetApiUsage : TApiUsage; overload;
function ZbGetApiUsage(StartDate, EndDate: TDate) : TApiUsage; overload;
function ZbActivityData(Email: String): Integer;

procedure Register;

implementation


    function ZbGetCredits : Integer;
    var
        UrlToAccess: string;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(BASE_URI, ENDPOINT_CREDITS, '?api_key=', ZbApiKey);
        response := ZBGetRequest(UrlToAccess);

        // attempt json parsing
        try
            Result := TZbJSon.Create(response.Payload).GetInteger('Credits');
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

    // Returns -1 if no activity data was found for the email
    function ZbActivityData(Email: String): Integer;
    var
        UrlToAccess: String;
        ActiveInDaysString: String;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(BASE_URI, ENDPOINT_ACTIVITY_DATA);
        UrlToAccess := Concat(UrlToAccess, '?api_key=', ZbApiKey);
        UrlToAccess := Concat(UrlToAccess, '&email=', Email);
        response := ZBGetRequest(UrlToAccess);

        // attempt json parsing
        try
            ActiveInDaysString := TZbJSon.Create(response.Payload).GetString('active_in_days');
            if ActiveInDaysString <> '' then
                Result := StrToInt(ActiveInDaysString)
            else
                Result := -1;

        except on e: Exception do
            begin
               error := ZbException.FromResponse(e.Message, response);
               error.MarkJsonError;
               raise error;
			end;
		end;
    end;


    procedure Register;
    begin
    end;

begin
end.

