unit ZbGeneric;

{$I zboptions.inc}{$H+}

interface

uses
    SysUtils, DateUtils, httpprotocol,
    ZbStructures, ZbUtility;

function ZbGetCredits : Integer;
function ZbGetApiUsage : TApiUsage; overload;
function ZbGetApiUsage(StartDate, EndDate: TDate) : TApiUsage; overload;
function ZbActivityData(Email: String): Integer;
function ZbFindEmail(Domain, FirstName, MiddleName, LastName String) : TZbFindEmailResponse; overload;
function ZbFindEmail(Domain, FirstName, LastName String) : TZbFindEmailResponse; overload;
function ZbDomainSearch(Domain String) : TZbFindEmailResponse; overload;

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
        UrlToAccess := Concat(UrlToAccess, '&email=', HTTPEncode(Email));
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

    function ZbFindEmail(Domain, FirstName, MiddleName, LastName String) : TZbFindEmailResponse;
    var
        UrlToAccess: string;
        response: TZbRequestResponse;
        error: ZbException;
    begin
        UrlToAccess := Concat(BASE_URI, ENDPOINT_EMAIL_FINDER);
        UrlToAccess := Concat(UrlToAccess, '?api_key=', ZbApiKey);
        UrlToAccess := Concat(UrlToAccess, '&domain=', HTTPEncode(Domain));

        if FirstName <> '' then
            UrlToAccess := Concat(UrlToAccess, '&first_name=', HTTPEncode(FirstName));
        if MiddleName <> '' then
            UrlToAccess := Concat(UrlToAccess, '&middle_name=', HTTPEncode(MiddleName));
        if LastName <> '' then
            UrlToAccess := Concat(UrlToAccess, '&last_name=', HTTPEncode(LastName));

        response := ZBGetRequest(UrlToAccess);
        try
            Result := TZbFindEmailResponseFromJson(response.Payload);
		except on e: Exception do
            begin
               error := ZbException.FromResponse(e.Message, response);
               error.MarkJsonError;
               raise error;
            end;
		end;
    end;

    function ZbFindEmail(Domain, FirstName, LastName String) : TZbFindEmailResponse;
    begin
        Result := ZbFindEmail(Domain, FirstName, '', LastName);
    end;

    function ZbDomainSearch(Domain String) : TZbFindEmailResponse;
    begin
        Result := ZbFindEmail(Domain, '', '', '');
    end;

    procedure Register;
    begin
    end;

begin
end.

