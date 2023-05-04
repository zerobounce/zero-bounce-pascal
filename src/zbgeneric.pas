unit ZbGeneric;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, DateUtils, openssl, opensslsockets, fpjson, jsonparser, fphttpclient,

    ZbUtility;

type
  TApiUsage = Record
    Total: Integer;
    StatusValid: Integer;
    StatusInvalid: Integer;
    StatusCatchAll: Integer;
    StatusDoNotMail: Integer;
    StatusSpamtrap: Integer;
    StatusUnknown: Integer;
    SubStatusToxic: Integer;
    SubStatusDisposable: Integer;
    SubStatusRoleBased: Integer;
    SubStatusPossibleTrap: Integer;
    SubStatusGlobalSuppression: Integer;
    SubStatusTimeoutExceeded: Integer;
    SubStatusMailServerTemporaryError: Integer;
    SubStatusMailServerDidNotRespond: Integer;
    SubStatusGreylisted: Integer;
    SubStatusAntispamSystem: Integer;
    SubStatusDoesNotAcceptMail: Integer;
    SubStatusExceptionOccurred: Integer;
    SubStatusFailedSyntaxCheck: Integer;
    SubStatusMailboxNotFound: Integer;
    SubStatusUnroutableIpAddress: Integer;
    SubStatusPossibleTypo: Integer;
    SubStatusNoDnsEntries: Integer;
    SubStatusRoleBasedCatchAll: Integer;
    SubStatusMailboxQuotaExceeded: Integer;
    SubStatusForcibleDisconnect: Integer;
    SubStatusFailedSmtpConnection: Integer;
    SubStatusMxForward: Integer;
    SubStatusAlternate: Integer;
    SubStatusBlocked: Integer;
    SubStatusAllowed: Integer;
    StartDate: TDate;
    EndDate: TDate;
  end;

function ZbGetCredits : Integer;
function ZbApiUsageFromJson(JsonContent: string): TApiUsage;
function ZbGetApiUsage : TApiUsage;
function ZbGetApiUsage(StartDate, EndDate: TDate) : TApiUsage;
procedure Register;

implementation

     function ZbApiUsageFromJson(JsonContent: string): TApiUsage;
     var
        ApiUsage: TApiUsage;
        JObject: TJSONObject;
        Day, Month, Year: Integer;
        DateString: String;

     begin
        JObject := TJSONObject(GetJSON(JsonContent));
        ApiUsage.Total :=                               JObject.Find('total').AsInteger;
        ApiUsage.StatusValid :=                         JObject.Find('status_valid').AsInteger;
        ApiUsage.StatusInvalid :=                       JObject.Find('status_invalid').AsInteger;
        ApiUsage.StatusCatchAll :=                      JObject.Find('status_catch_all').AsInteger;
        ApiUsage.StatusDoNotMail :=                     JObject.Find('status_do_not_mail').AsInteger;
        ApiUsage.StatusSpamtrap :=                      JObject.Find('status_spamtrap').AsInteger;
        ApiUsage.StatusUnknown :=                       JObject.Find('status_unknown').AsInteger;
        ApiUsage.SubStatusToxic :=                      JObject.Find('sub_status_toxic').AsInteger;
        ApiUsage.SubStatusDisposable :=                 JObject.Find('sub_status_disposable').AsInteger;
        ApiUsage.SubStatusRoleBased :=                  JObject.Find('sub_status_role_based').AsInteger;
        ApiUsage.SubStatusPossibleTrap :=               JObject.Find('sub_status_possible_trap').AsInteger;
        ApiUsage.SubStatusGlobalSuppression :=          JObject.Find('sub_status_global_suppression').AsInteger;
        ApiUsage.SubStatusTimeoutExceeded :=            JObject.Find('sub_status_timeout_exceeded').AsInteger;
        ApiUsage.SubStatusMailServerTemporaryError :=   JObject.Find('sub_status_mail_server_temporary_error').AsInteger;
        ApiUsage.SubStatusMailServerDidNotRespond :=    JObject.Find('sub_status_mail_server_did_not_respond').AsInteger;
        ApiUsage.SubStatusGreylisted :=                 JObject.Find('sub_status_greylisted').AsInteger;
        ApiUsage.SubStatusAntispamSystem :=             JObject.Find('sub_status_antispam_system').AsInteger;
        ApiUsage.SubStatusDoesNotAcceptMail :=          JObject.Find('sub_status_does_not_accept_mail').AsInteger;
        ApiUsage.SubStatusExceptionOccurred :=          JObject.Find('sub_status_exception_occurred').AsInteger;
        ApiUsage.SubStatusFailedSyntaxCheck :=          JObject.Find('sub_status_failed_syntax_check').AsInteger;
        ApiUsage.SubStatusMailboxNotFound :=            JObject.Find('sub_status_mailbox_not_found').AsInteger;
        ApiUsage.SubStatusUnroutableIpAddress :=        JObject.Find('sub_status_unroutable_ip_address').AsInteger;
        ApiUsage.SubStatusPossibleTypo :=               JObject.Find('sub_status_possible_typo').AsInteger;
        ApiUsage.SubStatusNoDnsEntries :=               JObject.Find('sub_status_no_dns_entries').AsInteger;
        ApiUsage.SubStatusRoleBasedCatchAll :=          JObject.Find('sub_status_role_based_catch_all').AsInteger;
        ApiUsage.SubStatusMailboxQuotaExceeded :=       JObject.Find('sub_status_mailbox_quota_exceeded').AsInteger;
        ApiUsage.SubStatusForcibleDisconnect :=         JObject.Find('sub_status_forcible_disconnect').AsInteger;
        ApiUsage.SubStatusFailedSmtpConnection :=       JObject.Find('sub_status_failed_smtp_connection').AsInteger;
        ApiUsage.SubStatusMxForward :=                  JObject.Find('sub_status_mx_forward').AsInteger;
        ApiUsage.SubStatusAlternate :=                  JObject.Find('sub_status_alternate').AsInteger;
        ApiUsage.SubStatusBlocked :=                    JObject.Find('sub_status_blocked').AsInteger;
        ApiUsage.SubStatusAllowed :=                    JObject.Find('sub_status_allowed').AsInteger;

        DateString := JObject.Find('start_date').AsString;
        SScanf(DateString, '%d/%d/%d', [@Month, @Day, @Year]);
        ApiUsage.StartDate := EncodeDateTime(Year, Month, Day, 0, 0, 0, 0);

        DateString := JObject.Find('end_date').AsString;
        SScanf(DateString, '%d/%d/%d', [@Month, @Day, @Year]);
        ApiUsage.EndDate := EncodeDateTime(Year, Month, Day, 0, 0, 0, 0);

        Result := ApiUsage;
     end;

    function ZbGetCredits : Integer;
    var
        UrlToAccess: string;
        ResponsePayload: string;
        StatusCode: integer;
        JObject: TJSONObject;
        Client: TFPHTTPClient;
        error: ZbException;
    begin
        InitSSLInterface;

        UrlToAccess := Concat(BASE_URI, ENDPOINT_CREDITS, '?api_key=', ZbApiKey);
        Client := HTTPClient.Create(nil);
        try
            ResponsePayload := Client.Get(UrlToAccess);
            StatusCode := Client.ResponseStatusCode;
        finally
            Client.Free;
        end;

        try
			JObject := TJSONObject(GetJSON(ResponsePayload));
            Result := JObject.Find('Credits').AsInteger;
        except on e: Exception do
            begin
               error := ZbException.Create(e.Message, ResponsePayload, StatusCode);
               error.MarkJsonError;
               raise error;
			end;
		end;
	end;

    function ZbGetApiUsage(StartDate, EndDate: TDate) : TApiUsage;
    var
        ApiUsage: TApiUsage;
        DateAuxString: String;
        UrlToAccess: string;
        ResponsePayload: string;
        StatusCode: integer;
        Client: TFPHTTPClient;
        error: ZbException;
    begin
        InitSSLInterface;
        UrlToAccess := Concat(BASE_URI, ENDPOINT_API_USAGE, '?api_key=', ZbApiKey);

        DateAuxString := Format(
            '%d-%d-%d',
            [YearOf(StartDate),
            MonthOf(StartDate),
            DayOf(StartDate)]
        );
        UrlToAccess := Concat(UrlToAccess, '&start_date=', DateAuxString);

        DateAuxString := Format(
            '%d-%d-%d',
            [YearOf(EndDate),
            MonthOf(EndDate),
            DayOf(EndDate)]
        );
        UrlToAccess := Concat(UrlToAccess, '&end_date=', DateAuxString);

        Client := HTTPClient.Create(nil);
	    try
	        ResponsePayload := Client.Get(UrlToAccess);
	        StatusCode := Client.ResponseStatusCode;

            if StatusCode > 299 then
            begin
                error := ZbException.Create(Client.ResponseStatusText, ResponsePayload, StatusCode);
                error.MarkHttpError;
                raise error;
            end;
	    finally
	        Client.Free;
	    end;

        try
            ApiUsage := ZbApiUsageFromJson(ResponsePayload);
		except on e: Exception do
            begin
               error := ZbException.Create(e.Message, ResponsePayload, StatusCode);
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

