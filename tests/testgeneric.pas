unit TestGeneric;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, fpcunit, testregistry,
   // ZeroBounce was installed in Lazarus and imported via Package > Package links
   ZbGeneric, ZbUtility;


const
   API_USAGE_RESPONSE: string = '{' +
      '"total": 7,' +
      '"status_valid": 5,' +
      '"status_invalid": 0,' +
      '"status_catch_all": 0,' +
      '"status_do_not_mail": 3,' +
      '"status_spamtrap": 0,' +
      '"status_unknown": 0,' +
      '"sub_status_toxic": 0,' +
      '"sub_status_disposable": 0,' +
      '"sub_status_role_based": 0,' +
      '"sub_status_possible_trap": 0,' +
      '"sub_status_global_suppression": 3,' +
      '"sub_status_timeout_exceeded": 0,' +
      '"sub_status_mail_server_temporary_error": 0,' +
      '"sub_status_mail_server_did_not_respond": 0,' +
      '"sub_status_greylisted": 0,' +
      '"sub_status_antispam_system": 0,' +
      '"sub_status_does_not_accept_mail": 0,' +
      '"sub_status_exception_occurred": 0,' +
      '"sub_status_failed_syntax_check": 0,' +
      '"sub_status_mailbox_not_found": 0,' +
      '"sub_status_unroutable_ip_address": 0,' +
      '"sub_status_possible_typo": 0,' +
      '"sub_status_no_dns_entries": 0,' +
      '"sub_status_role_based_catch_all": 0,' +
      '"sub_status_mailbox_quota_exceeded": 0,' +
      '"sub_status_forcible_disconnect": 0,' +
      '"sub_status_failed_smtp_connection": 0,' +
      '"sub_status_mx_forward": 0,' +
      '"sub_status_alternate": 1,' +
      '"sub_status_blocked": 0,' +
      '"sub_status_allowed": 0,' +
      '"start_date": "4/1/2023",' +
      '"end_date": "4/20/2023"' +
   '}';
   ERROR_MESSAGE: string = 'Mock error message';
   ERROR_PAYLOAD: string = '{"error": "Mock error message"}';

type

   TTestGeneric= class(TTestCase)
   protected
      procedure AssertEndpointCalled(const endpoint: string);
   published
      procedure TestCredits;
      procedure TestApiUsageParse;
      procedure TestApiUsageHttpError;
      procedure TestApiUsageRequest;
      procedure TearDown; override;
   end;


implementation

procedure TTestGeneric.TearDown;
begin
   if ZbResponseMock.Headers <> nil then
      ZbResponseMock.Headers.Free;
   ZbResponseMock := cDefaultMock;
end;

procedure TTestGeneric.AssertEndpointCalled(const endpoint: string);
var
   condition: Boolean;
begin
   condition := ZbResponseMock.UrlCalled.Contains(endpoint);
   AssertTrue('Endpoint '+ endpoint + 'not called', condition);
end;

procedure TTestGeneric.TestCredits;
var
   CreditsAmount: integer;
begin
   ZBMockResponse(200, '{"Credits":"50000"}');
   CreditsAmount := ZBGetCredits;
   AssertEndpointCalled(ENDPOINT_CREDITS);
   AssertEquals(CreditsAmount, 50000);
end;


procedure TTestGeneric.TestApiUsageParse;
var
   ApiUsage: TApiUsage;
begin
   ApiUsage := ZbApiUsageFromJson(API_USAGE_RESPONSE);
   AssertEquals('Expected total', ApiUsage.Total, 7);
   AssertEquals('Expected unknown', ApiUsage.StatusUnknown, 0);
end;

procedure TTestGeneric.TestApiUsageHttpError;
begin
   ZBMockResponse(401, ERROR_PAYLOAD);
   try
     ZbGetApiUsage;
     Fail('Test case should have failed');
   except on e: ZbException do
         begin
            AssertEquals(e.StatusCode, 401);
            AssertTrue('error content', e.Payload.contains(ERROR_MESSAGE));
		  end;
   end;
end;

procedure TTestGeneric.TestApiUsageRequest;
var
   ApiUsage: TApiUsage;
begin
   ZBMockResponse(200, API_USAGE_RESPONSE);
   ApiUsage := ZbGetApiUsage;
   AssertEndpointCalled(ENDPOINT_API_USAGE);
   AssertEquals('Expected total', ApiUsage.Total, 7);
   AssertEquals('Expected unknown', ApiUsage.StatusUnknown, 0);
end;

initialization
   RegisterTest(TTestGeneric);
end.

