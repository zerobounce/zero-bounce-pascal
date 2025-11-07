unit TestGeneric;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, fpcunit, testregistry,
   // ZeroBounce was installed in Lazarus and imported via Package > Package links
   BaseTest, MockValues, ZbGeneric, ZbStructures, ZbUtility;

type

   TTestGeneric= class(TBaseTestCase)
   protected
      procedure AssertFindMailValidStatusPayload(Response: TZbFindEmailResponse);
      procedure AssertDomainSearchValidStatusPayload(Response: TZbDomainSearchResponse);
   published
      procedure TestCredits;
      procedure TestApiUsageParse;
      procedure TestApiUsageHttpError;
      procedure TestApiUsageRequest;
      procedure TestActivityDataEndpoint;
      procedure TestActivityDataNotFound;
      procedure TestActivityDataFound;
      procedure TestFindMailInvalidStatusPayload;
      procedure TestFindMailValidStatusPayload;
      procedure TestDomainSearchInvalidStatusPayload;
      procedure TestDomainSearchValidStatusPayload;
   end;


implementation


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

procedure TTestGeneric.TestActivityDataEndpoint;
begin
   ZBMockResponse(200, ACTIVITY_DATA_FOUND);
   ZbActivityData('valid@example.com');
   AssertEndpointCalled(ENDPOINT_ACTIVITY_DATA);
end;

procedure TTestGeneric.TestActivityDataFound;
var
   ActivityAmount: Integer;
begin
   ZBMockResponse(200, ACTIVITY_DATA_FOUND);
   ActivityAmount := ZbActivityData('valid@example.com');
   AssertEquals('activity data', ActivityAmount, 180);
end;


procedure TTestGeneric.TestActivityDataNotFound;
var
   ActivityAmount: Integer;
begin
   ZBMockResponse(200, ACTIVITY_DATA_NOT_FOUND);
   ActivityAmount := ZbActivityData('valid@example.com');
   AssertEquals('activity data', ActivityAmount, -1);
end;

procedure TTestGeneric.AssertFindMailValidStatusPayload(Response: TZbFindEmailResponse);
begin
   AssertEquals('invalid Email value', Response.Email, 'john.doe@example.com');
   AssertEquals('invalid Domain value', Response.Domain, 'example.com');
end;

procedure TTestGeneric.AssertDomainSearchValidStatusPayload(Response: TZbDomainSearchResponse);
begin
   AssertEquals('invalid Domain value', Response.Domain, 'example.com');
   AssertTrue(
      'empty list of domain formats expected',
      Length(response.OtherDomainFormats) = 2
   );
   AssertEquals(
      'invalid DomainFormat value',
      response.OtherDomainFormats[0].Confidence,
      'high'
   );
   AssertEquals(
      'invalid DomainFormat value',
      response.OtherDomainFormats[1].Confidence,
      'medium'
   );
end;

procedure TTestGeneric.TestFindMailInvalidStatusPayload;
var
   Response: TZbFindEmailResponse;
begin
   ZBMockResponse(200, MOCK_FIND_MAIL_STATUS_INVALID);
   Response := ZbFindEmailByDomain('example.com', 'John', '', 'Doe');

   AssertEquals('invalid Email value', Response.Email, '');
   AssertEquals('invalid Domain value', Response.Domain, 'example.com');

end;

procedure TTestGeneric.TestFindMailValidStatusPayload;
var
   Response: TZbFindEmailResponse;
begin
   ZBMockResponse(200, MOCK_FIND_MAIL_STATUS_VALID);
   Response := ZbFindEmailByDomain('example.com', 'John', '', 'Doe');
   AssertFindMailValidStatusPayload(Response);
end;

procedure TTestGeneric.TestDomainSearchInvalidStatusPayload;
var
   Response: TZbDomainSearchResponse;
begin
   ZBMockResponse(200, MOCK_DOMAIN_SEARCH_STATUS_INVALID);
   Response := ZbDomainSearchByDomain('example.com', 'John', '', 'Doe');

   AssertEquals('invalid Domain value', Response.Domain, 'example.com');
   AssertTrue(
      'empty list of domain formats expected',
      Length(response.OtherDomainFormats) = 0
   );

end;

procedure TTestGeneric.TestDomainSearchValidStatusPayload;
var
   Response: TZbDomainSearchResponse;
begin
   ZBMockResponse(200, MOCK_DOMAIN_SEARCH_STATUS_VALID);
   Response := ZbDomainSearchByDomain('example.com', 'John', '', 'Doe');
   AssertDomainSearchValidStatusPayload(Response);
end;


initialization
   RegisterTest(TTestGeneric);
end.

