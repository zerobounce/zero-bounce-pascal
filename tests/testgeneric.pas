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
      procedure TestScoreEmailGet;
      procedure TestScoreEmailPost;
      procedure TestFindMailInvalidStatusPayload;
      procedure TestFindMailValidStatusPayload;
      procedure TestDomainSearchInvalidStatusPayload;
      procedure TestDomainSearchValidStatusPayload;
      procedure TestFindMailOmitsAnEmptyFirstName;
      procedure TestFindMailErrorBodyReachesTheCaller;
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

procedure TTestGeneric.TestScoreEmailGet;
var
   ScoreResult: TZbScoreEmailResult;
begin
   ZBMockResponse(200, SCORE_EMAIL_RESPONSE);
   ScoreResult := ZbScoreEmail('mark@gmail.com');
   AssertEndpointCalled(ENDPOINT_SCORE_EMAIL);
   AssertEquals('email', ScoreResult.Email, 'mark@gmail.com');
   AssertTrue('score present', ScoreResult.HasScore);
   AssertEquals('score', ScoreResult.Score, 10);
end;

procedure TTestGeneric.TestScoreEmailPost;
var
   ScoreResult: TZbScoreEmailResult;
begin
   ZBMockResponse(200, SCORE_EMAIL_RESPONSE);
   ScoreResult := ZbScoreEmailPost('mark@gmail.com');
   AssertEndpointCalled(ENDPOINT_SCORE_EMAIL);
   AssertEquals('email', ScoreResult.Email, 'mark@gmail.com');
   AssertEquals('score', ScoreResult.Score, 10);
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
   Response := ZbDomainSearchByDomain('example.com');

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
   Response := ZbDomainSearchByDomain('example.com');
   AssertDomainSearchValidStatusPayload(Response);
end;


{ guessformat takes first_name as an optional parameter, and the overloads
  that do not take every name pass '' for the rest. '&first_name=' is a blank
  value, not an omitted parameter, so it has to stay off the query. }
procedure TTestGeneric.TestFindMailOmitsAnEmptyFirstName;
begin
   ZBMockResponse(200, MOCK_FIND_MAIL_STATUS_VALID);

   ZbFindEmailByDomain('example.com', '');
   AssertEndpointCalled(ENDPOINT_EMAIL_FINDER);
   AssertTrue(
      'domain should be sent: ' + ZbResponseMock.UrlCalled,
      Pos('&domain=example.com', ZbResponseMock.UrlCalled) > 0
   );
   AssertFalse(
      'an empty first_name should not be sent: ' + ZbResponseMock.UrlCalled,
      Pos('first_name', ZbResponseMock.UrlCalled) > 0
   );

   { The same for the company-name routine, which had the same bug. }
   ZbFindEmailByCompanyName('ZeroBounce', '');
   AssertFalse(
      'an empty first_name should not be sent: ' + ZbResponseMock.UrlCalled,
      Pos('first_name', ZbResponseMock.UrlCalled) > 0
   );

   { A name that was given still travels, url-encoded. }
   ZbFindEmailByDomain('example.com', 'Ada Mae', '', 'Lovelace');
   AssertTrue(
      'first_name should be sent: ' + ZbResponseMock.UrlCalled,
      Pos('&first_name=' + EncodeParam('Ada Mae'), ZbResponseMock.UrlCalled) > 0
   );
   AssertTrue(
      'last_name should be sent: ' + ZbResponseMock.UrlCalled,
      Pos('&last_name=Lovelace', ZbResponseMock.UrlCalled) > 0
   );
end;

procedure TTestGeneric.TestFindMailErrorBodyReachesTheCaller;
var
   Raised: Boolean;
begin
   ZBMockResponse(400, '{"error":"Invalid API key or your account ran out of credits"}');
   Raised := False;
   try
      ZbFindEmailByDomain('example.com', 'Ada');
   except on E: ZbException do
      begin
         Raised := True;
         AssertEquals(400, E.StatusCode);
         AssertTrue(E.Payload, Pos('ran out of credits', E.Payload) > 0);
      end;
   end;
   AssertTrue('an HTTP 400 should be raised, not parsed', Raised);
end;

initialization
   RegisterTest(TTestGeneric);
end.

