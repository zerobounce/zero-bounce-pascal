unit TestGeneric;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, fpcunit, testregistry,
   // ZeroBounce was installed in Lazarus and imported via Package > Package links
   BaseTest, MockValues, ZbGeneric, ZbStructures, ZbUtility, ZbValidation, ZbPriority3;

type

   TTestGeneric= class(TBaseTestCase)
   protected
      procedure AssertDeadKey(const Name, Expected: String; E: ZbException);
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
      procedure TestDeadKeyValidateIsAnError;
      procedure TestDeadKeyActivityIsAnError;
      procedure TestDeadKeyApiUsageIsAnError;
      procedure TestDeadKeyFindEmailIsAnError;
      procedure TestDeadKeyDomainSearchIsAnError;
      procedure TestDeadKeyFiltersAreAnError;
      procedure TestDeadKeyCreditsAreMinusOne;
      procedure TestGreylistStillInProgressValidateIsAResult;
      procedure TestFilterMessageSuccessIsAResult;
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

{ LIVE (2026-11-11): what an invalid or drained key gets back, each with an
  HTTP 200 and no "success" field. Every call must raise with the API's own
  message instead of returning an empty result. }
const
   DRAINED = 'Invalid API key or your account ran out of credits';
   DEAD_KEY_ERROR = '{"error":"Invalid API key or your account ran out of credits"}';
   DEAD_KEY_MESSAGE = '{"Message":"Invalid API key or your account ran out of credits"}';
   DEAD_KEY_CAPITAL_ERROR = '{  "Error": "Invalid API key or your account ran out of credits"}';

procedure TTestGeneric.AssertDeadKey(const Name, Expected: String; E: ZbException);
begin
   AssertTrue(Name + ': ' + E.Message, Pos(Expected, E.Message) > 0);
   AssertEquals(Name + ' status', 200, E.StatusCode);
end;

procedure TTestGeneric.TestDeadKeyValidateIsAnError;
var
   Options: TZbValidateOptions;
begin
   ZBMockResponse(200, DEAD_KEY_ERROR);
   try
      ZbValidateEmail('a@example.com');
      Fail('validate: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('validate', DRAINED, e);
   end;
   Options := ZbDefaultValidateOptions;
   Options.UsePost := 1;
   ZBMockResponse(200, DEAD_KEY_ERROR);
   try
      ZbValidateEmail('a@example.com', '', Options);
      Fail('validate POST: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('validate POST', DRAINED, e);
   end;
end;

procedure TTestGeneric.TestDeadKeyActivityIsAnError;
begin
   ZBMockResponse(200, DEAD_KEY_ERROR);
   try
      ZbActivityData('a@example.com');
      Fail('activity: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('activity', DRAINED, e);
   end;
end;

procedure TTestGeneric.TestDeadKeyApiUsageIsAnError;
begin
   ZBMockResponse(200, '{"error":"Invalid API key"}');
   try
      ZbGetApiUsage;
      Fail('api usage: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('api usage', 'Invalid API key', e);
   end;
end;

procedure TTestGeneric.TestDeadKeyFindEmailIsAnError;
begin
   ZBMockResponse(200, DEAD_KEY_MESSAGE);
   try
      ZbFindEmailByDomain('example.com', 'john');
      Fail('find email: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('find email', DRAINED, e);
   end;
   ZBMockResponse(200, DEAD_KEY_MESSAGE);
   try
      ZbFindEmailByCompanyName('Example', 'john');
      Fail('find email by company: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('find email by company', DRAINED, e);
   end;
   ZBMockResponse(200, DEAD_KEY_MESSAGE);
   try
      {$WARN SYMBOL_DEPRECATED OFF}
      ZbFindEmail('example.com', 'john', 'doe');
      {$WARN SYMBOL_DEPRECATED ON}
      Fail('find email (deprecated): a dead key returned a result');
   except on e: ZbException do AssertDeadKey('find email (deprecated)', DRAINED, e);
   end;
end;

procedure TTestGeneric.TestDeadKeyDomainSearchIsAnError;
begin
   ZBMockResponse(200, DEAD_KEY_MESSAGE);
   try
      ZbDomainSearchByDomain('example.com');
      Fail('domain search: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('domain search', DRAINED, e);
   end;
   ZBMockResponse(200, DEAD_KEY_MESSAGE);
   try
      ZbDomainSearchByCompanyName('Example');
      Fail('domain search by company: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('domain search by company', DRAINED, e);
   end;
   ZBMockResponse(200, DEAD_KEY_MESSAGE);
   try
      {$WARN SYMBOL_DEPRECATED OFF}
      ZbDomainSearch('example.com');
      {$WARN SYMBOL_DEPRECATED ON}
      Fail('domain search (deprecated): a dead key returned a result');
   except on e: ZbException do AssertDeadKey('domain search (deprecated)', DRAINED, e);
   end;
end;

procedure TTestGeneric.TestDeadKeyFiltersAreAnError;
begin
   ZBMockResponse(200, DEAD_KEY_CAPITAL_ERROR);
   try
      ZbAddFilter('allow', 'email', 'x@example.com');
      Fail('add filter: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('add filter', DRAINED, e);
   end;
   ZBMockResponse(200, DEAD_KEY_CAPITAL_ERROR);
   try
      ZbDeleteFilter('allow', 'email', 'x@example.com');
      Fail('delete filter: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('delete filter', DRAINED, e);
   end;
   ZBMockResponse(200, DEAD_KEY_CAPITAL_ERROR);
   try
      ZbListFilters(False);
      Fail('list filters: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('list filters', DRAINED, e);
   end;
   ZBMockResponse(200, DEAD_KEY_CAPITAL_ERROR);
   try
      ZbListFilters(True);
      Fail('list filters POST: a dead key returned a result');
   except on e: ZbException do AssertDeadKey('list filters POST', DRAINED, e);
   end;
end;

{ -1 is the documented credit balance of an invalid key, not an error. }
procedure TTestGeneric.TestDeadKeyCreditsAreMinusOne;
begin
   ZBMockResponse(200, '{"Credits":"-1"}');
   AssertEquals(-1, ZbGetCredits);
end;

{ The documented answer to a repeated greylist_processing validate is a result
  whose only field is Message. The error-body checks must leave it alone. }
procedure TTestGeneric.TestGreylistStillInProgressValidateIsAResult;
var
   Validated: TZbValidationResult;
begin
   ZBMockResponse(200, '{"Message":"Greylist validation still in progress. Please try again later."}');
   Validated := ZbValidateEmail('a@example.com');
   AssertEquals('Greylist validation still in progress. Please try again later.', Validated.Message);
end;

{ A filter change answers with a bare Message on success. }
procedure TTestGeneric.TestFilterMessageSuccessIsAResult;
begin
   ZBMockResponse(200, '{"Message":"Filter has been added."}');
   AssertTrue(Pos('Filter has been added.', ZbAddFilter('allow', 'email', 'x@example.com')) > 0);
end;


initialization
   RegisterTest(TTestGeneric);
end.
