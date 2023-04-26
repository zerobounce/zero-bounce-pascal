unit TestGeneric;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, fpcunit, testregistry,
   // ZeroBounce was installed in Lazarus and imported via Package > Package links
   MockValues, ZbGeneric, ZbStructures, ZbUtility;

type

   TTestGeneric= class(TTestCase)
   protected
      procedure AssertEndpointCalled(const endpoint: string);
      procedure TearDown; override;
   published
      procedure TestCredits;
      procedure TestApiUsageHttpError;
      procedure TestApiUsageRequest;
   end;


implementation

procedure TTestGeneric.TearDown;
begin
   if ZbResponseMock.Headers <> nil then
      ZbResponseMock.Headers.Free;
   ZbResponseMock := cDefaultMock;
end;

procedure TTestGeneric.AssertEndpointCalled(const endpoint: string);
begin
   AssertTrue(
      'Endpoint '+ endpoint + ' not called',
      ZbResponseMock.UrlCalled.Contains(endpoint)
   );
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

