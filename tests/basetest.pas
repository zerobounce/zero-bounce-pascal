unit BaseTest;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testutils, testregistry,
    ZbUtility;

type

    TBaseTestCase= class(TTestCase)
    const
        MOCK_API_KEY = 'mock_api_key';
    protected
        procedure AssertEndpointCalled(const endpoint: string);
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure Test;
    end;

implementation

procedure TBaseTestCase.SetUp;
begin
   ZBSetApiKey(MOCK_API_KEY);
end;

procedure TBaseTestCase.TearDown;
begin
   ZbResponseMock := cDefaultMock;
end;

procedure TBaseTestCase.AssertEndpointCalled(const endpoint: string);
begin
   AssertTrue(
      'Called URL was not addressed to '+ endpoint,
      ZbResponseMock.UrlCalled.Contains(endpoint)
   );
end;

procedure TBaseTestCase.Test;
begin
end;

initialization

        RegisterTest(TBaseTestCase);
end.

