program debugger;

uses ZbGeneric, ZbUtility, ZbMock;


var
   CreditsAmount: integer;
begin
	    ZbSetApiKey('mock api key');
	    ZBSetHttpClient(TMockClient);
	    TMockClient.ExpectResponse(200, '{"Credits": "500000"}');
	    CreditsAmount := ZBGetCredits;
	    WriteLn('Credits: ', CreditsAmount);
end.

