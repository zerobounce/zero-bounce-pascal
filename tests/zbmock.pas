unit ZbMock;

{$mode ObjFPC}{$H+}

interface

uses
        Classes, SysUtils, httpdefs, httproute, fphttpclient, webutil;

            
type

    TMockClient = Class(TFPHTTPClient)
    Protected
        // fields containing mocking data of the next request
        FExpectedStatusCode: Integer; static;
        FExpectedResponseBody: String; static;
        FExpectedHeaders: String; static;

        // redeclaring private fields
        FResponseStatusCode: Integer;
        FResponseHeaders: TStrings;
    Public
        // mocking fields and methods
        FLastMockedMethod: String; static;
        FLastMockedUrl: String; static;

        class procedure ExpectResponse(StatusCode: Integer; body: String; headers: array of String);
        class procedure ExpectResponse(StatusCode: Integer; body: String);

        // overloads related to redeclared fields
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        Property ResponseStatusCode : Integer Read FResponseStatusCode;
        Property ResponseHeaders : TStrings Read FResponseHeaders;

        // overloaded method that prevents request and provides mocking
        Procedure DoMethod(Const AMethod,AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer); override;
    end;


implementation


    constructor TMockClient.Create(AOwner: TComponent);
    begin
        inherited Create(AOwner);
        FResponseStatusCode := 0;
        FResponseHeaders:=TStringList.Create;
        FResponseHeaders.NameValueSeparator:=':';
    end;

    destructor TMockClient.Destroy;
    begin
    FreeAndNil(FResponseHeaders);
    inherited Destroy;
    end;

    class procedure TMockClient.ExpectResponse(StatusCode: Integer; body: String; headers: array of String);
    var
        index_: integer;
    begin
        FExpectedStatusCode := StatusCode;
        FExpectedResponseBody := body;
        FExpectedHeaders := '';
        for index_ := low(headers) to high(headers) do
        begin
            FExpectedHeaders := FExpectedHeaders + headers[index_];
            if index_ < high(headers) then FExpectedHeaders := FExpectedHeaders + char(10);
        end;
    end;

    class procedure TMockClient.ExpectResponse(StatusCode: Integer; body: String);
    begin
        ExpectResponse(StatusCode, body, []);
    end;

    Procedure TMockClient.DoMethod(Const AMethod, AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer);
    var
        header: string;
        PairToSet: array of AnsiString;
        //SomeString: AnsiString;

    begin
        //FLastMockedMethod := AMethod;
        //FLastMockedUrl := AURL;
        //
        //FResponseStatusCode := FExpectedStatusCode;
        //FResponseHeaders.Clear;
        //if Length(FExpectedHeaders) > 0 then
        //begin
        //    for header in FExpectedHeaders.split(char(10)) do
        //    begin
        //        PairToSet := header.split(':');
        //        WriteLn('Left: ', PairToSet[0].trim(), ' | Right: ', PairToSet[1].trim());
        //        FResponseHeaders.AddPair(PairToSet[0].trim(), PairToSet[1].trim());
        //    end;
        //end;
        //
        //Terminate;
        //
        //Stream.WriteAnsiString(FExpectedResponseBody);
        inherited Domethod(AMethod, AURL, Stream, AllowedResponseCodes);
        WriteLn('Stream: >><<');
        WriteLn(Stream.ReadAnsiString);
        WriteLn('>><<');
        //Stream.Seek(0);
    end;

end.

