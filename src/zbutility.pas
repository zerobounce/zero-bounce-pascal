unit ZbUtility;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fphttpclient;

type
    HTTPClientClass = Class of TFPHTTPClient;
    ZbException = Class(Exception)
    public
          StatusCode: Integer;
          Payload: String;
          constructor Create(AMessage, APayload: String; AStatusCode: integer);
          procedure MarkHttpError;
          procedure MarkJsonError;
    end;

const
    BASE_URI = 'https://api.zerobounce.net/v2';
    BULK_URI = 'https://bulkapi.zerobounce.net/v2';
    ENDPOINT_CREDITS = '/getcredits';
    ENDPOINT_ACTIVITY_DATA = '/activity';
    ENDPOINT_VALIDATE = '/validate';
    ENDPOINT_API_USAGE = '/getapiusage';
    ENDPOINT_BATCH_VALIDATE = '/validatebatch';
    ENDPOINT_FILE_SEND = '/sendfile';
    ENDPOINT_FILE_STATUS = '/filestatus';
    ENDPOINT_FILE_RESULT = '/getfile';
    ENDPOINT_FILE_DELETE = '/deletefile';
    ENDPOINT_SCORING_SEND = '/scoring/sendfile';
    ENDPOINT_SCORING_STATUS = '/scoring/filestatus';
    ENDPOINT_SCORING_RESULT = '/scoring/getfile';
    ENDPOINT_SCORING_DELETE = '/scoring/deletefile';

var
    ZbApiKey: string = '';
    HTTPClient: HTTPClientClass = TFPHTTPClient;

    procedure ZBSetHttpClient (TClientClass: HTTPClientClass);
    procedure ZBSetApiKey ( ApiKey : string );
    procedure Register;
implementation

    constructor ZbException.Create(AMessage, APayload: String; AStatusCode: integer);
    begin
         inherited Create(AMessage);
         Payload := APayload;
         StatusCode := AStatusCode;
    end;


    procedure ZbException.MarkHttpError;
    begin
         Self.Message := 'Http Error: ' + Self.Message;
	end;

    procedure ZbException.MarkJsonError;
    begin
         Self.Message := 'Json Error: ' + Self.Message;
	end;

    procedure ZBSetApiKey ( ApiKey : string );
    begin
        ZbApiKey := ApiKey;
    end;

    procedure ZBSetHttpClient (TClientClass: HTTPClientClass);
    begin
        HTTPClient := TClientClass;
    end;

    procedure Register;
    begin
    end;

end.

