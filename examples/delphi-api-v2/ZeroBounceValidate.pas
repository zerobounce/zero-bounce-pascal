unit ZeroBounceValidate;

interface

const
  ZEROBOUNCE_DELPHI_API_V2_VERSION = '2.0.0';

function UrlEncode(const DecodedStr: string; Pluses: Boolean): string;
function IsValidStatus(const Status: string): Boolean;

implementation

uses
  System.SysUtils,
  System.StrUtils;

function UrlEncode(const DecodedStr: string; Pluses: Boolean): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(DecodedStr) do
  begin
    C := DecodedStr[I];
    if CharInSet(C, ['0'..'9', 'a'..'z', 'A'..'Z']) then
      Result := Result + C
    else if C = ' ' then
      Result := Result + IfThen(Pluses, '+', '%20')
    else
      Result := Result + '%' + IntToHex(Ord(C), 2);
  end;
end;

function IsValidStatus(const Status: string): Boolean;
var
  Upper: string;
begin
  Upper := UpperCase(Status);
  Result := (Pos('UNKNOWN', Upper) = 0) and (Pos('INVALID', Upper) = 0);
end;

end.
