program debugging;

{$mode ObjFPC}{$H+}

uses
    Classes, SysUtils,
    ZbGeneric, ZbUtility;


var
   CreditsAmount: integer;

begin
     ZBSetApiKey('mock api key');
     CreditsAmount := ZBGetCredits;
     WriteLn('Credits: ', CreditsAmount);
end.

