program debugging;

{$mode ObjFPC}{$H+}

uses
    Classes, SysUtils,
    ZbGeneric, ZbUtility;


var
   CreditsAmount: integer;

begin
     ZBInitialize('mock api key');
     CreditsAmount := ZBGetCredits;
     WriteLn('Credits: ', CreditsAmount);
end.

