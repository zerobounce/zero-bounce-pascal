{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ZeroBounce;

{$warn 5023 off : no warning about unused units}
interface

uses
        ZbGeneric, ZbUtility, ZbValidation, ZbStructures, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ZbGeneric', @ZbGeneric.Register);
  RegisterUnit('ZbUtility', @ZbUtility.Register);
  RegisterUnit('ZbValidation', @ZbValidation.Register);
  RegisterUnit('ZbStructures', @ZbStructures.Register);
end;

initialization
  RegisterPackage('ZeroBounce', @Register);
end.
