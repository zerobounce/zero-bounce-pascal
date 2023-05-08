{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ZeroBounce;

{$warn 5023 off : no warning about unused units}
interface

uses
        ZbBulk, ZbGeneric, ZbStructures, ZbUtility, ZbValidation, 
        LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ZbBulk', @ZbBulk.Register);
  RegisterUnit('ZbGeneric', @ZbGeneric.Register);
  RegisterUnit('ZbStructures', @ZbStructures.Register);
  RegisterUnit('ZbUtility', @ZbUtility.Register);
  RegisterUnit('ZbValidation', @ZbValidation.Register);
end;

initialization
  RegisterPackage('ZeroBounce', @Register);
end.
