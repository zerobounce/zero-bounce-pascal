{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ZeroBounce;

{$warn 5023 off : no warning about unused units}
interface

uses
        ZbBulk, ZbGeneric, ZbPriority3, ZbStructures, ZbUtility, ZbValidation,
        ZbWarmup, 
        LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ZbBulk', @ZbBulk.Register);
  RegisterUnit('ZbGeneric', @ZbGeneric.Register);
  RegisterUnit('ZbStructures', @ZbStructures.Register);
  RegisterUnit('ZbUtility', @ZbUtility.Register);
  RegisterUnit('ZbValidation', @ZbValidation.Register);
  RegisterUnit('ZbPriority3', @ZbPriority3.Register);
  RegisterUnit('ZbWarmup', @ZbWarmup.Register);
end;

initialization
  RegisterPackage('ZeroBounce', @Register);
end.
