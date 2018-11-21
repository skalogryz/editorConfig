{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit editorconfiglazext;

{$warn 5023 off : no warning about unused units}
interface

uses
  editorConfigReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('editorConfigReg', @editorConfigReg.Register);
end;

initialization
  RegisterPackage('editorconfiglazext', @Register);
end.
