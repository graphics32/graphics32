{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GR32_DSGN_Lazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  GR32.Design.Reg, GR32.Design.Misc, GR32.Design.ColorPicker, 
  GR32.Design.BitmapEditor, GR32.Design.Color32, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GR32.Design.Reg', @GR32.Design.Reg.Register);
end;

initialization
  RegisterPackage('GR32_DSGN_Lazarus', @Register);
end.
