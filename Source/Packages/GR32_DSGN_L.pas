{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit GR32_DSGN_L; 

interface

uses
  GR32_Reg, GR32_Dsgn_Misc, GR32_Dsgn_Color, GR32_Dsgn_Bitmap, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('GR32_Reg', @GR32_Reg.Register); 
end; 

initialization
  RegisterPackage('GR32_DSGN_L', @Register); 
end.
