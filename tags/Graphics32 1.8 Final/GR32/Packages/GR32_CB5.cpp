//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GR32_CB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\GR32.pas");
USEUNIT("..\GR32_Blend.pas");
USEUNIT("..\GR32_ByteMaps.pas");
USEUNIT("..\GR32_System.pas");
USEUNIT("..\GR32_DrawingEx.pas");
USEUNIT("..\GR32_Filters.pas");
USEUNIT("..\GR32_Image.pas");
USEUNIT("..\GR32_Layers.pas");
USEUNIT("..\GR32_LowLevel.pas");
USEUNIT("..\GR32_Polygons.pas");
USEUNIT("..\GR32_RangeBars.pas");
USEUNIT("..\GR32_Transforms.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
