//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEUNIT("..\GR32_Reg.pas");
USERES("..\GR32_Reg.dcr");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("GR32_CB5.bpi");
USEUNIT("..\GR32_Dsgn_Color.pas");
USEFORMNS("..\GR32_Dsgn_Bitmap.pas", GR32_dsgn_bitmap, PictureEditorForm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
   return 1;
}
//---------------------------------------------------------------------------
