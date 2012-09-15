//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ImgView_Layers_Ex.res");
USEFORMNS("MainUnit.pas", Mainunit, MainForm);
USEFORMNS("NewImageUnit.pas", Newimageunit, NewImageForm);
USEFORMNS("RGBALoaderUnit.pas", Rgbaloaderunit, RGBALoaderForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TMainForm), &MainForm);
                 Application->CreateForm(__classid(TNewImageForm), &NewImageForm);
                 Application->CreateForm(__classid(TRGBALoaderForm), &RGBALoaderForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
