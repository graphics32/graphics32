//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ByteMaps_Ex.res");
USEFORMNS("MainUnit.pas", Mainunit, Form1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
