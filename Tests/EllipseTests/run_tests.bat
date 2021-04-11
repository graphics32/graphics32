@call rsvars.bat
@dcc32.exe --no-config ^
-AForms=Vcl.Forms;Graphics=Vcl.Graphics;Controls=Vcl.Controls;Dialogs=Vcl.Dialogs;ExtCtrls=Vcl.ExtCtrls;Clipbrd=Vcl.Clipbrd ^
-NSSystem;Winapi ^
-U"%BDS%\lib\Win32\release" ^
EllipseTests.dpr
@if %errorlevel% neq 0 (pause & exit)

EllipseTests.exe
del EllipseTests.exe
