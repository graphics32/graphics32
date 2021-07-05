@REM This script expects Delphi to be on the PATH and uses the Win64 libraries.

@call rsvars.bat
@dcc64.exe --no-config ^
-B ^
-AForms=Vcl.Forms;Graphics=Vcl.Graphics;Controls=Vcl.Controls;Dialogs=Vcl.Dialogs;ExtCtrls=Vcl.ExtCtrls;Clipbrd=Vcl.Clipbrd ^
-NSSystem;Winapi ^
-U"%BDS%\lib\Win64\release" ^
EllipseTests.dpr
@if %errorlevel% neq 0 (pause & exit)

EllipseTests.exe
del EllipseTests.exe
