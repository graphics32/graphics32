program ResizeAndRotate;

{$R 'mainicon.res' '..\..\mainicon.rc'}

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {FormMain};

{$R '..\..\manifest.res'}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
