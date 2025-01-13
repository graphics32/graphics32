program TransparentForm;

{$R 'Media.res' 'Media.rc'}
{$R '..\..\manifest.res'}

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {FormMain};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
