program PSD_export;

uses
{$ifdef FPC}
  Interfaces,
{$endif FPC}
  Forms,
  UnitMain in 'UnitMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
