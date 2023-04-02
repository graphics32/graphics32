program ColorPicker;

uses
{$ifdef FPC}
  Interfaces,
{$endif FPC}
  Forms,
  MainUnit in 'MainUnit.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

