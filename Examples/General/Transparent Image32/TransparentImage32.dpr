program TransparentImage32;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  Main in 'Main.pas' {FormMain};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
