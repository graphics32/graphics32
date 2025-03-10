program Blurs;

{$R Media.rc}

uses
  Forms,
  Interfaces,
  MainUnit in 'MainUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmBlurs, FrmBlurs);
  Application.Run;
end.
