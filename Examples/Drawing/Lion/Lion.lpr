program Lion;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FrmTiger},
  LionData in 'LionData.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmTiger, FrmTiger);
  Application.Run;
end.
