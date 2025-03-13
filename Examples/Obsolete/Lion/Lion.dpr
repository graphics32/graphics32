program Lion;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmLion},
  LionData in 'LionData.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmLion, FrmLion);
  Application.Run;
end.

