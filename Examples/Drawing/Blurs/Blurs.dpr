program Blurs;

{$R 'Media.res' 'Media.rc'}
{$R 'mainicon.res' '..\..\mainicon.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

{$R '..\..\manifest.res'}

begin
  Application.Initialize;
  Application.CreateForm(TFrmBlurs, FrmBlurs);
  Application.Run;
end.
