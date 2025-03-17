program RenderText;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

{$R '..\..\manifest.res'}

begin
  Application.Initialize;
  Application.CreateForm(TFormRenderText, FormRenderText);
  Application.Run;
end.
