program RenderText_Ex;

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FormRenderText};

begin
  Application.Title:='Render Text Example';
  Application.Initialize;
  Application.CreateForm(TFormRenderText, FormRenderText);
  Application.Run;
end.
