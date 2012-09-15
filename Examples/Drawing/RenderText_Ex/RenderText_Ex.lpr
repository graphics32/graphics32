program RenderText_Ex;

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FormRenderText};

{$R *.res}

begin
  Application.Title:='GR32 Render Text Example';
  Application.Initialize;
  Application.CreateForm(TFormRenderText, FormRenderText);
  Application.Run;
end.
