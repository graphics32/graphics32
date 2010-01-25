program RenderText_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormRenderText};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRenderText, FormRenderText);
  Application.Run;
end.
