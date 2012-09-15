program Transform_Ex;

uses
  Forms,
  GR32_MediaPathLocator in '..\..\GR32_MediaPathLocator.pas',
  MainUnit in 'MainUnit.pas' {FormTranformExample};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTranformExample, FormTranformExample);
  Application.Run;
end.
