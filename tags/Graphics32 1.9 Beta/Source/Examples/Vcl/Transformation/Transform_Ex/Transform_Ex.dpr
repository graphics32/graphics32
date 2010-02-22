program Transform_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormTranformExample};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTranformExample, FormTranformExample);
  Application.Run;
end.
