program Rotate_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormRotateExample};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRotateExample, FormRotateExample);
  Application.Run;
end.
