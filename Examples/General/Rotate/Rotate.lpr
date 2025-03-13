program Rotate;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormRotateExample, FormRotateExample);
  Application.Run;
end.

