program CubicSpline;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormBezier};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBezier, FormBezier);
  Application.Run;
end.

