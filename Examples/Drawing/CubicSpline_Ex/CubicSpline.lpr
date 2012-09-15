program CubicSpline;

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FormBezier};

{$R *.res}

begin
  Application.Title:='Cubic Spline';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBezier, FormBezier);
  Application.Run;
end.

