program CubicSpline;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Title:='Cubic Spline';
  Application.Initialize;
  Application.CreateForm(TFormBezier, FormBezier);
  Application.Run;
end.

