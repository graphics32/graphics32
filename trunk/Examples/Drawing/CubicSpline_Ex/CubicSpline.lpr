program CubicSpline;

{$R CubicSpline.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormBezier, FormBezier);
  Application.Run;
end.

