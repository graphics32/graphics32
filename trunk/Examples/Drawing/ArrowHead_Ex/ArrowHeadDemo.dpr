program ArrowHeadDemo;

{$R 'ArrowHeadDemo.res' 'ArrowHeadDemo.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFmArrowHeadDemo, FmArrowHeadDemo);
  Application.Run;
end.
