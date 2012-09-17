program ArrowHeadDemo;

{$R ArrowHeadDemo.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Title := 'Arrow Head Demo';
  Application.Initialize;
  Application.CreateForm(TFmArrowHeadDemo, FmArrowHeadDemo);
  Application.Run;
end.
