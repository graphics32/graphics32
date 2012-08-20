program ArrowHeadDemo;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmArrowHeadDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ArrowHead Demo';
  Application.CreateForm(TFmArrowHeadDemo, FmArrowHeadDemo);
  Application.Run;
end.
