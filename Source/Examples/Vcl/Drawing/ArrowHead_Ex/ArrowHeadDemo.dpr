program ArrowHeadDemo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FmArrowHeadDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ArrowHead Demo';
  Application.CreateForm(TFmArrowHeadDemo, FmArrowHeadDemo);
  Application.Run;
end.
