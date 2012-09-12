program ArrowHeadDemo;

{$I GR32.inc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FmArrowHeadDemo};

{$R *.res}

begin
  Application.Title := 'Arrow Head Demo';
  Application.Initialize;
  Application.CreateForm(TFmArrowHeadDemo, FmArrowHeadDemo);
  Application.Run;
end.
