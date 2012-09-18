program ArrowHeadDemo;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmArrowHeadDemo};

begin
  Application.Initialize;
  Application.CreateForm(TFmArrowHeadDemo, FmArrowHeadDemo);
  Application.Run;
end.
