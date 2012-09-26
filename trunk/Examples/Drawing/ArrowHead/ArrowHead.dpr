program ArrowHead;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmArrowHead};

begin
  Application.Initialize;
  Application.CreateForm(TFmArrowHead, FmArrowHead);
  Application.Run;
end.
