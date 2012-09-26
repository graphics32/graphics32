program ArrowHead;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFmArrowHead, FmArrowHead);
  Application.Run;
end.
