program SVGviewer;

{$R 'Media.res' 'Media.rc'}
{$R '..\..\manifest.res'}

uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF }
  Forms,
  SVGviewerMain in 'SVGviewerMain.pas' {FormMain};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSVGviewer, FormSVGviewer);
  Application.Run;
end.
