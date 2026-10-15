program SVGviewer;

uses
  Forms,
  SVGviewerMain in 'SVGviewerMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSVGviewer, FormSVGviewer);
  Application.Run;
end.
