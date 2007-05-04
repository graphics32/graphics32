program ImgWarping_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  BrushAuxiliaries in 'BrushAuxiliaries.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
