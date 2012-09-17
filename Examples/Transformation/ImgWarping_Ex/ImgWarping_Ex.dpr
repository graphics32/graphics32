program ImgWarping_Ex;

{$R 'ImgWarping_Ex.res' 'ImgWarping_Ex.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas',
  BrushAuxiliaries in 'BrushAuxiliaries.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
