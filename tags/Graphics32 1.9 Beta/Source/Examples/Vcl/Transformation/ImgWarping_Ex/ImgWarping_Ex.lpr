program ImgWarping_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {MainForm},
  BrushAuxiliaries in 'BrushAuxiliaries.pas';

begin
  Application.Title:='Image Warping Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
