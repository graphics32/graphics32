program ImgWarping;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas',
  BrushAuxiliaries in 'BrushAuxiliaries.pas';

begin
  Application.Title:='Image Warping Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
