program Rotate_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  GR32_MediaPathLocator in '..\..\GR32_MediaPathLocator.pas',
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Title:='Rotate Example';
  Application.Initialize;
  Application.CreateForm(TFormRotateExample, FormRotateExample);
  Application.Run;
end.

