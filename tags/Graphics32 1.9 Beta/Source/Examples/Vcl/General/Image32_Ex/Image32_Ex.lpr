program Image32_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1};

begin
  Application.Title:='TImage32 Example';
  Application.Initialize;
  Application.CreateForm(TFormImage32Example, FormImage32Example);
  Application.Run;
end.
