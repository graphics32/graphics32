program Image32_Ex;

uses
  Interfaces,
  Forms,
  GR32_L,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1};

begin
  Application.Initialize;
  Application.CreateForm(TFormImage32Example, FormImage32Example);
  Application.Run;
end.
