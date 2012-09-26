program Transform;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormTranformExample, FormTranformExample);
  Application.Run;
end.
