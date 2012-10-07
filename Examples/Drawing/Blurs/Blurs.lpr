program Blurs;

{$R Media.rc}

uses
  Interfaces,
  Variants,
  Forms, imagesforlazarus,
  MainUnit in 'MainUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmBlurs, FrmBlurs);
  Application.Run;
end.
