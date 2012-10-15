program LineSimplification;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FrmLineSimplification};

begin
  Application.Initialize;
  Application.CreateForm(TFrmLineSimplification, FrmLineSimplification);
  Application.Run;
end.

