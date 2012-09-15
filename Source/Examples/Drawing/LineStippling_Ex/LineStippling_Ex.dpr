program LineStippling_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormLineStippling};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormLineStippling, FormLineStippling);
  Application.Run;
end.
