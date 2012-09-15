program LineStippling_Ex;

uses
  Interfaces,
  Forms,
  SysUtils,
  MainUnit in 'MainUnit.pas' {FormLineStippling};

{$R *.res}

begin
  Application.Title:='GR32 Line Stippling Example';
  Application.Initialize;
  Application.CreateForm(TFormLineStippling, FormLineStippling);
  Application.Run;
end.
