program LineStippling_Ex;

uses
  Interfaces,
  Forms,
  SysUtils,
  GR32_L,
  MainUnit in 'MainUnit.pas' {FormLineStippling};

begin
  Application.Title:='GR32 Line Stippling Example';
  Application.Initialize;
  Application.CreateForm(TFormLineStippling, FormLineStippling);
  Application.Run;
end.
