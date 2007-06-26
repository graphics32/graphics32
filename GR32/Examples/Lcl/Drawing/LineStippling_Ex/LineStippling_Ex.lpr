program LineStippling_Ex;

uses
  Interfaces,
  Forms, SysUtils,
  MainUnit in 'MainUnit.pas' {Form1}, GR32_L;

{$IFDEF Windows}
{$R *.RES}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
