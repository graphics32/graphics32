program TextDemo;

uses
  FastMM4,
{$IFNDEF CPUX64}
//  FastMove,
{$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
