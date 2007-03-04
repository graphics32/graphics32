program PixelF_Ex;

{$MODE Delphi}

uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
