program ParticleSwarm;

{$R 'mainicon.res' '..\..\mainicon.rc'}

uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {FormMain},
  HelpUnit in 'HelpUnit.pas' {FormHelp};

{$R '..\..\manifest.res'}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
