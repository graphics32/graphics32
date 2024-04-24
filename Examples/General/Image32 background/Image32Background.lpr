program Image32Background;

{$R Media.rc}
{$ifdef WINDOWS}
{$R '..\..\manifest.res'}
{$endif WINDOWS}

uses
  Interfaces,
  Forms,
  Main in 'Main.pas' {FormMain};

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
