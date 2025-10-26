program Image32Background;

(* Disabled until Lazarus fixes their stupid build;
** Compilation of resource files fail if the path contains spaces because
** they've forgotten to quote the source filename parameter.

{$R Media.rc}
*)

{$ifdef WINDOWS}
{$R '..\..\manifest.res'}
{$endif WINDOWS}

uses
  Interfaces,
  Forms,
  Main in 'Main.pas' {FormMain};

begin
  Application.Title:='Image32 background example';
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
