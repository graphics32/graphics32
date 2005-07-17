program Resamplers_Ex;

uses
  Forms,
  MainUnit in '..\..\..\..\Experimental\Michael\FineResample_Mod\FineResample_Mod\MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
