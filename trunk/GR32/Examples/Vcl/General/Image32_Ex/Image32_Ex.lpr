program Image32_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
