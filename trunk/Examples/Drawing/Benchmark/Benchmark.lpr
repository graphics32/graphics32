program Benchmark;

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas', GR32_L {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.