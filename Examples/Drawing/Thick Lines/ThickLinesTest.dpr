program ThickLinesTest;

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {FormThickLineTest},
  GR32.Lines.Thick in 'GR32.Lines.Thick.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormThickLineTest, FormThickLineTest);
  Application.Run;
end.
