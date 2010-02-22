program DocProcessorC;

{$APPTYPE CONSOLE}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  HTML_Tags in 'HTML_Tags.pas',
  SimpleDOM in 'SimpleDOM.pas',
  Utils in 'Utils.pas',
  DocStructure in 'DocStructure.pas',
  Pas2Html in 'Pas2Html.pas',
  DelphiParse in 'DelphiParse.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
