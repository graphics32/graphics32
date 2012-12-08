program DocProcessor;

{$R 'MainIcon.res' 'MainIcon.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  HTML_Tags in 'HTML_Tags.pas',
  SimpleDOM in 'SimpleDOM.pas',
  Utils in 'Utils.pas',
  DocStructure in 'DocStructure.pas',
  Pas2Html in 'Pas2Html.pas',
  DelphiParse in 'DelphiParse.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
