program Grow;





{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormGrow};

begin
  Application.Initialize;
  Application.CreateForm(TFormGrow, FormGrow);
  Application.Run;
end.
