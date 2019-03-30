program GammaBlur;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormGammaBlur};

begin
  Application.Initialize;
  Application.CreateForm(TFormGammaBlur, FormGammaBlur);
  Application.Run;
end.
