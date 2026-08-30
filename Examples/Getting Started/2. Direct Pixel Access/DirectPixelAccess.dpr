program DirectPixelAccess;

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormDirectPixelAccess, FormDirectPixelAccess);
  Application.Run;
end.
