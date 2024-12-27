program LineStippling;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

{$R '..\..\manifest.res'}

begin
  Application.Initialize;
  Application.CreateForm(TFormLineStippling, FormLineStippling);
  Application.Run;
end.
