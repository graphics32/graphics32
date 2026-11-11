program DrawingShapesAndAlphaBlending;

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormDrawingShapes, FormDrawingShapes);
  Application.Run;
end.
