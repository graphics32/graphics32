unit MainUnit;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image;

type
  TFormDrawingShapes = class(TForm)
    Image32: TImage32;
    procedure FormCreate(Sender: TObject);
  public
    procedure DrawBlendedShapes(Bitmap: TBitmap32);
  end;

var
  FormDrawingShapes: TFormDrawingShapes;

implementation

{$R *.dfm}

procedure TFormDrawingShapes.DrawBlendedShapes(Bitmap: TBitmap32);
begin
  Bitmap.SetSize(320, 200);
  Bitmap.Clear(clWhite32);

  // Draw an anti-aliased diagonal line (clBlue32)
  Bitmap.LineA(20, 20, 300, 180, clBlue32);

  // Fill a solid red rectangle (opaque)
  Bitmap.FillRect(30, 40, 150, 160, Color32(231, 76, 60, 255));

  // Fill an overlapping semi-transparent green rectangle (50% opacity) using FillRectT
  Bitmap.FillRectT(100, 80, 250, 180, Color32(46, 204, 113, 128));
end;

procedure TFormDrawingShapes.FormCreate(Sender: TObject);
begin
  Image32.SetupBitmap;
  DrawBlendedShapes(Image32.Bitmap);
end;

end.
