unit MainUnit;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image;

type
  TFormDirectPixelAccess = class(TForm)
    Image32: TImage32;
    procedure FormCreate(Sender: TObject);
  public
    procedure CreateCustomGradient(Bitmap: TBitmap32);
  end;

var
  FormDirectPixelAccess: TFormDirectPixelAccess;

implementation

{$R *.dfm}

procedure TFormDirectPixelAccess.CreateCustomGradient(Bitmap: TBitmap32);
var
  X, Y: Integer;
  R, G, B: Byte;
begin
  Bitmap.SetSize(256, 256);

  // Directly set each pixel color based on coordinates
  for Y := 0 to Bitmap.Height - 1 do
  begin
    for X := 0 to Bitmap.Width - 1 do
    begin
      R := Byte(X);
      G := Byte(Y);
      B := 128;
      // Pixel[] offers raw, high-speed per-pixel access
      Bitmap.Pixel[X, Y] := Color32(R, G, B, 255);
    end;
  end;
end;

procedure TFormDirectPixelAccess.FormCreate(Sender: TObject);
begin
  Image32.SetupBitmap;
  CreateCustomGradient(Image32.Bitmap);
end;

end.
