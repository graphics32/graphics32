unit MainUnit;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, GR32_Resamplers, GR32_Transforms;

type
  TFormResamplingAndRotation = class(TForm)
    Image32: TImage32;
    procedure FormCreate(Sender: TObject);
  public
    procedure RotateBitmap(Source, Target: TBitmap32; AngleDegrees: Single);
  end;

var
  FormResamplingAndRotation: TFormResamplingAndRotation;

implementation

{$R *.dfm}

uses
  Windows,
  Math;

procedure TFormResamplingAndRotation.RotateBitmap(Source, Target: TBitmap32; AngleDegrees: Single);
var
  Transform: TAffineTransformation;
  r: TFloatRect;
begin
  Transform := TAffineTransformation.Create;
  try
    // Set original source rectangle bounding box
    Transform.SrcRect := FloatRect(Source.BoundsRect);

    // Translate origin to center, rotate by angle, and translate back.
    // In other words: Rotate around center point (X, Y) by specified angle in degrees
    Transform.Rotate(Source.Width / 2, Source.Height / 2, AngleDegrees);

    // Get the size the bitmap will have once it's been rotated
    r := Transform.GetTransformedBounds(FloatRect(Source.BoundsRect));

    // Center the rotated result in the target bitmap (which will be larger
    // than the source because of the rotation)
    Transform.Translate((r.Width - Source.Width) / 2, (r.Height - Source.Height) / 2);

    // Size the target so it fits the rotated bitmap
    Target.SetSize(Ceil(r.Width), Ceil(r.Height));

    // Apply the transformation
    GR32_Transforms.Transform(Target, Source, Transform);
  finally
    Transform.Free;
  end;
end;

procedure TFormResamplingAndRotation.FormCreate(Sender: TObject);
var
  Source: TBitmap32;
  Sz: TSize;
begin
  Source := TBitmap32.Create;
  try
    // Setup our bitmap
    Source.SetSize(300, 200);

    Source.Font.Name := 'Arial';
    Source.Font.Size := 12;
    Source.Font.Style := [fsBold];

    // Filled blue-ish rectangle
    Source.FillRect(80, 55, 220, 145, Color32(52, 152, 219));

    // Get the pixel size of the text
    Sz := Source.TextExtent('Rotated Bitmap');

    // Draw text centered in the rectangle we drew above
    Source.RenderText((220 + 80 - Sz.cx) div 2, (145 + 55 - Sz.cy) div 2, 'Rotated Bitmap', clWhite32);

    // Attach a bilinear resampler to the bitmap for smooth rotated edges
    TLinearResampler.Create(Source);

    // Rotate the bitmap
    RotateBitmap(Source, Image32.Bitmap, 25.0);
  finally
    Source.Free;
  end;
end;

end.
