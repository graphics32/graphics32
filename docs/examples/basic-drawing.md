# Basic Drawing & Alpha Blending Example

This example demonstrates creating a `TBitmap32` and drawing translucent shapes.

```pascal
program BasicDrawingExample;

uses
  Classes, SysUtils, GR32;

var
  Bitmap: TBitmap32;
begin
  Bitmap := TBitmap32.Create;
  try
    // 1. Set dimensions and clear background to white
    Bitmap.SetSize(600, 400, False);
    Bitmap.Clear(clWhite32);

    // 2. Draw semi-transparent box (Red, 50% opacity)
    //    The pixels below the box are replaced.
    Bitmap.FillRectS(50, 50, 250, 250, clTrRed32);

    // 3. Draw overlapping semi-transparent box (Blue, 50% opacity)
    //    The box is blended with the pixels below it.
    Bitmap.FillRectTS(150, 150, 350, 350, clTrBlue32);

    // 4. Draw antialiased triangle
    Bitmap.PenColor := clBlack32;
    Bitmap.MoveTo(150, 100);
    Bitmap.LineToAS(350, 100);
    Bitmap.LineToAS(250, 273);
    Bitmap.LineToAS(150, 100);

    // 5. Save bitmap as a PNG
    Bitmap.SaveToFile('output.png');
  finally
    Bitmap.Free;
  end;
end.
```
