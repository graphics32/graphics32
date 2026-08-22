# Basic Drawing & Alpha Blending Example

This example demonstrates creating a `TBitmap32`, drawing translucent shapes, and rendering text or lines.

```pascal
program BasicDrawingExample;

{$MODE DELPHI}

uses
  Classes, SysUtils, GR32, GR32_Polygons;

var
  Bmp: TBitmap32;
begin
  Bmp := TBitmap32.Create;
  try
    // 1. Set dimensions and clear background to white
    Bmp.SetSize(600, 400);
    Bmp.Clear(clWhite32);

    // 2. Enable standard alpha blending
    Bmp.DrawMode := dmBlend;

    // 3. Draw semi-transparent rectangle (Red, 50% opacity)
    Bmp.FillRect(50, 50, 250, 250, Color32(128, 255, 0, 0));

    // 4. Draw overlapping semi-transparent rectangle (Blue, 50% opacity)
    Bmp.FillRect(150, 150, 350, 350, Color32(128, 0, 0, 255));

    // 5. Draw antialiased line
    Bmp.LineA(20, 20, 580, 380, clBlack32);

    // 6. Save bitmap
    Bmp.SaveToFile('output.png');
  finally
    Bmp.Free;
  end;
end.
```
