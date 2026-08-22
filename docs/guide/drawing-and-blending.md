# Drawing & Blending

Graphics32 supports high-speed line drawing, polygon fill, and alpha-blending primitives.

---

## Pixel Blending Modes

When drawing pixels onto a `TBitmap32`, Graphics32 supports several combine modes (`TDrawMode`):

- `dmOpaque`: Overwrites target pixel completely.
- `dmBlend`: Standard alpha blending using source pixel alpha channel.
- `dmTransparent`: Ignores pixels matching `TransparentColor`.
- `dmCustom`: Uses a user-assigned blending function callback.

```pascal
Bitmap.DrawMode := dmBlend;
```

---

## Primitive Drawing Methods

### Lines and Rectangles

```pascal
// Antialiased line
Bitmap.LineA(X1, Y1, X2, Y2, clBlack32);

// Filled rectangle with alpha blending
Bitmap.FillRect(Left, Top, Right, Bottom, Color32(128, 255, 0, 0));
```

### Vector Polygon Rasterization (`GR32_Polygons`)

```pascal
uses GR32, GR32_Polygons;

procedure DrawPolygon(Bitmap: TBitmap32);
var
  Poly: TPolygon32;
begin
  Poly := TPolygon32.Create;
  try
    Poly.Add(PointD(100, 100));
    Poly.Add(PointD(200, 50));
    Poly.Add(PointD(250, 200));
    Poly.Add(PointD(150, 250));

    // Draw filled anti-aliased polygon
    Poly.DrawFill(Bitmap, clBlue32);
  finally
    Poly.Free;
  end;
end;
```
