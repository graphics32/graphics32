---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.MoveTo
kind: Method
summary: "Moves the current drawing position to a new coordinate without drawing a connecting line."
overloads:
  - signature: "procedure MoveTo(const X, Y: TFloat); overload;"
    summary: "Moves the current path coordinate to absolute position (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Target X and Y coordinates."

  - signature: "procedure MoveTo(const P: TFloatPoint); overload; virtual;"
    summary: "Moves the current path coordinate to absolute point P."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Target point location."
---

## Description

`MoveTo` sets the current path drawing position to a specified absolute coordinate without drawing a line segment.

When building vector paths, calling `MoveTo` implicitly ends any active sub-path segment and starts a new sub-path contour.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    // Start first sub-path
    Canvas.MoveTo(20.0, 20.0);
    Canvas.LineTo(100.0, 20.0);
    Canvas.EndPath;

    // Start second sub-path at new coordinate
    Canvas.MoveTo(FloatPoint(20.0, 50.0));
    Canvas.LineTo(100.0, 50.0);
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
