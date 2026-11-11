---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.CurrentPoint
kind: Property
declaration: "property CurrentPoint: TFloatPoint read FCurrentPoint write FCurrentPoint;"
summary: "Specifies the current endpoint coordinate of the active vector path."
---

## Description

`CurrentPoint` holds the current 2D point coordinate `(X, Y)` of the active vector path in `TCustomPath`.

Drawing commands (such as [[LineTo]], [[CurveTo]], [[ConicTo]], [[MoveTo]]) read and update `CurrentPoint` to track path positioning.

## Example

```pascal
var
  Canvas: TCanvas32;
  Pt: TFloatPoint;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(100.0, 50.0);
    Pt := Canvas.CurrentPoint; // Pt = (100.0, 50.0)

    Canvas.LineToRelative(50.0, 0.0);
    Pt := Canvas.CurrentPoint; // Pt = (150.0, 50.0)
  finally
    Canvas.Free;
  end;
end;
```
