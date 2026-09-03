---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.HorizontalLineToRelative
kind: Method
declaration: "procedure HorizontalLineToRelative(const X: TFloat);"
summary: "Appends a horizontal line segment by adding a relative X offset."
parameters:
  - name: X
    type: TFloat
    description: "Relative X offset to add to the current position."
---

## Description

`HorizontalLineToRelative` appends a horizontal line segment extending from [[CurrentPoint]] by distance `X`. The target endpoint coordinate is `(CurrentPoint.X + X, CurrentPoint.Y)`.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(50.0, 100.0);
    Canvas.HorizontalLineToRelative(75.0); // Line to (125, 100)
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
