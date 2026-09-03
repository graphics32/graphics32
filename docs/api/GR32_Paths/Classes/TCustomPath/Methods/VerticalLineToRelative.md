---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.VerticalLineToRelative
kind: Method
declaration: "procedure VerticalLineToRelative(const Y: TFloat);"
summary: "Appends a vertical line segment by adding a relative Y offset."
parameters:
  - name: Y
    type: TFloat
    description: "Relative Y offset to add to the current position."
---

## Description

`VerticalLineToRelative` appends a vertical line segment extending from [[CurrentPoint]] by distance `Y`. The target endpoint coordinate is `(CurrentPoint.X, CurrentPoint.Y + Y)`.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(50.0, 50.0);
    Canvas.VerticalLineToRelative(100.0); // Line to (50, 150)
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
