---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.VerticalLineTo
kind: Method
declaration: "procedure VerticalLineTo(const Y: TFloat);"
summary: "Appends a vertical line segment to an absolute Y coordinate."
parameters:
  - name: Y
    type: TFloat
    description: "Target absolute Y coordinate."
---

## Description

`VerticalLineTo` appends a vertical line segment from [[CurrentPoint]] to absolute coordinate `(CurrentPoint.X, Y)` and updates `CurrentPoint.Y` to `Y`.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(100.0, 10.0);
    Canvas.VerticalLineTo(150.0); // Line from (100, 10) to (100, 150)
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
