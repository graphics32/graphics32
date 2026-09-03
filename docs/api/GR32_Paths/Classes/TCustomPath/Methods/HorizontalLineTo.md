---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.HorizontalLineTo
kind: Method
declaration: "procedure HorizontalLineTo(const X: TFloat);"
summary: "Appends a horizontal line segment to an absolute X coordinate."
parameters:
  - name: X
    type: TFloat
    description: "Target absolute X coordinate."
---

## Description

`HorizontalLineTo` appends a horizontal line segment from [[CurrentPoint]] to the absolute coordinate `(X, CurrentPoint.Y)` and updates `CurrentPoint.X` to `X`.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(10.0, 50.0);
    Canvas.HorizontalLineTo(200.0); // Draws line from (10, 50) to (200, 50)
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
