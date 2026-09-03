---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.LineToRelative
kind: Method
summary: "Appends a straight line segment using relative offsets from the current drawing position."
overloads:
  - signature: "procedure LineToRelative(const X, Y: TFloat); overload;"
    summary: "Appends a line segment offset by (X, Y) from the current position."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Horizontal and vertical distance offsets."

  - signature: "procedure LineToRelative(const P: TFloatPoint); overload;"
    summary: "Appends a line segment offset by point P from the current position."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Relative point offset."
---

## Description

`LineToRelative` appends a straight line segment calculated by adding a relative coordinate offset to [[CurrentPoint]]. Upon completion, `CurrentPoint` is updated to the newly calculated location.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(50.0, 50.0);
    // Draw 100 pixels right, then 50 pixels down
    Canvas.LineToRelative(100.0, 0.0);
    Canvas.LineToRelative(0.0, 50.0);
    Canvas.EndPath(False);
  finally
    Canvas.Free;
  end;
end;
```
