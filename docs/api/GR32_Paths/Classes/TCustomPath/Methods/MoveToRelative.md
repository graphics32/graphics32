---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.MoveToRelative
kind: Method
summary: "Moves the current drawing position by a relative offset from the current coordinate."
overloads:
  - signature: "procedure MoveToRelative(const X, Y: TFloat); overload;"
    summary: "Moves the current position by relative offset (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Horizontal and vertical coordinate offsets."

  - signature: "procedure MoveToRelative(const P: TFloatPoint); overload;"
    summary: "Moves the current position by relative point offset P."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Relative point offset."
---

## Description

`MoveToRelative` adjusts the current drawing position by adding a relative coordinate offset to [[CurrentPoint]] without creating a line segment.

Calling `MoveToRelative` starts a new sub-path contour at the computed target position.

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(50.0, 50.0);
    Canvas.LineTo(100.0, 50.0);
    Canvas.EndPath;

    // Move down 30 pixels from current position (100, 50) -> (100, 80)
    Canvas.MoveToRelative(0.0, 30.0);
    Canvas.LineTo(150.0, 80.0);
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
