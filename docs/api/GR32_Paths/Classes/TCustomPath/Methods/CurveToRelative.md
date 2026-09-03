---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.CurveToRelative
kind: Method
summary: "Appends a cubic Bezier curve segment using relative coordinate offsets."
overloads:
  - signature: "procedure CurveToRelative(const X1, Y1, X2, Y2, X, Y: TFloat); overload;"
    summary: "Appends a cubic Bezier curve with relative control points (X1, Y1) and (X2, Y2) to relative endpoint (X, Y)."
    parameters:
      - name: X1, Y1
        type: TFloat
        description: "Relative offsets for first control point."
      - name: X2, Y2
        type: TFloat
        description: "Relative offsets for second control point."
      - name: X, Y
        type: TFloat
        description: "Relative offsets for endpoint."

  - signature: "procedure CurveToRelative(const X2, Y2, X, Y: TFloat); overload;"
    summary: "Appends a smooth cubic Bezier curve using relative second control point (X2, Y2) and relative endpoint (X, Y)."
    parameters:
      - name: X2, Y2
        type: TFloat
        description: "Relative offsets for second control point."
      - name: X, Y
        type: TFloat
        description: "Relative offsets for endpoint."

  - signature: "procedure CurveToRelative(const C1, C2, P: TFloatPoint); overload;"
    summary: "Appends a cubic Bezier curve using relative control points C1, C2 and relative endpoint P."
    parameters:
      - name: C1
        type: TFloatPoint
        description: "Relative first control point offset."
      - name: C2
        type: TFloatPoint
        description: "Relative second control point offset."
      - name: P
        type: TFloatPoint
        description: "Relative endpoint offset."

  - signature: "procedure CurveToRelative(const C2, P: TFloatPoint); overload;"
    summary: "Appends a smooth cubic Bezier curve using relative second control point C2 and relative endpoint P."
    parameters:
      - name: C2
        type: TFloatPoint
        description: "Relative second control point offset."
      - name: P
        type: TFloatPoint
        description: "Relative endpoint offset."
---

## Description

`CurveToRelative` appends a cubic Bezier curve segment where all control points and target coordinates are specified as relative offsets from [[CurrentPoint]].

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(50.0, 100.0);
    // Draw curve relative to current position (50, 100)
    Canvas.CurveToRelative(20.0, -50.0, 80.0, 50.0, 100.0, 0.0);
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
