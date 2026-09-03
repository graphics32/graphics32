---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.ConicToRelative
kind: Method
summary: "Appends a quadratic (conic) Bezier curve segment using relative coordinate offsets."
overloads:
  - signature: "procedure ConicToRelative(const X1, Y1, X, Y: TFloat); overload;"
    summary: "Appends a quadratic Bezier curve with relative control point (X1, Y1) to relative endpoint (X, Y)."
    parameters:
      - name: X1, Y1
        type: TFloat
        description: "Relative control point offsets."
      - name: X, Y
        type: TFloat
        description: "Relative endpoint offsets."

  - signature: "procedure ConicToRelative(const P1, P: TFloatPoint); overload;"
    summary: "Appends a quadratic Bezier curve using relative control point P1 and relative endpoint P."
    parameters:
      - name: P1
        type: TFloatPoint
        description: "Relative control point offset."
      - name: P
        type: TFloatPoint
        description: "Relative endpoint offset."

  - signature: "procedure ConicToRelative(const X, Y: TFloat); overload;"
    summary: "Appends a smooth quadratic Bezier curve to relative endpoint (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Relative endpoint offsets."

  - signature: "procedure ConicToRelative(const P: TFloatPoint); overload;"
    summary: "Appends a smooth quadratic Bezier curve to relative endpoint P."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Relative endpoint offset."
---

## Description

`ConicToRelative` appends a quadratic (conic) Bezier curve segment where control points and endpoints are specified as relative offsets from [[CurrentPoint]].

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(50.0, 100.0);
    Canvas.ConicToRelative(50.0, -80.0, 100.0, 0.0);
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
