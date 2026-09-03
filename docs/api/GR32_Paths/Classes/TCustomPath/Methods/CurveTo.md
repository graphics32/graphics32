---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.CurveTo
kind: Method
summary: "Appends a cubic Bezier curve segment to the vector path."
overloads:
  - signature: "procedure CurveTo(const X1, Y1, X2, Y2, X, Y: TFloat); overload;"
    summary: "Appends a cubic Bezier curve using explicit control points (X1, Y1) and (X2, Y2) to endpoint (X, Y)."
    parameters:
      - name: X1, Y1
        type: TFloat
        description: "First control point coordinates."
      - name: X2, Y2
        type: TFloat
        description: "Second control point coordinates."
      - name: X, Y
        type: TFloat
        description: "Endpoint coordinates."

  - signature: "procedure CurveTo(const X2, Y2, X, Y: TFloat); overload;"
    summary: "Appends a smooth cubic Bezier curve, automatically deriving the first control point from the previous control point."
    parameters:
      - name: X2, Y2
        type: TFloat
        description: "Second control point coordinates."
      - name: X, Y
        type: TFloat
        description: "Endpoint coordinates."

  - signature: "procedure CurveTo(const C1, C2, P: TFloatPoint); overload; virtual;"
    summary: "Appends a cubic Bezier curve using control points C1, C2 and endpoint P."
    parameters:
      - name: C1
        type: TFloatPoint
        description: "First control point."
      - name: C2
        type: TFloatPoint
        description: "Second control point."
      - name: P
        type: TFloatPoint
        description: "Destination endpoint."

  - signature: "procedure CurveTo(const C2, P: TFloatPoint); overload; virtual;"
    summary: "Appends a smooth cubic Bezier curve using control point C2 and endpoint P."
    parameters:
      - name: C2
        type: TFloatPoint
        description: "Second control point."
      - name: P
        type: TFloatPoint
        description: "Destination endpoint."
---

## Description

`CurveTo` appends a cubic Bezier curve segment extending from [[CurrentPoint]] to the designated endpoint `P` (or `X, Y`).

Cubic Bezier curves use two control points to shape the curve tangent:
- Standard signatures accept both control points `C1` and `C2` explicitly.
- Smooth curve signatures (`CurveTo(C2, P)`) accept only `C2` and automatically calculate `C1` by reflecting the last control point of a preceding `CurveTo` operation across `CurrentPoint`. If the preceding command was not a cubic curve, `C1` defaults to `CurrentPoint`.

Curvature subdivision tolerance during flattening is governed by [[CBezierTolerance]].

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(10.0, 100.0);
    // Draw S-curve segment
    Canvas.CurveTo(50.0, 10.0, 150.0, 190.0, 200.0, 100.0);
    // Smooth continuation
    Canvas.CurveTo(250.0, 10.0, 300.0, 100.0);
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
