---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.ConicTo
kind: Method
summary: "Appends a quadratic (conic) Bezier curve segment to the vector path."
overloads:
  - signature: "procedure ConicTo(const X1, Y1, X, Y: TFloat); overload;"
    summary: "Appends a quadratic Bezier curve using control point (X1, Y1) to endpoint (X, Y)."
    parameters:
      - name: X1, Y1
        type: TFloat
        description: "Control point coordinates."
      - name: X, Y
        type: TFloat
        description: "Endpoint coordinates."

  - signature: "procedure ConicTo(const P1, P: TFloatPoint); overload; virtual;"
    summary: "Appends a quadratic Bezier curve using control point P1 and endpoint P."
    parameters:
      - name: P1
        type: TFloatPoint
        description: "Control point."
      - name: P
        type: TFloatPoint
        description: "Destination endpoint."

  - signature: "procedure ConicTo(const X, Y: TFloat); overload;"
    summary: "Appends a smooth quadratic Bezier curve, automatically deriving the control point from the previous conic control point."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Endpoint coordinates."

  - signature: "procedure ConicTo(const P: TFloatPoint); overload; virtual;"
    summary: "Appends a smooth quadratic Bezier curve to point P, automatically reflecting the previous control point."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Destination endpoint."
---

## Description

`ConicTo` appends a quadratic (conic) Bezier curve segment extending from [[CurrentPoint]] to endpoint `P` (or `X, Y`).

Quadratic Bezier curves use a single control point `P1`:
- Explicit signatures accept control point `P1` and endpoint `P`.
- Smooth conic signatures (`ConicTo(P)`) calculate `P1` automatically by reflecting the control point of the preceding `ConicTo` operation across `CurrentPoint`. If the previous command was not a conic curve, `P1` defaults to `CurrentPoint`.

Quadratic curve flattening tolerance is controlled by [[QBezierTolerance]].

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    Canvas.MoveTo(20.0, 100.0);
    // Quadratic arc with control point at (100, 10)
    Canvas.ConicTo(100.0, 10.0, 180.0, 100.0);
    // Smooth continuation to (260, 100)
    Canvas.ConicTo(260.0, 100.0);
    Canvas.EndPath;
  finally
    Canvas.Free;
  end;
end;
```
