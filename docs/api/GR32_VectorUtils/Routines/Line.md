---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: Line
kind: Function
summary: "Generates a 2-point line segment contour."
overloads:
  - signature: "function Line(const P1, P2: TFloatPoint): TArrayOfFloatPoint; overload;"
    summary: "Generates a line segment contour from P1 to P2."
    parameters:
      - name: P1, P2
        type: TFloatPoint
        description: "Start and end coordinates."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Line(const X1, Y1, X2, Y2: TFloat): TArrayOfFloatPoint; overload;"
    summary: "Generates a line segment contour from (X1, Y1) to (X2, Y2)."
    parameters:
      - name: X1, Y1, X2, Y2
        type: TFloat
        description: "Start and end coordinates."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
---

## Description

`Line` constructs a 2-point line segment polygon array.
