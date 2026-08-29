---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: VertLine
kind: Function
declaration: "function VertLine(const X, Y1, Y2: TFloat): TArrayOfFloatPoint;"
summary: "Generates a 2-point vertical line segment contour."
parameters:
  - name: X, Y1, Y2
    type: TFloat
    description: "X, start Y, and end Y coordinates."
---

## Description

`VertLine` constructs a `TArrayOfFloatPoint` containing a vertical line segment from `(X, Y1)` to `(X, Y2)`.
