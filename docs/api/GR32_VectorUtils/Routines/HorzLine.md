---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: HorzLine
kind: Function
declaration: "function HorzLine(const X1, Y, X2: TFloat): TArrayOfFloatPoint;"
summary: "Generates a 2-point horizontal line segment contour."
parameters:
  - name: X1, Y, X2
    type: TFloat
    description: "Start X, Y, and end X coordinates."
---

## Description

`HorzLine` constructs a `TArrayOfFloatPoint` containing a horizontal line segment from `(X1, Y)` to `(X2, Y)`.
