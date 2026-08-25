---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineToS
kind: Method
scope: Public
declaration: "procedure LineToS(X, Y: Integer);"
summary: "Draws a clipped opaque line from current pen position to (X, Y) and updates pen position."
parameters:
  - name: X, Y
    type: Integer
    description: "Target end pixel coordinates."
---

## Description

`LineToS` draws a line from `PenPos` to `(X, Y)` and updates `PenPos` to `(X, Y)`.
