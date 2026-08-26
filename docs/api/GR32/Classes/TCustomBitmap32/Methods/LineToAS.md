---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineToAS
kind: Method
scope: Public
declaration: "procedure LineToAS(X, Y: Integer);"
summary: "Draws a clipped anti-aliased line from current pen position to (X, Y) and updates pen position."
parameters:
  - name: X, Y
    type: Integer
    description: "Target end pixel coordinates."
---

## Description

`LineToAS` draws an anti-aliased line from `PenPos` to `(X, Y)`.
