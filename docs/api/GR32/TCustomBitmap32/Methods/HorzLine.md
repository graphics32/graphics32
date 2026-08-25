---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.HorzLine
kind: Method
scope: Public
declaration: "procedure HorzLine(X1, Y, X2: Integer; Value: TColor32);"
summary: "Draws an unclipped opaque horizontal line segment at integer coordinates."
parameters:
  - name: X1, Y, X2
    type: Integer
    description: "Start X, Y row, and end X coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`HorzLine` fills horizontal pixels from `X1` to `X2` at row `Y` directly without clipping checks or blending.
