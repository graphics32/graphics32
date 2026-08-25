---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.VertLine
kind: Method
scope: Public
declaration: "procedure VertLine(X, Y1, Y2: Integer; Value: TColor32);"
summary: "Draws an unclipped opaque vertical line segment at integer coordinates."
parameters:
  - name: X, Y1, Y2
    type: Integer
    description: "X column, start Y, and end Y coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`VertLine` fills vertical pixels from `Y1` to `Y2` at column `X` directly without clipping checks or blending.
