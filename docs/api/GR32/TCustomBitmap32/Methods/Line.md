---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Line
kind: Method
scope: Public
declaration: "procedure Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);"
summary: "Draws an unclipped opaque arbitrary line segment between two integer points."
parameters:
  - name: X1, Y1, X2, Y2
    type: Integer
    description: "Start (X1, Y1) and end (X2, Y2) pixel coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel (X2, Y2) in the rendering pass."
---

## Description

`Line` draws an unclipped 1-pixel wide line segment between $(X1, Y1)$ and $(X2, Y2)$.
