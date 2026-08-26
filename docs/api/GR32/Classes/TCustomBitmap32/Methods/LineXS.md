---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineXS
kind: Method
scope: Public
declaration: "procedure LineXS(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False);"
summary: "Draws a clipped line segment at fixed-point coordinates."
parameters:
  - name: X1, Y1, X2, Y2
    type: TFixed
    description: "16.16 fixed-point start and end coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel."
---

## Description

`LineXS` draws a clipped line segment using 16.16 fixed-point coordinates.
