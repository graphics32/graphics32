---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.VertLineX
kind: Method
scope: Public
declaration: "procedure VertLineX(X, Y1, Y2: TFixed; Value: TColor32);"
summary: "Draws an unclipped vertical line segment at fixed-point coordinates."
parameters:
  - name: X, Y1, Y2
    type: TFixed
    description: "Fixed-point X column, start Y, and end Y coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`VertLineX` draws an unclipped vertical line at 16.16 fixed-point coordinates.
