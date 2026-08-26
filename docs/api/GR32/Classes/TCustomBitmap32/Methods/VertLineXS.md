---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.VertLineXS
kind: Method
scope: Public
declaration: "procedure VertLineXS(X, Y1, Y2: TFixed; Value: TColor32);"
summary: "Draws a clipped vertical line segment at fixed-point coordinates."
parameters:
  - name: X, Y1, Y2
    type: TFixed
    description: "Fixed-point X column, start Y, and end Y coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`VertLineXS` draws a clipped vertical line at 16.16 fixed-point coordinates.
