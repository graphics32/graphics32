---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineXSP
kind: Method
scope: Public
declaration: "procedure LineXSP(X1, Y1, X2, Y2: TFixed; L: Boolean = False);"
summary: "Draws a clipped stippled line segment at fixed-point coordinates."
parameters:
  - name: X1, Y1, X2, Y2
    type: TFixed
    description: "16.16 fixed-point start and end coordinates."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel."
---

## Description

`LineXSP` draws a clipped stippled line using fixed-point coordinates.
