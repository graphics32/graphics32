---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineToXSP
kind: Method
scope: Public
declaration: "procedure LineToXSP(X, Y: TFixed);"
summary: "Draws a clipped stippled line from current fixed-point pen position to (X, Y)."
parameters:
  - name: X, Y
    type: TFixed
    description: "Target 16.16 fixed-point coordinates."
---

## Description

`LineToXSP` draws a stippled line from `PenPosF` to fixed-point `(X, Y)`.
