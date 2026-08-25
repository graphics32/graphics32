---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineToXS
kind: Method
scope: Public
declaration: "procedure LineToXS(X, Y: TFixed);"
summary: "Draws a clipped line from current fixed-point pen position to (X, Y) and updates pen position."
parameters:
  - name: X, Y
    type: TFixed
    description: "Target 16.16 fixed-point coordinates."
---

## Description

`LineToXS` draws a line from `PenPosF` to fixed-point `(X, Y)`.
