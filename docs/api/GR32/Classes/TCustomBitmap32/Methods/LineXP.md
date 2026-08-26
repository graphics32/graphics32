---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineXP
kind: Method
scope: Public
declaration: "procedure LineXP(X1, Y1, X2, Y2: TFixed; L: Boolean = False);"
summary: "Draws an unclipped stippled line segment at fixed-point coordinates."
parameters:
  - name: X1, Y1, X2, Y2
    type: TFixed
    description: "16.16 fixed-point start and end coordinates."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel."
---

## Description

`LineXP` draws an unclipped stippled line using active stipple pattern settings.
