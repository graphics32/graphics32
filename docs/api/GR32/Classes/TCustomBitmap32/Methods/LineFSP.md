---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineFSP
kind: Method
scope: Public
declaration: "procedure LineFSP(X1, Y1, X2, Y2: Single; L: Boolean = False);"
summary: "Draws a clipped stippled line segment at floating-point coordinates."
parameters:
  - name: X1, Y1, X2, Y2
    type: Single
    description: "Single-precision floating-point start and end coordinates."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel."
---

## Description

`LineFSP` draws a clipped stippled line using floating-point coordinates.
