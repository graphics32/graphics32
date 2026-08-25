---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineFP
kind: Method
scope: Public
declaration: "procedure LineFP(X1, Y1, X2, Y2: Single; L: Boolean = False);"
summary: "Draws an unclipped stippled line segment at floating-point coordinates."
parameters:
  - name: X1, Y1, X2, Y2
    type: Single
    description: "Single-precision floating-point start and end coordinates."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel."
---

## Description

`LineFP` draws an unclipped stippled line using floating-point coordinates.
