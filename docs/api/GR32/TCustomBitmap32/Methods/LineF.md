---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineF
kind: Method
scope: Public
declaration: "procedure LineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False);"
summary: "Draws an unclipped line segment at floating-point coordinates."
parameters:
  - name: X1, Y1, X2, Y2
    type: Single
    description: "Single-precision floating-point start and end coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel."
---

## Description

`LineF` draws an unclipped line segment specified using single-precision floating-point values.
