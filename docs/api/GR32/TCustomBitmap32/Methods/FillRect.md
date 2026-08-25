---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.FillRect
kind: Method
scope: Public
declaration: "procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);"
summary: "Fills an unclipped rectangular area with an opaque TColor32 value."
parameters:
  - name: X1, Y1, X2, Y2
    type: Integer
    description: "Rectangle corner coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`FillRect` fills the rectangle `(X1, Y1, X2, Y2)` directly without boundary checks or blending.
