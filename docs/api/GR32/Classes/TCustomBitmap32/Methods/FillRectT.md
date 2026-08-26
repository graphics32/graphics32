---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.FillRectT
kind: Method
scope: Public
declaration: "procedure FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);"
summary: "Fills an unclipped rectangular area using alpha blending."
parameters:
  - name: X1, Y1, X2, Y2
    type: Integer
    description: "Rectangle corner coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`FillRectT` fills a rectangle using active `DrawMode` and `CombineMode` blending rules.
