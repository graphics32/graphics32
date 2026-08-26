---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.VertLineT
kind: Method
scope: Public
declaration: "procedure VertLineT(X, Y1, Y2: Integer; Value: TColor32);"
summary: "Draws an unclipped alpha-blended vertical line segment at integer coordinates."
parameters:
  - name: X, Y1, Y2
    type: Integer
    description: "X column, start Y, and end Y coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`VertLineT` draws an unclipped vertical line using active blending mode rules.
