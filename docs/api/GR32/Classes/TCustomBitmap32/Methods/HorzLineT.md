---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.HorzLineT
kind: Method
scope: Public
declaration: "procedure HorzLineT(X1, Y, X2: Integer; Value: TColor32);"
summary: "Draws an unclipped alpha-blended horizontal line segment at integer coordinates."
parameters:
  - name: X1, Y, X2
    type: Integer
    description: "Start X, Y row, and end X coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`HorzLineT` draws an unclipped horizontal line using active blending mode rules.
