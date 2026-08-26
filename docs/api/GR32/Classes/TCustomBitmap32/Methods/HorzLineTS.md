---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.HorzLineTS
kind: Method
scope: Public
declaration: "procedure HorzLineTS(X1, Y, X2: Integer; Value: TColor32);"
summary: "Draws a clipped alpha-blended horizontal line segment at integer coordinates."
parameters:
  - name: X1, Y, X2
    type: Integer
    description: "Start X, Y row, and end X coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`HorzLineTS` draws a clipped horizontal line using active blending mode rules.
