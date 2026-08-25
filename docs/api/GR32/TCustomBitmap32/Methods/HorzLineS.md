---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.HorzLineS
kind: Method
scope: Public
declaration: "procedure HorzLineS(X1, Y, X2: Integer; Value: TColor32);"
summary: "Draws a clipped opaque horizontal line segment at integer coordinates."
parameters:
  - name: X1, Y, X2
    type: Integer
    description: "Start X, Y row, and end X coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`HorzLineS` draws a horizontal line segment clipped against `ClipRect`.
