---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.HorzLineXS
kind: Method
scope: Public
declaration: "procedure HorzLineXS(X1, Y, X2: TFixed; Value: TColor32);"
summary: "Draws a clipped horizontal line segment at fixed-point coordinates."
parameters:
  - name: X1, Y, X2
    type: TFixed
    description: "Fixed-point start X, Y row, and end X coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`HorzLineXS` draws a clipped horizontal line at 16.16 fixed-point coordinates.
