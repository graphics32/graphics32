---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineAS
kind: Method
scope: Public
declaration: "procedure LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);"
summary: "Draws a clipped anti-aliased line segment between two integer points."
parameters:
  - name: X1, Y1, X2, Y2
    type: Integer
    description: "Start and end pixel coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel."
---

## Description

`LineAS` renders an anti-aliased line segment clipped against `ClipRect`.
