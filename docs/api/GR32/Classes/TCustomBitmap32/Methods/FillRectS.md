---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.FillRectS
kind: Method
scope: Public
summary: "Fills a clipped rectangular area with an opaque TColor32 value."
overloads:
  - signature: "procedure FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;"
    summary: "Fills rectangle specified by integer coordinates."
    parameters:
      - name: X1, Y1, X2, Y2
        type: Integer
        description: "Rectangle corner coordinates."
      - name: Value
        type: TColor32
        description: "32-bit ARGB color."
  - signature: "procedure FillRectS(const ARect: TRect; Value: TColor32); overload;"
    summary: "Fills rectangle specified by TRect structure."
    parameters:
      - name: ARect
        type: TRect
        description: "Target TRect."
      - name: Value
        type: TColor32
        description: "32-bit ARGB color."
---

## Description

`FillRectS` fills a rectangle clipped against `ClipRect`.
