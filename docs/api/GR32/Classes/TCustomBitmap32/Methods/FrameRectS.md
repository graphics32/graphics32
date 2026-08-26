---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.FrameRectS
kind: Method
scope: Public
summary: "Draws a 1-pixel wide clipped rectangular frame."
overloads:
  - signature: "procedure FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;"
    summary: "Draws 1-pixel frame specified by integer coordinates."
    parameters:
      - name: X1, Y1, X2, Y2
        type: Integer
        description: "Rectangle corner coordinates."
      - name: Value
        type: TColor32
        description: "32-bit ARGB color."
  - signature: "procedure FrameRectS(const ARect: TRect; Value: TColor32); overload;"
    summary: "Draws 1-pixel frame specified by TRect."
    parameters:
      - name: ARect
        type: TRect
        description: "Target TRect."
      - name: Value
        type: TColor32
        description: "32-bit ARGB color."
---

## Description

`FrameRectS` draws an opaque 1-pixel outline frame around the specified rectangle.
