---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.FrameRectTS
kind: Method
scope: Public
summary: "Draws a 1-pixel wide clipped alpha-blended rectangular frame."
overloads:
  - signature: "procedure FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;"
    summary: "Draws alpha-blended frame specified by integer coordinates."
    parameters:
      - name: X1, Y1, X2, Y2
        type: Integer
        description: "Rectangle corner coordinates."
      - name: Value
        type: TColor32
        description: "32-bit ARGB color."
  - signature: "procedure FrameRectTS(const ARect: TRect; Value: TColor32); overload;"
    summary: "Draws alpha-blended frame specified by TRect."
    parameters:
      - name: ARect
        type: TRect
        description: "Target TRect."
      - name: Value
        type: TColor32
        description: "32-bit ARGB color."
---

## Description

`FrameRectTS` draws an alpha-blended 1-pixel outline frame.
