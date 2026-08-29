---
layout: doc
docType: api
unit: GR32_Geometry
entity: OffsetRect
kind: Function
summary: "Translates a rectangle structure by X and Y coordinate offsets."
overloads:
  - signature: "function OffsetRect(const Rct: TFloatRect; const DeltaX, DeltaY: TFloat): TFloatRect; overload;"
    summary: "Translates TFloatRect by scalar DeltaX and DeltaY floating-point offsets."
    parameters:
      - name: Rct
        type: TFloatRect
        description: "Rectangle to offset."
      - name: DeltaX, DeltaY
        type: TFloat
        description: "Displacement along X and Y axes."
  - signature: "function OffsetRect(const Rct: TFloatRect; const Delta: TFloatPoint): TFloatRect; overload;"
    summary: "Translates TFloatRect by TFloatPoint displacement vector."
    parameters:
      - name: Rct
        type: TFloatRect
        description: "Rectangle to offset."
      - name: Delta
        type: TFloatPoint
        description: "Displacement vector."
  - signature: "function OffsetRect(const Rct: TFixedRect; const DeltaX, DeltaY: TFixed): TFixedRect; overload;"
    summary: "Translates TFixedRect by fixed-point DeltaX and DeltaY offsets."
    parameters:
      - name: Rct
        type: TFixedRect
        description: "Rectangle to offset."
      - name: DeltaX, DeltaY
        type: TFixed
        description: "Fixed-point displacement."
  - signature: "function OffsetRect(const Rct: TFixedRect; const DeltaX, DeltaY: TFloat): TFixedRect; overload;"
    summary: "Translates TFixedRect by floating-point DeltaX and DeltaY offsets."
    parameters:
      - name: Rct
        type: TFixedRect
        description: "Rectangle to offset."
      - name: DeltaX, DeltaY
        type: TFloat
        description: "Floating-point displacement."
  - signature: "function OffsetRect(const Rct: TFixedRect; const Delta: TFixedPoint): TFixedRect; overload;"
    summary: "Translates TFixedRect by TFixedPoint displacement vector."
    parameters:
      - name: Rct
        type: TFixedRect
        description: "Rectangle to offset."
      - name: Delta
        type: TFixedPoint
        description: "Fixed-point displacement vector."
  - signature: "function OffsetRect(const Rct: TFixedRect; const Delta: TFloatPoint): TFixedRect; overload;"
    summary: "Translates TFixedRect by TFloatPoint displacement vector."
    parameters:
      - name: Rct
        type: TFixedRect
        description: "Rectangle to offset."
      - name: Delta
        type: TFloatPoint
        description: "Floating-point displacement vector."
---

## Description

`OffsetRect` shifts both top-left and bottom-right corners of rectangle `Rct` by the specified displacement offsets.
