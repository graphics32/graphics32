---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: Color32FloatPoint
kind: Function
summary: "Constructs and initializes a TColor32FloatPoint record from a point coordinate and color."
overloads:
  - signature: "function Color32FloatPoint(Color: TColor32; Point: TFloatPoint): TColor32FloatPoint; overload;"
    summary: "Creates a TColor32FloatPoint record from a TColor32 color and TFloatPoint record."
    parameters:
      - name: Color
        type: TColor32
        description: "32-bit ARGB color value."
      - name: Point
        type: TFloatPoint
        description: "2D floating-point position point."
  - signature: "function Color32FloatPoint(Color: TColor32; X, Y: TFloat): TColor32FloatPoint; overload;"
    summary: "Creates a TColor32FloatPoint record from a TColor32 color and scalar X, Y float coordinates."
    parameters:
      - name: Color
        type: TColor32
        description: "32-bit ARGB color value."
      - name: X, Y
        type: TFloat
        description: "Horizontal and vertical floating-point coordinates."
---

## Description

`Color32FloatPoint` is a convenience constructor routine for creating [[TColor32FloatPoint]] records inline without requiring multi-step variable initialization.

## See Also
- [[TColor32FloatPoint]]
