---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: TranslatePolygon
kind: Function
summary: "Translates polygon coordinates by horizontal and vertical offsets."
overloads:
  - signature: "function TranslatePolygon(const Points: TArrayOfFloatPoint; OffsetX, OffsetY: TFloat): TArrayOfFloatPoint; overload;"
    summary: "Translates a floating-point polygon by (OffsetX, OffsetY)."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: OffsetX, OffsetY
        type: TFloat
        description: "Horizontal and vertical offsets."

  - signature: "function TranslatePolygon(const Points: TArrayOfFixedPoint; Offsetx, OffsetY: TFixed): TArrayOfFixedPoint; overload;"
    summary: "Translates a fixed-point polygon by (OffsetX, OffsetY)."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
      - name: Offsetx, OffsetY
        type: TFixed
        description: "Horizontal and vertical offsets."
---

## Description

`TranslatePolygon` adds `OffsetX` and `OffsetY` to all vertex coordinates in `Points`.
