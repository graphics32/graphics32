---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: TranslatePolygonInplace
kind: Function
summary: "Translates polygon coordinates in-place."
overloads:
  - signature: "procedure TranslatePolygonInplace(const Points: TArrayOfFloatPoint; OffsetX, OffsetY: TFloat); overload;"
    summary: "Translates floating-point polygon vertices in-place."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices (modified in-place)."
      - name: OffsetX, OffsetY
        type: TFloat
        description: "Offsets."

  - signature: "procedure TranslatePolygonInplace(const Points: TArrayOfFixedPoint; Offsetx, OffsetY: TFixed); overload;"
    summary: "Translates fixed-point polygon vertices in-place."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices (modified in-place)."
      - name: Offsetx, OffsetY
        type: TFixed
        description: "Offsets."
---

## Description

`TranslatePolygonInplace` adds `OffsetX` and `OffsetY` directly to vertex coordinates in `Points`.
