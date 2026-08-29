---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: ScalePolygonInplace
kind: Function
summary: "Scales polygon coordinates in-place."
overloads:
  - signature: "procedure ScalePolygonInplace(const Points: TArrayOfFloatPoint; ScaleX, ScaleY: TFloat); overload;"
    summary: "Scales floating-point polygon vertices in-place."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices (modified in-place)."
      - name: ScaleX, ScaleY
        type: TFloat
        description: "Scale multipliers."

  - signature: "procedure ScalePolygonInplace(const Points: TArrayOfFixedPoint; ScaleX, ScaleY: TFixed); overload;"
    summary: "Scales fixed-point polygon vertices in-place."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices (modified in-place)."
      - name: ScaleX, ScaleY
        type: TFixed
        description: "Scale multipliers."
---

## Description

`ScalePolygonInplace` multiplies vertex coordinates by `ScaleX` and `ScaleY` directly in the source array.
