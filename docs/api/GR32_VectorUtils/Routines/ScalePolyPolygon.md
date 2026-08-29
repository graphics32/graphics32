---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: ScalePolyPolygon
kind: Function
summary: "Scales multi-contour polygon coordinates."
overloads:
  - signature: "function ScalePolyPolygon(const Points: TArrayOfArrayOfFloatPoint; ScaleX, ScaleY: TFloat): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Returns a scaled copy of a multi-contour floating-point polygon."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Polygon contours."
      - name: ScaleX, ScaleY
        type: TFloat
        description: "Scale multipliers."

  - signature: "function ScalePolyPolygon(const Points: TArrayOfArrayOfFixedPoint; ScaleX, ScaleY: TFixed): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Returns a scaled copy of a multi-contour fixed-point polygon."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours."
      - name: ScaleX, ScaleY
        type: TFixed
        description: "Scale multipliers."
---

## Description

`ScalePolyPolygon` multiplies vertex coordinates across all contours in `Points` by `ScaleX` and `ScaleY`.
