---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: ScalePolygon
kind: Function
summary: "Scales polygon coordinates by horizontal and vertical scale factors."
overloads:
  - signature: "function ScalePolygon(const Points: TArrayOfFloatPoint; ScaleX, ScaleY: TFloat): TArrayOfFloatPoint; overload;"
    summary: "Returns a scaled copy of a floating-point polygon."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: ScaleX, ScaleY
        type: TFloat
        description: "Horizontal and vertical scale multipliers."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function ScalePolygon(const Points: TArrayOfFixedPoint; ScaleX, ScaleY: TFixed): TArrayOfFixedPoint; overload;"
    summary: "Returns a scaled copy of a fixed-point polygon."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
      - name: ScaleX, ScaleY
        type: TFixed
        description: "Scale multipliers."

    returns:
      - type: TArrayOfFixedPoint
        description: "A [[TArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`ScalePolygon` multiplies vertex coordinates by `ScaleX` and `ScaleY` and returns a new polygon array.
