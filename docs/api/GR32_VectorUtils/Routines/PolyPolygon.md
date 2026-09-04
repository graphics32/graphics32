---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: PolyPolygon
kind: Function
summary: "Wraps a single polygon contour into a single-element multi-contour polygon array."
overloads:
  - signature: "function PolyPolygon(const Points: TArrayOfFloatPoint): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Wraps a TArrayOfFloatPoint into a TArrayOfArrayOfFloatPoint."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."

    returns:
      - type: TArrayOfArrayOfFloatPoint
        description: "A [[TArrayOfArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function PolyPolygon(const Points: TArrayOfFixedPoint): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Wraps a TArrayOfFixedPoint into a TArrayOfArrayOfFixedPoint."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."

    returns:
      - type: TArrayOfArrayOfFixedPoint
        description: "A [[TArrayOfArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`PolyPolygon` converts a single polygon vertex array into a multi-contour array with length 1.
