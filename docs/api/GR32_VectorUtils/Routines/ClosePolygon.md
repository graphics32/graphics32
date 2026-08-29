---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: ClosePolygon
kind: Function
summary: "Ensures a polygon vertex array is explicitly closed by appending the start point if necessary."
overloads:
  - signature: "function ClosePolygon(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint; overload;"
    summary: "Closes a floating-point polygon vertex array."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."

  - signature: "function ClosePolygon(const Points: TArrayOfFixedPoint): TArrayOfFixedPoint; overload;"
    summary: "Closes a fixed-point polygon vertex array."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Polygon vertices."
---

## Description

`ClosePolygon` checks whether the final vertex in `Points` equals the first vertex; if they differ, the first vertex is appended to close the contour.
