---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: FloatPointToFixedPoint
kind: Function
summary: "Converts floating-point vertex arrays to fixed-point vertex arrays."
overloads:
  - signature: "function FloatPointToFixedPoint(const Points: TArrayOfFloatPoint): TArrayOfFixedPoint; overload;"
    summary: "Converts a TArrayOfFloatPoint to TArrayOfFixedPoint."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Floating-point vertices."

  - signature: "function FloatPointToFixedPoint(const Points: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Converts a multi-contour TArrayOfArrayOfFloatPoint to TArrayOfArrayOfFixedPoint."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Floating-point polygon contours."
---

## Description

`FloatPointToFixedPoint` converts floating-point coordinate arrays into 16.16 fixed-point coordinate arrays.
