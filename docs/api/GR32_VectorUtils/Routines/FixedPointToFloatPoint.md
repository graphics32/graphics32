---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: FixedPointToFloatPoint
kind: Function
summary: "Converts fixed-point vertex arrays to floating-point vertex arrays."
overloads:
  - signature: "function FixedPointToFloatPoint(const Points: TArrayOfFixedPoint): TArrayOfFloatPoint; overload;"
    summary: "Converts a TArrayOfFixedPoint to TArrayOfFloatPoint."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point vertices."

  - signature: "function FixedPointToFloatPoint(const Points: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Converts a multi-contour TArrayOfArrayOfFixedPoint to TArrayOfArrayOfFloatPoint."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours."
---

## Description

`FixedPointToFloatPoint` converts 16.16 fixed-point coordinate arrays into single-precision floating-point coordinate arrays.
