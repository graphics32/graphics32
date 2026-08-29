---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: PointToFixedPoint
kind: Function
summary: "Converts integer TPoint arrays to fixed-point TFixedPoint vertex arrays."
overloads:
  - signature: "function PointToFixedPoint(const Points: TArrayOfPoint): TArrayOfFixedPoint; overload;"
    summary: "Converts a TArrayOfPoint to TArrayOfFixedPoint."
    parameters:
      - name: Points
        type: TArrayOfPoint
        description: "Integer point array."

  - signature: "function PointToFixedPoint(const Points: TArrayOfArrayOfPoint): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Converts a multi-contour TArrayOfArrayOfPoint to TArrayOfArrayOfFixedPoint."
    parameters:
      - name: Points
        type: TArrayOfArrayOfPoint
        description: "Multi-contour integer point array."
---

## Description

`PointToFixedPoint` converts integer `TPoint` arrays into 16.16 fixed-point `TFixedPoint` coordinate arrays.
