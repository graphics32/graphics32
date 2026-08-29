---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: PointToFloatPoint
kind: Function
summary: "Converts integer TPoint arrays to floating-point TFloatPoint vertex arrays."
overloads:
  - signature: "function PointToFloatPoint(const Points: TArrayOfPoint): TArrayOfFloatPoint; overload;"
    summary: "Converts a TArrayOfPoint to TArrayOfFloatPoint."
    parameters:
      - name: Points
        type: TArrayOfPoint
        description: "Integer point array."

  - signature: "function PointToFloatPoint(const Points: TArrayOfArrayOfPoint): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Converts a multi-contour TArrayOfArrayOfPoint to TArrayOfArrayOfFloatPoint."
    parameters:
      - name: Points
        type: TArrayOfArrayOfPoint
        description: "Multi-contour integer point array."
---

## Description

`PointToFloatPoint` converts integer `TPoint` arrays into floating-point `TFloatPoint` coordinate arrays.
