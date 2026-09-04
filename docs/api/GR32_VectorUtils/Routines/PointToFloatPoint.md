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

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function PointToFloatPoint(const Points: TArrayOfArrayOfPoint): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Converts a multi-contour TArrayOfArrayOfPoint to TArrayOfArrayOfFloatPoint."
    parameters:
      - name: Points
        type: TArrayOfArrayOfPoint
        description: "Multi-contour integer point array."

    returns:
      - type: TArrayOfArrayOfFloatPoint
        description: "A [[TArrayOfArrayOfFloatPoint]] array containing generated polygon coordinates."
---

## Description

`PointToFloatPoint` converts integer `TPoint` arrays into floating-point `TFloatPoint` coordinate arrays.
