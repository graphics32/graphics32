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

    returns:
      - type: TArrayOfFixedPoint
        description: "A [[TArrayOfFixedPoint]] array containing generated polygon coordinates."
  - signature: "function PointToFixedPoint(const Points: TArrayOfArrayOfPoint): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Converts a multi-contour TArrayOfArrayOfPoint to TArrayOfArrayOfFixedPoint."
    parameters:
      - name: Points
        type: TArrayOfArrayOfPoint
        description: "Multi-contour integer point array."

    returns:
      - type: TArrayOfArrayOfFixedPoint
        description: "A [[TArrayOfArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`PointToFixedPoint` converts integer `TPoint` arrays into 16.16 fixed-point `TFixedPoint` coordinate arrays.
