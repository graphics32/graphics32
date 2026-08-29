---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: BuildNormals
kind: Function
summary: "Computes perpendicular unit normal vectors for polygon vertices."
overloads:
  - signature: "function BuildNormals(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint; overload;"
    summary: "Computes floating-point vertex unit normal vectors."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."

  - signature: "function BuildNormals(const Points: TArrayOfFixedPoint): TArrayOfFixedPoint; overload;"
    summary: "Computes fixed-point vertex unit normal vectors."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
---

## Description

`BuildNormals` calculates perpendicular unit normal vectors for each vertex in `Points`, used during polygon inflation (`Grow`).
