---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: BuildPolygonF
kind: Function
aliases: [BuildPolygonX]
summary: "Constructs polygon arrays from floating-point or fixed-point coordinate parameter arrays."
overloads:
  - signature: "function BuildPolygonF(const Data: array of TFloat): TArrayOfFloatPoint; overload;"
    summary: "Constructs a TArrayOfFloatPoint from an array of alternating X, Y float values."
    parameters:
      - name: Data
        type: array of TFloat
        description: "Array of alternating X, Y coordinate values."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function BuildPolygonX(const Data: array of TFixed): TArrayOfFixedPoint; overload;"
    summary: "Constructs a TArrayOfFixedPoint from an array of alternating X, Y fixed-point values."
    parameters:
      - name: Data
        type: array of TFixed
        description: "Array of alternating X, Y fixed-point values."

    returns:
      - type: TArrayOfFixedPoint
        description: "A [[TArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`BuildPolygonF` and `BuildPolygonX` convert inline open arrays of alternating X and Y coordinates into structured `TArrayOfFloatPoint` or `TArrayOfFixedPoint` polygon vertex arrays.
