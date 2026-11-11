---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: VertexReduction
kind: Function
summary: "Removes duplicate or near-collinear vertices within an epsilon distance threshold."
overloads:
  - signature: "function VertexReduction(const Points: TArrayOfFloatPoint; Epsilon: TFloat = 1): TArrayOfFloatPoint; overload;"
    summary: "Simplifies a floating-point polygon by removing vertices closer than Epsilon distance."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: Epsilon
        type: TFloat
        description: "Minimal distance threshold."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function VertexReduction(const Points: TArrayOfFixedPoint; Epsilon: TFixed = FixedOne): TArrayOfFixedPoint; overload;"
    summary: "Simplifies a fixed-point polygon by removing vertices closer than Epsilon distance."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
      - name: Epsilon
        type: TFixed
        description: "Minimal distance threshold."

    returns:
      - type: TArrayOfFixedPoint
        description: "A [[TArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`VertexReduction` simplifies polygon contours by eliminating redundant or near-duplicate consecutive vertices that lie closer than `Epsilon` distance.
