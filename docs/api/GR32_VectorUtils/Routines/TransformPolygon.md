---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: TransformPolygon
kind: Function
summary: "Applies a geometric TTransformation matrix to polygon coordinates."
overloads:
  - signature: "function TransformPolygon(const Points: TArrayOfFloatPoint; Transformation: TTransformation): TArrayOfFloatPoint; overload;"
    summary: "Applies Transformation to a floating-point polygon."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polygon vertices."
      - name: Transformation
        type: TTransformation
        description: "Geometric transformation instance."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function TransformPolygon(const Points: TArrayOfFixedPoint; Transformation: TTransformation): TArrayOfFixedPoint; overload;"
    summary: "Applies Transformation to a fixed-point polygon."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polygon vertices."
      - name: Transformation
        type: TTransformation
        description: "Geometric transformation instance."

    returns:
      - type: TArrayOfFixedPoint
        description: "A [[TArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`TransformPolygon` applies the transformation matrix `Transformation` to all vertices in `Points`.
