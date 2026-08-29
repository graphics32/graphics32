---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: TransformPolyPolygon
kind: Function
summary: "Applies a geometric TTransformation matrix to multi-contour polygon coordinates."
overloads:
  - signature: "function TransformPolyPolygon(const Points: TArrayOfArrayOfFloatPoint; Transformation: TTransformation): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Applies Transformation to multi-contour floating-point polygons."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Polygon contours."
      - name: Transformation
        type: TTransformation
        description: "Geometric transformation instance."

  - signature: "function TransformPolyPolygon(const Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Applies Transformation to multi-contour fixed-point polygons."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFixedPoint
        description: "Fixed-point polygon contours."
      - name: Transformation
        type: TTransformation
        description: "Geometric transformation instance."
---

## Description

`TransformPolyPolygon` applies `Transformation` across all contours in `Points`.
