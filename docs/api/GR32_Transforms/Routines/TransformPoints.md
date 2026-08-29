---
layout: doc
docType: api
unit: GR32_Transforms
entity: TransformPoints
kind: Function
declaration: "function TransformPoints(Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation): TArrayOfArrayOfFixedPoint;"
summary: "Transforms a multi-polygon vertex array using a specified TTransformation."
parameters:
  - name: Points
    type: TArrayOfArrayOfFixedPoint
    description: "Source array of fixed-point polygon contours."
  - name: Transformation
    type: TTransformation
    description: "Transformation object to apply."
---

## Description

`TransformPoints` applies `Transformation` to all fixed-point vertices in `Points` and returns a newly allocated transformed vertex array structure.
